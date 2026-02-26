library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(digest)
library(rlang)

#####################
# READ IN DATA
#####################

# Pearson
pearson <- bind_rows(
  list.files("./raw_data/pearson/fall_24", "\\.csv$", full.names = TRUE) %>%
    map_dfr( ~ read.csv(.x) %>% mutate(semester = "Fall 24")),
  list.files("./raw_data/pearson/spring_25", "\\.csv$", full.names = TRUE) %>%
    map_dfr( ~ read.csv(.x) %>% mutate(semester = "Spring 25"))) %>% 
  select(-External.StudentId, -Student.Tags)

# SIS
#sis_cols <- read_excel("./raw_data/sis/SIS_cols_to_keep.xlsx", 1)%>% as.list()
sis_spring <- read.csv("./raw_data/sis/125_1251.csv") %>%
  mutate(Term = as.integer(Term),
         Section = as.integer(Section))

sis_fall <- read_excel("./raw_data/sis/125_F24.xlsx", 2)%>%
  mutate(Term = as.integer(Term),
         Section = as.integer(Section))

sis_all <- full_join(sis_spring, sis_fall,
                     by = c("Term", "Class", "Section",
                           "First.Name" = "First Name",
                           "Last.Name" = "Last Name" ))%>%
  mutate("section_code" = paste(Term, Class.Nbr, sep = "-"),
         `First Term Enrolled Cd Ugrd` = as.integer(`First Term Enrolled Cd Ugrd`),
         `Last Term Enrolled` = as.integer(`Last Term Enrolled`))


# Canvas
canvas_files_list_spring <- list.files("./raw_data/canvas",
                                       pattern = "^2025-05-13T1.*\\.csv$",
                                       full.names = TRUE)

canvas_files_list_fall <- list.files("./raw_data/canvas",
                                     pattern =  "^.*2024.*\\.csv$",
                                     full.names = TRUE)
canvas <- data.frame()

for (i in 1:length(canvas_files_list_fall)) {
  temp_data <- read.csv(canvas_files_list_fall[i]) %>%
    mutate(SIS.Login.ID = as.character(SIS.Login.ID))
  canvas <- bind_rows(canvas, temp_data)
}

for (i in 1:length(canvas_files_list_spring)){
  temp_data <- read.csv(canvas_files_list_spring[i]) %>%
    mutate(SIS.Login.ID = as.character(SIS.Login.ID))
  canvas <- bind_rows(canvas, temp_data)
}

#####################
# CREATING HASH
#####################

# Adding section IDs
pearson <- pearson %>%
  mutate(section_code = str_extract(Course, "\\b\\d{4}-\\d{4,5}\\b")) %>%
  mutate(ID_string = paste(First.Name, Last.Name, section_code, sep = ""))%>%
  mutate(Term = as.integer(str_extract(section_code, "\\b\\d{4}")))

# Could leave this messy with 'Points Possible' and 'Test Student' still in the data
canvas <- canvas %>%
  mutate(section_code = str_extract(Section, "\\b\\d{4}-\\d{4,5}\\b")) %>%
  separate(Student,
           into = c("Last.Name", "First.Name"),
           sep = ",\\s*", # comma + optional space
           remove = TRUE) %>%
  filter(!is.na(First.Name),
         First.Name != "Test") %>%
  mutate(ID_string = paste(First.Name, Last.Name, section_code, sep = "")) %>%
  mutate(Term = as.integer(str_extract(section_code, "\\d{4}")))

# Create Hashed Identifier
# additional string so hashes cannot be reverse engineered by those with the data
salt <- "LMC_stuff"
index_df <- sis_all %>%
  select(First.Name, Last.Name, section_code, X.Emplid, Term) %>%
  mutate(hash_string = paste(First.Name, Last.Name, X.Emplid, sep = "")) %>%
  # Keep only one row per student
  distinct(hash_string, .keep_all = TRUE)%>%
  mutate(hashed_id = vapply(paste0(hash_string, salt), digest, character(1), algo = "sha256")) %>%
  mutate(ID_string = paste(First.Name, Last.Name, section_code, sep = ""))

canvas_hashed <- left_join(canvas,
     index_df,
     by = c(
       "Last.Name" = "Last.Name",
       "First.Name" = "First.Name",
       "Term" = "Term"
     ),
     relationship = "many-to-one"
)

# Select students without a hashed ID based on emplid 
# and give one based on ID_string
no_hashed_id_df <- canvas_hashed %>% filter(is.na(hash_string)) %>%
  mutate(hash_string = paste(First.Name, Last.Name, SIS.User.ID, sep = "")) %>%
  # Keep only one row per student
  distinct(hash_string, .keep_all = TRUE)%>%
  mutate(hashed_id = vapply(paste0(hash_string, salt), digest, character(1), algo = "sha256")) %>%
  rename(ID_string = ID_string.x) %>%
  select(First.Name, Last.Name, X.Emplid, hash_string, hashed_id, ID_string, SIS.User.ID)

index_df <- full_join(index_df, no_hashed_id_df,
                   by = c("hashed_id" = "hashed_id")) %>%
  mutate(Identifier = 1:n()) %>%
  rename("First.Name" = "First.Name.x",
         "Last.Name" = "Last.Name.x")

#####################
# APPLY HASH
#####################

pearson_hashed <- left_join(
  pearson,
  index_df,
  by = c(
    "Last.Name" = "Last.Name",
    "First.Name" = "First.Name",
    "Term" = "Term"
  ),
  relationship = "many-to-one"
)

canvas_hashed <- left_join(canvas,
  index_df,
  by = c(
    "Last.Name" = "Last.Name",
    "First.Name" = "First.Name",
    "Term" = "Term"
  ),
  relationship = "many-to-one"
)

SIS_hashed <- left_join(sis_all,
   index_df,
   by = c(
     "Last.Name" = "Last.Name",
     "First.Name" = "First.Name",
     "Term" = "Term"
   ),
   relationship = "many-to-many")

##########################
# Function to join redundant columns in SIS; From ChatGPT
##########################
# Redundant cols were created in merges above

coalesce_many <- function(data, cols) {
  for (x in cols) {
    data <- data %>%
      mutate(
        !!sym(x$new) := coalesce(
          .data[[x$primary]],
          .data[[x$secondary]]
        )
      )
  }
  data
}


dup_cols <- list(
  
  # Demographics
  list(new = "Age", primary = "Age.x", secondary = "Age.y"),
  list(new = "Sex", primary = "Sex.x", secondary = "Sex.y"),
  
  # Class / Course identifiers
  list(new = "Class.Nbr", primary = "Class.Nbr", secondary = "Class Nbr"),
  
  # Grades
  list(new = "Official.Grade", primary = "Official.Grade", secondary = "Official Grade"),
  
  # Enrollment status
  list(new = "Status.Reason", primary = "Status.Reason", secondary = "Status Reason"),
  
  # Ethnicity
  list(new = "IPEDS.Ethnicity", primary = "IPEDS.Ethnicity", secondary = "IPEDS Ethnicity"),
  
  # First-generation indicator
  list(
    new = "1st.Gen.College.Std.Flag",
    primary = "X1st.Gen.College.Std.Flag",
    secondary = "1st Gen College Std Flag"
  ),
  
  # Academic level
  list(
    new = "Academic.Level.Begin.of.Term",
    primary = "Academic.Level.Begin.of.Term",
    secondary = "Academic Level Begin of Term"
  ),
  
  # GPA
  list(new = "Cum.GPA", primary = "Cum.GPA", secondary = "Cum GPA"),
  
  # Units
  list(new = "Total.Cum.Units", primary = "Total.Cum.Units", secondary = "Total Cum Units"),
  list(new = "Total.NAU.Units", primary = "Total.NAU.Units", secondary = "Total NAU Units"),
  
  # Academic plans
  list(
    new = "Primary.Academic.Plan",
    primary = "Primary.Academic.Plan",
    secondary = "Primary Academic Plan"
  ),
  list(new = "Sub.Plan", primary = "Sub.Plan", secondary = "Sub Plan"),
  
  # Campus information
  list(new = "Student.Campus", primary = "Student.Campus", secondary = "Student Campus"),
  list(new = "Class.Campus.Cd", primary = "Class.Campus.Cd", secondary = "Class Campus Cd"),
  list(new = "Instruction.Mode", primary = "Instruction.Mode", secondary = "Instruction Mode"),
  
  # Enrollment history
  list(
    new = "First.Term.Enrolled.Cd.Ugrd",
    primary = "First.Term.Enrolled.Cd.Ugrd",
    secondary = "First Term Enrolled Cd Ugrd"
  ),
  list(
    new = "Last.Term.Enrolled",
    primary = "Last.Term.Enrolled",
    secondary = "Last Term Enrolled"
  ),
  
  # Home location
  list(new = "Home.Country", primary = "Home.Country", secondary = "Home Country"),
  
  # Section codes
  list(new = "section_code", primary = "section_code.x", secondary = "section_code.y")
)

# Apply function
SIS_coalesced <- coalesce_many(SIS_hashed, dup_cols)

##########################
# REMOVE IDENTIFYING DATA
##########################
pearson <- pearson_hashed %>% select(Identifier,
                                     Status,
                                     Course,       
                                     Course.Code,
                                     Assignment.Title,
                                     Assignment.Category,
                                     Assignment.Tag,
                                     Time.Spent,
                                     Score,
                                     semester,
                                     section_code.x,
                                     Term) %>%
  rename("section_code" = "section_code.x")

# Remove identifying values
canvas <- canvas_hashed %>% select(-First.Name,
                                  -Last.Name,
                                  -ID,
                                  -SIS.Login.ID,
                                  -SIS.User.ID.x,
                                  -SIS.User.ID.y,
                                  -ID_string.x,
                                  -X.Emplid.x,
                                  -X.Emplid.y,
                                  -ID_string,
                                  -hash_string.x,
                                  -hash_string.y,
                                  -hashed_id,
                                  -ID_string.y,
                                  -First.Name.y.y,
                                  -Last.Name.y.y,
                                  -section_code.y) %>%
  # Re-order so identifiers are first
  select(Identifier, Term, section_code.x, everything())

SIS <- SIS_coalesced %>% select(-X.Emplid,
                             -Student.Name,
                             -First.Name,
                             -Last.Name,
                             -NAU.Email.Address,
                             -UID,
                             -Preferred.Phone,
                             -Full.Mail.Address,
                             -Mail.Street.Address,
                             -Mail.Street.Line.2,
                             -Mail.Street.Line.3,
                             -Mail.Street.Line.4,
                             -Home.Street.Address,
                             -Home.Street.Line.2,
                             -Home.Street.Line.3,
                             -Home.Street.Line.4,
                             -Extract.Date,
                             -`*Emplid`,
                             -X.Emplid.x,
                             -hash_string.x,
                             -hashed_id,
                             -ID_string.x,
                             -First.Name.y.y,
                             -Last.Name.y.y,
                             -X.Emplid.y,
                             -hash_string.y,
                             -ID_string.y,
                             -SIS.User.ID,
                             -FERPA.Protected,
                             -Birth.Date,
                             -`Birth Date`,
                             -Mail.Zip.Code,
                             -Age.x, -Age.y,
                             -Sex.x, -Sex.y,
                             -`Class Nbr`,
                             -`Official Grade`,
                             -`Status Cd`, -`Status Reason`,
                             -`IPEDS Ethnicity`,
                             -`1st Gen College Std Flag`,
                             -`Academic Level Begin of Term`,
                             -`Cum GPA`,
                             -`Total Cum Units`,
                             -`Total NAU Units`,
                             -`Primary Academic Plan`,
                             -`Sub Plan`,
                             -`Student Campus`,
                             -`Class Campus Cd`,
                             -`Instruction Mode`,
                             -`First Term Enrolled Cd Ugrd`,
                             -`Last Term Enrolled`,
                             -`Home Country`,
                             -section_code.x, -section_code.y) %>%
  select(Identifier, everything())

# Save as Files
write_csv(index_df, file ="./cleaned_data/index.csv")
write_csv(pearson, file = "./cleaned_data/pearson.csv")
write_csv(canvas, file = "./cleaned_data/canvas.csv")
write_csv(SIS, file = "./cleaned_data/SIS.csv")


