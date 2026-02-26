
# categorizing Canvas column names

###########
library(dplyr)
library(tidyverse)
library(stringr)
library(text2vec)
library(purrr)
library(irlba)
###########


# Categories
# Function to auto-categorize learning items
categorize_learning_items_auto <- function(names_vector) {
  
  # Extract category: text before first double dot or numeric code
  extract_category <- function(name) {
    # Remove trailing numbers and dots
    name_clean <- str_remove(name, "\\.\\d+\\.$")
    
    # Take everything before ".." as category
    category <- str_split(name_clean, "\\.\\.", simplify = TRUE)[1]
    
    # Replace dots with spaces for readability (optional)
    category <- str_replace_all(category, "\\.", " ")
    
    return(category)
  }
  
  data.frame(
    Original = names_vector,
    Category = sapply(names_vector, extract_category)
  ) %>% arrange(Category)
}

##############################
#Kmeans Clustering - From ChatGPT
##############################
cluster_column_names <- function(
    names,
    max_k = 25,
    top_terms = 3,
    prefix_words = 4,
    svd_dims = 50,
    seed = 42
) {
  # -----------------------------
  # 1. Clean names and normalize numeric/date codes
  # -----------------------------
  clean_names <- names |>
    str_to_lower() |>
    str_replace_all("\\b\\d{1,2}[\\.\\-]\\d{1,2}(\\.\\.|\\.)?\\d{0,4}\\b", "{date}") |>
    str_replace_all("\\b\\d{5,}\\b", "{code}") |>
    str_replace_all("[\\._]", " ") |>
    str_squish()
  
  # Extract first few words as prefix for clustering
  prefixes <- sapply(str_split(clean_names, " "), function(words) {
    paste(head(words, prefix_words), collapse = " ")
  })
  
  # -----------------------------
  # 2. TF-IDF matrix
  # -----------------------------
  it <- itoken(prefixes, progressbar = FALSE)
  dtm <- create_dtm(it, vectorizer = vocab_vectorizer(create_vocabulary(it)))
  dtm_tfidf <- fit_transform(dtm, TfIdf$new())
  
  # -----------------------------
  # 3. Optional dimensionality reduction
  # -----------------------------
  dtm_reduced <- if (svd_dims < ncol(dtm_tfidf)) {
    irlba(dtm_tfidf, nv = svd_dims)$u %*% diag(irlba(dtm_tfidf, nv = svd_dims)$d)
  } else dtm_tfidf
  
  # -----------------------------
  # 4. Determine best k using elbow method
  # -----------------------------
  set.seed(seed)
  wss <- sapply(2:max_k, function(k) kmeans(dtm_reduced, centers = k, nstart = 5)$tot.withinss)
  d2 <- diff(diff(wss))
  best_k <- min(which.min(d2) + 2, max_k)
  
  # -----------------------------
  # 5. KMeans clustering
  # -----------------------------
  km <- kmeans(dtm_reduced, centers = best_k, nstart = 25)
  
  # -----------------------------
  # 6. Cluster labeling (vectorized)
  # -----------------------------
  tfidf_matrix <- as.matrix(dtm_tfidf)
  cluster_means <- rowsum(tfidf_matrix, km$cluster) / as.vector(table(km$cluster))
  cluster_labels <- apply(cluster_means, 1, function(avg) {
    top <- order(avg, decreasing = TRUE)[1:min(top_terms, length(avg))]
    str_to_title(paste(colnames(tfidf_matrix)[top], collapse = "."))
  })
  
  # -----------------------------
  # 7. Return results
  # -----------------------------
  tibble(
    original_name = names,
    clean_name = clean_names,
    prefix = prefixes,
    cluster = km$cluster,
    cluster_label = cluster_labels[km$cluster]
  ) |>
    arrange(cluster)
}

##############################
# IMPLEMENTATION
##############################
canvas_data <- read.csv("./cleaned_data/canvas.csv", nrows = 2)
Canvas_colnames <- as.list(colnames(canvas_data))

#Categories implementation
categorized_cols <- categorize_learning_items_auto(Canvas_colnames)
head(categorized_cols)

# Kmeans implementation
clustered_cols <- cluster_column_names(Canvas_colnames)
head(clustered_cols)




