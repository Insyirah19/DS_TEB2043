############################################################
# PERSON 4 – MODEL DEVELOPER
# ITEM-BASED COLLABORATIVE FILTERING
# COSINE SIMILARITY
############################################################

# Install packages (run once)
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("proxy")
# install.packages("tibble")

# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(proxy)
library(tibble)

############################################################
# 1) LOAD CLEANED DATASET
############################################################

data <- read_excel("C:/Users/insyi/Downloads/cleaned_movielens_data.xlsx")
head(data)

############################################################
# 2️) SELECT IMPORTANT COLUMNS
############################################################

rating_data <- data %>%
  select(user_id, item_id, rating)

head(rating_data)

############################################################
# 3️) CALCULATE USER MEAN RATINGS
############################################################

user_means <- rating_data %>%
  group_by(user_id) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE))

############################################################
# 4️) NORMALIZE RATINGS
############################################################

normalized_data <- rating_data %>%
  left_join(user_means, by = "user_id") %>%
  mutate(normalized_rating = rating - mean_rating) # NA remains NA

############################################################
# 5️) CREATE NORMALIZED USER-ITEM MATRIX
############################################################

normalized_user_item_matrix <- normalized_data %>%
  select(user_id, item_id, normalized_rating) %>%
  pivot_wider(
    names_from = item_id,
    values_from = normalized_rating
  ) %>%
  column_to_rownames(var = "user_id")  # user_id becomes rownames

############################################################
# 6️) CONVERT TO MATRIX
############################################################

matrix_data <- as.matrix(normalized_user_item_matrix)

############################################################
# 7️) COMPUTE MOVIE SIMILARITY (COSINE)
############################################################

movie_similarity <- simil(
  t(matrix_data),                  # transpose so movies are rows
  method = "cosine",
  use = "pairwise.complete.obs"    # handle missing ratings
)

movie_similarity_matrix <- as.matrix(movie_similarity)

############################################################
# 8️) RECOMMENDATION FUNCTION
############################################################

recommend_movies <- function(movie_id, similarity_matrix, top_n = 5) {
  similarity_scores <- similarity_matrix[movie_id, ]
  sorted_scores <- sort(similarity_scores, decreasing = TRUE)
  recommended_movies <- names(sorted_scores)[2:(top_n + 1)]  # skip self
  return(recommended_movies)
}

############################################################
# 9️) CREATE MOVIE LOOKUP TABLE
############################################################

movie_lookup <- data %>%
  select(item_id, movie_title) %>%
  distinct()

############################################################
# 10) EXAMPLE: TOP 5 RECOMMENDATIONS FOR MOVIE_ID = 50
############################################################

recommended_ids <- recommend_movies("50", movie_similarity_matrix, top_n = 5)

recommended_movies <- movie_lookup %>%
  filter(item_id %in% as.numeric(recommended_ids))

recommended_movies

############################################################
# 11) OPTIONAL: SHOW TOP 10 SIMILAR MOVIES
############################################################

similarity_scores <- movie_similarity_matrix["50", ]
sort(similarity_scores, decreasing = TRUE)[1:10]
