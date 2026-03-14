# Install packages (run once)
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("proxy")
install.packages("tibble")

# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(proxy)
library(tibble)

############################################################
# 1 LOAD CLEANED DATASET
############################################################

data <- read_excel("C:/Users/insyi/Downloads/cleaned_movielens_data.xlsx")

head(data)

############################################################
# 2 SELECT IMPORTANT COLUMNS
############################################################

rating_data <- data %>%
  select(user_id, item_id, rating)

head(rating_data)

############################################################
# 3 CREATE USER ITEM MATRIX
############################################################

user_item_matrix <- rating_data %>%
  pivot_wider(
    names_from = item_id,
    values_from = rating,
    values_fill = 0
  )

user_item_matrix <- user_item_matrix %>%
  column_to_rownames(var = "user_id")

head(user_item_matrix)

############################################################
# 4 NORMALIZE RATINGS (REMOVE USER BIAS)
############################################################

user_means <- rating_data %>%
  group_by(user_id) %>%
  filter(rating > 0) %>%
  summarise(mean_rating = mean(rating))

normalized_data <- rating_data %>%
  left_join(user_means, by = "user_id") %>%
  mutate(normalized_rating =
           ifelse(rating > 0,
                  rating - mean_rating,
                  0))

############################################################
# 5 CREATE NORMALIZED USER ITEM MATRIX
############################################################

normalized_user_item_matrix <- normalized_data %>%
  select(user_id, item_id, normalized_rating) %>%
  pivot_wider(
    names_from = item_id,
    values_from = normalized_rating,
    values_fill = 0
  ) %>%
  column_to_rownames(var = "user_id")

head(normalized_user_item_matrix)

############################################################
# 6 CONVERT TO MATRIX
############################################################

matrix_data <- as.matrix(normalized_user_item_matrix)

############################################################
# 7 CALCULATE MOVIE SIMILARITY (COSINE)
############################################################

movie_similarity <- simil(
  t(matrix_data),
  method = "cosine"
)

movie_similarity_matrix <- as.matrix(movie_similarity)

############################################################
# 8 VIEW SIMILARITY MATRIX
############################################################

movie_similarity_matrix[1:10,1:10]

############################################################
# 9 RECOMMENDATION FUNCTION
############################################################

recommend_movies <- function(movie_id,
                             similarity_matrix,
                             top_n = 5){
  
  similarity_scores <- similarity_matrix[movie_id, ]
  
  sorted_scores <- sort(similarity_scores,
                        decreasing = TRUE)
  
  recommended_movies <- names(sorted_scores)[2:(top_n + 1)]
  
  return(recommended_movies)
}

############################################################
# 10 GENERATE TOP 5 RECOMMENDATIONS
############################################################

recommend_movies(50, movie_similarity_matrix)

############################################################
# 11 OPTIONAL – SHOW TOP 10 SIMILAR MOVIES
############################################################

similarity_scores <- movie_similarity_matrix[50, ]

sort(similarity_scores,
     decreasing = TRUE)[1:10]

############################################################
# 12 CONVERT MOVIE IDS TO MOVIE TITLES
############################################################

# Create movie lookup table
movie_lookup <- data %>%
  select(item_id, movie_title) %>%
  distinct()

############################################################
# 13 GET RECOMMENDED MOVIE TITLES
############################################################

recommended_ids <- recommend_movies(50, movie_similarity_matrix)

recommended_movies <- movie_lookup %>%
  filter(item_id %in% as.numeric(recommended_ids))

recommended_movies

