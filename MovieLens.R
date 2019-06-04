# library(dslabs)

# Create edx set and validation set
###################################
if (file.exists("edxData.Rda")) {
  print("Loading Data from File")
  load("edxData.Rda")
} else {
# Note: this process could take a couple of minutes
  print("Loading Data from Source")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

save(edx, validation, file = "edxData.Rda")
}
# Edx set and validation set created
#####



# Explore Data Set
#######################
head(edx)
summary(edx)


# Extract the Year from the Title
edx <- edx %>% extract(title, c("title_name", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = T) %>% mutate(year=as.integer(year))

validation <- validation %>% extract(title, c("title_name", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = T) %>% mutate(year=as.integer(year))
 
edx %>%   summarize(n_users = n_distinct(userId),n_movies = n_distinct(movieId))



#Ratings per Movie
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 50, color = "blue") + 
  scale_x_log10() + 
  ggtitle("Number of Ratings per Movie")


#Ratings per user
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 50, color = "blue") + 
  scale_x_log10() + 
  ggtitle("Users")
#######################




### Calculate RMSE for true ratings + predicted ratings

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}






str(movielens)
str(edx)
table(edx$rating)
table(edx$movieId)
1018+9677
length(unique(edx$movieId))
length(unique(edx$userId))
table(edx$genres)
filter(edx, genres %in% c('War')) 
str_detect(edx$genres, "Drama") %>% sum()
str_detect(edx$genres, "Comedy") %>% sum()
str_detect(edx$genres, "Thriller") %>% sum()
str_detect(edx$genres, "Romance") %>% sum()

table(edx$rating) %>% sort(decreasing = TRUE) %>% head()

?sort
