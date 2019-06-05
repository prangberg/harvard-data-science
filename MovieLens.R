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

edx_split_genres  <- edx  %>% separate_rows(genres, sep = "\\|")
valid_split_genres <- validation  %>% mutate(year = as.numeric(str_sub(validation$title,-5,-2))) %>% separate_rows(genres, sep = "\\|")

# Extract the Year from the Title in both data sets
edx <- edx %>% extract(title, c("title_name", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = T) %>% mutate(year=as.integer(year))
validation <- validation %>% extract(title, c("title_name", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = T) %>% mutate(year=as.integer(year))


genres_edx  <- edx  %>% separate_rows(genres, sep = "\\|")
genres_valid <- validation  %>% mutate(year = as.numeric(str_sub(validation$title,-5,-2))) %>% separate_rows(genres, sep = "\\|")

#Count number pf distinct users and movies 
edx %>%   summarize(n_users = n_distinct(userId),n_movies = n_distinct(movieId))

#Number of Ratings per Movie
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 50, color = "blue") + 
  scale_x_log10() + 
  ggtitle("Number of Ratings per Movie")

#Number of Ratings per user
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 50, color = "blue") + 
  scale_x_log10() + 
  ggtitle("Users")

#Movies per year
edx %>%   count(year) %>%  ggplot (aes(x=year, y=n)) +geom_line(color="blue")

#Average Rating per year
edx %>% group_by(year) %>% summarize(mean_rating = mean(rating)) %>% ggplot(aes(x=year, y=mean_rating)) +geom_line(color="blue")

edx %>% group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  geom_point() +
  geom_smooth()
#FORMAT WEXELN

#Average Rating per user
edx %>% group_by(userId) %>% summarize(mean_rating = mean(rating)) %>% ggplot(aes(x=userId, y=mean_rating)) +geom_line(color="blue")
## Graph not meaningful!



#Ratings per genre
edx_split_genres %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 0.5*se, ymax = avg + 0.5*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Calculate RMSE for true ratings + predicted ratings

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#MODEL 1 - - Overall average for all movies
avg <- mean(edx$rating)
avg

Basic_Model_rmse <-RMSE(validation$rating, avg)
Basic_Model_rmse

# Determine mean rating for each individual movie
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(m_avg = mean(rating))
movie_avgs %>% qplot(m_avg, geom ="histogram", bins = 20, data = ., color = I("blue"))

# Movie Effect
ImpactMovie <- edx %>% 
  group_by(movieId) %>%
  summarize(ImpactMovie = sum(rating - avg)/(n()+2))

ImpactMovie
ImpactMovie %>% qplot(ImpactMovie, geom ="histogram", bins = 40, data = ., color = I("blue"))

#MODEL 2 - - Add Movie Effect to model

prediction_MovieEffect <- validation %>% 
  left_join(ImpactMovie, by='movieId') %>%
  mutate(pred = avg + ImpactMovie) 

model_2_rmse <- RMSE(validation$rating,prediction_MovieEffect$pred)
model_2_rmse

# User Effect
ImpactUser <- edx %>% 
  group_by(userId) %>%
  summarize(ImpactUser = sum(rating - avg)/(n()+2))

ImpactUser
ImpactUser %>% qplot(ImpactUser, geom ="histogram", bins = 40, data = ., color = I("blue"))

#MODEL 3 - - Add User Effect to model

prediction_UserEffect <- validation %>% 
  left_join(ImpactUser, by='userId') %>%
  mutate(pred = avg + ImpactUser) 

model_3_rmse <- RMSE(validation$rating,prediction_UserEffect$pred)
model_3_rmse


#MODEL 4 - -  User Effect AND Movie Effect

prediction_UserMovieEffect <- validation %>% 
  left_join(ImpactUser, by='userId') %>%
  left_join(ImpactMovie, by='movieId') %>%
  mutate(pred = avg + ImpactUser + ImpactMovie) 

model_4_rmse <- RMSE(validation$rating,prediction_UserMovieEffect$pred)
model_4_rmse



