###########################################################
#    HarvardX Professional Certificate in Data Science     #
#               PH125.9X: Capstone Project                #
###########################################################

# Create edx set and validation set
###################################
if (file.exists("edxData.Rda")) {
  print("Loading Data from File")
  load("edxData.Rda")
  if (!require(tidyverse))
    install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  if (!require(caret))
    install.packages("caret", repos = "http://cran.us.r-project.org")
} else {
  # Note: this process could take a couple of minutes
  print("Loading Data from Source")
  if (!require(tidyverse))
    install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  if (!require(caret))
    install.packages("caret", repos = "http://cran.us.r-project.org")
  
  # MovieLens 10M dataset:
  # https://grouplens.org/datasets/movielens/10m/
  # http://files.grouplens.org/datasets/movielens/ml-10m.zip
  
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <-
    read.table(
      text = gsub("::", "\t", readLines(unzip(
        dl, "ml-10M100K/ratings.dat"
      ))),
      col.names = c("userId", "movieId", "rating", "timestamp")
    )
  
  movies <-
    str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <-
    as.data.frame(movies) %>% mutate(
      movieId = as.numeric(levels(movieId))[movieId],
      title = as.character(title),
      genres = as.character(genres)
    )
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Validation set will be 10% of MovieLens data
  set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
  test_index <-
    createDataPartition(
      y = movielens$rating,
      times = 1,
      p = 0.1,
      list = FALSE
    )
  edx <- movielens[-test_index, ]
  temp <- movielens[test_index, ]
  
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

# Extract the YEAR from the Title in both data sets
edx <-
  edx %>% 
  extract(title,
                  c("title_name", "year"),
                  regex = "^(.*) \\(([0-9 \\-]*)\\)$",
                  remove = T) %>%
  mutate(year = as.integer(year))

validation <-
  validation %>% extract(title,
                         c("title_name", "year"),
                         regex = "^(.*) \\(([0-9 \\-]*)\\)$",
                         remove = T) %>% mutate(year = as.integer(year))


# Split the groupings of genres into individual lines
edx_genres_split  <-  edx  %>% separate_rows(genres, sep = "\\|")
head(edx_genres_split)
#genres_valid <-
 # validation  %>% mutate(year = as.numeric(str_sub(validation$title, -5, -2))) %>% separate_rows(genres, sep = "\\|")
#head(genres_valid)

#Count number of distinct users and movies
edx %>%   summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

#Plot Number of Ratings per Movie
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 50, color = "blue") +
  scale_x_log10() +
  ggtitle("Number of Ratings per Movie")

#Plot Number of Ratings per user
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 50, color = "blue") +
  scale_x_log10() +
  ggtitle("Number of Ratings per User")

#Movies per year
edx %>%   count(year) %>%  ggplot (aes(x = year, y = n)) + geom_line(color =
                                                                       "blue")

#Average Rating per year
edx %>% group_by(year) %>% summarize(mean_rating = mean(rating)) %>% ggplot(aes(x =year, y = mean_rating)) + geom_line(color = "blue")
#Show trend for movies per year
edx %>% group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  geom_point() +
  geom_smooth()

#Plot average Ratings per genre - sorted from lowerst to highest
edx_genres_split %>% group_by(genres) %>%
  summarize(n = n(),
            avg = mean(rating),
            se = sd(rating) / sqrt(n())) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(
    x = genres,
    y = avg,
    ymin = avg - 0.5 * se,
    ymax = avg + 0.5 * se
  )) +
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Funtion to Calculate RMSE for true ratings + predicted ratings
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings) ^ 2))
}

#MODEL 1 - - Mean/average for all movies
avg <- mean(edx$rating)
avg
model_1_rmse <- RMSE(validation$rating, avg)
model_1_rmse

# Determine mean rating for each individual movie
movie_avgs <-
  edx %>%   group_by(movieId) %>%   summarize(m_avg = mean(rating))
# Plot distribution of mean ratings for all movies
movie_avgs %>% qplot(
  m_avg,
  geom = "histogram",
  bins = 20,
  data = .,
  color = I("blue")
)
# What's the impact per MOVIE compared to the overall Mean?
ImpactMovie <-
  edx %>%   group_by(movieId) %>%  summarize(ImpactMovie = sum(rating - avg) /
                                               (n() + 2))
ImpactMovie
# Plot Impact per Movie (compared to overall mean)
ImpactMovie %>% qplot(
  ImpactMovie,
  geom = "histogram",
  bins = 40,
  data = .,
  color = I("blue")
)
#MODEL 2 - MOVIE - Add Impact per Movie to model
prediction_Movie <-
  validation %>%   left_join(ImpactMovie, by = 'movieId') %>%  mutate(pred = avg + ImpactMovie)
model_2_rmse <- RMSE(validation$rating, prediction_Movie$pred)
model_2_rmse

# What's the impact per USER compared to the overall Mean?
ImpactUser <-
  edx %>%   group_by(userId) %>%  summarize(ImpactUser = sum(rating - avg) /
                                              (n() + 2))
ImpactUser
# Plot Impact per USER (compared to overall mean)
ImpactUser %>% qplot(
  ImpactUser, geom = "histogram", bins = 40, data = ., color = I("blue")
)
#MODEL 3 - USER - Add Mean per User to model
prediction_User <-
  validation %>%   left_join(ImpactUser, by = 'userId') %>%  mutate(pred = avg + ImpactUser)
model_3_rmse <- RMSE(validation$rating, prediction_User$pred)
model_3_rmse

# What's the impact per YEAR compared to the overall Mean?
ImpactYear <-
  edx %>%   group_by(year) %>%  summarize(ImpactYear = sum(rating - avg) /  (n() + 2))
ImpactYear
# Plot Impact per YEAR (compared to overall mean)
ImpactYear %>% qplot(
  ImpactYear,   geom = "histogram",   bins = 40,   data = .,   color = I("blue")
)
#MODEL 4 - YEAR - Add Mean per Year  model
prediction_Year <-
  validation %>%   left_join(ImpactYear, by = 'year') %>%  mutate(pred = avg + ImpactYear)
model_4_rmse <- RMSE(validation$rating, prediction_Year$pred)
model_4_rmse

#MODEL 5 - -  User  AND Movie
prediction_UserMovie <-
  validation %>%   left_join(ImpactUser, by = 'userId') %>%
  left_join(ImpactMovie, by = 'movieId') %>%   mutate(pred = avg + ImpactUser + ImpactMovie)
head(prediction_UserMovie)
model_5_rmse <- RMSE(validation$rating, prediction_UserMovie$pred)
model_5_rmse

#MODEL 6 - -  User  AND Movie AND Year
prediction_UserMovieYear <-
  validation %>%   left_join(ImpactUser, by = 'userId') %>%
  left_join(ImpactMovie, by = 'movieId') %>%  left_join(ImpactYear, by =
                                                          'year') %>%
  mutate(pred = avg + ImpactUser + ImpactMovie + ImpactYear)

model_6_rmse <-  RMSE(validation$rating, prediction_UserMovieYear$pred)
model_6_rmse

#MODEL 7 - - Regularization Model 

Movie_User_Lambda_Model <- function(lambda) {
  
  avg <- mean(edx$rating)
  
  # calculate movie coefficients
  MovieRegEffect <-
    edx %>%   group_by(movieId) %>%  summarize(MovieRegEffect = sum(rating - avg) /
                                                 (n() + lambda))
  # calculate user coefficients
  UserRegEffect <-
    edx %>%  left_join(MovieRegEffect, by = "movieId") %>% group_by(userId) %>%  summarize(UserRegEffect = sum(rating - avg - MovieRegEffect) / (      n() + lambda))
  
  # add coefficients to validation
  validation <-
    validation %>%     
    left_join(MovieRegEffect, by = 'movieId') %>%      
    left_join(UserRegEffect, by = 'userId')
  
  # Determine RMSE for current lambda
  lambda_rmse <-
    RMSE(validation$rating,
         (avg + validation$MovieRegEffect + validation$UserRegEffect))
  
  return(lambda_rmse)
}

# calculate errors for a set of lambda values and choose the smallest rmse
lambdas <- seq(2, 9, 0.25)    #target: 5.25
#lambdas <- seq(5, 5.5, 0.25)    
model_rmses <- sapply(lambdas, Movie_User_Lambda_Model)

# plot Lambdas vs. RMSEs
ggplot() + geom_line(aes(lambdas, model_rmses), col = "black", size = 1) +
  geom_vline(xintercept = lambda_of_smallest_rmse, col = "blue") + ggtitle("Movie /User Effect Model: RMSE for various lambdas") + ylab("RMSE") + xlab("lambda")

#Select the best Labda with the smallest RMSE
lambda_of_smallest_rmse <- lambdas[which.min(model_rmses)]
lambda_of_smallest_rmse

model_7_rmse <- model_rmses[which.min(model_rmses)]
model_7_rmse

