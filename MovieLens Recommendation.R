##### Load the Libraries #####

library(tidyverse)
library(caret)
library(lubridate)
library(data.table)
library(ggplot2)
library(psych)

##### Pull Data from MovieLens Website ####

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile() # create temporary file location
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl) # download dataset to temporary file location

# parse downloaded data to create ratings dataset
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# parse downloaded data to create movie dataset
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres") # name columns

# coerce dataset into data frame
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

# join ratings and movies data tables to create movielens dataset
movielens <- left_join(ratings, movies, by = "movieId")

##### Create Train (edx) and Test (validation) Datasets #####

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # set seed to 1
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE) # create index with 10% of data for test set
edx <- movielens[-test_index,] # create edx (train) dataset from test index
temp <- movielens[test_index,] # create temporary (test) dataset from test index

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# remove unneeded variables in global environment
rm(dl, ratings, movies, test_index, temp, movielens, removed) 

head(edx) # display first six rows of data 
summary(edx) # display summary statistics

##### Data Cleaning #####

edx_clean <- edx %>%
  # coerce timestamp into date_time data type
  mutate(timestamp = as_datetime(timestamp)) %>%
  # pull year from title and add it as new column,
  # remove parenthesis 
  # and set as numeric
  mutate(year = substring(title, nchar(title)-5)) %>%
  mutate(year = as.numeric(gsub(year, pattern = "\\(|\\)", replacement = ""))) %>%
  # calculate time between movie's release and review
  mutate(yearsbetween = as.numeric(year(timestamp) - year)) %>%
  # separate out rows with multiple genres into multiple duplicate rows
  separate_rows(genres, sep = "\\|")

head(edx_clean) # display first six rows of cleaned data 
summary(edx_clean) # display summary statistics of cleaned data

# perform the same cleaning processes on the validation set
validation_clean <- validation %>%
  mutate(timestamp = as_datetime(timestamp)) %>%
  mutate(year = substring(title, nchar(title)-5)) %>%
  mutate(year = as.numeric(gsub(year, pattern = "\\(|\\)", replacement = ""))) %>%
  mutate(yearsbetween = as.numeric(year(timestamp) - year)) %>%
  separate_rows(genres, sep = "\\|")

##### Cursory Data Analysis and Visualizations #####

avg_rating <- mean(edx_clean$rating) # calculate average rating
median_rating <- median(edx_clean$rating) # calculate median rating

# number of unique movies and users in the edx dataset 
edx_clean %>% 
  summarize(uniqueUsers = n_distinct(userId), 
            uniqueMovies = n_distinct(movieId))

# group by ratings, summarize frequency of each rating 
# and arrange data in descending order of number of ratings
edx_ratings <- edx_clean %>%
  group_by(rating) %>%
  summarize(numRatings = n()) %>%
  mutate(rating = as.factor(rating)) %>%
  arrange(desc(numRatings))

head(edx_ratings) # display first six rows of data 

# plot rating distribution
edx_ratings %>%
  ggplot(aes(x = rating, y = numRatings)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = c(0e+06, 2e+06, 4e+06, 6e+06),
                     labels = c(0, 2, 4, 6)) +
  geom_vline(aes(xintercept = "4", colour = "Mean"), linetype = "longdash") +
  geom_vline(aes(xintercept = "3.5", colour = "Median"), linetype = "longdash") +
  labs(x = "Rating", y = "No. of Ratings (mil)", 
       title = "Number of Ratings vs Rating", colour = "Rating", 
       size = "No. of Ratings (mil)") +
  theme_minimal()

# plot frequency for each rating
edx_ratings %>%
  ggplot(aes(x = rating, y = numRatings)) +
  geom_point(aes(size = numRatings)) +
  geom_point(aes(8, 6730401, colour = "Mean"), size = 5.7) +
  geom_point(aes(7, 2110690, colour = "Median"), size = 3.5) +
  scale_size_continuous(breaks = c(0e+06, 2e+06, 4e+06, 6e+06), 
                        labels = c(0, 2, 4, 6),
                        limits = c(0, 7*10^6)) +
  scale_y_continuous(breaks = c(0e+06, 2e+06, 4e+06, 6e+06),
                     labels = c(0, 2, 4, 6)) +
  labs(x = "Rating", y = "No. of Ratings (mil)", 
       title = "Number of Ratings vs Rating", colour = "Rating", 
       size = "No. of Ratings (mil)") +
  theme_minimal()

# group by movie, summarize by rating counts, calculate average ratings 
# and arrange data in descending order of number of ratings
edx_movies <- edx_clean %>%
  group_by(movieId) %>%
  summarize(title = title[1], numRatings = n(), avgRating = mean(rating)) %>%
  arrange(desc(numRatings))

headTail(edx_movies) # display first six rows of data 

# plot the number of ratings for top 10 movies
edx_movies %>%
  arrange(desc(numRatings)) %>%
  head(10) %>%
  ggplot(aes(x = title, y = numRatings)) +
  geom_bar(stat = "identity") +
  labs(x = "Movie Title", y = "No. of Ratings", 
       title = "Number of Ratings vs Top 10 Movie Titles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot distirbution of movie ID and no. of ratings 
edx_clean %>%
  count(movieId) %>%
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, colour = "black") +
  scale_x_continuous(trans = "log10", 
                     breaks = c(1e+01, 1e+03, 1e+05),
                     labels = c(10, 100, "100000")) +
  labs(x = "Movie ID", y = "Number of Ratings", 
       title = "Distribution of Movie ID and Number of Ratings") +
  theme_minimal()

# number of movie ratings per genre
# and average ratings per genre
genre_ratings <- edx_clean %>%
  group_by(genres) %>%
  summarize(numRatings = n(), avgRating = mean(rating)) %>%
  arrange(desc(numRatings))

headTail(genre_ratings) # display first and last six rows of data 

# plot the number of ratings for all genres
# removing movies without genres listed (only 7)
genre_ratings %>%
  slice(seq(1, 19)) %>% # remove movies with unlisted genres
  ggplot(aes(x = reorder(genres, -numRatings), y = numRatings)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = c(0e+06, 1e+06, 2e+06, 3e+06, 4e+06),
                     labels = c(0, 1, 2, 3, 4)) +
  labs(x = "Genre", y = "No. of Ratings (mil)", 
       title = "Number of Ratings vs Genres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot average ratings by genres
genre_ratings %>%
  slice(seq(1, 19)) %>% # remove movies with unlisted genres
  ggplot(aes(x = reorder(genres, numRatings), y = avgRating)) +
  geom_point(aes(size = numRatings)) +
  scale_size_continuous(breaks = c(0e+06, 1e+06, 2e+06, 3e+06, 4e+06), 
                        labels = c(0, 1, 2, 3, 4),
                        limits = c(0, 4*10^6)) +
  labs(x = "Genre", y = "Average Rating", 
       title = "Ratings by Genres", size = "No. of Ratings (mil)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot the trend of some of the genres over the years
edx_clean %>%
  select(movieId, year, genres) %>%
  mutate(genres = as.factor(genres)) %>%
  group_by(year, genres) %>%
  summarize(count = n()) %>%
  filter(year > 1930) %>%
  filter(genres %in% c("War", "Sci-Fi", "Animation", "Western")) %>%
  ggplot(aes(x = year, y = count)) +
  geom_line(aes(colour = genres)) +
  labs(x = "Year", y = "Count", 
       title = "Some of the Popular Genres in Recent Years") +
  theme_minimal()
 
# plot distirbution of user ID and no. of ratings 
edx_clean %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  labs(x = "User ID", y = "Number of Ratings", 
       title = "Distribution of User ID and Number of Ratings") +
  theme_minimal()

# group by year, summarize by no. of ratings and average ratings
edx_years <- edx_clean %>%
  group_by(year) %>%
  summarize(numRatings = n(), avgRating = mean(rating))

headTail(edx_years)

# plot average rating vs year to visualize trend of users and their rating habits
edx_years %>%
  ggplot(aes(x = year, y = avgRating)) +
  geom_point(aes(size = numRatings), alpha = 0.7) +
  geom_smooth(size = 0.7) +
  labs(x = "Year", y = "Average Rating", 
       title = "Trend of Users' Rating Habits over the Years",
       size = "No. of Ratings") +
  theme_minimal()

# group by years between release and review, 
# summarize by no. of ratings and average ratings
edx_yearsbetween <- edx_clean %>%
  group_by(yearsbetween) %>%
  summarize(numRatings = n(), avgRating = mean(rating))

headTail(edx_yearsbetween)

# plot average rating vs years between release and review 
# to visualize trend of ratings
edx_yearsbetween %>%
  ggplot(aes(x = yearsbetween, y = avgRating)) +
  geom_point(aes(size = numRatings), alpha = 0.5) +
  scale_size_continuous(breaks = c(0e+06, 2e+06, 4e+06), 
                        labels = c(0, 2, 4),
                        limits = c(0, 5*10^6)) +
  labs(x = "Years between Movie Release and Rating", y = "Average Rating", 
       title = "Average Rating vs Years between Movie Release and Rating", 
       size = "No. of Ratings (mil)") +
  theme_minimal()

##### Define Function to Calculate RMSE ######

# define function that takes true ratings and predicted ratings
# and calculates residual mean squared error
rmse <- function(trueRating, predictedRating){
  sqrt(mean((predictedRating - trueRating)^2))
}

##### Model 1 - Average #####

# average rating in edx dataset
avg_rating <- mean(edx_clean$rating)
# calculate rmse for model
rmse_avg <- rmse(validation_clean$rating, avg_rating) 
# create a table to display all the calculated rmses
model_rmses <- tibble(model = "Average", rmse = rmse_avg)

##### Model 2 - Movie Effect #####

# group by movie Id, calculate movie effect by taking 
# average of difference between rating and average rating 
movie_effect <- edx_clean %>%
  group_by(movieId) %>%
  summarize(e_m = mean(rating - avg_rating))

# take validation set, join matching rows from movie_effect 
# dataframe to validation dataframe and calculate predicted ratings
# by summing average rating and movie effect, then pull predicted rating results
movieRating_pred <- validation_clean %>%
  left_join(movie_effect, by = "movieId") %>%
  mutate(predictedRating = avg_rating + e_m) %>%
  pull(predictedRating)

# calculate rmse for movie effect model
rmse_movieEffect <- rmse(validation_clean$rating, movieRating_pred)
# append result to a table to store
model_rmses <- model_rmses %>%
  bind_rows(tibble(model = "Average + Movie Effect",
                   rmse = rmse_movieEffect))

##### Model 3 - User Effect #####

# group by user Id, calculate user effect by taking 
# average of difference between rating and average rating 
user_effect <- edx_clean %>%
  group_by(userId) %>%
  summarize(e_u = mean(rating - avg_rating))

# take validation set, join matching rows from user_effect 
# dataframe to validation dataframe and calculate predicted ratings
# by summing average rating and user effect, then pull predicted rating results
userRating_pred <- validation_clean %>%
  left_join(user_effect, by = "userId") %>%
  mutate(predictedRating = avg_rating + e_u) %>%
  pull(predictedRating)

# calculate rmse for user effect model
rmse_userEffect <- rmse(validation_clean$rating, userRating_pred)
# append result to a table to store
model_rmses <- model_rmses %>%
  bind_rows(tibble(model = "Average + User Effect",
                   rmse = rmse_userEffect))

##### Model 4 - Genre Effect #####

# group by genre, calculate genre effect by taking 
# average of difference between rating and average rating
genre_effect <- edx_clean %>%
  group_by(genres) %>%
  summarize(e_g = mean(rating - avg_rating))

# take validation set, join matching rows from genre_effect 
# dataframe to validation dataframe and calculate predicted ratings
# by summing average rating and genre effect, then pull predicted rating results
genreRating_pred <- validation_clean %>%
  left_join(genre_effect, by = "genres") %>%
  mutate(predictedRating = avg_rating + e_g) %>%
  pull(predictedRating)

# calculate rmse for user effect model
rmse_genreEffect <- rmse(validation_clean$rating, genreRating_pred)
# append result to a table to store
model_rmses <- model_rmses %>%
  bind_rows(tibble(model = "Average + Genre Effect",
                   rmse = rmse_genreEffect))

##### Model 5 - Year Effect #####

# group by year, calculate genre effect by taking 
# average of difference between rating and average rating
year_effect <- edx_clean %>%
  group_by(year) %>%
  summarize(e_y = mean(rating - avg_rating))

# take validation set, join matching rows from year_effect 
# dataframe to validation dataframe and calculate predicted ratings
# by summing average rating and year effect, then pull predicted rating results
yearRating_pred <- validation_clean %>%
  left_join(year_effect, by = "year") %>%
  mutate(predictedRating = avg_rating + e_y) %>%
  pull(predictedRating)

# calculate rmse for user effect model
rmse_yearEffect <- rmse(validation_clean$rating, yearRating_pred)
# append result to a table to store
model_rmses <- model_rmses %>%
  bind_rows(tibble(model = "Average + Year Effect",
                   rmse = rmse_yearEffect))

##### Model 6 - Years between Effect #####

# group by years between, calculate years between effect by taking 
# average of difference between rating and average rating
yearsbetween_effect <- edx_clean %>%
  group_by(yearsbetween) %>%
  summarize(e_yb = mean(rating - avg_rating))

# take validation set, join matching rows from yearsbetween_effect 
# dataframe to validation dataframe and calculate predicted ratings
# by summing average rating and years between effect, then pull predicted rating results
yearsbetweenRating_pred <- validation_clean %>%
  left_join(yearsbetween_effect, by = "yearsbetween") %>%
  mutate(predictedRating = avg_rating + e_yb) %>%
  pull(predictedRating)

# calculate rmse for user effect model
rmse_yearsbetweenEffect <- rmse(validation_clean$rating, yearsbetweenRating_pred)
# append result to a table to store
model_rmses <- model_rmses %>%
  bind_rows(tibble(model = "Average + Years between Effect",
                   rmse = rmse_yearsbetweenEffect))

##### Model 7 - Movie Regularization ##### 

lambdas <- seq(0, 10, 0.25) # define a set of lambdas to test

regMovies_rmses <- sapply(lambdas, function(l){
  regMovies_effect <- edx_clean %>%
    group_by(movieId) %>%
    summarize(e_m = sum(rating - avg_rating) / (n() + l)) 
  regMovies_pred <- validation_clean %>%
    left_join(regMovies_effect, by = "movieId") %>%
    mutate(predictedRating = avg_rating + e_m) %>%
    pull(predictedRating)
  return(rmse(validation_clean$rating, regMovies_pred))
})

# return minimum rmse
minregMovies_effect <- min(regMovies_rmses)
# append result to a table to store
model_rmses <- model_rmses %>%
  bind_rows(tibble(model = "Average + Movie Effect + Regularization",
                   rmse = minregMovies_effect))

##### Model 8 - Combine the Best Effects #####

model_rmses # display calculated RMSEs

movieuser_effect <- edx_clean %>%
  left_join(movie_effect, by = "movieId") %>%
  group_by(userId) %>%
  summarize(e_u = mean(rating - avg_rating - e_m))

movieuserRating_pred <- validation_clean %>%
  left_join(movie_effect, by = "movieId") %>%
  left_join(movieuser_effect, by = "userId") %>%
  mutate(predictedRating = avg_rating + e_m + e_u) %>%
  pull(predictedRating)

rmse_movieuserEffect <- rmse(validation_clean$rating, movieuserRating_pred)
model_rmses <- model_rmses %>%
  bind_rows(tibble(model = "Average + Movie Effect + User Effect",
                   rmse = rmse_movieuserEffect))

model_rmses[8,] # display best model rmse results



