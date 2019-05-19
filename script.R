#############################################################
# Create edx set, and validation set
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("caret", repos = "http://cran.us.r-project.org")

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

set.seed(1)
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


#############################################################
# Understanding the data
#############################################################


##How many different movies are in the edx dataset?
length(unique(edx$movieId))

##How many different users are in the edx dataset?
length(unique(edx$userId))

##rating

edx %>% group_by(rating) %>% summarise(n = n()) %>% 
  ggplot(aes(rating,n)) + geom_bar(stat = "identity")

#####user rating

user_rating = edx %>% group_by(userId) %>% summarise(avg_rating = mean(rating),sd_rating = sd(rating))
user_rating %>% ggplot(aes(avg_rating)) + geom_histogram(bins = 20)

####movie rating

movie_rating =  edx %>% group_by(movieId) %>% summarise(avg_rating = mean(rating),sd_rating = sd(rating))
movie_rating %>% ggplot(aes(avg_rating)) + geom_histogram(bins = 20)


#######genres

if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

?str_extract_all

#sum(str_count(edx$genres,"Drama"))
#edx %>% filter(str_detect(edx$genres,"Drama")) %>% nrow()


genres = unique(unlist(str_extract_all(edx$genres,"[A-Z][^|]+")))
genres

count_genres = sapply(genres, function(x){
  n = sum(str_count(edx$genres,x))
})


df_genres = data.frame(genres, count_genres) 
df_genres %>%
  mutate(genres = reorder(genres, count_genres)) %>%
  ggplot(aes(genres, count_genres)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("")


####genre effect

ratings_genres = map_df(genres, function(x){
  n = edx %>% filter(str_detect(edx$genres,x)) %>% summarise(Genre = x, avg = mean(rating), sd= sd(rating))
})
ratings_genres %>% knitr::kable()


###########
head(edx$timestamp)
library(lubridate)
?as_datetime
edx$date = as_datetime(edx$timestamp, origin = "1970-01-01")
edx$year = year(edx$date)

edx %>% mutate(timest = round_date(date, "year"))%>% group_by(timest) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(timest,avg_rating)) + geom_point()


year(edx$date)
######movie with more number of ratings

mrating = edx %>% group_by(movieId) %>% mutate(n = n()) %>% select(title,n)
mrating$title[which.max(mrating$n)]



####################################################################
#MODEL##############################################################
####################################################################


####just the average
mu_hat <- mean(edx$rating)
mu_hat

naive_rmse <- caret::RMSE(validation$rating, mu_hat)
naive_rmse

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
rmse_results

#####genre effect
genres_avgs = map_dbl(1:length(unique(edx$genres)), function(x){
  mu = mean(ratings_genres$avg[str_detect(unique(edx$genres)[x],ratings_genres$Genre)])
})
genres_mu = data.frame(genres = unique(edx$genres), mu_0 = genres_avgs, stringsAsFactors = F)

avg_genres = edx %>% 
  left_join(genres_mu, by="genres") %>% mutate(mu = ifelse(genres == "(no genres listed)", mu_hat, mu_0))


predicted_ratings = validation %>%
  left_join(genres_mu, by="genres") %>%
  .$mu


model_1_rmse = caret::RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre Effect Model",  
                                     RMSE = model_1_rmse ))

rmse_results

#####movie effect
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_hat))

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu_hat + b_i ) %>%
  .$pred


model_2_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_2_rmse ))
rmse_results


#####users effect
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="User + Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results

##########

####regularized movie effect

lambdas <- seq(0, 10, 0.25)

just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu_hat), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- edx %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu_hat + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, edx$rating))
})
rmses
plot(rmses)
lambda = lambdas[which.min(rmses)]
lambda

movie_reg_avgs <- avg_genres %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda), n_i = n()) 

predicted_ratings <- validation %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu_hat + b_i) %>%
  .$pred

model_4_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_4_rmse ))
rmse_results

