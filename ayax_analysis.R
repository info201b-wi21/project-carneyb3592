library(dplyr)

data <- read.csv("tweets_01-08-2021.csv")
approval_ratings <- read.csv('approval_topline.csv')

# Clean up tweets data
presidency_tweets <- data[ data$date > '2017-01-23',]
presidency_tweets <- presidency_tweets[presidency_tweets$isRetweet == 'f',]
presidency_tweets$date <- as.Date(presidency_tweets$date)
tweets_by_date <- presidency_twretweetseets %>% count(date)

# Clean up approval data
approval_ratings$date <- as.Date(approval_ratings$timestamp)
approval_ratings_by_date <- approval_ratings  %>%
  group_by(modeldate) %>%
  summarize(Mean_Approval_Estimate = mean(approve_estimate, na.rm=TRUE))

approval_ratings_by_date$date <- strptime(as.character(approval_ratings_by_date$modeldate), "%m/%d/%Y")
approval_ratings_by_date <- approval_ratings_by_date[ approval_ratings_by_date$date > '2017-01-23',]

# Prepare tweets data
average_retweets_by_date <- presidency_tweets  %>%
  group_by(date) %>%
  summarize(Mean_Retweets = mean(retweets, na.rm=TRUE))

average_favourite_by_date <- presidency_tweets  %>%
  group_by(date) %>%
  summarize(Mean_Favourites = mean(favorites, na.rm=TRUE))

approval_ratings_by_date$date <- as.Date(approval_ratings_by_date$date)
final_df <- merge(x = approval_ratings_by_date, y = tweets_by_date, by = "date")
final_df <- merge(x = final_df, y = average_favourite_by_date,by = "date")
final_df <- merge(x = final_df, y = average_retweets_by_date,by = "date")

cor(final_df$Mean_Approval_Estimate, y= final_df$n)
# .28
cor(final_df$Mean_Approval_Estimate, y= final_df$Mean_Favourites)
# .216
cor(final_df$Mean_Approval_Estimate, y= final_df$Mean_Retweets)
# .193