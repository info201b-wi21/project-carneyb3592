library(dplyr)
library(ggplot2)
data <- read.csv("data/tweets_01-08-2021.csv")
approval_ratings <- read.csv('data/approval_topline.csv')

# Clean up tweets data
presidency_tweets <- data[ data$date > '2017-01-23',]
presidency_tweets <- presidency_tweets[presidency_tweets$isRetweet == 'f',]
presidency_tweets$date <- as.Date(presidency_tweets$date)
tweets_by_date <- presidency_tweets %>% count(date)

# Clean up approval data
approval_ratings$date <- as.Date(approval_ratings$timestamp,"%m/%d/%Y")
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

correlation_of_number <- cor(final_df$Mean_Approval_Estimate, y= final_df$n)
# .28
correlation_of_favorites <-cor(final_df$Mean_Approval_Estimate, y= final_df$Mean_Favourites)
# .216
correlation_of_retweets <- cor(final_df$Mean_Approval_Estimate, y= final_df$Mean_Retweets)
# .193

approval_to_favourites_plot <- ggplot(final_df) +
  geom_col(mapping = aes(x = Mean_Approval_Estimate, y = Mean_Favourites, fill = "Mean Favorites per day"), width = .9) + 
  geom_col(mapping = aes(x = Mean_Approval_Estimate, y = Mean_Retweets, fill = "Mean Retweets"), width = .9) + 
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Number of Tweets/Retweets")) + 
  scale_fill_manual(values = c("#FF9E99", "#1F78B4"))+
  labs(title = "Trumps Approval Rating vs Twitter Interaction",
       x = "Approval Rating",
       y = "Mean Favorites per day",
       fill = "") +
  scale_x_continuous(labels = function(x) paste0(x, "%"))
plot(approval_to_favourites_plot)

