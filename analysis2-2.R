library("dplyr")
library("tidyr")
library("ggplot2")

# loading packages and dfs
tweets_df = read.csv("data/tweets_01-08-2021.csv")
approval_rating_df = read.csv("data/approval_topline.csv")

lawsuits_df = read.csv("data/trump_lawsuits.csv")
##View(tweets_df)
##View(approval_rating_df)
#View(lawsuits_df)



approval_rating_df <- approval_rating_df %>%
  filter(subgroup == "All polls") %>%
  mutate(modeldate = as.Date(modeldate,"%m/%d/%Y")) %>%
  group_by(modeldate) %>%
  summarise(approve_estimate = mean(approve_estimate),
            approve_hi = mean(approve_hi),
            approve_lo = mean(approve_lo),
            disapprove_estimate = mean(disapprove_estimate),
            disapprove_hi = mean(disapprove_hi),
            disapprove_lo = mean(disapprove_lo)
  )
#View(approval_rating_df)
#In approval_ratings_df, because there are multiple subgroups, we are summarizing 
#the values given to find the mean out of the three subgroups: voter, adults, and all polls


## Approval Rating statistics
number_columns_approval <- ncol(approval_rating_df)

feature_names_approval <- colnames(approval_rating_df)

approval_date_range <- range(approval_rating_df$modeldate)

approval_rate_estimate_range <- range(approval_rating_df$approve_estimate)

approval_rate_hi_range <- range(approval_rating_df$approve_hi)

approval_rate_lo_range <- range(approval_rating_df$approve_lo)

disapproval_rate_estimate_range <- range(approval_rating_df$disapprove_estimate)

disapproval_rate_hi_range <- range(approval_rating_df$disapprove_hi)

disapproval_rate_lo_range <- range(approval_rating_df$disapprove_lo)

approval_rate_estimate_mean <- mean(approval_rating_df$approve_estimate)

approval_rate_hi_mean <- mean(approval_rating_df$approve_hi)

approval_rate_lo_mean <- mean(approval_rating_df$approve_lo)

disapproval_rate_estimate_mean <- mean(approval_rating_df$disapprove_estimate)

disapproval_rate_hi_mean <- mean(approval_rating_df$disapprove_hi)

disapproval_rate_lo_mean <- mean(approval_rating_df$disapprove_lo)



## Tweets data statistics
number_columns_tweets <- ncol(tweets_df)

feature_names_tweets <- colnames(tweets_df)

total_tweets <- tweets_df %>%
  summarise(tweets = n()) %>%
  pull()

tweet_date_range <- range(tweets_df$date)

tweet_favorites_max <- max(tweets_df$favorites)

tweet_retweets_max <- max(tweets_df$retweets)

tweet_favorites_min <- pull(tweets_df %>%
                              filter(date > "2017-01-22" & isDeleted == "f") %>%
                              filter(favorites > 0) %>%
                              filter(favorites == min(favorites)) %>%
                              select(favorites))

tweet_retweets_min <- pull(tweets_df %>%
                             filter(date > "2017-01-22" & isDeleted == "f") %>%
                             filter(retweets > 0) %>%
                             filter(retweets == min(retweets)) %>%
                             select(retweets))

retweet_count <- pull(tweets_df %>%
                        filter(isRetweet == "t") %>%
                        summarise(total_retweets = n()))

delete_count <- pull(tweets_df %>%
                       filter(isDeleted == "t") %>%
                       summarise(total_deletions = n()))  

## Lawsuits data statistics
number_columns_lawsuits <- ncol(lawsuits_df)

feature_names_lawsuits <- colnames(lawsuits_df)

lawsuit_date_range <- range(lawsuits_df$date)

total_lawsuits <- pull(summarise(lawsuits_df,n()))


approval_rate_plot <-  ggplot() +
  geom_line(data = approval_rating_df, 
            mapping = aes(x = modeldate,y = approve_estimate, color = "Approval Estimate")) +
  geom_line(data = approval_rating_df, 
            mapping = aes(x = modeldate,y = approve_hi, color = "Approval High")) +
  geom_line(data = approval_rating_df, 
            mapping = aes(x = modeldate,y = approve_lo, color = "Approval Low")) +
  labs(title = "Trump Approval Rating Over Time",
       x = "Date",
       y = "Approval Rating",
       color = "") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) 

plot(approval_rate_plot)

disapproval_rate_plot <-  ggplot() +
  geom_line(data = approval_rating_df, 
            mapping = aes(x = modeldate,y = disapprove_estimate, color = "Disapproval Estimate")) +
  geom_line(data = approval_rating_df, 
            mapping = aes(x = modeldate,y = disapprove_hi, color = "Disapproval High")) +
  geom_line(data = approval_rating_df, 
            mapping = aes(x = modeldate,y = disapprove_lo, color = "Disapproval Low")) +
  labs(title = "Trump Disapproval Rating Over Time",
       x = "Date",
       y = "Disapproval Rating",
       color = "") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) 

plot(disapproval_rate_plot)

temporary_plottable_tweet_df <- tweets_df %>%
  filter(date > "2017-01-22") %>%
  mutate(date = format(as.Date(date),"%Y")) %>%
  filter(isRetweet == "f") %>%
  group_by(date) %>%
  summarise(tweet_count = n())

plottable_tweet_df <- tweets_df %>%
  filter(date > "2017-01-22") %>%
  mutate(date = format(as.Date(date),"%Y")) %>%
  filter(isRetweet == "t") %>%
  group_by(date) %>%
  summarise(retweet_count = n()) %>%
  left_join(temporary_plottable_tweet_df) %>%
  pivot_longer(cols = !date)


#View(plottable_tweet_df)

tweet_plot <- ggplot(data = plottable_tweet_df) +
  geom_col(mapping = aes(x = date, y = value, fill = name), 
           position = position_dodge2(reverse = T)) +
  scale_fill_brewer(palette = "Paired",
                    labels = c("retweet_count"="Retweets",
                               "tweet_count"= "Tweets")) +
  labs(
    title = "Trump's Twitter Activity During Presidency",
    x = "Year",
    y = "Number of Posts",
    fill = ""
    
  )

plot(tweet_plot)  

