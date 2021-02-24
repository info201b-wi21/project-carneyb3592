library("dplyr")
library("tidyr")
library("ggplot2")
tweets_df = read.csv("data/tweets_01-08-2021.csv")
approval_rating_df = read.csv("data/approval_topline.csv")
lawsuits_df = read.csv("data/trump_lawsuits.csv")
View(tweets_df)
View(approval_rating_df)
View(lawsuits_df)



approval_rating_df <- approval_rating_df %>%
  mutate(modeldate = as.Date(modeldate,"%m/%d/%Y")) %>%
  group_by(modeldate) %>%
  summarise(approve_estimate = mean(approve_estimate),
            approve_hi = mean(approve_hi),
            approve_lo = mean(approve_lo),
            disapprove_estimate = mean(disapprove_estimate),
            disapprove_hi = mean(disapprove_hi),
            disapprove_lo = mean(disapprove_lo),
  )
View(approval_rating_df)
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
number_columns_approval <- ncol(tweets_df)
feature_names_approval <- colnames(tweets_df)
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
       y = "Approval Rating (%)",
       color = "")
plot(approval_rate_plot)

disapproval_rate_plot <-  ggplot() +
  geom_line(data = approval_rating_df, 
            mapping = aes(x = modeldate,y = disapprove_estimate, color = "Approval Estimate")) +
  geom_line(data = approval_rating_df, 
            mapping = aes(x = modeldate,y = disapprove_hi, color = "Approval High")) +
  geom_line(data = approval_rating_df, 
            mapping = aes(x = modeldate,y = disapprove_lo, color = "Approval Low")) +
  labs(title = "Trump Disapproval Rating Over Time",
       x = "Date",
       y = "Disapproval Rating (%)",
       color = "")
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


View(plottable_tweet_df)

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




lawsuits_df <- lawsuits_df %>%
  mutate(dateFiled = as.Date(dateFiled))
View(lawsuits_df)
temporary_approval_df <- approval_rating_df %>%
  group_by(modeldate) %>%
  summarise(modeldate = mean(modeldate),
            approve_estimate = mean(approve_estimate),
            approve_hi = mean(approve_hi),
            approve_lo = mean(approve_lo))
View(temporary_approval_df)
colnames(temporary_approval_df)[1] <- "dateFiled"
lawsuits_to_approval_df <- lawsuits_df %>%
  summarise(dateFiled,caseName,jurisdiction,capacity,type,issue) %>%
  left_join(temporary_approval_df) %>%
  drop_na(approve_estimate) %>%
  arrange(-desc(dateFiled))
View(lawsuits_to_approval_df)

case_with_lowest_approval <- lawsuits_to_approval_df %>%
  filter(approve_estimate == min(approve_estimate))
View(case_with_lowest_approval)
case_with_highest_approval <- lawsuits_to_approval_df %>%
  filter(approve_estimate == max(approve_estimate))
View(case_with_highest_approval)

lawsuits_to_approval_plot <- ggplot() +
  geom_point(data = lawsuits_to_approval_df,mapping = aes(x = dateFiled,y = approve_estimate,color = "Court Cases")) +
  geom_line(data = temporary_approval_df, 
            mapping = aes(x = dateFiled,y = approve_estimate)) 
  
  
plot(lawsuits_to_approval_plot)

approval_plot <- ggplot(data = temporary_approval_df, 
                        mapping = aes(x = dateFiled,y = approve_estimate)) +
  geom_line()
plot(approval_plot)
## 1
# The question is During his presidency, were the legal issues that Trump faced 
# an indicator of the public perception of him? If so, to what degree?
# We are looking for any correlation with President Donald Trumps legal issues
# and his approval rating.