library(dplyr)
library(tidyverse)
library(ggplot2)

tweets_df = read.csv("data/tweets_01-08-2021.csv")
approval_rating_df = read.csv("data/approval_topline.csv")
lawsuits_df = read.csv("data/trump_lawsuits.csv")
View(tweets_df)
View(approval_rating_df)
View(lawsuits_df)

# 1 - Introduction:
# In this question I will be exploring the trumps approval ratings and rates at which he tweeted. Approval ratings are just what
# percentage of th US population approve of Tump. The rate at which Trump tweets is generally the amount he tweets in a single
# day. 

# 2 - Explanation:
# When doing my analysis I found the top three the highs and lows for Trumps approval ratings.
# Then I found the number of tweets he sent out for those days, and the average of those weeks.

# 3 - results: 
# show plots and stuff

# 4 - Evaluation:
# There is a slight correlation between the rise in tweets and a rise the approval rates in this VERY small subset, 
# of data. Due to this being a small isolated subset it is hard to say that there is any correlation or causation
# with a lack of very convincing evidence.
# I was expecting a correlation between tweets and approval rates, and I was expecting an increase in tweets to correlate
# with a lower approval rate.
#
# https://www.usnews.com/news/national-news/articles/2017-12-15/drug-overdose-deaths-continue-to-soar
# https://en.wikipedia.org/wiki/Portal:Current_events/December_2017
# During the lowest approval rates in December of 2017 Trump was going through legal trouble and some troubling news
# Regarding overdoses in the US. The number of overdoses was 
# by end May 2017 was 66,324, up 17% when compared to the previous 12-month period.
# and at this same time, A Federal District Judge for Eastern Pennsylvania temporarily enjoins the Trump administration from 
# implementing new rules that change the Obamacare contraceptive mandate. 
# California, Delaware, Maryland, Massachusetts, New York, Virginia and Washington have 
# also sued the federal government over the rules.
#
# https://www.cbsnews.com/news/trump-signs-coronavirus-relief-package-today-2020-03-27/
# 
# In just before April 2020 a 2 trillion relief bill was approved.
#
# ALthough tweeting more in this small subset shows a small correlation between tweeting more in the past week,
# and higher approval ratings, it is a more likely case that these events surrounding those dates were the
# cause for approval ratings and the tweets were a reaction to the current events of that time.
# (High tweet rates in response to good press, low tweet rates in response to bad press)

# save for later
tweet_date_modified_df <- tweets_df %>% 
  mutate(date = as.Date(gsub(date, pattern=" 0:00:00", replacement="", fixed=T))) %>% 
  filter(date >= as.Date("2017-1-23"))

approval_rating_date_formatted_df <- approval_rating_df %>% 
  mutate(date = as.Date(format(as.Date(modeldate, '%m/%d/%Y'), '%Y-%m-%d')))


#Exploratory report start

#approval wrangling
approval_highs_df <- approval_rating_date_formatted_df %>%
  filter(subgroup == "All polls") %>% 
  slice_max(approve_estimate, n = 3)

approval_lows_df <- approval_rating_date_formatted_df %>% 
  filter(subgroup == "All polls") %>% 
  slice_min(approve_estimate, n = 3)

# Use for analysis
high_approval_dates <- approval_highs_df %>% 
  pull(date)

low_approval_dates <- approval_lows_df %>% 
  pull(date)

#Tweet wrangling
tweets_grouped_day_df <- tweet_date_modified_df %>% 
  group_by(date) %>% 
  summarise(date = unique(date), num_tweets = length(unique(id))) %>% 
  mutate(average_past_week = (num_tweets + lag(num_tweets, 7) + 
                                lag(num_tweets, 6) +
                                lag(num_tweets, 5) +
                                lag(num_tweets, 4)+
                                lag(num_tweets, 3)+
                                lag(num_tweets, 2)+
                                lag(num_tweets, 1)
  ) / 7)
view(tweets_grouped_day_df)

## average daily tweets between 2018 and 2021
average_daily_tweets <- tweets_grouped_day_df %>% 
  pull(num_tweets) %>% 
  mean()

# combine data
num_tweets_on_low_approval_dates_df <- approval_lows_df %>% 
  left_join(tweets_grouped_day_df) %>% 
  select(date, approve_estimate, num_tweets, average_past_week) %>% 
  mutate(average_daily_tweets = average_daily_tweets) %>% 
  pivot_longer(cols = c(num_tweets, average_past_week, average_daily_tweets), names_to = "tweet_values")
view(num_tweets_on_low_approval_dates_df)

num_tweets_on_high_approval_dates_df <- approval_highs_df %>% 
  left_join(tweets_grouped_day_df) %>% 
  select(date, approve_estimate, num_tweets, average_past_week) %>% 
  mutate(average_daily_tweets = average_daily_tweets) %>% 
  pivot_longer(cols = c(num_tweets, average_past_week, average_daily_tweets), names_to = "tweet_values")
view(num_tweets_on_high_approval_dates_df)

tweet_rates_for_low_approve_plot <- ggplot(num_tweets_on_low_approval_dates_df) +
  geom_col(aes(x = value, y = as.character(approve_estimate), 
               fill = tweet_values), position = position_dodge2(reverse = FALSE)) +
  scale_fill_manual("Tweets", 
                    labels = c("Average Daily Tweets", "Average Tweets Past Week", "Approval Date Tweets"), 
                    values = c("#a6cee3", "#b2df8a", "#1f78b4")) +
  labs(x = "Number of tweets", y = "Three Lowest Approval Ratings") +
  ggtitle("Tweet Stats for Lowest Approval Ratings")

tweet_rates_for_high_approve_plot <- ggplot(num_tweets_on_high_approval_dates_df) +
  geom_col(aes(x = value, y = as.character(approve_estimate), 
               fill = tweet_values), position = position_dodge2(reverse = FALSE)) +
  scale_fill_manual("Tweets", 
                    labels = c("Average Daily Tweets", "Average Tweets Past Week", "Approval Date Tweets"), 
                    values = c("#a6cee3", "#b2df8a", "#1f78b4")) +
  labs(x = "Number of tweets", y = "Three Highest Approval Ratings") +
  ggtitle("Tweet Stats for Highest Approval Ratings")









