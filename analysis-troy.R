# loading packages and dfs
library("tidyverse")
library("ggplot2")


tweets_df <- read.csv("data/tweets_01-08-2021.csv")
approval_rating_df <- read.csv("data/approval_topline.csv")
lawsuits_df <- read.csv("data/trump_lawsuits.csv")



# changes modeldate column from char class to date class
approval_rating_df <- approval_rating_df %>%
  mutate(modeldate = as.Date(modeldate,"%m/%d/%Y"))


tweets_df <- tweets_df %>%
                mutate(date = as.Date(date))

# note: first approval-rating date recorded is 01/23/2017


# Data Wrangling for Finding the number of actions (tweets/retweets) Trump used per month, INCLUDING DELETED TWEETS


num_of_tweets_per_day_df <- tweets_df %>%
                                filter(isRetweet == "f") %>%
                                    filter(date > "2017-01-22") %>%
                                      group_by(date) %>%
                                        mutate(date = format(date, "%Y-%m-%d")) %>%
                                          mutate(date = as.Date(date)) %>%
                                            summarize(num_of_tweets_per_day = n()) %>%
                                              complete(date = seq.Date(min(as.Date("2017-01-23")), max(date), by = "day")) %>%       # adds in missing dates to make it possible for left_join
                                                mutate(num_of_tweets_per_day = replace_na(num_of_tweets_per_day, 0))                 # replaces na values for 0

num_of_retweets_per_day_df <- tweets_df %>%
                                  filter(isRetweet == "t") %>%
                                    filter(date > "2017-01-22") %>%
                                      group_by(date) %>%
                                        mutate(date = format(date, "%Y-%m-%d")) %>%
                                          mutate(date = as.Date(date)) %>%
                                            group_by(date) %>%
                                              summarize(num_of_retweets_per_day = n()) %>%
                                                complete(date = seq.Date(min(as.Date("2017-01-23")), max(as.Date("2021-01-08")), by = "day")) %>%       
                                                  mutate(num_of_retweets_per_day = replace_na(num_of_retweets_per_day, 0))



total_num_of_re_tweets_per_day_df <- left_join(num_of_retweets_per_day_df, num_of_tweets_per_day_df) %>%
                                        summarize(date, total_re_tweets = num_of_retweets_per_day + num_of_tweets_per_day) # calculate total number of tweets and retweets for each day 




# APPROVAL ESTIMATE
approval_estimate_rating_df <- approval_rating_df %>%
                                    filter(modeldate > "2017-01-22") %>%
                                      group_by(modeldate) %>%
                                        filter(subgroup == "All polls") %>%
                                          select(modeldate, approve_estimate)
                                        

# DISAPPROVAL ESTIMATE
disapproval_estimate_rating_df <- approval_rating_df %>%
                                        filter(modeldate > "2017-01-22") %>%
                                          group_by(modeldate) %>%
                                            filter(subgroup == "All polls") %>%
                                                select(modeldate, disapprove_estimate)

# JOINING DFs
combined_approval_rating_df <- left_join(approval_estimate_rating_df, disapproval_estimate_rating_df) %>%
                                group_by(modeldate) %>%
                                  rename(date = modeldate) 
                                    

combined_approval_rating_df <- left_join(total_num_of_re_tweets_per_day_df, combined_approval_rating_df, by = "date") %>%
                                  select(date, total_re_tweets, approve_estimate, disapprove_estimate)


# PLOT
approval_re_tweet_plot <- ggplot(combined_approval_rating_df) +
                   geom_col(mapping = aes(x = date, y = approve_estimate, fill = "Approval Rating"), width = 1) + 
                    geom_col(mapping = aes(x = date, y = total_re_tweets, fill = "Tweets/Retweets"), width = 1) + 
                      scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Number of Tweets/Retweets")) + 
                        scale_fill_brewer(palette = "Paired")+
                        labs(title = "Former President Trump's Twitter Activity in Relation To His Approval Rating, January 2017 - January 2021",
                             x = "Year",
                             y = "Approval Rate (%)",
                             fill = "") 


disapproval_re_tweet_plot <- ggplot(combined_approval_rating_df) +
                              geom_col(mapping = aes(x = date, y = disapprove_estimate, fill = "Disapproval Rating"), width = 1) + 
                                geom_col(mapping = aes(x = date, y = total_re_tweets, fill = "Tweets/Retweets"), width = 1) + 
                                  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Number of Tweets/Retweets")) + 
                                    scale_fill_manual(values = c("#FF9E99", "#1F78B4"))+
                                      labs(title = "Former President Trump's Twitter Activity in Relation To His Disapproval Rating, January 2017 - January 2021",
                                           x = "Year",
                                           y = "Disapproval Rate (%)",
                                           fill = "") 



################################### Questions

# My Question: Did Trump's social media usage change the approval ratings of his voters? 
# If so, by how much and what indicators point towards the change?.

# 1. 
# By "social media usage", we mean to analyze the number of tweets and retweets posted by @realDonaldTrump, and we will be examining 
# its relationship with former President Trump's approval and disapproval ratings. For this question, we will be counting deleted
# tweets and retweets, since they are still purposefully posted online. 

# 2. 
# Before doing any data wrangling, we had to change the scope of the data; we would be focusing on Twitter activity and approval ratings spanning from
# the beginning of his presidency (January 23, 2017), to when he was suspended from Twitter (January 8, 2021). We first worked on `tweets_df`; after filtering for this time period, we then 
# calculated for the total number of tweets and retweets in each day. One aspect to note is that Trump did not use Twitter every day, meaning there were gaps in
# the data. To counter this, we mutated the data to add the days where there were no tweets or retweets posted. In `approval_rating_df`, we once again filtered for the time period, and then
# filtered for the subgroup "All Polls". Our group decided that rather than focusing on voters or adult, it would be best if we were to use the ratings provided by all polls. This allowed us
# to get the estimated approval and disapproval ratings. Finally, we joined the two estimated ratings together, compiling one approval rating dataframe, and then joined it with the tweets. 

# 3. 
#See plots above

# 4. 
# According to our data and visualizations, there doesn't seem to be any evident correlation between the number of tweets/retweets and approval rating. 
# Although the number of posts has increased significantly during Trump's presidency, his approval rate has remained quite stagnant. 
# Trump's lowest approval rating was 36.40314%, which occurred on December 16, 2017. His highest, 47.76497%, happened on January 25, 2017. 
# For the majority of his presidency, however, Trump's approval rating remained in the low 40s, sometimes dipping into the high 30s. 
# Even in the moments where Trump was posting hundreds of tweets and retweets, his approval rating stayed in the low 40s. In our other plot that examined Trump's disapproval rating,
# we once again noticed that his disapproval ratings were stagnant; Trump's disapproval rating peaked (57.50006%) in December 16, 2017, when he was rarely tweeting (8 tweets).
# As mentioned earlier, Trump began to tweet much more frequently in the second half of his presidency, 2019-2020, yet his disapproval rating stayed in the low fifties. 
# Taking all of this into consideration, we have come to the conclusion that there isn't a relationship between Trump's tweeting activity and his approval rating. 
