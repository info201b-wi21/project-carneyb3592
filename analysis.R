library("dplyr")
library("tidyr")
library("ggplot2")

# loading packages and dfs
library("tidyverse")
library("ggplot2")
tweets_df = read.csv("data/tweets_01-08-2021.csv")
approval_rating_df = read.csv("data/approval_topline.csv")

lawsuits_df = read.csv("data/trump_lawsuits.csv")
View(tweets_df)
View(approval_rating_df)
View(lawsuits_df)



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


## Section 3 Data Wrangling
## 1
# The question is During his presidency, were the legal issues that Trump faced 
# an indicator of the public perception of him? If so, to what degree?
# We are looking for any correlation with President Donald Trumps legal issues
# and his approval rating.

## 2
# Before doing any data wrangling on the approval rating data-frame, we had to
# change our scope of the data. We decided that to use the sub group "All groups"
# instead of voters and/or adults, as this would best represent the whole. Secondly,
# we filtered the data for the start of his presidency (January 23, 2017).
# I first mutated the approval rating data-frame to add a column that represents
# the change in approval rating to the next day. (e.g. 1/1/2020 = 45.5, 
# 1/2/2020 = 45.6, so the change of 1/1/2020 would be .1). I then joined this 
# data-frame with the lawsuits data-frame to be bale to compare the effects of 
# different lawsuits on the approval rating.
#
## 3

lawsuits_df <- lawsuits_df %>%
  mutate(dateFiled = as.Date(dateFiled))

View(lawsuits_df)

temporary_approval_df <- approval_rating_df %>%
  mutate(change_in_approval_of_next_day = lead(approve_estimate) - approve_estimate)

View(temporary_approval_df)

colnames(temporary_approval_df)[1] <- "dateFiled"

lawsuits_to_approval_df <- lawsuits_df %>%
  summarise(dateFiled,caseName,jurisdiction,capacity,type,issue) %>%
  left_join(temporary_approval_df) %>%
  drop_na(approve_estimate) %>%
  arrange(-desc(dateFiled))

View(lawsuits_to_approval_df)






##line + scatter plot for lawsuits to approval rating
lawsuits_to_approval_plot <- ggplot() +
  geom_point(data = lawsuits_to_approval_df,mapping = aes(x = dateFiled,y = approve_estimate, fill = "Lawsuits"), color = "red") +
  geom_line(data = temporary_approval_df, 
            mapping = aes(x = dateFiled,y = approve_estimate)) +
  labs(
    title = "Trump's Lawsuits",
    x = "Date",
    y = "Approval Rating",
    fill = ""
    
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Dark2")
  
plot(lawsuits_to_approval_plot)





## scatter plot for approval rating change
change_in_approval_plot <- ggplot(data = lawsuits_to_approval_df,mapping = aes(x = dateFiled,y = change_in_approval_of_next_day)) +
  geom_point(mapping = aes(color = ifelse( change_in_approval_of_next_day > 0, "Positive", "Negative"))) +
  labs(title = "Court Cases Change in Approval Rating",
       x = "Date",
       y = "Change in Approval") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_manual(name="Change", values = c("red","darkgreen")) +
  guides(color = guide_legend(reverse = TRUE))


plot(change_in_approval_plot)

top_three_postive_changes <- lawsuits_to_approval_df %>%
  arrange(desc(change_in_approval_of_next_day)) %>%
  head(n = 3L)

top_three_negative_changes <- lawsuits_to_approval_df %>%
  arrange(-desc(change_in_approval_of_next_day)) %>%
  head(n = 3L)

changes_in_rating_df <- lawsuits_to_approval_df %>%
  select(caseName,change_in_approval_of_next_day)

case_with_lowest_approval <- lawsuits_to_approval_df %>%
  filter(approve_estimate == min(approve_estimate))

View(case_with_lowest_approval)

case_with_highest_approval <- lawsuits_to_approval_df %>%
  filter(approve_estimate == max(approve_estimate))

average_change_per_issue <- lawsuits_to_approval_df %>%
  group_by(issue) %>%
  summarise(change_in_approval_of_next_day = mean(abs(change_in_approval_of_next_day)),
            number_of_cases = n()) %>%
  filter(number_of_cases > 2)

amount_of_positive_changes <- lawsuits_to_approval_df %>%
  filter(change_in_approval_of_next_day > 0) %>%
  summarise(amount = n()) %>%
  pull()

amount_of_negative_changes <- lawsuits_to_approval_df %>%
  filter(change_in_approval_of_next_day < 0) %>%
  summarise(amount = n()) %>%
  pull()

amount_of_no_changes <- lawsuits_to_approval_df %>%
  filter(change_in_approval_of_next_day == 0) %>%
  summarise(amount = n()) %>%
  pull()

View(amount_of_positive_changes)
View(amount_of_no_changes)
View(average_change_per_issue)
View(case_with_highest_approval)
View(changes_in_rating_df)
View(top_three_negative_changes)
View(top_three_postive_changes)


## 4
# Through our analysis, we found very few court cases that could have had a 
# major effect on Trump's approval rating. There was no change above 0.7% within
# that. The greatest positive change was a 0.69048%, followed by 0.57166% and 
# 0.45039%. The greatest negative changes were -0.62866%,-0.36466%, and -0.25494%.
# Since Trump's approval rating remained relatively stagnant throughout his 
# presidency at around low 40s, we have come to the conclusion that very few, 
# almost none had an effect, since Trump's approval rating was likely shifted by
# various other factors. We have determine that if any cases had an effect, it 
# would likely be ones with above 0.5% change in approval rating of the next day,
# which concluded to be PEN America v. Trump, Clifford v. Trump, and California v. Ross. When researching 
# these lawsuits online, we found a variety of major news articles for PEN America v. Trump,
# yet none for the others. We can only then assume that the only lawsuit that had a significant effect
# on Trump's approval rating was PEN America v. Trump. However, this is only accounting
# for the court case, at the same time, there were news articles on the day the case
# was filed about Trump's attack on Stephanie Clifford about her "Horseface" 
# (https://www.nytimes.com/2018/10/16/us/politics/trump-women-insults.html).
# When examining the relationship between the type of cases and change in approval
# rating, we found that cases regarding 2016 Trump campaign and Census issues had
# the highest average change at 0.27683% and 0.24039333%. While these were not 
# the highest values overall, they were the highest for issues with more than 2
# cases, as 1-2 cases could lead to skewed results. We also found that there was
# almost an exact 1:1 ratio (24:26) of positive to negative changes with three 
# at exactly 0. Overall, we can see little to no correlation with Trump's 
# lawsuits and his approval rating. While some cases appear to have an effect, there
# are a variety of factors that could have played into the change and we cannot 
# fully determine if these lawsuits had an effect.

# changes modeldate column from char class to date class
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
                                        select(modeldate, approve_estimate)
                                        

# DISAPPROVAL ESTIMATE
disapproval_estimate_rating_df <- approval_rating_df %>%
                                        filter(modeldate > "2017-01-22") %>%
                                          group_by(modeldate) %>%
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
