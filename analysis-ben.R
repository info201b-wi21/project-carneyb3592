library("dplyr")
library("tidyr")
library("ggplot2")




tweets_df <- read.csv("data/tweets_01-08-2021.csv")
approval_rating_df <- read.csv("data/approval_topline.csv")
lawsuits_df <- read.csv("data/trump_lawsuits.csv")


# loading packages and dfs

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
#the values given to find the mean out of the three subgroups: voter, adults, and all

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

#View(lawsuits_df)

temporary_approval_df <- approval_rating_df %>%
  mutate(change_in_approval_of_next_day = lead(approve_estimate) - approve_estimate)

#View(temporary_approval_df)

colnames(temporary_approval_df)[1] <- "dateFiled"

lawsuits_to_approval_df <- lawsuits_df %>%
  summarise(dateFiled,caseName,jurisdiction,capacity,type,issue) %>%
  left_join(temporary_approval_df) %>%
  drop_na(approve_estimate) %>%
  arrange(-desc(dateFiled))

#View(lawsuits_to_approval_df)


lawsuits_plot <- ggplot() +
  geom_point(data = lawsuits_to_approval_df,mapping = aes(x = dateFiled,y = approve_estimate, fill = "Lawsuits"), color = "red") +
  labs(
    title = "Trump's Lawsuits",
    x = "Date",
    y = "Approval Rating",
    fill = ""
    
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Dark1")



## line + scatter plot for lawsuits to approval rating
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

top_three_positive_changes <- lawsuits_to_approval_df %>%
  arrange(desc(change_in_approval_of_next_day)) %>%
  head(n = 3L) %>%
  select(caseName,change_in_approval_of_next_day)
colnames(top_three_positive_changes)[1] <- "Case Name"
colnames(top_three_positive_changes)[2] <- "Change in Approval Rating (%)"

top_three_negative_changes <- lawsuits_to_approval_df %>%
  arrange(-desc(change_in_approval_of_next_day)) %>%
  head(n = 3L) %>%
  select(caseName,change_in_approval_of_next_day)
colnames(top_three_negative_changes)[1] <- "Case Name"
colnames(top_three_negative_changes)[2] <- "Change in Approval Rating (%)"


changes_in_rating_df <- lawsuits_to_approval_df %>%
  select(caseName,change_in_approval_of_next_day)

case_with_lowest_approval <- lawsuits_to_approval_df %>%
  filter(approve_estimate == min(approve_estimate))

#View(case_with_lowest_approval)

case_with_highest_approval <- lawsuits_to_approval_df %>%
  filter(approve_estimate == max(approve_estimate))

average_change_per_issue <- lawsuits_to_approval_df %>%
  group_by(issue) %>%
  summarise(change_in_approval_of_next_day = mean(abs(change_in_approval_of_next_day)),
            number_of_cases = n()) %>%
  arrange(desc(change_in_approval_of_next_day))
colnames(average_change_per_issue)[1] <- "Issue"
colnames(average_change_per_issue)[2] <- "Change in Approval Rating (%)"
colnames(average_change_per_issue)[3] <- "Amount of Cases"
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

#View(amount_of_positive_changes)
#View(amount_of_no_changes)
#View(average_change_per_issue)
#View(case_with_highest_approval)
#View(changes_in_rating_df)
#View(top_three_negative_changes)
#View(top_three_postive_changes)


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

