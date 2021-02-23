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
  mutate(modeldate = as.Date(modeldate,"%m/%d/%Y"))
View(approval_rating_df)
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

lawsuits_to_approval_plot <- ggplot(data = lawsuits_to_approval_df,mapping = aes(x = dateFiled,y = approve_estimate)) +
  geom_point() +
  geom_label(
    label = lawsuits_to_approval_df$caseName,
    nudge_x = 0.25, 
    nudge_y = 0.25
  )
  
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