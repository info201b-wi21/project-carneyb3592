library(dplyr)
library(tidyverse)
library(ggplot2)
library(shiny)

tweets_df_lance = read.csv("data/tweets_01-08-2021.csv")
approval_rating_df_lance_lance = read.csv("data/approval_topline.csv")

# Data wrangling/Exploratory report stuff
tweet_date_modified_df <- tweets_df_lance_lance %>% 
  mutate(date = as.Date(gsub(date, pattern=" 0:00:00", replacement="", fixed=T))) %>% 
  filter(date >= as.Date("2017-1-23"))

approval_rating_date_formatted_df <- approval_rating_df_lance %>% 
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

num_tweets_on_high_approval_dates_df <- approval_highs_df %>% 
  left_join(tweets_grouped_day_df) %>% 
  select(date, approve_estimate, num_tweets, average_past_week) %>% 
  mutate(average_daily_tweets = average_daily_tweets) %>% 
  pivot_longer(cols = c(num_tweets, average_past_week, average_daily_tweets), names_to = "tweet_values")

# Server function
server <- function(input, output) {
  emission_switch <- reactive({
    emission <- switch(input$Approval,
                       norm = rnorm,
                       unif = runif,
                       lnorm = rlnorm,
                       exp = rexp,
                       rnorm)
  })
  
  output$plot <- renderPlot(
    if (input$Approval == "Low") {
      tweet_rates_for_low_approve_plot <- ggplot(num_tweets_on_low_approval_dates_df) +
        geom_col(aes(x = value, y = as.character(approve_estimate), 
                     fill = tweet_values), position = position_dodge2(reverse = FALSE)) +
        scale_fill_manual("Tweets", 
                          labels = c("Average Daily Tweets", "Average Tweets Past Week", "Approval Date Tweets"), 
                          values = c("#a6cee3", "#b2df8a", "#1f78b4")) +
        labs(x = "Number of tweets", y = "Three Lowest Approval Ratings") +
        ggtitle("Tweet Stats for Lowest Approval Ratings")
      
      return(tweet_rates_for_low_approve_plot)
    }
    else {
      tweet_rates_for_high_approve_plot <- ggplot(num_tweets_on_high_approval_dates_df) +
        geom_col(aes(x = value, y = as.character(approve_estimate), 
                     fill = tweet_values), position = position_dodge2(reverse = FALSE)) +
        scale_fill_manual("Tweets", 
                          labels = c("Average Daily Tweets", "Average Tweets Past Week", "Approval Date Tweets"), 
                          values = c("#a6cee3", "#b2df8a", "#1f78b4")) +
        labs(x = "Number of tweets", y = "Three Highest Approval Ratings") +
        ggtitle("Tweet Stats for Highest Approval Ratings")
      
      return(tweet_rates_for_high_approve_plot)
    }
  )
  
  output$plot_description <- renderText(
    if (input$Approval == "Low") {
      return(paste("This plot is showing different rates at which trump tweeted on dates he had the three lowest approval ratings."))
    }
    else {
      return(paste("This plot is showing different rates at which trump tweeted on dates he had the three highest approval ratings."))
    }
  )
  
  output$analysis_text <- renderText(
    if (input$Approval == "Low") {
      return(paste("These three approval ratings were taken on the dates ", 
                   low_approval_dates[1], ", ", low_approval_dates[2], ", and ", low_approval_dates[3], ". ", 
                   "During these dates, Trump was going through legal trouble and some troubling news ",
                   "regarding overdoses in the US. The number of overdoses was, by the end of May 2017 was 66,324, ", 
                   "up 17% when compared to the previous 12-month period. ",
                   "at this same time, A Federal District Judge for Eastern Pennsylvania temporarily enjoins ", 
                   "the Trump administration from implementing new rules that change the Obamacare contraceptive mandate. ", 
                   "California, Delaware, Maryland, Massachusetts, New York, Virginia and Washington ",
                   "also sued the federal government over the rules. ", 
                   sep = ""))
    }
    else {
      return(paste("These three approval ratings were taken on the dates ",
                   high_approval_dates[1], ", ", high_approval_dates[2], ", ", high_approval_dates[3],
                   ". Around these dates in April a 2 trillion dollar covid relief bill had been signed.", 
                   sep = ""))
    }
  )
  
  output$summary_text <- renderText({
    return(paste("While there is a slight correlation between the rise in tweets and a rise", 
                 "the approval rates in this VERY small subset",
                 "of data. Due to this being a small isolated subset it is hard to",
                 "say that there is any correlation or causation between tweet rate and approval rate",
                 "with a lack of very convincing evidence.",
                 "The research I have done on these dates leads me to believe that the cause for these highs",
                 "and lows was from the current events happening during those time periods",
                 sep = " "))
  })

}