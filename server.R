#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)


my_server <- shinyServer(function(input, output) {
    tweets_df = read.csv("data/tweets_01-08-2021.csv")
    approval_rating_df = read.csv("data/approval_topline.csv")
    
    
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
    
    
    

    
    
    output$dis_approval_plot <- renderPlot({
        
        
        
        
        ggplot(combined_approval_rating_df) +
            geom_col(mapping = aes(x = date, y = switch(input$dis_approval, 
                                                        "Approval Ratings" = approve_estimate,
                                                        "Disapproval Ratings" = disapprove_estimate),
                                   fill = switch(input$dis_approval,
                                                 "Approval Ratings" = "Approval Rating",
                                                 "Disapproval Ratings" = "Disapproval Rating")), width = 1) + 
            geom_col(mapping = aes(x = date, y = total_re_tweets, fill = "Tweets/Retweets"), width = 1) + 
                scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Number of Tweets/Retweets")) + 
                    switch(input$dis_approval,
                           "Approval Ratings" = scale_fill_brewer(palette = "Paired"),
                           "Disapproval Ratings" = scale_fill_manual(values = c("#FF9E99", "#1F78B4")))+
                        labs(title = paste("Former President Trump's Twitter Activity and", input$dis_approval, "between 01/2017 and 01/2021", sep = " "),
                             x = "Year",
                             y = paste(input$dis_approval, "(%)"),
                             fill = "")
        
        
        
    })
    
    
    
    
})
