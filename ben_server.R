library("shiny")
library("dplyr")
library("tidyverse")
library("ggplot2")

get_data <- function(file_a,file_t){
  tweets_df <- file_t
  approval_rating_df <- file_a
  
}
tweets_df <- read.csv("data/tweets_01-08-2021.csv")
approval_rating_df <- read.csv("data/approval_topline.csv")
#View(approval_rating_df)
approval_rating_df_ben <- approval_rating_df %>%
  mutate(modeldate = as.Date(modeldate,"%m/%d/%Y"))  %>%
  filter(modeldate > "2017-01-22" & modeldate < "2021-01-09") %>%
  filter(subgroup == "All polls") %>%
  select(modeldate, approve_estimate)

tweets_count_df_ben <- tweets_df %>%
  filter(date > "2017-01-22") %>%
  group_by(date) %>%
  mutate(date = as.Date(date)) %>%
  summarize(num_of_tweets_per_day = n()) %>%
  complete(date = seq.Date(min(as.Date("2017-01-23")), max(date), by = "day")) %>%       # adds in missing dates to make it possible for left_join
  mutate(num_of_tweets_per_day = replace_na(num_of_tweets_per_day, 0))  
#View(tweets_count_df)
tweets_df_ben <- tweets_df %>%
  mutate(date = as.Date(date)) %>%
  filter(date > "2017-01-22")

plottable_data <- approval_rating_df_ben %>%
  left_join(tweets_df_ben,by = c("modeldate" = "date")) %>%
  left_join(tweets_count_df_ben, by = c("modeldate" = "date")) %>% 
  select(modeldate, approve_estimate, num_of_tweets_per_day) %>%
  group_by(modeldate) %>%
  summarise(modeldate = mean(modeldate),approve_estimate = mean(approve_estimate),num_of_tweets_per_day= mean(num_of_tweets_per_day))
  
#View(plottable_data)
# Define server logic required to draw a histogram
ben_server <- function(input, output) {
  
  #Bens Work
        output$plot_approval <- renderPlot({
          ggplot(data = approval_rating_df_ben) +
            geom_line(mapping = aes(x = modeldate,y = approve_estimate)) +
            labs(
              title = "Trump's Approval Rating Over Time",
              x = "Date",
              y = "Approval Rating"
            ) +
            theme(title = element_text(size = 16)) +
            scale_y_continuous(labels = function(x) paste0(x, "%"))
          
          
        })
        
        output$plot_interactive <- renderPlot ({
          
          approval_rating_df_ben <- filter(approval_rating_df_ben, modeldate >= input$year_input[1] & modeldate <= input$year_input[2])
          tweets_df_ben <- filter(tweets_df_ben, date >= input$year_input[1] & date <= input$year_input[2])
          plottable_data <- filter(plottable_data, modeldate >= input$year_input[1] & modeldate <= input$year_input[2])
          
          ggplot()+
            geom_line(data = approval_rating_df_ben, mapping = aes(x = modeldate,y = approve_estimate)) +
            geom_point(data = plottable_data,mapping = aes(x = modeldate,y = approve_estimate, color = num_of_tweets_per_day)) +
            scale_color_continuous(type  = "viridis") +
          labs(
            title = "Tweets along Approval rating",
            x = "Date",
            y = "Approval Rating",
            color = "Number of Tweets",
            caption = "Each dot represents the Tweets made by @realDonaldTrump on that day."
          ) +
          theme(title = element_text(size = 16)) +
          scale_y_continuous(labels = function(x) paste0(x, "%"))
          
          
        })
        output$info <- renderPrint({
          output_df <- approval_rating_df %>%
            left_join(tweets_df,by = c("modeldate" = "date"))
          
          output_data <- nearPoints(output_df,input$plot_click, xvar = "modeldate",yvar = "approve_estimate")%>%
            select(modeldate,text)
          
          make_text <- function(text_df){
            output_message <- ""
            for(text in text_df){
              output_message <- paste0(output_message, text_df$modeldate, ": ", text_df$text)
            }
            return(output_message)
          }
          
          output_text <- make_text(output_data)
          return(output_text)
        })
  
  #Troys Work
      approval_rating_df <- approval_rating_df %>%
        mutate(modeldate = as.Date(modeldate,"%m/%d/%Y"))
      
      
      tweets_df <- tweets_df %>%
        mutate(date = as.Date(date))
      
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
          scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Number of Tweets/Retweets"),labels = function(x) paste0(x, "%")) + 
          switch(input$dis_approval,
                 "Approval Ratings" = scale_fill_brewer(palette = "Paired"),
                 "Disapproval Ratings" = scale_fill_manual(values = c("#FF9E99", "#1F78B4")))+
          labs(title = paste("Former President Trump's Twitter Activity and", input$dis_approval, "between 01/2017 and 01/2021", sep = " "),
               x = "Year",
               y = input$dis_approval,
               fill = "")
        
        
      })
  #Lances Work
      
      
  #Ayax's Work
      
      
  
}
