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
View(approval_rating_df)
approval_rating_df <- approval_rating_df %>%
  mutate(modeldate = as.Date(modeldate,"%m/%d/%Y"))  %>%
  filter(modeldate > "2017-01-22" & modeldate <"2021-01-09") %>%
  filter(subgroup == "All polls") %>%
  select(modeldate, approve_estimate)

tweets_count_df <- tweets_df %>%
  filter(date > "2017-01-22") %>%
  group_by(date) %>%
  mutate(date = as.Date(date)) %>%
  summarize(num_of_tweets_per_day = n()) %>%
  complete(date = seq.Date(min(as.Date("2017-01-23")), max(date), by = "day")) %>%       # adds in missing dates to make it possible for left_join
  mutate(num_of_tweets_per_day = replace_na(num_of_tweets_per_day, 0))  
View(tweets_count_df)
tweets_df <- tweets_df %>%
  mutate(date = as.Date(date)) %>%
  filter(date > "2017-01-22")

plottable_data <- approval_rating_df %>%
  left_join(tweets_df,by = c("modeldate" = "date")) %>%
  left_join(tweets_count_df, by = c("modeldate" = "date")) %>% 
  select(modeldate, approve_estimate, num_of_tweets_per_day) %>%
  group_by(modeldate) %>%
  summarise(modeldate = mean(modeldate),approve_estimate = mean(approve_estimate),num_of_tweets_per_day= mean(num_of_tweets_per_day))
  
View(plottable_data)
# Define server logic required to draw a histogram
my_server <- function(input, output) {
  
  
  # Makes a label based on value selected
  #table
  #output$data <- renderTable({
  #   return(my_table)
  #})
  
  #Message for table
  #output$message_table <- renderText({
  #   return(my_message)
  #})
  
  #Message for plot
  #output$message_plot <- renderText({
  #   return(my_message)
  #})
  
  #plot
  output$plot <- renderPlot ({
    
    ggplot()+
      geom_line(data = approval_rating_df, mapping = aes(x = modeldate,y = approve_estimate)) +
      geom_point(data = plottable_data,mapping = aes(x = modeldate,y = approve_estimate, color = num_of_tweets_per_day)) +
      scale_color_continuous(type  = "viridis") +
    labs(
      title = "Temp",
      x = "Year",
      y = "approval rating",
      color = "Number of Tweets"
    )
    
    
  })
  
  
}
