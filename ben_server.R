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
approval_rating_df <- approval_rating_df %>%
  mutate(modeldate = as.Date(modeldate,"%m/%d/%Y"))  %>%
  filter(modeldate > "2017-01-22" & modeldate < "2021-01-09") %>%
  filter(subgroup == "All polls") %>%
  select(modeldate, approve_estimate)

tweets_count_df <- tweets_df %>%
  filter(date > "2017-01-22") %>%
  group_by(date) %>%
  mutate(date = as.Date(date)) %>%
  summarize(num_of_tweets_per_day = n()) %>%
  complete(date = seq.Date(min(as.Date("2017-01-23")), max(date), by = "day")) %>%       # adds in missing dates to make it possible for left_join
  mutate(num_of_tweets_per_day = replace_na(num_of_tweets_per_day, 0))  
#View(tweets_count_df)
tweets_df <- tweets_df %>%
  mutate(date = as.Date(date)) %>%
  filter(date > "2017-01-22")

plottable_data <- approval_rating_df %>%
  left_join(tweets_df,by = c("modeldate" = "date")) %>%
  left_join(tweets_count_df, by = c("modeldate" = "date")) %>% 
  select(modeldate, approve_estimate, num_of_tweets_per_day) %>%
  group_by(modeldate) %>%
  summarise(modeldate = mean(modeldate),approve_estimate = mean(approve_estimate),num_of_tweets_per_day= mean(num_of_tweets_per_day))
  
#View(plottable_data)
# Define server logic required to draw a histogram
my_server <- function(input, output) {
  
  
  output$plot_approval <- renderPlot({
    ggplot(data = approval_rating_df) +
      geom_line(mapping = aes(x = modeldate,y = approve_estimate)) +
      labs(
        title = "Trump's Approval Rating Over Time",
        x = "Date",
        y = "Approval Rating"
      ) +
      theme(title = element_text(size = 16)) +
      scale_y_continuous(labels = function(x) paste0(x, "%"))
    
    
  })
  
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
  output$plot_interactive <- renderPlot ({
    
    approval_rating_df <- filter(approval_rating_df, modeldate >= input$year_input[1] & modeldate <= input$year_input[2])
    tweets_df <- filter(tweets_df, date >= input$year_input[1] & date <= input$year_input[2])
    plottable_data <- filter(plottable_data, modeldate >= input$year_input[1] & modeldate <= input$year_input[2])
    
    ggplot()+
      geom_line(data = approval_rating_df, mapping = aes(x = modeldate,y = approve_estimate)) +
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
  
}
