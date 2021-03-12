library("shiny")
library("dplyr")
library("tidyverse")
library("ggplot2")

get_tweets_data <- function(file){
  tweets_df <- file
}
get_approval_data <- function(file){
  approval_rating_df <- file
}
#tweets_df <- read.csv("data/tweets_01-08-2021.csv")
#approval_rating_df <- read.csv("data/approval_topline.csv")
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
        #Approval Plot
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
        output$ben_table <- renderTable({
          output_table <- filter(plottable_data, modeldate >= input$year_input[1] & modeldate <= input$year_input[2])
          output_table <- mutate(output_table,modeldate = as.character(modeldate))
          colnames(output_table)[1] <- "Year"
          colnames(output_table)[2] <- "Apporoval Rating (%)"
          colnames(output_table)[3] <- "Number of Tweets"
          
          return(output_table)
          
        })
        #Interactive Plot with tweets
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
            color = "Number of Tweets"
          ) +
          theme(title = element_text(size = 16)) +
          scale_y_continuous(labels = function(x) paste0(x, "%"))
          
          
        })
        
        #Tweets output
        output$info <- renderPrint({
          
          output_df <- approval_rating_df_ben %>%
            left_join(tweets_df_ben,by = c("modeldate" = "date"))
          
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
          if(output_text == ": : "){
            return("Click on the Dots to see what he tweeted!")
          }
          return(output_text)
        })
  
        
  #Troys Work
        
        
      approval_rating_df_troy <- approval_rating_df %>%
        mutate(modeldate = as.Date(modeldate,"%m/%d/%Y"))
      
      
      tweets_df_troy <- tweets_df %>%
        mutate(date = as.Date(date))
      
      num_of_tweets_per_day_df <- tweets_df_troy %>%
        filter(isRetweet == "f") %>%
        filter(date > "2017-01-22") %>%
        group_by(date) %>%
        mutate(date = format(date, "%Y-%m-%d")) %>%
        mutate(date = as.Date(date)) %>%
        summarize(num_of_tweets_per_day = n()) %>%
        complete(date = seq.Date(min(as.Date("2017-01-23")), max(date), by = "day")) %>%       # adds in missing dates to make it possible for left_join
        mutate(num_of_tweets_per_day = replace_na(num_of_tweets_per_day, 0))                 # replaces na values for 0
      
      num_of_retweets_per_day_df <- tweets_df_troy %>%
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
      approval_estimate_rating_df <- approval_rating_df_troy %>%
        filter(modeldate > "2017-01-22") %>%
        group_by(modeldate) %>%
        filter(subgroup == "All polls") %>%
        select(modeldate, approve_estimate)
      
      
      # DISAPPROVAL ESTIMATE
      disapproval_estimate_rating_df <- approval_rating_df_troy %>%
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
      
      
      
      
      
      #Troy Bar plot
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
               fill = "") + 
          theme(title = element_text(size = 16))
        
        
      })
  #Lances Work
      tweets_df_lance = tweets_df
      approval_rating_df_lance = approval_rating_df
      
      # Data wrangling/Exploratory report stuff
      tweet_date_modified_df <- tweets_df_lance %>% 
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
      
      emission_switch <- reactive({
        emission <- switch(input$Approval,
                           norm = rnorm,
                           unif = runif,
                           lnorm = rlnorm,
                           exp = rexp,
                           rnorm)
      })
      
      #High/Low plot
      output$plot <- renderPlot(
        if (input$Approval == "Low") {
          tweet_rates_for_low_approve_plot <- ggplot(num_tweets_on_low_approval_dates_df) +
            geom_col(aes(x = value, y = as.character(approve_estimate), 
                         fill = tweet_values), position = position_dodge2(reverse = FALSE)) +
            scale_fill_manual("Tweets", 
                              labels = c("Average Daily Tweets", "Average Tweets Past Week", "Approval Date Tweets"), 
                              values = c("#a6cee3", "#b2df8a", "#1f78b4")) +
            labs(x = "Number of tweets", y = "Three Lowest Approval Ratings") +
            ggtitle("Tweet Stats for Lowest Approval Ratings") + theme(title = element_text(size = 16))
          
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
            ggtitle("Tweet Stats for Highest Approval Ratings") + theme(title = element_text(size = 16))
          
          return(tweet_rates_for_high_approve_plot)
        }
      )
      
      #Plot description text
      output$plot_description <- renderText(
        if (input$Approval == "Low") {
          return(paste("This plot is showing different rates at which trump tweeted on dates he had the three lowest approval ratings."))
        }
        else {
          return(paste("This plot is showing different rates at which trump tweeted on dates he had the three highest approval ratings."))
        }
      )
      
      #analysis text
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
      
      
      #Summary text
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
  #Ayax's Work
      data <-  tweets_df
      approval_ratings <- approval_rating_df
      
      presidency_tweets <- data[ data$date > '2017-01-23',]
      presidency_tweets <- presidency_tweets[presidency_tweets$isRetweet == 'f',]
      presidency_tweets$date <- as.Date(presidency_tweets$date)
      tweets_by_date <- presidency_tweets %>% count(date)
      
      # Clean up approval data
      approval_ratings$date <- as.Date(approval_ratings$timestamp,"%m/%d/%Y")
      approval_ratings_by_date <- approval_ratings  %>%
        group_by(modeldate) %>%
        summarize(Mean_Approval_Estimate = mean(approve_estimate, na.rm=TRUE))
      
      approval_ratings_by_date$date <- strptime(as.character(approval_ratings_by_date$modeldate), "%m/%d/%Y")
      approval_ratings_by_date <- approval_ratings_by_date[ approval_ratings_by_date$date > '2017-01-23',]
      
      # Prepare tweets data
      average_retweets_by_date <- presidency_tweets  %>%
        group_by(date) %>%
        summarize(Mean_Retweets = mean(retweets, na.rm=TRUE))
      
      average_favourite_by_date <- presidency_tweets  %>%
        group_by(date) %>%
        summarize(Mean_Favourites = mean(favorites, na.rm=TRUE))
      
      approval_ratings_by_date$date <- as.Date(approval_ratings_by_date$date)
      final_df <- merge(x = approval_ratings_by_date, y = tweets_by_date, by = "date")
      final_df <- merge(x = final_df, y = average_favourite_by_date,by = "date")
      final_df <- merge(x = final_df, y = average_retweets_by_date,by = "date")
      
      correlation_of_number <- cor(final_df$Mean_Approval_Estimate, y= final_df$n)
      # .28
      correlation_of_favorites <-cor(final_df$Mean_Approval_Estimate, y= final_df$Mean_Favourites)
      # .216
      correlation_of_retweets <- cor(final_df$Mean_Approval_Estimate, y= final_df$Mean_Retweets)
      # .193
      output$ayax_table <- renderTable({
        final_table <- filter(final_df, Mean_Approval_Estimate > input$approval_choice[1] & Mean_Approval_Estimate < input$approval_choice[2])
        final_table <- select(final_df,!date)
        colnames(final_table)[1] <- "Year"
        colnames(final_table)[2] <- "Mean Approval Rating"
        colnames(final_table)[3] <- "Count of Tweets"
        colnames(final_table)[4] <- "Mean Favourites"
        colnames(final_table)[5] <- "Mean Retweets"
        return(final_table)
      })
      
      #Interaction plot
      output$interaction_plot <- renderPlot({
        
        plottable_final_df <- filter(final_df, Mean_Approval_Estimate > input$approval_choice[1] & Mean_Approval_Estimate < input$approval_choice[2])
        
        ggplot(plottable_final_df) +
          geom_col(mapping = aes(x = Mean_Approval_Estimate, y = Mean_Favourites, fill = "Mean Favorites per day"), width = .9) + 
          geom_col(mapping = aes(x = Mean_Approval_Estimate, y = Mean_Retweets, fill = "Mean Retweets"), width = .9) + 
          scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Number of Tweets/Retweets")) + 
          scale_fill_manual(values = c("#FF9E99", "#1F78B4"))+
          labs(title = "Trumps Approval Rating vs Twitter Interaction",
               x = "Approval Rating",
               y = "Mean Favorites per day",
               fill = "") +
          scale_x_continuous(labels = function(x) paste0(x, "%")) + theme(title = element_text(size = 16))
        
        
        
      })
      
  
}
