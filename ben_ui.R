library("shiny")
library("dplyr")
get_data <- function(file_a,file_t){
  tweets_df <- file_t
  approval_rating_df <- file_a
  
}

approval_rating_df <- approval_rating_df %>%
  mutate(modeldate = as.Date(modeldate,"%m/%d/%Y"))


tweets_df <- tweets_df %>%
  mutate(date = as.Date(date)) %>%
  filter(date > "2017-01-22")


tweets_date_range <- range(tweets_df$date)

# Define UI for application that draws a histogram
my_ui <- fluidPage(
  
  # Application title
  titlePanel("Trump's Twitter vs Approval Rating."),
  
  #paragraph of info 
  p("This website contains all the tweets made by and the approval rating of, Donald Trump."),
  strong("IMPORTANT NOTICE: If tweets appear as NA or do not show up any text, it means there are no tweets for that day. \n \n
         If you click the dots on the graph, it will show all the tweets made by @realDonaldTrump on that Day!"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = "year_input",
                  "Date",
                  start = tweets_date_range[1],
                  end = tweets_date_range[2],
                  min = tweets_date_range[1],
                  max = tweets_date_range[2],
                  sep = " to "),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Visualization",
                           plotOutput("plot", click = "plot_click"),
                           strong("Click on the dots to view what tweets were made that day!"),
                           verbatimTextOutput("info")))
    )
  )
)