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




# Define UI for application that draws a histogram
my_ui <- fluidPage(
  
  # Application title
  titlePanel("Trump's Twitter vs Approval Rating."),
  
  #paragraph of info 
  p("This website contains information for almost every country and their carbon dioxide emissions. You can view this as tables of values or a graph."),
  strong("IMPORTANT NOTICE: If values appear as N/A or do not show up in the graphs, that means that no data for that value exists."),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year_input",
                  "Year",
                  min = 1900,
                  max = 2000,
                  value = c(1900,2000),
                  sep = ""),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Table",tableOutput("data"),textOutput(outputId = "message_table")),
                  tabPanel("Visualization",plotOutput("plot")))
    )
  )
)