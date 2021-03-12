library(shiny)
library(ggplot2)

ui <- fluidPage(
  h1("Highest and Lowest Approval Rates In Comparison To Tweet Rates and Events"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("Approval",
                   label = "Approval Ratings",
                   choices = c("Low", "High"),
                   selected = "Low"
      )
    ),
    
    mainPanel(
      plotOutput("plot"), 
      textOutput("plot_description"),
      br(),
      textOutput("analysis_text"),
      br(),
      textOutput("summary_text")
  )),
  
  tags$style("#plot_description{font-size: 16px;
                   font-style: italic;
                   }")
)