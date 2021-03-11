#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define UI for application that draws a histogram
#shinyUI(fluidPage(
#
    # Application title
#    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
#    sidebarLayout(
#        sidebarPanel(
#            sliderInput("bins",
#                        "Number of bins:",
#                        min = 1,
#                        max = 50,
#                        value = 30)
#        ),

        # Show a plot of the generated distribution
#        mainPanel(
#            plotOutput("distPlot")
#        )
#    )
#))



my_ui <- fluidPage(
    titlePanel("Trump's Twitter Activity and Approval Ratings Between 2017 and 2021"),
    
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                inputId = "dis_approval",
                label = "Select Graph Content",
                choices = c("Approval Ratings" , "Disapproval Ratings")
            )
        ),
        mainPanel(
            plotOutput("dis_approval_plot")
        )
    )
    
)

