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
    p("The plots below mark former president Donald Trump's Twitter activity, specifically his tweets and retweets, and the averages of his approval ratings per day."),
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
    ),
    p("According to our data and visualizations, there doesn't seem to be any evident correlation between the number of tweets/retweets and approval rating. 
 Although the number of posts has increased significantly during Trump's presidency, his approval rate has remained quite stagnant. 
 Trump's lowest approval rating was 36.40314%, which occurred on December 16, 2017. His highest, 47.76497%, happened on January 25, 2017. 
 For the majority of his presidency, however, Trump's approval rating remained in the low 40s, sometimes dipping into the high 30s. 
 Even in the moments where Trump was posting hundreds of tweets and retweets, his approval rating stayed in the low 40s. In our other plot that examined Trump's disapproval rating,
 we once again noticed that his disapproval ratings were stagnant; Trump's disapproval rating peaked (57.50006%) in December 16, 2017, when he was rarely tweeting (8 tweets).
 As mentioned earlier, Trump began to tweet much more frequently in the second half of his presidency, 2019-2020, yet his disapproval rating stayed in the low fifties. 
 Taking all of this into consideration, we have come to the conclusion that there isn't a relationship between Trump's tweeting activity and his approval rating. ")
    
)

