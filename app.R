<<<<<<< HEAD
library(shiny)
source("ben_ui.R")
source("ben_server.R")
tweets_df <- read.csv("data/tweets_01-08-2021.csv")
approval_rating_df <- read.csv("data/approval_topline.csv")
get_data(approval_rating_df,tweets_df)
shinyApp(ui = my_ui,server = my_server)
||||||| 6fb53e1
=======
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
source("server.R")
source("ui.R")


shinyApp(server = my_server, ui = my_ui)



>>>>>>> troy-branch
