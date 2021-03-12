
library("shiny")
library("dplyr")
library("tidyverse")
library(tidyr)
library("ggplot2")
source("ben_ui.R")
source("ben_server.R")
tweets_df <- read.csv("data/tweets_01-08-2021.csv")
approval_rating_df <- read.csv("data/approval_topline.csv")
get_approval_data(approval_rating_df)
get_tweets_data(tweets_df)
shinyApp(ui = ben_ui,server = ben_server)


