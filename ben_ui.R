library("shiny")
library("dplyr")
get_data <- function(file_a,file_t){
  tweets_df <- file_t
  approval_rating_df <- file_a
  
}
tweets_df <- read.csv("data/tweets_01-08-2021.csv")
approval_rating_df <- read.csv("data/approval_topline.csv")
approval_rating_df <- approval_rating_df %>%
  mutate(modeldate = as.Date(modeldate,"%m/%d/%Y"))


tweets_df <- tweets_df %>%
  mutate(date = as.Date(date)) %>%
  filter(date > "2017-01-22")


tweets_date_range <- range(tweets_df$date)

# Define UI for application that draws a histogram
my_ui <- navbarPage("Analyzing the effect of Social Media on Donald Trump",
  
  #intro panel
  tabPanel("Intro",
  sidebarLayout( mainPanel(
  
  #paragraph of info 
    h2("Problem Domain"),  
    p("For our project, we decided to investigate the domain of social media and 
    its impact on U.S. politics, specifically focusing on the presidency. Within 
    this domain, we intend on analyzing former president Donald Trump's Twitter 
    account and all of his 'tweets'. Twitter is a widely used social media platform 
    where users can write 280-character long messages (in the form of 'tweets') 
    for other Twitter users to read. When other Twitter users view a tweet, they 
    have the option to 'like' reply to the tweet, or 'quote-retweet' (retweeting 
    with an added comment); all of these options count as an 'interaction', so the 
    number of interactions that a tweet has is an indicator of how many people legitimately 
    viewed it. The president is given an official Twitter account under the alias @POTUS and 
    has the ability to adopt 'followers' from the previous president's Twitter account. 
    When Trump became president in 2016, he took the 'followers', giving the Twitter 
    account a large boost in interactions. Throughout his presidency, however, he still
    used his personal account @realDonaldTrump, which we will be analyzing for this project."),
    h2("Data Sets"),
    h3("Twitter Data"),
    a("Twitter Dataset link",href="https://www.thetrumparchive.com/"),
    br(),
    p("The data set is an archive of Donald Trump's tweets during his presidency, 
      beginning in September 2016 and ending on January 8th 2021 upon his ban 
      ]from Twitter. The data set is composed of 56,572 tweets along with timestamps 
      of when the tweets were posted as well as retweets/favorites for each tweet. 
      The source of this data set was created by Brendan Patrick Brown, a programmer/engineer from Georgetown University."),
    h3("Approal Rating Data"),
    a("Approval Rating Dataset link", href = "https://github.com/fivethirtyeight/data/tree/master/trump-approval-ratings"),
    br(),
    p("This data set contains the results of various poll results over his term. 
      The data was retrieved from the FiveThirtyEight GitHub repository. This 
      data was compiled by the opinion poll analyst organization, FiveThirtyEight. 
      We do not know necessarily how the data was collected for each poll that 
      FiveThirtyEight analyzed, as they were collected from various websites, 
      however, we do know FiveThirtyEight has a history of credibility and strict 
      quality when analyzing datasets and poll results. This data set will help 
      when answering all of our questions as a major theme in our analysis is the public perception of the previous president. "),
    br(),
    p("To View more about our data sets, visit our in-depth report linked below!"),
    a("In-Depth Report", href = "https://info201b-wi21.github.io/project-carneyb3592/index.html"), width = 6),
  sidebarPanel(plotOutput("plot_approval"),
    width = 6)
    
  )),
  
    #Bolded notice  
    # Sidebar with a slider input for number of bins
  #Summary Analysis Panel
  tabPanel("Summary",
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
                             plotOutput("plot_interactive", click = "plot_click"),
                             strong("Click on the dots to view what tweets were made that day!"),
                             verbatimTextOutput("info")))
      )
  ))
)