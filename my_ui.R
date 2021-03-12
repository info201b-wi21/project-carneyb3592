library("shiny")
library("dplyr")

tweets_df <- read.csv("data/tweets_01-08-2021.csv")
approval_rating_df <- read.csv("data/approval_topline.csv")
approval_rating_df_ui <- approval_rating_df %>%
  mutate(modeldate = as.Date(modeldate,"%m/%d/%Y"))


tweets_df_ui <- tweets_df %>%
  mutate(date = as.Date(date)) %>%
  filter(date > "2017-01-22")

approval_rating_range <- range(approval_rating_df_ui$approve_estimate)
tweets_date_range <- range(tweets_df_ui$date)

# Define UI for application that draws a histogram
my_ui <- fluidPage(titlePanel("Analyzing the effect of Social Media on Donald Trump"),
  navbarPage(NULL,
  
  #intro panel
  tabPanel("Intro",
  sidebarLayout( mainPanel(
  
      #Intro Text
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
          from Twitter. The data set is composed of 56,572 tweets along with timestamps 
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
        p("To view more about our data sets, visit our in-depth report linked below!"),
        a("In-Depth Report", href = "https://info201b-wi21.github.io/project-carneyb3592/index.html"), width = 6),
      sidebarPanel(plotOutput("plot_approval"),
        width = 6)
    
  ),br(),
  br(),
  p("Made By Ben Carney, Troy Lu, Lance Haughen, Ayax Franco")
  
  ),
  
    #Bolded notice  
    # Sidebar with a slider input for number of bins
  #Summary Analysis Panel
  tabPanel("Summary Analysis",
    titlePanel("Summary Analysis"),
    sidebarLayout(
      sidebarPanel(
        dateRangeInput(inputId = "year_input",
                    "Select Your Date Range",
                    start = tweets_date_range[1],
                    end = tweets_date_range[2],
                    min = tweets_date_range[1],
                    max = tweets_date_range[2],
                    sep = " to "),
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Visualization",plotOutput("plot_interactive", click = "plot_click"),
                em("Each dot in the graph above represents the tweets made by @realDonaldTrump on that day.", style = "font-size:16px"),
                br(),
                br(),
                verbatimTextOutput("info")),
          tabPanel("Table",tableOutput("ben_table"))
      ))
  )),
  #Tweets vs Approval rating page
  tabPanel("Tweets vs Approval Rating",
           titlePanel("Did Trump's social media usage change the approval ratings of his voters? If so, by how much and what indicators point towards the change?"),
           sidebarLayout(
             sidebarPanel(
               radioButtons(
                 inputId = "dis_approval",
                 label = "Select Graph Content",
                 choices = c("Approval Ratings" , "Disapproval Ratings")
               )
               
               
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Visualization",
                  plotOutput("dis_approval_plot"),
                  em("The plot above mark former president Donald Trump's Twitter activity, specifically his tweets and retweets, and the averages of his approval ratings per day.",style = "font-size:16px"),
                   br(),
                  br(),
                  p("According to our data and visualizations, there doesn't seem to be any evident correlation between the number of tweets/retweets and approval rating. 
             Although the number of posts has increased significantly during Trump's presidency, his approval rate has remained quite stagnant. 
             Trump's lowest approval rating was 36.40314%, which occurred on December 16, 2017. His highest, 47.76497%, happened on January 25, 2017. 
             For the majority of his presidency, however, Trump's approval rating remained in the low 40s, sometimes dipping into the high 30s. 
             Even in the moments where Trump was posting hundreds of tweets and retweets, his approval rating stayed in the low 40s. In our other plot that examined Trump's disapproval rating,
             we once again noticed that his disapproval ratings were stagnant; Trump's disapproval rating peaked (57.50006%) in December 16, 2017, when he was rarely tweeting (8 tweets).
             As mentioned earlier, Trump began to tweet much more frequently in the second half of his presidency, 2019-2020, yet his disapproval rating stayed in the low fifties. 
             Taking all of this into consideration, we have come to the conclusion that there isn't a relationship between Trump's tweeting activity and his approval rating. ")
               ),
               tabPanel("Table",tableOutput("dis_approval_table"))
          
               
             )))
            
           
           
  ),
  #Approval Rates vs Real Events page
  tabPanel("Approval Rates vs Real Events",
           titlePanel("When were Trump's approval ratings at all time lows and at all 
                      time highs and how much was he tweeting in that time period? What 
                      major events were happening in that time period?"),
           
           sidebarLayout(
             sidebarPanel(
               radioButtons("Approval",
                            label = "Approval Ratings",
                            choices = c("Low", "High"),
                            selected = "Low"
               )
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Visualization",
                   plotOutput("plot"), 
                   textOutput("plot_description"),
                   br(),
                   textOutput("analysis_text"),
                   br(),
                   textOutput("summary_text"),
                   h3("Relevant Links"),
                   a("General News", href = "https://en.wikipedia.org/wiki/Portal:Current_events/December_2017"),
                   br(),
                   a("Drug Overdose Information", href = "https://www.usnews.com/news/national-news/articles/2017-12-15/drug-overdose-deaths-continue-to-soar"),
                   br(),
                   a("Coronavirus Relief Package", href = "https://www.cbsnews.com/news/trump-signs-coronavirus-relief-package-today-2020-03-27/")
                 ),
                 tabPanel("Table",tableOutput("lance_table"))
                )
             )
             ),
           
           tags$style("#plot_description{font-size: 16px;
                   font-style: italic;
                   }")
           
           
           ),
  # Approval Rating vs Twitter Interactions page
  tabPanel("Approval Rating vs Twitter Interactions",
           titlePanel("Did Trump's tweets affect public opinion over the course of his presidency? If so, to what extent?"),
           sidebarLayout(
             sidebarPanel(
               sliderInput(
                 "approval_choice",
                 label = "Select the Approval Rating Range",
                 min = approval_rating_range[1],
                 max = approval_rating_range[2],
                 value = c(approval_rating_range[1],approval_rating_range[2]),
                post = "%",
                step = 2,
                dragRange = T
               )
             ),
             
             mainPanel(tabsetPanel(
               tabPanel("Visualization",
               plotOutput("interaction_plot"),
               em("The plot above shows the Twitter interactions (retweets/favorites) compared to approval rating.",style = "font-size:16px"),
               br(),
               br(),
               p("The sheer volume of tweets had the most correlation with the approval rating 
               with a correlation factor of 28%. The second most impact was the amount of 
               favorites which ended up having a correlation factor of 21.6% correlation 
               with approval ratings. Surprisingly, retweets had the least amount of correlation
               only coming in with 19.3% correlation to approval rating." ),
               br(),
               p("In conclusion, it was established that there was some correlation between 
               favorites, retweets, volume of tweets and Trump's daily approval rating. 
               The volume of tweets seemed to have the most impact on the approval rating,
               with favorites trailing, and retweets being the least correlated with approval
               rating. It is important that while the figure of approximately twenty percent 
               may seem low it is reflective of a healthy amount of correlation. Had the 
               correlation been high, it would have shown that his Trump's approval rating
               is highly correlated with tweets and not other important factors such as policy 
               implementations and media coverage which also influence his approval rating. If
               his correlation had been lower it would have shown that his tweets have minimal 
               impact on his approval rating which would also be concerning because twitter is 
               an outlet for Trump to voice his public opinion on current issues and a near zero
               correlation would indicate that the public is in almost complete disregard and 
               indifferent to trump's twitter usage.")),
               tabPanel("Table",tableOutput("ayax_table"))
             ))
             
           )
           
           
           
           
           
           
           )
))