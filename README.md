# INFO 201 Course Project

**Group:** *E1*

**Members:** *Benjamin Carney, Troy Lu, Ayax Franco, Lance Haugen*

This repo will contain the code and data for a course project
for the _Technical Foundations of Informatics_ course at the UW iSchool.

## Section 1:

For our project, we decided to investigate the domain of social media and its impact on U.S. politics, specifically focusing on the presidency. Within this domain, we intend on analyzing former president Donald Trump’s Twitter account and all of his “tweets”. Twitter is a widely used social media platform where users can write 280-character long messages (in the form of “tweets”) for other Twitter users to read. When other Twitter users view a tweet, they have the option to “like”, reply to the tweet, or “quote-retweet” (retweeting with an added comment); all of these options count as an “interaction”, so the number of interactions that a tweet has is an indicator of how many people legitimately viewed it. The president is given an official Twitter account under the alias **@POTUS** and has the ability to adopt “followers” from the previous president’s Twitter account. When Trump became president in 2016, he took the “followers”, giving the Twitter account a large boost in interactions. Throughout his presidency, however, he still used his personal account **@realDonaldTrump**, which we will be analyzing for this project.

The reason why our group chose this domain was because during Donald Trump’s presidency, there were thousands of tweets published under his name, often gathering millions of interactions and the attention of the media. With more and more people gaining access to the internet each day, social media is quickly becoming another way for U.S. politics to reach and influence people. There are a few existing projects available online that analyze Trump’s Twitter accounts; some focus on his [tweeting routine](https://www.brandwatch.com/blog/react-realdonaldtrump-vs-potus/) while others studied the contents of the tweets, e.g., [criticizing certain members of Congress](https://www.npr.org/2019/10/10/768646968/as-summer-heated-up-trumps-tweets-about-non-white-democrats-intensified).


## Section 2:

### Analysis Q1:
**Did Trump’s weighted tweets affect public opinion over the course of his presidency? If so, to what extent?**

This question lets us quantify the weight of Trump’s tweets, by the amount of public response for each tweet. This will be measured by the sum of the retweets and favorites (with retweets having a higher degree of weight than favorites). Using this data we can build a timeline of public opinion/approval rating and compare that to the varying weights each tweet of and attempt to establish a potential correlation between Trump’s most popular tweets and his forgotten ones. This question is significant because it allows us to measure the influence that social media can have on the publics opinions regarding presidency.


### Analysis Q2:
 **When were Trump's approval ratings at all time lows and at all time highs and how much was he tweeting in that time period? What major events were happening in that time period?**

 This would allow us to use our approval ratings data to address the possibility of correlation vs causation. Without addressing the possibility of correlation and not causation we may naively make the conclusion that any time Trump tweeted it negatively or positively affected his approval ratings. This question is significant because it allows us to analyze the context behind the tweets and what major events were going on at the time to check the existence of a connection between the two.

### Analysis Q3:
**Did Trump’s social media usage change the approval ratings of his voters? If so, by how much and what indicators point towards the change?**

The question allows us to explore multiple avenues to answer it, such as the number of published tweets about a specific topic or a pattern in how many tweets are published/deleted per month. With this direct comparison, we are able to analyze the impact social media has on people’s perception of U.S. politics. We will be analyzing factors such as the approval rating timeline compared to his social media usage during those times. This question is open-ended which allows us to view possible relationships between the two factors and whether they exist. With our findings we can analyze further ideas on the effect of social media in the modern world.

### Analysis Q4:
**During his presidency, were the legal issues that Trump faced an indicator of the public perception of him? If so, to what degree?**

This question allows us to explore the effect that lawsuits have on a president at the time. It will be interesting to see how different lawsuits affect his approval rating in different ways. We can answer this question by looking at both Trump’s approval rating over time and a data set of all of Trump’s lawsuits. With this comparison, we can help determine whether the tweets made were a direct causation or correlation with his approval rating. This question allows us to determine the validity of other questions as a side point to consider when analyzing the change in approval rating.




## Section 3
### Data Set 1: Twitter Archive:
The data set contains an archive of Donald Trump’s tweets during his presidency, beginning in September 2016 and ending on January 8th 2021 upon his ban from Twitter. The data set is composed of 56,572 tweets along with timestamps of when the tweets were posted as well as retweets/favorites for each tweet. All Tweets before September 2016 were added manually. The source of this data set was created by Brendan Patrick Brown, a programmer/engineer from Georgetown University.
The following site contains the link to the [archive](https://www.thetrumparchive.com): and the [GitHub repository](https://github.com/bpb27/tta-elastic):
Tweets were captured on a 60 second time interval and were recorded into the database.
The data set is credible and has been cited by “FactCheck.org, PolitiFact, Snopes, and Wikipedia” among other major news media. This data set will be quantitatively helpful in identifying correlations and patterns among Trump’s tweets and analyzing the effect those tweets have on public opinion.


### Data Set 2: Approval Rating Data:
This data set contains the results of different poll results over his term.
The data was retrieved from the FiveThirtyEight [GitHub repository](https://github.com/fivethirtyeight/data/tree/master/trump-approval-ratings).
This data was compiled by the opinion poll analyst organization, FiveThirtyEight. We do not know necessarily how the data was collected for each poll that FiveThirtyEight analyzed, as they were collected from various websites, however we do know FiveThirtyEight has a history of credibility and strict quality when analyzing datasets and poll results. This data set will help when answering all of our questions as a major theme in our analysis is public perception of the previous president.

### Data Set 3: Trump Lawsuits Data:
The data set contains all of Trump’s lawsuits leading up to May 31st, 2019.
The data was retrieved from FiveThirtyEight’s [GitHub repository](https://github.com/fivethirtyeight/data/blob/master/trump-lawsuits/trump-lawsuits.csv)
and was collected by the same company. The data for this CSV file was collected by datamining the internet as this is public information available to everyone. This helps with Question #4 as it provides the data for all of his lawsuits along with their date so we can see how they affect the approval rating trendline above.
