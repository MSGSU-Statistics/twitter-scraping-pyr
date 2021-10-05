# https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html

library (rtweet)
library(dplyr)
library(stopwords)
library(wordcloud)
library(tidytext)
library(ggplot2)
library(jsonlite)

# creds <- fromJSON("creds.json")
# twitter_token <- create_token(
#   app = creds$app_name,
#   consumer_key = creds$consumerKey,
#   consumer_secret = creds$consumerSecret,
#   access_token = creds$access_token,
#   access_secret = creds$access_token_secret,
#   set_renv = TRUE
# )
# get_tokens()

# saveRDS(twitter_token, "tokens")

twitter_token <- readRDS("tokens.rds")
# https://twitter.com/search-advanced
# https://developer.twitter.com/en/docs/twitter-api/v1/tweets/search/api-reference/get-search-tweets

# son 6-9 gun arasi ve maksimum 18 bin tweet aranabilir
ist <- search_tweets("istatistik",
                n = 1000,
                include_rts = FALSE,
                lang = "tr")

head(as.data.frame(ist))
dim(ist)

# kullanicidan maks 3200 tweet alinabiliyor
msgsu <- get_timeline("@msgsuniversite", n = 3200)
dim(msgsu)

# Remove retweets
msgsu_tweets_organic <-
  msgsu[msgsu$is_retweet == FALSE,]

# Remove replies
msgsu_tweets_organic <-
  subset(msgsu_tweets_organic,
         is.na(msgsu_tweets_organic$reply_to_status_id))

msgsu_tweets_organic <-
  msgsu_tweets_organic %>% arrange(-favorite_count)


msgsu_tweets_organic <-
  msgsu_tweets_organic %>% arrange(-retweet_count)

head(msgsu_tweets_organic)


# Creating a data frame
# data <- data.frame(
#   category = c("Organic", "Retweets", "Replies"),
#   count = c(2856, 192, 120)
# )

data <-
  msgsu %>% group_by(is_retweet, is_quote) %>% summarise(n = n()) %>% as.data.frame() %>%
  select(n) %>% data.frame() %>% bind_cols("category" = c("Organic", "Retweets", "Replies"))

data

# Adding columns
data$fraction = data$n / sum(data$n)
data$percentage = data$n / sum(data$n) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n = -1))

data

# Specify what the legend should say
Type_of_Tweet <- paste(data$category, data$percentage, "%")
ggplot(data,
       aes(
         ymax = ymax,
         ymin = ymin,
         xmax = 4,
         xmin = 3,
         fill = Type_of_Tweet
       )) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# SHOW THE RATIO OF REPLIES/RETWEETS/ORGANIC TWEETS ####

colnames(msgsu)[colnames(msgsu) == "screen_name"] <- "Twitter_Account"
ts_plot(group_by(msgsu, Twitter_Account), "year") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL,
    y = NULL,
    title = "Frequency of Tweets from @msgsuniversite",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

ts_plot(group_by(msgsu, Twitter_Account), "month") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL,
    y = NULL,
    title = "Frequency of Tweets from @msgsuniversite",
    subtitle = "Tweet counts aggregated by month",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# SHOW FROM WHERE THE TWEETS ARE PUBLISHED ####

msgsu_app <- msgsu %>%
  select(source) %>%
  group_by(source) %>%
  summarize(count = n())

msgsu_app
# msgsu_app <- subset(msgsu_app, count > 11)

data <- data.frame(category = msgsu_app$source,
                   count = msgsu_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n = -1))

Source <- paste(data$category,"-", "%",round(data$percentage, 3))
ggplot(data, aes(
  ymax = ymax,
  ymin = ymin,
  xmax = 4,
  xmin = 3,
  fill = Source
)) +
  geom_rect() +
  coord_polar(theta = "y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# SHOW THE MOST FREQUENT WORDS FOUND IN THE TWEETS ####
# regex 101
# https://regex101.com
msgsu_tweets_organic$text <-
  gsub("https\\S*", "", msgsu_tweets_organic$text) # https iceren metin cikariliyor
msgsu_tweets_organic$text <-
  gsub("@\\S*", "", msgsu_tweets_organic$text)
msgsu_tweets_organic$text  <-  gsub("amp", "", msgsu_tweets_organic$text)
msgsu_tweets_organic$text  <-
  gsub("[\r\n]", " ", msgsu_tweets_organic$text)
msgsu_tweets_organic$text  <-
  gsub("[[:punct:]]", "", msgsu_tweets_organic$text)

tweets <- msgsu_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)

sw <- stopwords::stopwords("tr", source = "stopwords-iso")

# tweets <- tweets %>%
#   anti_join(stop_words)

tweets <- tweets %>%
  filter(!word %in% sw)


tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(
    y = "Count",
    x = "Unique words",
    title = "Most frequent words found in the tweets of @msgsuniversite",
    subtitle = "Stop words removed from the list"
  )

# SHOW THE MOST FREQUENTLY USED HASHTAGS ####
set.seed(2021)
msgsu_tweets_organic$hashtags <-
  as.character(msgsu_tweets_organic$hashtags)

msgsu_tweets_organic$hashtags <-
  gsub("c\\(", "", msgsu_tweets_organic$hashtags)

wordcloud(
  msgsu_tweets_organic$hashtags,
  min.freq = 5,
  scale = c(3.5, .5),
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2"),
  # use.r.layout = F
)

# SHOW THE ACCOUNTS FROM WHICH MOST RETWEETS ORIGINATE ####
wordcloud(
  msgsu$retweet_screen_name,
  min.freq = 3,
  scale = c(2, .5),
  random.order = FALSE,
  rot.per = 0.25,
  colors = brewer.pal(8, "Dark2")
)


# PERFORM A SENTIMENT ANALYSIS OF THE TWEETS ####
library(syuzhet)
# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(tweets,
                from = "UTF-8",
                to = "ASCII",
                sub = "")

# removing retweets, in case needed
tweets <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)", "", tweets)

# removing mentions, in case needed
tweets <- gsub("@\\w+", "", tweets)

ew_sentiment <- get_nrc_sentiment((tweets))
sentimentscores <- data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"

sentimentscores <-
  cbind("sentiment" = rownames(sentimentscores), sentimentscores)

rownames(sentimentscores) <- NULL
ggplot(data = sentimentscores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiments") + ylab("Scores") +
  ggtitle("Total sentiment based on scores") +
  theme_minimal()
