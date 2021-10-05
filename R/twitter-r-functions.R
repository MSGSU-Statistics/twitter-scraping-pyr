library (rtweet)
library(dplyr)
library(stopwords)
library(wordcloud)
library(tidytext)
library(ggplot2)
library(jsonlite)


twitter_token <- readRDS("tokens.rds")


get_user_timeline <- function(query) {
  timeline <- get_timeline(paste0("@", query), n = 3200)
  
  print(dim(timeline))
  return(timeline)
}

get_tweets <- function(query,
                       language = "tr",
                       count = 1000) {
  tweets <- search_tweets(query,
                          n = count,
                          include_rts = FALSE,
                          lang = language)
  print(dim(tweets))
  return(tweets)
}

user <- "msgsuniversite"
timeline <- get_user_timeline(user)

q <- "istatistik"
query <- get_tweets(q)


function(tweets) {
  if (!is.data.frame(tweets))
    stop("input data frame olmali!")
  organic_tweets <-  tweets[tweets$is_retweet == FALSE,]
  
  
}

summarizer <- function(tweets, timeline = T) {
  if (timeline == T) {
    data <-
      tweets %>% group_by(is_retweet, is_quote) %>% summarise(n = n()) %>% as.data.frame() %>%
      select(n) %>% data.frame() %>% bind_cols("category" = c("Organic", "Retweets", "Replies"))
  }
  else {
    data <-
      tweets %>% group_by(is_retweet, is_quote) %>% summarise(n = n()) %>% as.data.frame() %>%
      select(n) %>% data.frame() %>% bind_cols("is_quote" = c("True", "False"))
  }
  
  return(data)
}


timeline_sum <- summarizer(timeline)
query_sum <- summarizer(query, F)



timeline_viz <- function(df) {
  df$fraction = df$n / sum(df$n)
  df$percentage = df$n / sum(df$n) * 100
  df$ymax = cumsum(df$fraction)
  df$ymin = c(0, head(df$ymax, n = -1))
  
  tweet_types <-
    paste(df$category, "-", round(df$percentage, 2), "%")
  ggplot(df,
         aes(
           ymax = ymax,
           ymin = ymin,
           xmax = 4,
           xmin = 3,
           fill = tweet_types
         )) +
    geom_rect() +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(legend.position = "right")
  
  
}

timeline_viz(timeline_sum)
timeline_viz(query_sum)


time_plots <- function(df, user, time_span = "year") {
  colnames(df)[colnames(df) == "screen_name"] <- user
  ts_plot(group_by(df, "user"), time_span) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = sprintf("Frequency of Tweets from @%s", user),
      subtitle = "Tweet counts aggregated by year",
      caption = "\nSource: Data collected from Twitter's REST API via rtweet"
    )
}


time_plots(timeline, user)
time_plots(timeline, user, "month")


app_viz <- function(df){
  df_app <- df %>%
    select(source) %>%
    group_by(source) %>%
    summarize(count = n())
  
  print(df_app)
  
  data <- data.frame(category = df_app$source,
                     count = df_app$count)
  data$fraction = data$count / sum(data$count)
  data$percentage = data$count / sum(data$count) * 100
  data$ymax = cumsum(data$fraction)
  data$ymin = c(0, head(data$ymax, n = -1))
  
  Source <- paste(data$category, "-", "%", round(data$percentage, 3))
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
  
}


app_viz(timeline)

