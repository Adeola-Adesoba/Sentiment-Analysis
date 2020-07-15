# TEXT MINING & SENTIMENT ANALYSIS ----


# 1.0 LIBRARIES ----

# Text
library(tidytext)    # Tidy text mining
library(textdata)    # Needed for AFINN

# Visualization
library(plotly)
library(ggwordcloud) # Extension for wordclouds

# Core
library(tidyverse)
library(tidyquant)

# 2.0 TIDY TEXT ----

tweets_covid19 = read_rds("tweets_covid19.rds")
tweets_covid19 %>% glimpse()

# Tidy the data
tweets_tokenized_tbl <- tweets_covid19 %>%
    select(text) %>%
    rowid_to_column() %>%
    unnest_tokens(word, text)

tweets_tokenized_tbl %>% glimpse()  

tweets_tokenized_tbl %>% count(word, sort = TRUE)

# 3.0 SENTIMENT ANALYSIS ----

# 3.1 Sentiment Dictionaries 

get_sentiments(lexicon = "bing")  # CategoricalPositive / Negative 

get_sentiments(lexicon = "afinn") # Assigns polarity

# 3.2 Joining Sentiment Dictionaries with Tokenized Text

sentiment_bing_tbl <- tweets_tokenized_tbl %>%
    inner_join(get_sentiments("bing"))

sentiment_bing_tbl

# 3.3 Measuring Sentiment

# Overall Sentiment
sentiment_bing_tbl %>% count(sentiment)

# Sentiment by user
sentiment_by_row_id_tbl <- sentiment_bing_tbl %>%
    select(-word) %>%
    count(rowid, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
    mutate(sentiment = positive - negative) %>%
    left_join(
        tweets_covid19 %>% select(screen_name, text) %>% rowid_to_column()
    ) 

sentiment_by_row_id_tbl

# 4.0 POLARITY VISUALIZATION -----

label_wrap <- label_wrap_gen(width = 60)

data_formatted <- sentiment_by_row_id_tbl %>%
    mutate(text_formatted = str_glue("Row ID: {rowid}
                                     Screen Name: {screen_name}
                                     Text: 
                                     {label_wrap(text)}"))
data_formatted

g <- data_formatted %>%
    ggplot(aes(rowid, sentiment)) +
    
    geom_line(color = "#2c3e50", alpha = 0.5) +
    geom_point(aes(text = text_formatted), color = "#2c3e50") +
    
    geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
    
    geom_hline(aes(yintercept = mean(sentiment)), color = "blue") +
    geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "red") +
    geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "red") +
    theme_tq() +
    labs(title = "Sentiment Polarity", x = "Twitter User", y = "Sentiment")
g

ggplotly(g, tooltip = "text") %>%
    layout(
        xaxis = list(
            rangeslider = list(type = "date")
        )
    )
# 5.0 WORDCLOUD -----
        
sentiment_by_word_tbl <- sentiment_bing_tbl %>%
    count(word, sentiment, sort = TRUE) 

sentiment_by_word_tbl

word_freq = sentiment_by_word_tbl %>%
    slice(1:100) %>%
    mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
    ggplot(aes(label = word, color = sentiment, size = n)) +
    geom_text_wordcloud_area() + 
    facet_wrap(~ sentiment, ncol = 2) +
    theme_tq() +
    scale_color_tq() +
    scale_size_area(max_size = 12) +
    labs(title = "Word Cloud")
word_freq

