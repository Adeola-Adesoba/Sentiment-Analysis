# LIBRARIES ----
library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)
library(rtweet)
library(tidytext)
library(ggwordcloud)
library(tidyverse)
library(tidyquant)
#---- API SETUP FOR SHINY ----


source("scripts/geocode_for_free.R")

# SAVE YOUR TWITTER APP TOKEN AS AN RDS FILE
# YOU CAN THEN READ THE RDS FILE IF USED ON SHINYAPPS.IO

token <- create_token(
    
    # Replace with your Twitter app name
    app             = "Covid19twitteranalysis",
    
    # Replace with your App API Key
    
    consumer_key    = "some_key",
    
    # Replace with your App Secret Key
    
    consumer_secret = "some_secret"
)

token %>% write_rds("../my_twitter_token.rds")

token <- read_rds("../my_twitter_token.rds")


# ---- 1.0 UI ----
ui <- navbarPage(
    title = "Shiny Twitter",
    collapsible = TRUE,
    inverse     = TRUE, 
    theme       = shinytheme("paper"),
    
    shiny::tabPanel(
        title = "Hash Tag Tracker",
        sidebarLayout(
            sidebarPanel(
                shiny::textInput(inputId = "query", label = "Topic / Hashtag", value = "#COVID19"),
                sliderInput(
                    inputId = "n_tweets",
                    label   = "Number of tweets:",
                    min     = 1,
                    max     = 1500,
                    value   = 100),
                shiny::textInput(inputId = "location", label = "Location", value = "Little Rock, AR"),
                sliderInput(
                    inputId = "n_miles",
                    label   = "Twitter Search Radius (miles)",
                    min     = 1,
                    max     = 1500,
                    value   = 1000),
                shiny::actionButton(inputId = "submit", "Submit", class = "btn-primary")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                div(
                    class = "row",
                    div(
                        class = "col-sm-8 panel",
                        div(class = "panel-heading", h5("Sentiment Polarity")),
                        div(class = "panel-body", plotlyOutput(outputId = "plotly", height = "250px"))
                    ),
                    div(
                        class = "col-sm-4 panel",
                        div(class = "panel-heading", h5("Tweet Proximity")),
                        div(class = "panel-body", leafletOutput(outputId = "leaflet", height = 250))
                    )
                ),
                
                div(
                    class = "row",
                    div(
                        class = "col-sm-12 panel",
                        div(class = "panel-heading", h5("Sentiment Word Cloud")),
                        div(class = "panel-body", plotOutput(outputId = "wordcloud", height = "400px"))
                    )
                )
            )
        )
        
    )
    
)


# ---- 2.0 SERVER ----
server <- function(session, input, output) {
    
    # 2.1 Setup Reactive Values ----
    rv <- reactiveValues()
    
    observeEvent(input$submit, {
        # Process data
        
        rv$geocode <- input$location %>% geocode_for_free() %>% near_geocode(input$n_miles)
        
        rv$data <-  search_tweets(
            q           = input$query, 
            n           = input$n_tweets, 
            include_rts = FALSE, 
            geocode     = rv$geocode,
            lang        = "en",
            token       = token
        )
        
        rv$tweet_sentiment <- rv$data %>%
            select(text) %>%
            rowid_to_column() %>%
            unnest_tokens(word, text) %>%
            inner_join(get_sentiments("bing"))
        
    }, ignoreNULL = FALSE)
    
    # output$code <- renderPrint({
    #     # Used for debugging
    #     req(rv$data, nrow(rv$data) > 1)
    #     
    #     rv$data
    # })
    
    # 2.2 Plotly ----
    output$plotly <- renderPlotly({
        req(rv$tweet_sentiment, rv$data) 
        
        sentiment_by_row_id_tbl <- rv$tweet_sentiment %>%
            select(-word) %>%
            count(rowid, sentiment) %>%
            pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
            mutate(sentiment = positive - negative) %>%
            left_join(
                rv$data %>% select(screen_name, text) %>% rowid_to_column()
            ) 
        
        label_wrap <- label_wrap_gen(width = 60)
        
        data_formatted <- sentiment_by_row_id_tbl %>%
            mutate(text_formatted = str_glue("Row ID: {rowid}
                                     Screen Name: {screen_name}
                                     Text: 
                                     {label_wrap(text)}"))
        
        g <- data_formatted %>%
            ggplot(aes(rowid, sentiment)) +
            geom_line(color = "#2c3e50", alpha = 0.5) +
            geom_point(aes(text = text_formatted), color = "#2c3e50") +
            geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
            geom_hline(aes(yintercept = mean(sentiment)), color = "blue") +
            geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "red") +
            geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "red") +
            theme_tq() +
            labs(x = "Twitter User", y = "Sentiment")
        
        ggplotly(g, tooltip = "text") %>%
            layout(
                xaxis = list(
                    rangeslider = list(type = "date")
                )
            )
        
    })
    
    # 2.3 Leaflet -----
    output$leaflet <- renderLeaflet({
        
        req(rv$geocode)
        
        data_prepared <- tibble(
            location = rv$geocode
        ) %>%
            separate(location, into = c("lat", "lon", "distance"), sep = ",", remove = FALSE) %>%
            mutate(distance = distance %>% str_remove_all("[^0-9.-]")) %>%
            mutate_at(.vars = vars(-location), as.numeric) 
        
        data_prepared %>%
            leaflet() %>%
            setView(data_prepared$lon, data_prepared$lat, zoom = 3) %>%
            addTiles() %>%
            addMarkers(~lon, ~lat, popup = ~as.character(location), label = ~as.character(location)) %>%
            addCircles(lng = ~lon, lat = ~lat, weight = 1, radius = ~distance/0.000621371)
        
    })
    
    # 2.4 Wordcloud ----
    output$wordcloud <- renderPlot({
        
        req(rv$data)
        
        tweets_tokenized_tbl <- rv$data %>%
            select(text) %>%
            rowid_to_column() %>%
            unnest_tokens(word, text)
        
        sentiment_bing_tbl <- tweets_tokenized_tbl %>%
            inner_join(get_sentiments("bing"))
        
        sentiment_by_word_tbl <- sentiment_bing_tbl %>%
            count(word, sentiment, sort = TRUE) 
        
        sentiment_by_word_tbl %>%
            slice(1:100) %>%
            mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
            ggplot(aes(label = word, color = sentiment, size = n)) +
            geom_text_wordcloud_area() + 
            facet_wrap(~ sentiment, ncol = 2) +
            theme_tq(base_size = 30) +
            scale_color_tq() +
            scale_size_area(max_size = 16) 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
