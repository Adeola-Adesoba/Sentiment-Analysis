# Twitter API
library(rtweet)    

# Interactive Maps
library(tmaptools)
library(leaflet)  

# Core
library(tidyverse)

source("scripts/geocode_for_free.R")
token <- create_token(
    
    # Replace with your Twitter app name
    app             = "Covid19twitteranalysis", 
    
    # Replace with your App API Key 
    consumer_key    = "some_key", 
    
    # Replace with your App Secret Key
    consumer_secret = "some_secret" 
)

#search_tweets
 search_tweets()
tweets_covid19 = rtweet::search_tweets(
    q = "#COVID19",      # Search query
    n = 2000,             # Number of results
    lang = "en",         # Language
    include_rts = FALSE  # Don't include retweets if want unique tweets
)
glimpse(tweets_covid19)
tweets_covid19 %>% write_rds(path = "tweets_covid19.rds")
                                 #"data/frontline/tweets_covid19.rds")

# 1994 Tweets related to COVID, Downloaded on May 12, 2020
tweets_covid19 <- read_rds("tweets_covid19.rds")
  #  "data/frontline/tweets_covid19.rds")
#"C:\Users\adeol\Desktop\data\tweets_covid19.rds"
# 2.2 Results 

tweets_covid19 %>% glimpse()

# User info
tweets_covid19 %>% slice(1:5) %>% select(screen_name, location, description) 

# Tweet info
tweets_covid19 %>% slice(1:5) %>% select(text, url)

# Hashtags info
tweets_covid19 %>% slice(1:5) %>% select(hashtags) %>% unnest_wider(hashtags)

# URL's in the Tweet
tweets_covid19 %>% slice(1:5) %>% select(urls_expanded_url) %>% unnest(urls_expanded_url)


# - Real-time twitter action of what people are talking about

rt <- stream_tweets(timeout = 5)

rt %>% glimpse()


# Geocoding Coordinates
lookup_coords("london, uk") # Requires Google Maps API (Costs)
lookup_coords("usa") # Pre-programmed


# Geocoding Function
geocode_for_free("london, uk") 
geocode_for_free("usa") 

# Apply to streaming tweets
#rt <- stream_tweets(geocode_for_free("usa"), timeout = 5)
#rt

#rt %>% glimpse()

rt <- stream_tweets(geocode_for_free("london, uk"), timeout = 5)
rt

rt %>% glimpse()

# Apply to search tweets london

st <- search_tweets(
    q = "#COVID19", 
    n = 500, 
    include_rts = FALSE, 
    lang = "en",
    geocode = geocode_for_free("london, uk") %>% near_geocode(100)
)

st %>% glimpse()

st %>%
    select(contains("coords")) %>%
    unnest_wider(geo_coords) %>%
    filter(!is.na(...1))

#Apply to search tweets usa

st_usa <- search_tweets(
    q = "#COVID19", 
    n = 300, 
    include_rts = FALSE, 
    lang = "en",
    geocode = geocode_for_free("usa") %>% near_geocode(100)
)

st_usa %>% glimpse()

st_usa %>%
    select(contains("coords")) %>%
    unnest_wider(geo_coords) %>%
    filter(!is.na(...1))

# MAP

quakes[1:20,] %>%
    leaflet() %>% 
    addTiles() %>%
    addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))
quakes[1:20,]

# Mapping our Tweets

st %>%
    select(screen_name, text, coords_coords) %>%
    unnest_wider(coords_coords) %>%
    filter(!is.na(...1)) %>%
    set_names(c("screen_name", "text", "lon", "lat")) %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(~lon, ~lat, popup = ~as.character(text), label = ~as.character(screen_name))

# Use a Circle to indicate location of tweets

data_prepared <- tibble(
    location = geocode_for_free("london, uk") %>% near_geocode(100)
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

