# Install and load require packages
install.packages(c("SuperLearner", "ipred", "adabag", "randomForest", "dplyr", "e1071", "nnet",
                   "tidytext", "rtweet", "ggplot2", "tidyverse", "gridExtra", "tidyr", "plyr", 
                   "randomForest", "kernlab", "neuralnet", "ggrepel", "translateR", "gtrendsR",
                   "MASS", "DMwR", "C50", "fUnitRoots", "radarchart", "DT", "tsoutliers", "tseries",
                   "MTS", "vars", "syuzhet", "forecastML", "twitteR", "wordcloud2", "plotly",
                   "BBmisc", "grid", "ggplotify", "UBL", "reshape2", "ROAuth", "caret", "kernlab",
                   "cowplot", "mice", "mlr", "ranger", "ROSE", "C50", "gmodels", "RTextTools",
                   "Boruta", "tidyverse", "ROAuth", "gtrendsR", "plyr", "maps", "ggrepel", "gbm",
                   "translateR", "tidyr", "tm", "wordcloud2", "qdap", "radarchart", "MASS", "shiny",
                   "caret", "class", "cowplot", "ggmap", "forecast", "partykit", "party", "RTextTools",
                   "flexdashboard", "textdata", "tsoutliers", "plotly", "Hmisc"))
# Load librarieslibrary(tsoutliers)7
loading <- c("SuperLearner", "ipred", "adabag", "randomForest", "e1071", "nnet", "tsoutliers",
             "tidytext", "rtweet", "ggplot2", "tidyverse", "gridExtra", "tidyr", "plotly",
             "randomForest", "kernlab", "neuralnet", "ggrepel", "translateR", "gtrendsR",
             "MASS", "DMwR", "C50", "fUnitRoots", "radarchart", "DT", "tsoutliers", "tseries",
             "MTS", "vars", "syuzhet", "forecastML", "twitteR", "wordcloud2", "plotly",
             "BBmisc", "grid", "ggplotify", "UBL", "reshape2", "ROAuth", "caret", "kernlab",
             "cowplot", "mice", "mlr", "ranger", "ROSE", "C50", "gmodels", "textdata",
             "Boruta", "tidyverse", "ROAuth", "gtrendsR", "dplyr", "maps", "ggrepel", "gbm",
             "translateR", "tidyr", "tm", "wordcloud2", "qdap", "radarchart", "MASS", "shiny",
             "caret", "class", "cowplot", "ggmap", "forecast", "partykit", "party", "flexdashboard")
lapply(loading, require, character.only = TRUE)
setwd("D:\\1 Masters\\Dataset\\trainingandtestdata")

# Scrape and analyst data from Google trends
vaccine_line <- gtrends(c("vaccine", "vaccination", "swine", "covid-19"), geo = "US", time = "all")
plot.gtrends.silent <- function(x, ...) {df <- x$interest_over_time
df$date <- as.Date(df$date) 
df$hits <- if(typeof(df$hits) == 'character'){ 
  as.numeric(gsub('<','',df$hits)) } else { df$hits} 
df$legend <- paste(df$keyword, " (", df$geo, ")", sep = "") 
p <- ggplot(df, aes_string(x = "date", y = "hits", color = "legend")) + 
  geom_line(size = 2) + 
  xlab("Date") + 
  ylab("Search hits") + 
  ggtitle("") + 
  theme_bw() 
invisible(p)}
Project_Theme = theme(
  axis.title.x = element_text(size = 19),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 22),
  legend.text = element_text(color = "black", size = 15),
  plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))
my_plot <- plot.gtrends.silent(vaccine_line) 
my_plot + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + Project_Theme +
  ggtitle("Google Search Interest (US)")

# side effects interest 
other_line <- gtrends(c("immunization", "mainstream media",
                        "pharmaceutical industry", "vaccine"), geo = "US", time = "all")
other_plot <- plot.gtrends.silent(other_line) 
other_plot + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + Project_Theme +
  ggtitle("Google Search Interest in Vaccine Related Topics Over Time (US)")

# media and pharmaceutical companies 
side_effects_line <- gtrends(c("vaccine side effects", "thimerosal", "vaccines autism",
                               "vaccine autism link"), geo = "US", time = "all")
side_effects_plot <- plot.gtrends.silent(side_effects_line) 
side_effects_plot + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + Project_Theme +
  ggtitle("Google Search Interest in Vaccine Side Effects Over Time (US)")

# Interest in vaccines by state
my_theme <- function() {theme_bw() + theme(panel.background = element_blank(),
                plot.background = element_rect(fill = "seashell"), panel.border = element_blank(),
                strip.background = element_blank(), plot.margin = unit(c(.5, .5, .5, .5), "cm"), 
                panel.spacing = unit(3, "lines"), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), legend.background = element_blank(),
                legend.key = element_blank(), legend.title = element_blank())}
my_theme2 <- function() {
  my_theme() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())}
gtrends_vaccine <- gtrends(c("vaccine"), time  = "today 12-m", geo = c("US"))
gtrends_vaccine <- gtrends_vaccine$interest_by_region
statesMap    <- map_data("state")
gtrends_vaccine$region  <- sapply(gtrends_vaccine$location,tolower)
final_gtrends   <- merge(statesMap ,gtrends_vaccine,by="region")
regionLabels <- aggregate(cbind(long, lat) ~ region, data=final_gtrends, 
                          FUN=function(x) mean(range(x)))
final_gtrends %>% ggplot() + geom_polygon(aes(x=long, y=lat,
  group=group, fill = hits)) + 
  guides(fill = "colorbar") + geom_text_repel(data = regionLabels,
  aes(long, lat, label = region), size = 5) + scale_fill_distiller(palette =
  "Reds") + my_theme2() + theme(legend.position = "none") + theme(plot.title =
  element_text(hjust = 0.5)) + coord_fixed(1.3) + 
  ggtitle("Google search interest for 'vaccine' in the US over the last 12 months")

setwd("D:\\1 Masters\\Dataset\\Final Datasets")
gtrends_iot <- vaccine_line$interest_over_time
write.csv(gtrends_iot, 'gtrends.csv') 

# Twitter Analysis 
setup_twitter_oauth('jNjVkpRdmRceYEiZyO33t8CXu', # api key
                    'HdIKY3YgNRxNLVwEK4Qz4gX0YdmpoWKUUrnICXcfxhmPCFEJl1',
                    '1051535732777648129-YUDQFUzojiCNl2iQ3xPpyAIOmDM3bB',
                    'r2u3LMszcp2fWu7NtDBIFsJim19i10MvzHC8CX9HQp9Bv')
sanangelo <- search_tweets('vax OR vaccine OR vaccinated OR vaccination OR immunization OR immunized',
                           geocode = "31.4638,-100.4370,127mi", include_rts = FALSE)
sanangelo$place <- 'San Angelo'
amarillo <- search_tweets('vax OR vaccine OR vaccinated OR vaccination OR immunization OR immunized',
                          geocode = "35.2220,-101.8313,73mi", include_rts = FALSE)
amarillo$place <- 'Amarillo'
Lubbock <- search_tweets('vax OR vaccine OR vaccinated OR vaccination OR immunization OR immunized', 
                         geocode = "33.5779,-101.8552,49mi", n=1000, include_rts = FALSE)
Lubbock$place <- 'Lubbock'
Austin <- search_tweets('vax OR vaccine OR vaccinated OR vaccination OR immunization OR immunized',
                        geocode = "30.2672,-97.7431,57mi", n=1000, include_rts = FALSE)
Austin$place <- 'Austin'
Houston <- search_tweets('vax OR vaccine OR vaccinated OR vaccination OR immunization OR immunized',
                         geocode = "29.7604,-95.3698,62mi", n=20000, include_rts = FALSE)
Houston$place <- 'Houston'
SevenSisters <- search_tweets('vax OR vaccine OR vaccinated OR vaccination OR immunization OR immunized',
                              geocode = "28.0106,-98.5392,104mi", n=1000, include_rts = FALSE)
SevenSisters$place <- 'Seven Sisters'
Athens <- search_tweets('vax OR vaccine OR vaccinated OR vaccination OR immunization OR immunized',
                        geocode = "32.2049,-95.8555,104mi", n=2000, include_rts = FALSE)
Athens$place <- 'Athens'
combineddataframe <- rbind(Athens,SevenSisters,Houston,Austin,Lubbock,amarillo,sanangelo)
combineddataframe$text<- na.omit(combineddataframe$text)
combineddataframe = combineddataframe[!duplicated(combineddataframe$text),]

sp_sanangelo <- search_tweets('vacuna', geocode = "31.4638,-100.4370,127mi", include_rts = FALSE)
sp_sanangelo$place <- 'San Angelo'
sp_amarillo <- search_tweets('vacuna', geocode = "35.2220,-101.8313,73mi", include_rts = FALSE)
sp_amarillo$place <- 'Amarillo'
sp_Lubbock <- search_tweets('vacuna', geocode = "33.5779,-101.8552,49mi", n=1000, include_rts = FALSE)
sp_Lubbock$place <- 'Lubbock'
sp_Austin <- search_tweets('vacuna', geocode = "30.2672,-97.7431,57mi", n=1000, include_rts = FALSE)
sp_Austin$place <- 'Austin'
sp_Houston <- search_tweets('vacuna', geocode = "29.7604,-95.3698,62mi", n=20000, include_rts = FALSE)
sp_Houston$place <- 'Houston'
sp_SevenSisters <- search_tweets('vacuna', geocode = "28.0106,-98.5392,104mi", n=1000, include_rts = FALSE)
sp_SevenSisters$place <- 'Seven Sisters'
sp_Athens <- search_tweets('vacuna', geocode = "32.2049,-95.8555,104mi", n=2000, include_rts = FALSE)
sp_Athens$place <- 'Athens'
spanish_tweets <- rbind(sp_Athens, sp_SevenSisters, sp_Houston, sp_Austin,
                        sp_Lubbock, sp_amarillo, sp_sanangelo)
spanish_tweets <- spanish_tweets[!duplicated(spanish_tweets$text),]
spanish_tweets$text<- na.omit(spanish_tweets$text)
spanish_tweets[,5, drop = FALSE]

# translate spanish tweets
translated <- translate(dataset = spanish_tweets, content.field = 'text',
                        google.api.key = "AIzaSyAGMlOFArvFwjFJOcbRY5JVNdq8jAoQLj0", 
                        source.lang = 'es', target.lang = 'en')
colnames(combineddataframe)
colnames(translated)
comb <- combineddataframe[ ,c(3, 13, 14, 17, 32, 91, 5)]
tTweets <- translated[ ,c(3, 13, 14, 17, 32, 91, 92)]
colnames(tTweets)
colnames(comb)
tTweets[ ,7, drop = FALSE]

colnames(tTweets)[7] <- "text"
final_texas_tweets <- rbind(tTweets, comb)
head(final_texas_tweets)

# convert to dataframe with all characters to write to csv
texas_tweets_df <- final_texas_tweets
texas_tweets_df2 = data.frame(lapply(texas_tweets_df, as.character), stringsAsFactors=FALSE)
setwd("D:\\1 Masters\\Dataset\\Project Datasets")
todays_date <- Sys.Date()
output_name <- paste("Twitter ", todays_date,".csv", sep="")
write.csv(texas_tweets_df2, file = output_name)

# from https://rtweet.info/ 
p <- final_texas_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "Date", y = "Number of Tweets",
    title = "Volume of Vaccine Related Tweets By Date")
w <- p + theme(axis.text=element_text(size=18),
               axis.title=element_text(size=18))
q <- w + theme(plot.title = element_text(size=20, face="bold"))
Texas_Tweets_Plot <- q + theme(plot.title = element_text(hjust = 0.5))
Texas_Tweets_Plot

# Change column names and classes
colnames(final_texas_tweets)
texasdf <- final_texas_tweets %>% dplyr::select(Date = created_at, Text = text,
                                         Favourite_Count = favorite_count, Number_of_Retweets = retweet_count,
                                         Language = lang, Hashtags = hashtags, Location = place)
selected <- c("Date", "Text", "Location", "Language", "Hashtags")
texasdf[selected] <- lapply(texasdf[selected], as.character)
sapply(texasdf, class)

# remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
texasdf$Text <- sapply(texasdf$Text, removeSpecialChars)
texasdf$Date <- as.Date(texasdf$Date)
texasdf$Text <- sapply(texasdf$Text, tolower)
fix.contractions <- function(doc) { 
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", "not", doc)
  doc <- gsub("'ll", "will", doc)
  doc <- gsub("'re", "are", doc)
  doc <- gsub("'ve", "have", doc)
  doc <- gsub("'m", "am", doc)
  doc <- gsub("'d", "would", doc)
  doc <- gsub("'s", "", doc)
  return(doc)}
texasdf$Text <- sapply(texasdf$Text, fix.contractions)
str(texasdf[50, ]$Text, nchr.max = 300)

#define some colors to use throughout
twitter_hue <- c("#AA4371", "#c00000", "#37004D", "#000080", "#008000")

theme_project <- function() 
{theme(plot.title = element_text(hjust = 0.5),
       axis.text.x = element_blank(), 
       axis.ticks = element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.position = "none")}

final_tweet_filtered <- texasdf %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  dplyr::filter(nchar(word) > 3)
p <- final_tweet_filtered %>%
  dplyr::count(word, sort = TRUE) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = twitter_hue[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("Word") + 
  ylab("Word Count") +
  ggtitle("Most Common Words in Texas Tweets") +
  coord_flip()
p + theme(axis.text=element_text(size=10),
               axis.title=element_text(size=10))

undesirable_words <- c("https", "vaccine", "time",
                       "news", "vaccinated", "vaccines",
                       "vaccination", "doesn", "months", "anti",
                       "stop", "ready", "world", "people")

final_tweet_filtered <- texasdf %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words) %>%
  dplyr::distinct() %>%
  dplyr::filter(!word %in% undesirable_words) %>%
  dplyr::filter(nchar(word) > 3)
r <- final_tweet_filtered %>%
  dplyr::count(word, sort = TRUE) %>%
  top_n(25) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = twitter_hue[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("Word") + 
  ylab("Word Count") +
  ggtitle("Most Common Words In Vaccine Related Texas Tweets") +
  coord_flip()
z <- r + theme(axis.text=element_text(size=18),
               axis.title=element_text(size=18))
z + theme(plot.title = element_text(size=20, face="bold"))

new_sentiments <- get_sentiments("afinn")
names(new_sentiments)[names(new_sentiments) == 'value'] <- 'score'
new_sentiments <- new_sentiments %>% mutate(lexicon = "afinn", 
                                            sentiment = ifelse(score >= 0, "positive", "negative"),
                                            words_in_lexicon = n_distinct((word)))

tweets_words_counts <- final_tweet_filtered %>% dplyr::count(word, sort = TRUE)
wordcloud2(tweets_words_counts[1:13000, ], size = .5)

texas_nrc <- final_tweet_filtered %>%
  inner_join(get_sentiments("nrc")) %>%
  dplyr::filter(!sentiment %in% c("positive", "negative"))

syuzhet_vector <- get_sentiment(final_tweet_filtered$word, method="nrc")
nrc_plot <- texas_nrc %>%
  group_by(sentiment) %>%
  dplyr::summarise(word_count = dplyr::n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + 
  theme_project() +
  labs(x = "Emotion", y = "Word Count") +
  scale_y_continuous(limits = c(0, 25000)) +
  ggtitle("Sentiment Of Vaccine Related Tweets In Texas") +
  coord_flip()
nrc_plot <- nrc_plot + theme(axis.text=element_text(size=18),
                             axis.title=element_text(size=18))
nrc_plot + theme(plot.title = element_text(size=20, face="bold"))

# Overall sentiment (positive or negative)
vaccine_bing <- final_tweet_filtered %>% inner_join(get_sentiments("bing"))
sent_texas <- vaccine_bing %>% group_by(sentiment) %>%
  dplyr::summarise(word_count = dplyr::n()) %>%
  ungroup() %>% mutate(sentiment = reorder(sentiment, word_count)) %>% 
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() + guides(fill = FALSE) + theme_project() + 
  labs(x = NULL, y = "Word Count") + scale_y_continuous(limits = c(0, 15000)) +
  ggtitle("Sentiment of Vaccine Related Tweets") + coord_flip()
sent_texas
#Get the count of words per sentiment for each area in Texas 
texas_sentiment_nrc <- texas_nrc %>%
  dplyr::group_by(Location, sentiment) %>%
  dplyr::count(Location, sentiment) %>%
  dplyr::select(Location, sentiment, sentiment_city_count = n)
texas_sentiment <- texas_nrc %>%
  dplyr::count(Location) %>%
  dplyr::select(Location, city_total = n)

#Join the two and create a percent field
city_radar_chart <- texas_sentiment_nrc %>%
  inner_join(texas_sentiment, by = "Location") %>%
  mutate(percent = sentiment_city_count / city_total * 100 ) %>%
  dplyr::filter(Location %in% c("Austin","Houston","San Angelo")) %>%
  dplyr::select(-sentiment_city_count, -city_total) %>%
  spread(Location, percent) %>%
  chartJSRadar(showToolTipLabel = TRUE,
               main = "Vaccine Related Texas Tweets Radar")
city_radar_chart

vaccine_bigrams <- texasdf %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2)
bigrams_separated <- vaccine_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  dplyr::filter(!word1 %in% stop_words$word) %>%
  dplyr::filter(!word2 %in% stop_words$word) %>%
  dplyr::filter(!word1 %in% undesirable_words) %>%
  dplyr::filter(!word2 %in% undesirable_words)

bigram_texas <- bigrams_filtered %>%
  dplyr::filter(word1 != word2) %>%
  dplyr::filter(Location != "NA") %>%
  unite(bigram, word1, word2, sep = " ") %>%
  inner_join(texasdf) %>%
  count(bigram, Location, sort = TRUE) %>%
  group_by(Location) %>%
  slice(seq_len(7)) %>%
  ungroup() %>%
  arrange(Location, n) %>%
  mutate(row = row_number())

bigram_texas %>%
  ggplot(aes(row, n, fill = Location)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Location, scales = "free_y") +
  xlab(NULL) + ylab(NULL) +
  scale_x_continuous(
    breaks = bigram_texas$row,
    labels = bigram_texas$bigram) +
  theme_project() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle("Bigrams Per Region of Texas") +
  coord_flip()

# Sentiment of most comonly occuring words
texas_nrc <- final_tweet_filtered %>%
  inner_join(get_sentiments("nrc"))

texas_sentiment <- texas_nrc %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(10)) %>%
  ungroup()
w <- texas_sentiment %>%
  ggplot(aes(word, 1, label = word, fill = sentiment )) +
  geom_point(color = "transparent") +
  geom_label_repel(force = 1,nudge_y = .5,  
                   direction = "y",
                   box.padding = 0.05,
                   segment.color = "transparent",
                   size = 9) +
  facet_grid(~sentiment) +
  theme_project() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 20)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Sentiment of Most Commonly Occuring Words") +
  coord_flip()
w  + theme(plot.title = element_text(size=24, face="bold"))

year_sentiment_nrc <- texas_nrc %>%
  group_by(Location, sentiment) %>%
  dplyr::count(Location, sentiment) %>%
  dplyr::select(Location, sentiment, sentiment_city_count = n)
year_sentiment_nrc
#Get the total count of sentiment words per year (not distinct)
total_sentiment_year <- texas_nrc %>%
  dplyr::count(Location) %>%
  dplyr::select(Location, city_total = n)
# Tweets by location
texas_location <- final_tweet_filtered %>%
  inner_join(get_sentiments("nrc")) %>%
  dplyr::filter(!sentiment %in% c("positive", "negative"))
tweets_by_location <- texas_location %>%
  dplyr::count(Location) %>%
  dplyr::select(Location, city_total = n)

tweets_by_location$Location <- factor(total_sentiment_year$Location, 
                                      levels=c("Lubbock", "San Angelo", "Amarillo",
                                               "Seven Sisters", "Austin", "Houston", "Athens"))
tweets_by_location <- tweets_by_location[!grepl("NA", tweets_by_location$Location),]
p <- ggplot(data = tweets_by_location, aes(x = Location, y = city_total, fill = Location)) +
  geom_bar(stat="identity") + xlab("Tweet Count") + ggtitle("Number of Vaccine Related Tweets By Location") +
  labs(x = "Location", y = "Number of Tweets")
q <- p + coord_flip()
q <- q + theme(legend.position = "none")
q <- q + theme(axis.text=element_text(size=18), axis.title=element_text(size=18))
q + theme(plot.title = element_text(size=20, face="bold"))

# NIS Data Mining
getwd()
setwd("D:\\1 Masters\\Dataset\\Final Datasets")
rawdf <- read.csv("raw.csv", header=T, na.strings=c(""), stringsAsFactors = T)
rawd <- read.csv("rawwer.csv", header=T, na.strings=c(""), stringsAsFactors = T)

install.packages("plyr")
library(plyr)
library(adabag)
library(tuneRanger)
library(mlr)
library(ROSE)
library(tidyverse)
library(BBmisc)

imbalanceddata <- rawd[ ,c(4, 11, 14, 16, 18, 19, 21, 23, 25, 29, 30, 36, 37, 45, 72, 176, 181, 183)]
imbalanceddat <- rawd[ ,c(4, 11, 14, 16, 18, 19, 21, 23, 25, 29, 30, 36, 37, 45, 72, 176, 181, 183)]
colnames(imbalanceddat) = c("Adequate_Data", "Duration_BF", "Household_Size", "Was_Child_Breastfed",
                            "Child_Number", "WIC", "Education_Status", "Firstborn", "Income_Group",
                            "Mother_Age_Group", "Marital_Status", "Race", "House_Ownership_Status",
                            "Provider_Facility", "Vaccination_Status", "Insurance_Type",
                            "Number_Providers", "Region")
colnames(imbalanceddata) = c("Adequate_Data", "Duration_BF", "Household_Size", "Was_Child_Breastfed", 
                             "Child_Number", "WIC", "Education_Status", "Firstborn", "Income_Group",
                             "Mother_Age_Group", "Marital_Status", "Race", "House_Ownership_Status",
                             "Provider_Facility", "Vaccination_Status", "Insurance_Type", "Number_Providers",
                             "Region")
imbalanceddata <- rbind(imbalanceddat, imbalanceddata) 
imbalanceddata<-na.omit(imbalanceddata)
write.csv(imbalanceddata, file= "NIS_dataset.csv") 

setwd("D:\\1 Masters\\Dataset\\Project Datasets")
imbalanceddata <- read.csv("NIS_dataset.csv", header=T, na.strings = c(""), stringsAsFactors = T)

sapply(imbalanceddata, class)
imbalanceddata$Duration <- as.numeric(imbalanceddata$Duration)

# View factor levels
table(imbalanceddata$Provider_Facility)
table(imbalanceddata$Education_Status)
table(imbalanceddata$House_Ownership_Status)
table(imbalanceddata$Firstborn)
table(imbalanceddata$Mother_Age_Group)
table(imbalanceddata$Marital_Status)
table(imbalanceddata$Race)
table(imbalanceddata$Number_Providers)
table(imbalanceddata$Household_Size)
table(imbalanceddata$Insurance_Type)
table(imbalanceddata$Was_Child_Breastfed)
table(imbalanceddata$Child_Number)
table(imbalanceddata$WIC)

# Clean data
imbalanceddata <- imbalanceddata[!grepl("REFUSED|NEVER HEARD OF WIC|DON'T KNOW", imbalanceddata$WIC),]
imbalanceddata <- imbalanceddata[!grepl("OTHER ARRANGMENT|REFUSED|DON'T KNOW", 
                                        imbalanceddata$House_Ownership_Status),]
imbalanceddata <- imbalanceddata[!grepl("DON'T KNOW|REFUSED", imbalanceddata$Was_Child_Breastfed),]
imbalanceddata <- imbalanceddata[!grepl("NA|TYPE OF PROVIDER UNKNOWN|REFUSED",
                                        imbalanceddata$Provider_Facility),]

imbalanceddata$Provider_Facility <- factor(imbalanceddata$Provider_Facility)
imbalanceddata$WIC <- factor(imbalanceddata$WIC)
imbalanceddata$House_Ownership_Status <- factor(imbalanceddata$House_Ownership_Status)
imbalanceddata$Number_Providers <- factor(imbalanceddata$Number_Providers)
imbalanceddata$Was_Child_Breastfed <- factor(imbalanceddata$Was_Child_Breastfed) 
imbalanceddata$Child_Number <- factor(imbalanceddata$Child_Number) 
imbalanceddata$Education_Status <- factor(imbalanceddata$Education_Status) 
imbalanceddata$Mother_Age_Group <- factor(imbalanceddata$Mother_Age_Group) 
imbalanceddata$Marital_Status <- factor(imbalanceddata$Marital_Status) 
imbalanceddata$Race <- factor(imbalanceddata$Race) 
imbalanceddata$Provider_Facility <- factor(imbalanceddata$Provider_Facility) 
imbalanceddata$Insurance_Type <- factor(imbalanceddata$Insurance_Type) 
imbalanceddata$Number_Providers <- factor(imbalanceddata$Number_Providers) 

table(imbalanceddata$Provider_Facility)
table(imbalanceddata$WIC)
table(imbalanceddata$House_Ownership_Status)
table(imbalanceddata$Number_Providers)
table(imbalanceddata$Was_Child_Breastfed)

imbalanceddata <- imbalanceddata[ ,c(3:19)]

# Analyse numerical features
range(imbalanceddata$Duration)
range(imbalanceddata$Income_Group)
mean(imbalanceddata$Duration)
mean(imbalanceddata$Income_Group)
var(imbalanceddata$Duration)
var(imbalanceddata$Income_Group)

levels(imbalanceddata$Vaccination_Status) <- c("Unvaccinated", "Vaccinated")

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 25),
  axis.title.y = element_text(size = 25),
  plot.title = element_text(size = 22))

bf_histogram <- qplot(imbalanceddata$Duration, geom = "histogram", 
                      main = "Histogram for Breastfeeding Duration", xlab = "Breastfeeding Duration",
                      fill = 'red') + theme(legend.position = "none") + My_Theme
income_histogram <- qplot(imbalanceddata$Income_Group, geom = "histogram", 
                          main = "Histogram for Income", xlab = "Income Group",
                          fill = 'red') + theme(legend.position = "none") + My_Theme
boxplot_bf <- ggplot(imbalanceddata, aes(x = Duration)) + geom_boxplot(fill = 'red') + 
  coord_flip() + ggtitle("Boxplot of Breastfeeding Duration") + My_Theme
boxplot_income <- ggplot(imbalanceddata, aes(x = Income_Group)) + geom_boxplot(fill = 'red') + 
  coord_flip() + ggtitle("Boxplot of Income Group") + My_Theme
plot_grid(boxplot_bf, boxplot_income, bf_histogram, income_histogram, nrow = 2, 
          ncol= 2, labels = "AUTO", label_size= 20, align = "v")

# Statistical Analysis
# code based on https://www.datacamp.com/community/tutorials/feature-selection-R-boruta

# https://www.analyticsvidhya.com/blog/2015/10/inferential-descriptive-statistics-beginners-r/
# https://towardsdatascience.com/data-analysis-and-visualisations-using-r-955a7e90f7dd 

# Test whether or not 
# The p-values are both below 0.05, which suggests that there is a statistically significant difference
# between vaccination rates between those with different incomes and those that were breastfed for 
# different durations 
t.test(imbalanceddata$Duration ~ imbalanceddata$Vaccination_Status,var.equal=TRUE)
t.test(imbalanceddata$Income_Group ~ imbalanceddata$Vaccination_Status,var.equal=TRUE)

# Chi-sqaured testing
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Household_Size)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Was_Child_Breastfed)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Child_Number)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$WIC)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Education_Status)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Firstborn)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Mother_Age_Group)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Marital_Status)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Race)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$House_Ownership_Status)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Provider_Facility)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Insurance_Type)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Number_Providers)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Region)

# ANOVA Testing
grouped_education <- group_by(imbalanceddata, Education_Status)
summarise(grouped_education, group_mean = mean(Income_Group, na.rm = TRUE))
education_ANOVA <- lm(Income_Group ~ Education_Status, data=imbalanceddata)
anova(education_ANOVA)

grouped_household <- group_by(imbalanceddata, Household_Size)

summarise(grouped_household, group_mean = mean(Income_Group, na.rm = TRUE))
household_ANOVA <- lm(Income_Group ~ Household_Size, data=imbalanceddata)
anova(household_ANOVA)

grouped_race <- group_by(imbalanceddata, Race)
summarise(grouped_race, group_mean = mean(Income_Group, na.rm = TRUE))
race_ANOVA <- lm(Income_Group ~ Race, data=imbalanceddata)
anova(race_ANOVA)

grouped_insurance <- group_by(imbalanceddata, Insurance_Type)
summarise(grouped_insurance, group_mean = mean(Income_Group, na.rm = TRUE))
insurance_ANOVA <- lm(Income_Group ~ Race, data=imbalanceddata)
anova(insurance_ANOVA)
table(imbalanceddata$Education_Status)
# First set of plots
LT = dim(imbalanceddata)[1]
imbalanceddata$Race <- revalue(imbalanceddata$Race, 
                               c("HISPANIC"="HISPANIC", "NON-HISPANIC BLACK ONLY"="BLACK", 
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "MULTIPLE RACE", 
                                 "NON-HISPANIC WHITE ONLY"="WHITE"))
imbalanceddata$Education_Status <- revalue(imbalanceddata$Education_Status, 
                                           c("< 12 YEARS"="<12y", "> 12 YEARS, NON-COLLEGE GRAD"=">12y Non-Grad",
                                             "12 YEARS" = "12y", "COLLEGE GRAD"="Graduate"))
imbalanceddata$Education_Status <- factor(imbalanceddata$Education_Status, 
                                          levels=c("Graduate", ">12y Non-Grad", "12y", "<12y"))
imbalanceddata$Insurance_Type <- revalue(imbalanceddata$Insurance_Type, 
                                         c("ANY MEDICAID" = "Medicaid", "OTHER INSURANCE"="Other", 
                                           "PRIVATE INSURANCE ONLY" = "Private", "UNINSURED" = "Uninsured", 
                                           "OTHER INSURANCE (CHIP, IHS, MILITARY, OR OTHER, ALONE OR IN COMB. WITH PRIVATE INSURANCE)"="Public"))
imbalanceddata$Insurance_Type <- factor(imbalanceddata$Insurance_Type, 
                                        levels=c("Private", "Public", "Medicaid", "Other", "Uninsured"))
imbalanceddata$Child_Number <- factor(imbalanceddata$Child_Number, 
                                      levels=c("ONE", "TWO OR THREE", "FOUR OR MORE"))
imbalanceddata$Income_Level <- cut(imbalanceddata$Income_Group, 3,  
                                   include.lowest=TRUE, labels=c("Low", "Medium", "High"))

plot1 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Household_Size,fill =
                  Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot3 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Child_Number,fill =
                  Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot5 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Education_Status,fill =
                  Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot9 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Race,fill =
                  Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot10 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Insurance_Type,fill = 
                  Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot16 <-ggplot(data = imbalanceddata, aes(x = Income_Level,fill =
                  Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot_grid(plot1, plot3, plot5, plot16, plot10, plot9, nrow = 3, ncol = 2,
          labels = "AUTO", label_size = 26, align = "v")

# Second set of plots
imbalanceddata$Marital_Status <- revalue(imbalanceddata$Marital_Status, 
     c("MARRIED"="Married", 
     "NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED/LIVING WITH PARTNER" = "Not Currently Married"))
plot2 <- ggplot(data = imbalanceddata[1:LT,], aes(x = Was_Child_Breastfed,fill = Vaccination_Status)) +
  geom_bar(position="fill") + theme(legend.position = "none")
plot4 <- ggplot(data = imbalanceddata[1:LT,], aes(x = WIC,fill = Vaccination_Status)) +
  geom_bar(position="fill") + theme(legend.position = "none")
plot6 <- ggplot(data = imbalanceddata[1:LT,], aes(x = Firstborn,fill = Vaccination_Status)) +
  geom_bar(position="fill") + theme(legend.position = "none")
plot8 <- ggplot(data = imbalanceddata[1:LT,], aes(x = Marital_Status,fill = Vaccination_Status)) +
  geom_bar(position="fill") + theme(legend.position = "none")
plot_grid(plot2, plot4, plot6, plot8, nrow = 2, ncol = 2, labels = "AUTO",
          label_size = 26, align = "v")

# Third set of plots
imbalanceddata$Provider_Facility <- revalue(imbalanceddata$Provider_Facility, 
                                            c("ALL HOSPITAL FACILITIES" = "All", "ALL MILITARY/OTHER FACILITIES" =
                                                "Military", "ALL PRIVATE FACILITIES" = "Private", 
                                              "ALL PUBLIC FACILITIES" = "Public", "MIXED"="Mixed"))
imbalanceddata$Provider_Facility <- factor(imbalanceddata$Provider_Facility, 
                                           levels=c("Private", "Public", "Medicaid", "Other", "Uninsured"))
plot11 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Number_Providers,
                                                  fill = Vaccination_Status)) + geom_bar(position="fill") + ylab("Frequency")
plot12 <-ggplot(data = imbalanceddata[1:LT,], aes(x = House_Ownership_Status,
            fill = Vaccination_Status)) + geom_bar(position="fill") + ylab("Frequency") +
            theme(legend.position = "none")
plot13 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Provider_Facility,
                                                  fill = Vaccination_Status)) + geom_bar(position="fill") + ylab("Frequency") +
  theme(legend.position = "none")
plot14 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Mother_Age_Group,
                                                  fill = Vaccination_Status)) + geom_bar(position="fill") + ylab("") 
plot_grid(plot11, plot12, plot13, plot14, nrow = 2, ncol = 2, labels = "AUTO",
          label_size = 26, align = "v")

#Remove income level factor (already have income group)
imbalanceddata <- imbalanceddata[ ,c(1:17)]

# Normalise numeircal columns
imbalanceddata$Duration <- normalize(imbalanceddata$Duration, 
                                     method = "standardize", range = c(0, 1), 
                                     margin = 1L, on.constant = "quiet")
imbalanceddata$Income_Group <- normalize(imbalanceddata$Income_Group, 
                                         method = "standardize", range = c(0, 1),
                                         margin = 1L, on.constant = "quiet")

# Create testing and training datasets
sample <- floor(0.80 * nrow(imbalanceddata))
set.seed(567)
train_ind <- sample(seq_len(nrow(imbalanceddata)), size = sample)
training <- imbalanceddata[train_ind, ]
test <- imbalanceddata[-train_ind, ]

# Check imbalance of data
table(imbalanceddata$Vaccination_Status)
round(prop.table(table(imbalanceddata$Vaccination_Status)),4)*100
ggplot(data = imbalanceddata, aes(x = Vaccination_Status, fill = Vaccination_Status)) +
  geom_bar(stat="count") +
  xlab("Vaccination Status") + ylab("Number of Individuals") +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=18)) +
  theme(plot.title = element_text(size=30, face="bold")) +
  ggtitle("Comparison of Vaccination Rates") +
  theme(plot.title = element_text(hjust = 0.5))

# Treat imbalanced data
#Undersample
under_trains <- ovun.sample(Vaccination_Status ~ ., data = training, method = "under", N = 9700, seed = 1)$data
table(under_trains$Vaccination_Status)
set.seed(322)
under_model <- randomForest(Vaccination_Status~., data = under_trains)
under_pred <- predict(under_model, test)
under_cm <- confusionMatrix(test$Vaccination_Status, under_pred)
under_cm

# Mixture- minority class is oversampled and majority class is undersampled
both_trains <- ovun.sample(Vaccination_Status ~ ., data = training, method = "both", p=0.5, N=11400, seed = 1)$data
table(both_trains$Vaccination_Status)
set.seed(2323)
both_model <- randomForest(Vaccination_Status~., data = both_trains)
both_pred <- predict(both_model, test)
both_cm <- confusionMatrix(test$Vaccination_Status, both_pred)
both_cm
# SYnthetically generate data using ROSE
synth_trains <- ROSE(Vaccination_Status ~ ., data = training, seed = 1)$data
table(synth_trains$Vaccination_Status)
synth_model <- randomForest(Vaccination_Status~., data = synth_trains)
synth_pred <- predict(synth_model, test)
synth_cm <- confusionMatrix(test$Vaccination_Status, synth_pred)
synth_cm

# Oversample
over_trains <- ovun.sample(Vaccination_Status ~ ., data = training, method = "over",N = 36200)$data
table(over_trains$Vaccination_Status)
set.seed(3232)
over_model <- randomForest(Vaccination_Status~., data = over_trains)
over_pred <- predict(over_model, test)
over_cm <- confusionMatrix(test$Vaccination_Status, over_pred)
over_cm

# Check performance of each sampling method based on accuracy of Random Forest
compare_samples <- rbind(over_cm$overall, synth_cm$overall, both_cm$overall, under_cm$overall)
compare_samples <- as.data.frame(compare_samples)
compare_samples$Method <- c('Oversampled', 'Synthetic Data', 'Under/Oversampled', 'Undersampled')

# Feature importance
Project_Theme = theme(
  axis.title.x = element_text(size = 19),
  axis.text.x = element_text(size = 19),
  axis.title.y = element_text(size = 20),
  plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))
imb <- ggplot(compare_samples, aes(x= reorder(Method, -Accuracy), weight=Accuracy, fill = Method))
imb <- imb + geom_bar() + ggtitle("Comparison of Accuracy of Sampling Methods") + Project_Theme
imb <- imb + xlab("Sampling Method") + ylab("Accuracy") 
imb + scale_fill_discrete(name = "Sampling Method")

# use oversampling as training data
trains <- over_trains

# Create training and test dataset for Rmarkdown file
setwd("D:\\AD")
write.csv(test, file= "test_data.csv")
write.csv(trains,"training_data.csv")
# Variable Importance
# Random Forest
set.seed(232) 
imp_rf <- randomForest(Vaccination_Status ~ ., data=trains,  importance=TRUE) 
var_imp <- varImpPlot(imp_rf)
var_imp <- as.data.frame(var_imp)
var_imp$varnames <- rownames(var_imp)
rownames(var_imp) <- NULL  

ggplot(var_imp, aes(x=reorder(varnames, MeanDecreaseAccuracy),
                    weight=MeanDecreaseAccuracy, fill=varnames)) + 
  geom_bar() + scale_fill_discrete(name="Variable Group") +
  ylab("Importance") + xlab("Variable") +
  coord_flip() + theme(legend.position = "none") + 
  ggtitle("Random Forest Variable Importance") +
  theme(plot.title = element_text(hjust = 0.5))

# Boruta Algorithm
set.seed(432)
var_imp_boruta <- Boruta(Vaccination_Status~., data = trains, doTrace = 2)
print(var_imp_boruta)

# 1. Random Forest
trained <- sample_n(trains, 10000)
trains <- na.omit(trains)
test <- na.omit(test)
set.seed(3232)
tuning_rf <- tuneRF(trains[,-14], trains[,14], stepFactor = 2.5, plot = TRUE)
plot(tuning_rf)
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3,
                        search = 'grid')
tunegrid <- expand.grid(.mtry=c(16))
modellist <- list()
for(ntree in c(100, 250, 500, 1000)) {set.seed(3233)
  fit <- caret::train(Vaccination_Status~., data = trained,
                      method = "rf", tuneGrid = tunegrid, trControl = control, ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit}
results <- resamples(modellist)
summary(results)

set.seed(1234)
untuned_rf_model <- randomForest(Vaccination_Status ~., data = trains)
untuned_rf_pred <- predict(untuned_rf_model, test)
set.seed(323)
tuned_rf_model <- randomForest(Vaccination_Status ~., data = trains, ntree = 100, mtry = 16)
tuned_rf_pred <- predict(untuned_rf_model, test)

confusionMatrix(untuned_rf_pred,
                test$Vaccination_Status)
confusionMatrix(tuned_rf_pred,
                test$Vaccination_Status)

# 2. Bagging
set.seed(322) 
untuned_bag <- bagging( 
  formula = Vaccination_Status ~ ., 
  data = trains) 
untuned_bagging_pred <- predict(untuned_bag, test) 
untuned_bagging_target <- as.factor(untuned_bagging_pred$class)
set.seed(322) 
bagging_model <- bagging( 
  formula = Vaccination_Status ~ ., 
  data = trains, 
  nbagg = 100)
tuned_bagging_pred <- predict(bagging_model, test) 
tuned_bagging_target <- as.factor(tuned_bagging_pred$class) 

confusionMatrix(untuned_bagging_target,
                test$Vaccination_Status)
confusionMatrix(tuned_bagging_target,
                test$Vaccination_Status) 

# 3. KNN
# Make the factor levels suitable for models such as knn. This keeps the variables as factors but changes
# the factor level names to simple numerical values that for models

selected <-c("Was_Child_Breastfed", "Household_Size", "Child_Number", "Firstborn", 
             "Insurance_Type", "Education_Status", "Was_Child_Breastfed", 
             "Race", "Mother_Age_Group", "Marital_Status", "House_Ownership_Status", 
             "Provider_Facility", "Number_Providers", "WIC", "Region")
for (i in selected){trains[,i] <- as.numeric(trains[,i])}
for (i in selected){trains[,i] <- as.factor(trains[,i])}
for (i in selected){test[,i] <- as.numeric(test[,i])}
for (i in selected){test[,i] <- as.factor(test[,i])}
# From https://rpubs.com/njvijay/16251
trains <- na.omit(trains)
test <- na.omit(test)
trained <- sample_n(trains, 10000)
set.seed(232)
knn.cross <- tune.knn(x = trained[,-14], y = trained[,14], k = 1:25, 
                      tunecontrol = tune.control(sampling = "cross"), cross=10) 
plot(knn.cross, main = "Optimal Value for K in KNN") 

# Train knn model with default and tuned values for k 
set.seed(324)
knn_untuned <- kNN(Vaccination_Status ~ .,trains, test, norm=FALSE) 
set.seed(232)
knn_tuned <- kNN(Vaccination_Status ~ .,trains, test, norm=FALSE, k = 1) 

confusionMatrix(knn_untuned,
                test$Vaccination_Status)
confusionMatrix(knn_tuned,
                test$Vaccination_Status)

# 4. Support Vector Machines 
# Comparing different svm kernels 
set.seed(1234)
svm_vanilla <- ksvm(Vaccination_Status ~ ., data = trains, kernel = "vanilladot")
vanilla_pred <- predict(svm_vanilla, test)
confusionMatrix(vanilla_pred, test$Vaccination_Status)
vanillaconfusion$overall
vanilla <- vanillaconfusion$overall
vanilla
vanilla$kernel <- 'vanilla'
vanilla <- data.frame(as.list(vanilla))

set.seed(2313)
svm_rbfdot <- ksvm(Vaccination_Status ~ ., data = trains, kernel = "rbfdot")
rbfdot_pred <- predict(svm_rbfdot, test)
rbfdotconfusion <- confusionMatrix(rbfdot_pred, test$Vaccination_Status)
rbfdotconfusion$overall
rbf <- rbfdotconfusion$overall
rbf
rbf$kernel <- 'rbf_dot'
rbf <- data.frame(as.list(rbf))

set.seed(1234)
svm_laplacedot <- ksvm(Vaccination_Status ~ ., data = trains, kernel = "laplacedot")
laplacedot_pred <- predict(svm_laplacedot, test)
laplacedot_confusion <- confusionMatrix(laplacedot_pred, test$Vaccination_Status)
laplacedot_confusion$overall
laplacedot <- laplacedot_confusion$overall
laplacedot
laplacedot$kernel <- 'laplace_dot'
laplacedot <- data.frame(as.list(laplacedot))

set.seed(1234)
svm_classifier <- ksvm(Vaccination_Status ~ ., data = trains, kernel = "besseldot")
besseldot_pred <- predict(svm_classifier, test)
besseldot_confusion <- confusionMatrix(besseldot_pred, test$Vaccination_Status)
besseldot_confusion$overall
besseldot <- besseldot_confusion$overall
besseldot
besseldot$kernel <- 'bessel_dot'
besseldot <- data.frame(as.list(besseldot))
svmkernelcombined <- rbind(rbf, vanilla, laplacedot, besseldot)
ggplot(data=svmkernelcombined, aes(x=reorder(kernel, -Accuracy), y=Accuracy, fill = kernel)) +
  geom_bar(stat="identity") +
  coord_cartesian(ylim=c(0.5, 0.633)) +
  ggtitle("Accuracy of Each SVM Kernel") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Kernel")

# Tune cost function of SVM
set.seed(1424)
svmtune_cost <- tune(svm,
                     Vaccination_Status ~ .,
                     data = trained,
                     kernel='linear',
                     ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 10)))
plot(svmtune_cost, main="Tuning SVM Model: Error vs Cost")
warnings()
set.seed(1424)
svmtune <- tune(svm,
                Vaccination_Status ~ .,
                data = trained,
                kernel='linear',
                ranges=list(sigma = c(0.001,0.003,0.006,0.009)))
plot(svmtune, main="Tuning SVM Model: Error vs Sigma")
warnings()
# Test tuned SVM model
set.seed(323)
untuned_svm_model <- ksvm(Vaccination_Status ~ ., data = trains)
untuned_svm_pred <- predict(untuned_svm_model, test)
set.seed(1424)
tuned_ksvm_model = ksvm(Vaccination_Status~., data=trains, 
                        kpar=list(sigma = .001), C = 10,
                        kernel = "besseldot")
tuned_svm_pred <- predict(tuned_ksvm_model, test)

confusionMatrix(untuned_svm_pred,
                test$Vaccination_Status)
confusionMatrix(tuned_svm_pred, 
                test$Vaccination_Status)

# 5. Naive Bayes 
features <- setdiff(names(trained), "Vaccination_Status") 
x <- trained[, features] 
y <- trained$Vaccination_Status 
train_control <- trainControl(method = "cv", number = 10) 
search_grid <- expand.grid(usekernel = c(TRUE, FALSE),
                           fL = 0:5, adjust = seq(0, 5, by = 1)) 
set.seed(1234) 
tuning_nb_model <- caret::train( 
  x = x, 
  y = y, 
  method = "nb",   trControl = train_control, 
  tuneGrid = search_grid, 
  preProc = c("BoxCox", "center", "scale")) 
plot(tuning_nb_model)

set.seed(434)
untuned_naive_bayes_model <- naiveBayes(Vaccination_Status ~ ., data = trains)
untuned_naive_bayes <- predict(untuned_naive_bayes_model, test)
search_grid <- expand.grid(usekernel = c(TRUE), fL = 1, adjust = 1)  
set.seed(237)
tuned_nb_model <- caret::train( x = x, y = y, method = "nb",  tuneGrid = search_grid,   
                                preProc = c("BoxCox", "center", "scale", "pca"))  
tuned_naive_bayes <- predict(tuned_nb_model, test, type = "raw") 
confusionMatrix(untuned_naive_bayes,
                test$Vaccination_Status) 
confusionMatrix(tuned_naive_bayes,
                test$Vaccination_Status)

# 6. C5.0
# Tune C50 model
fitControl <- trainControl(method = "repeatedcv", 
                           number = 5, repeats = 3, returnResamp="all")
grid <- expand.grid( .winnow = c(TRUE), .trials=c(1, 25, 50, 75, 100), .model="tree" ) 
set.seed(1231)
C50_tune <- caret::train(x = trained[-14],
                         y = trained[,14], 
                         tuneGrid=grid, 
                         trControl=fitControl, 
                         method="C5.0",VERBOSE=FALSE) 
plot(C50_tune, main = "Plot of Optimal Trials for C5.0",
     xlab = "Trials", ylab = "accuracy")

grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(100), .model="tree" )
set.seed(1231)
C50_tune_winnow <- caret::train(x=trained[-14] ,
                                y=trained[,14],
                                tuneGrid=grid,
                                trControl=fitControl,
                                method="C5.0",VERBOSE=FALSE)
winnowplot <- C50_tune_winnow$results
winnowplot <- ggplot(data=winnowplot, aes(x = Accuracy, y = winnow, fill = winnow)) +
  geom_bar(stat="identity") +
  ggtitle("Accuracy of C50 Model Based on Winnow Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Accuracy") + theme(legend.position = "none")
winnowplot

# Build tuned and untuned C50
set.seed(1244)
C50_default <- C5.0(trains[-14], trains$Vaccination_Status)
untuned_C50_pred <- predict(C50_default, test)
grid <- expand.grid( .winnow = c(FALSE), .trials=c(100), .model="tree" )
set.seed(1231)
C50_tuned <- caret::train(x=trains[-14] ,
                          y=trains[,14],
                          tuneGrid=grid,
                          method="C5.0",VERBOSE=FALSE)
tuned_C50_pred <- predict(C50_tuned, test)

confusionMatrix(untuned_C50_pred,
                test$Vaccination_Status)
confusionMatrix(tuned_C50_pred,
                test$Vaccination_Status)

# 7. gbm
gbmGrid <- expand.grid(interaction.depth = seq(1,20, by = 2),
                       n.trees = c(25, 50, 100, 200), shrinkage = c(0, 1),
                       n.minobsinnode = 10)
boostctrl <- trainControl(method = "repeatedcv", number = 10, 
                          repeats = 1, classProbs = FALSE, savePredictions = T)
set.seed(323)
tuning_gbm <- caret::train(Vaccination_Status ~ ., 
                           data = trained, method = "gbm", trControl = boostctrl,
                           verbose = FALSE, tuneGrid = gbmGrid)
plot(tuning_gbm, main = "Tuning GBM")

# build tuned/untuned model
training <- trains
training$Vaccination_Status <- as.numeric(training$Vaccination_Status)-1
set.seed(232)
untuned_gbm <- gbm(Vaccination_Status ~ ., data = training, n.trees = 100)
untuned_gbm_pred <- predict(untuned_gbm, test, n.trees =  100)
untuned_gbm_pred <- ifelse(untuned_gbm_pred >= 0.5, 0, 1)
target <- as.numeric(test[,14])-1
untuned_gbm_pred <- as.factor(target)

set.seed(3233)
tuned_gbm <- gbm(Vaccination_Status ~ ., data = training,
                 n.trees = 200, interaction.depth = 20)
tuned__gbm_pred <- predict(tuned_gbm, test, n.trees = 200)
tuned__gbm_pred <- ifelse(tuned__gbm_pred >= 0, 0, 1)
tuned__gbm_pred <- as.factor(tuned__gbm_pred)

testing <- test
testing$Vaccination_Status <- as.numeric(testing$Vaccination_Status)-1
testing$Vaccination_Status <- as.factor(testing$Vaccination_Status)
confusionMatrix(untuned_gbm_pred,
                testing$Vaccination_Status)
confusionMatrix(tuned__gbm_pred,
                testing$Vaccination_Status)

# 8. C-Forest
ctree_control <- trainControl(method = 'cv', number=5,summaryFunction=defaultSummary)  
ctree_tune_grid <- expand.grid(mincriterion=c(0.010, 0.255, 0.50, 0.745, 0.990),  
                               maxdepth = seq(15, 50, 5))

# create formula without region (c-forest produces errors with region because of it's many levels)
ctree_formula <- Vaccination_Status ~ Education_Status + Income_Group +   
  Number_Providers + Household_Size + Child_Number + Insurance_Type +   
  Provider_Facility + Marital_Status + Duration + Race + House_Ownership_Status +  
  Mother_Age_Group + Firstborn 
set.seed(1234)
ctree_tune <- caret::train(ctree_formula, data=trained, method = 'ctree2',
                           trControl = ctree_control,tuneGrid = ctree_tune_grid)  
plot(ctree_tune, main = "Tuning C-Forest", ylab = "Accuracy")

testing <- test[ ,c(1:16)]

# build tuned/untuned c-forest
set.seed(343)
untuned_cforest <- cforest(ctree_formula, data = trains)
untuned_ctree_pred <- predict(untuned_cforest, test) 

Grid <- expand.grid(mincriterion=c(0.01),  
                    maxdepth = c(15))
set.seed(1234)
ctree_tune <- caret::train(ctree_formula, data=trains, method = 'ctree2',   
                           trControl=fitControl,tuneGrid=Grid)  
tuned_ctree_pred <- predict(ctree_tune, testing) 

confusionMatrix(untuned_ctree_pred,
                test$Vaccination_Status)  
confusionMatrix(tuned_ctree_pred,
                test$Vaccination_Status)

# 9. Neural Network
set.seed(1234)
tune_nnet <- tune.nnet(Vaccination_Status ~., data=trained, size = 1:15) 
plot(tune_nnet, main = "Tuning Neural Network")

fitControl <- trainControl(method = "repeatedcv",  
                           number = 10,  
                           repeats = 1,  
                           classProbs = TRUE,  
                           summaryFunction = twoClassSummary) 
nnetGrid <-  expand.grid(size = c(10),
                         decay = c(0.1, 0.5, 0.9, 1.3)) 
set.seed(1234) 
nnetFit <- caret::train(Vaccination_Status ~ .,  
                        data = trained, 
                        method = "nnet", 
                        metric = "ROC", 
                        trControl = fitControl, 
                        tuneGrid = nnetGrid, 
                        verbose = FALSE) 
nnetFit
plot(nnetFit, main = "Neural Network Accuracy By Weight Decay") 

set.seed(232)
untuned_nnet <- nnet(Vaccination_Status ~., data=trains, size = 10)  
untuned_nnet_pred <- predict(untuned_nnet, test) 
untuned_nnet_pred <- ifelse(untuned_nnet_pred >= 0.5,0,1)  
untuned_nnet_pred <- as.factor(untuned_nnet_pred)
set.seed(434)
tuned_nnet <- nnet(Vaccination_Status ~., data=trains, size = 10, decay = 0.1)
tuned_nnet_pred <- predict(tuned_nnet, test)
tuned_nnet_pred <- ifelse(tuned_nnet_pred >= 0.5,0,1)  
tuned_nnet_pred <- as.factor(tuned_nnet_pred)

confusionMatrix(untuned_nnet_pred,
                nnet_test)
confusionMatrix(tuned_nnet_pred,
                nnet_test)

# 10. Superlearner
trained <- sample_n(trains, 10000)
test <- na.omit(test)
y <- as.numeric(trained[,14])-1
ytest <- as.numeric(test[,14])-1
x <- trained[ ,c(1:13, 15:17)]
xtest <- test[ ,c(1:13, 15:17)]
set.seed(323)
CV_sl <- CV.SuperLearner(y, x, V =3, 
                         family=binomial(), 
                         SL.library=list("SL.ranger",
                                         "SL.xgboost", "SL.ipredbagg"))
plot(CV_sl)

y <- as.numeric(trains[,14])-1
x <- trains[ ,c(1:13, 15:17)]
set.seed(323)
three_sl <- SuperLearner(y, 
                         x, 
                         family=binomial(), 
                         SL.library=list("SL.ranger",
                                         "SL.xgboost",
                                         "SL.ipredbagg")) 
three__sl_pred <- predict.SuperLearner(three_sl, newdata=xtest) 
conv.preds <- ifelse(three__sl_pred$pred>=0.5,0,1) 
conv.pred <- ytest 
conv.preds <- as.factor(conv.preds) 
conv.pred <- as.factor(conv.pred) 
three_sl_cm <- confusionMatrix(conv.pred, conv.preds) 
three_sl_cm

set.seed(323)
two_sl <- SuperLearner(y, 
                       x, 
                       family=binomial(), 
                       SL.library=list("SL.ranger",
                                       "SL.xgboost")) 
two_sl_pred <- predict.SuperLearner(two_sl, newdata=xtest) 
conv.preds <- ifelse(two_sl_pred$pred>=0.5,0,1) 
conv.pred <- ytest
conv.preds <- as.factor(conv.preds) 
conv.pred <- as.factor(conv.pred) 
two_sl_cm <- confusionMatrix(conv.pred, conv.preds) 
two_sl_cm

# one model- ranger 
set.seed(323)
single_sl <- SuperLearner(y, 
                          x, 
                          family=binomial(), 
                          SL.library=list("SL.ranger")) 
single_sl_pred <- predict.SuperLearner(single_sl, newdata=xtest) 
conv.preds <- ifelse(single_sl_pred$pred>=0.5,0,1) 
conv.pred <- ytest
conv.preds <- as.factor(conv.preds) 
conv.pred <- as.factor(conv.pred) 
single_sl_cm <- confusionMatrix(conv.pred, conv.preds) 
single_sl_cm

# tuned
SL.tuning.ranger <- function(...){
  SL.ranger(..., num.trees=100, mtry=16)}
set.seed(323)
tuned_sl <- SuperLearner(y, 
                         x, 
                         family=binomial(), 
                         SL.library=list("SL.tuning.ranger",
                                         "SL.xgboost")) 
tuned_sl <- predict.SuperLearner(tuned_sl, newdata=xtest)
conv.preds <- ifelse(tuned_sl$pred>=0.5,0,1) 
conv.pred <- ytest
conv.preds <- as.factor(conv.preds) 
conv.pred <- as.factor(conv.pred) 
tuned_sl_cm <- confusionMatrix(conv.pred, conv.preds) 
tuned_sl_cm

# Model performance
Accuracy  <- c(72.7, 54.67, 64.55, 68.22, 68.59, 64.01, 62.59, 76.12, 59.47, 71.66)
Sensitivity <- c(17.8, 58.97, 40.86, 38.34, 26.24, 43.06, 46.62, 30.01, 55.29, 19.62)
Specificity <- c(87.09, 53.53, 70.78, 76.04, 79.68, 69.5, 66.77, 80, 60.56, 85.3)
models <- c('Random Forest', 'Bagging', 'Boosting', 'C-Forest', 'KNN', 'Neural Network', 
            'SVM', 'Superlearner', 'Naïve Bayes', 'C5.0')
combined <- cbind(Accuracy, Sensitivity, Specificity)
combined <- as.data.frame(combined)
combined <- cbind(combined, models)
plot_acc <- ggplot(data = combined, aes(x = reorder(models, Accuracy), y = Accuracy)) +
  geom_bar(stat="identity", fill = "steelblue") + xlab("Model") + coord_flip()
plot_sens <- ggplot(data = combined, aes(x = reorder(models, Sensitivity), y = Sensitivity)) +
  geom_bar(stat="identity", fill = "purple") + xlab("Model") + coord_flip()
plot_spec <- ggplot(data = combined, aes(x = reorder(models, Specificity), y = Specificity)) +
  geom_bar(stat="identity", fill = "red") + xlab("Model") + coord_flip()
plot_grid(plot_acc, plot_spec, plot_sens,
          nrow = 2, ncol = 2, labels = "AUTO", label_size = 18, align = "v")

# Time Series Forecasting
# For Windows
setwd("D:\\Niall Mannion")
# For Mac
setwd("/Users/Niall Mannion")

install.packages("plyr")
library(plyr)
library(reshape2)
# Preparing data for flexdashboard datatable
headers = read.csv('timeseries.csv', skip = 1, header = F, nrows = 1, as.is = T)
ts = read.csv('timeseries.csv', skip = 2, header = F)
colnames(ts)= headers
tsdf <- ts[ ,c(1,2,8,15,22,29,36,43,50,57,64,71,78,85,92,99,106,112,119,126,132,139,146,152)]
tsdf <- melt(tsdf, id.vars=c("Names"))
tsdf <- spread(tsdf, Names, value)
colnames(tsdf)
newsat <- tsdf[ ,c(1, 4, 5, 6, 7, 18, 21, 22, 23, 24, 29, 32, 34, 35, 39, 43, 44, 45, 46, 
                   51, 54, 55, 56, 62, 63, 64, 65, 70, 71, 72, 73, 74, 75, 76, 81,
                   82, 90, 91, 92, 96, 108, 109, 110, 111, 112, 126, 127, 128, 134, 138, 139)]
timeseries <- newsat
colnames(timeseries)
timeseries <- t(timeseries)
timeseries <- as.data.frame(timeseries)
write.csv(timeseries, "datatable_timeseries.csv")

# For Windows
setwd("D:\\Niall Mannion\\Documents")
# For Mac
setwd("/Users/Niall Mannion/Documents")
write.csv(trains, file= 'training_data')
write.csv(trains, file= 'test_data')
trains <- read.csv("training_data.csv", header=T, skip = 1, na.strings=c(""), stringsAsFactors = T)
test <- read.csv("test_data.csv", header=T, skip = 1, na.strings=c(""), stringsAsFactors = T)

timeseries <- read.csv("timeseries.csv", header=T, skip = 1, na.strings=c(""), stringsAsFactors = T)
tsdf <- timeseries[ ,c(1,2,8,15,22,29,36,43,50,57,64,71,78,85,92,99,106,112,119,126,132,139,146,152)]
news <- melt(tsdf, id.vars=c("Names"))
newsa <- spread(news, Names, value)
newsat <- newsa[ ,c(7, 91, 112)]
newsat

mymts = ts(newsat, frequency = 1, start = c(1995, 1))
plot(mymts, main = "Timeseries of Vaccination Rates")

# Outliers
newsat <- newsa[ ,c(112)] 
untreated = ts(newsat, frequency = 1, start = c(1995, 1))
outliers_excess_ts <- tso(untreated, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)
title(main = "Outlier Plot of Texas Timeseries", cex.main = 1.5, font.main = 1, line = 2.25)
title(ylab = "Vaccination Rate", line = 3, cex.lab = 1.2)
title(xlab = "Year", line = 4, cex.lab = 1.2)

outliers_excess_ts$outliers
(outliers_idx <- outliers_excess_ts$outliers$ind)
n <- length(untreated)
mo_tc <- outliers("TC", outliers_idx)
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_excess_ts$outliers["coefhat"])
tc_effect <- coefhat*tc
Outlier_Plot <- ts(tc_effect, frequency = frequency(untreated), start = start(untreated))
Treated <- untreated - Outlier_Plot
plot(cbind(Treated, untreated, Outlier_Plot),
     main = "Plot of Texas Vaccination Time Series Outlier Treatment", xlab = "Year")
Texas <- Treated

newsat <- newsa[ ,c(7)] 
untreated = ts(newsat, frequency = 1, start = c(1995, 1))
outliers_excess_ts <- tso(untreated, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)
title(main = "Outlier Plot of Arkansas Timeseries", cex.main = 1.5, font.main = 1, line = 2.25)
title(ylab = "Vaccination Rate", line = 3, cex.lab = 1.2)
title(xlab = "Year", line = 4, cex.lab = 1.2)

outliers_excess_ts$outliers
(outliers_idx <- outliers_excess_ts$outliers$ind)
n <- length(untreated)
mo_tc <- outliers("TC", outliers_idx)
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_excess_ts$outliers["coefhat"])
tc_effect <- coefhat*tc
Outlier_Plot <- ts(tc_effect, frequency = frequency(untreated), start = start(untreated))
Treated <- untreated - Outlier_Plot
plot(cbind(Treated, untreated, Outlier_Plot),
     main = "Plot of Arkansas Vaccination Time Series Outlier Treatment", xlab = "Year")
Arkansas <- Treated

newsat <- newsa[ ,c(91)] 
untreated = ts(newsat, frequency = 1, start = c(1995, 1))
outliers_excess_ts <- tso(untreated, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)
title(main = "Outlier Plot of Oklahoma Timeseries", cex.main = 1.5, font.main = 1, line = 2.25)
title(ylab = "Vaccination Rate", line = 3, cex.lab = 1.2)
title(xlab = "Year", line = 4, cex.lab = 1.2)

outliers_excess_ts$outliers
(outliers_idx <- outliers_excess_ts$outliers$ind)
n <- length(untreated)
mo_tc <- outliers("TC", outliers_idx)
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_excess_ts$outliers["coefhat"])
tc_effect <- coefhat*tc
Outlier_Plot <- ts(tc_effect, frequency = frequency(untreated), start = start(untreated))
Treated <- untreated - Outlier_Plot
plot(cbind(Treated, untreated, Outlier_Plot),
     main = "Plot of Oklahoma Vaccination Time Series Outlier Treatment", xlab = "Year")
Oklahoma <- Treated

adjusted_ts <- cbind(Oklahoma, Texas, Arkansas)
apply(adjusted_ts, 2, adfTest, lags = 0, type = "c", 
      title = "ADF Testfor Vaccination Timeseries Data")

ts_window <- window(adjusted_ts, start = 1995, end = c(2012))
texas_ts <- adjusted_ts[,2]
stnry = diffM(ts_window)
var.a <- vars::VAR(stnry, lag.max = 10, ic = "AIC", type = "none") 
fcast = predict(var.a, n.ahead = 5)
Texas_fcast = fcast$fcst[2]
x = Texas_fcast$Texas[,1]
tail(ts_window)
x = cumsum(x) + 78.46

meanf_ts <- meanf(texas_ts, h = 5)
rwf_ts <- rwf(texas_ts, h = 5)
snaive_ts <- snaive(texas_ts, h = 5)

target_ts <- window(texas_ts, start = 2013)
target_ts <- as.numeric(target_ts)

forecast_ML <- x
accuracy(meanf_ts, target_ts)
accuracy(rwf_ts, target_ts)
accuracy(snaive_ts, target_ts)
accuracy(forecast_ML, target_ts)
meanf_ts
rwf_ts
snaive_ts
forecast_ML

stnry <- diffM(adjusted_ts)
autoplot(ts(stnry, start = c(1990, 1), frequency = 1), lwd =  1.6) +
  ggtitle("Time Series Plot of the stationary Texas Vaccination Time-Series") +
  ylab("Vaccination Rate") + xlab("Year")

var.a <- vars::VAR(stnry, lag.max = 10, ic = "AIC", type = "none")
fcast = predict(var.a, n.ahead = 5)
par(mar = c(2.5, 2.5, 2.5, 2.5))
plot(fcast)

Texas_fcast = fcast$fcst[2]
x = Texas_fcast$Texas[,1]
tail(adjusted_ts)
x = cumsum(x) + 74.1
par(mar = c(4, 4, 1, 4))
Forecast_Tex = ts(c(x), start= c(2017, 1), frequency = 1)
plot(Forecast_Tex, main = "Forecasted Vaccination Rates for Texas", xlab = "Year", ylab = "Vaccination Rates")
Texas_Forecast = ts(c(adjusted_ts[,2], x), start = c(1995, 1), frequency = 1)
Texas_df <- as.data.frame(Texas_Forecast[1:28])
colnames(Texas_df) <- c("Texas")

Arkansas_fcast = fcast$fcst[3] 
Arkansas_fcast
y = Arkansas_fcast$Arkansas[,1]
tail(adjusted_ts)
y = cumsum(y) + 73.89
par(mar = c(2.5,2.5,1,2.5))
Forecast_Ark = ts(c(y), start= c(2017, 1), frequency = 1)
plot(Forecast_Ark, main = "Forecasted Vaccination Rates for Arkansas", xlab = "Year", ylab = "Vaccination Rates")
Arkansas_Forecast =ts(c(adjusted_ts[,3], y), start = c(1995,1), frequency = 1)
Arkansas_df <- as.data.frame(Arkansas_Forecast[1:28]) 
colnames(Arkansas_df) <- c("y")

Oklahoma_fcast = fcast$fcst[1] 
Oklahoma_fcast
z = Oklahoma_fcast$Oklahoma[,1]
tail(adjusted_ts)
z = cumsum(z) + 71.4
par(mar = c(2.5,2.5,1,2.5))
Forecast_Okl = ts(c(z), start= c(2017, 1), frequency = 1)
plot(Forecast_Okl, main = "Forecasted Vaccination Rates for Oklahoma", xlab = "Year", ylab = "Vaccination Rates")
Oklahoma_Forecast =ts(c(adjusted_ts[,1], z), start = c(1995,1), frequency = 1)
Oklahoma_df <- as.data.frame(Oklahoma_Forecast[1:28]) 
colnames(Oklahoma_df) <- c("z")

combined_ts <- cbind(Texas_Forecast, Arkansas_Forecast, Oklahoma_Forecast)
combined_ts
combined_df <- as.data.frame(combined_ts)
combined_df$Year <- seq(1995, 2022, by = 1)
reshaped_ts <- melt(combined_df, id="Year")
forecast_theme <- theme(axis.title.x = element_text(size = 15),
                  axis.text.x = element_text(size = 15),
                  axis.title.y = element_text(size= 15),
                  plot.title = element_text(size = 17, hjust = 0.5))
ggplot(data = reshaped_ts, aes(x = Year, y = value, colour = variable)) + ylab("Immunization Rate") +
  ggtitle("Forecasted Immunization Rates for US States") + geom_line(size = 2.1) + forecast_theme
