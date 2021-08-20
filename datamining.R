library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(tm)
library(ggplot2)

# ===== FUNCTIONS =====
freqPlot <- function(dfmat, plot_title, top_n = 20){
  dfmat %>% 
    textstat_frequency(n = top_n) %>% 
    ggplot(aes(x = reorder(feature, frequency), y = frequency, 
               label = frequency, fill = "#FF6666")) +
    geom_bar(stat='identity', show.legend=FALSE) +
    coord_flip() +
    labs(x = NULL, y = "Frequency") +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    ggtitle(plot_title) +
    theme_minimal()
}

# ===== READ DATA =====

# Read raw data
data <- data.table::fread('rawdata.csv')

# ===== DATA CLEANING =====

# Remove non-alphabets, URLs, and Twitter handles
regex <- "[ \t]+$|(\\s+)|http\\S+|@\\w+|[^a-zA-z ']"
remove <- c('like','said','just','also','using','still','many','much','well','already')
data$Content <- sapply(data$Content, str_replace_all, regex, " ")
data$Content <- sapply(data$Content, removeWords, remove)

# Remove whitespace in column names
names(data) <- gsub("\\s+", "", names(data))

# Set appropriate data types
data$PostDate <- gsub('^\\w*\\s*', '', data$PostDate)
data$PostDate <- dmy_hms(data$PostDate)

# ===== TEXT ANALYSIS =====

library(quanteda)
library(quanteda.textstats)

data <- data %>% select(PostDate, ChannelSiteType, Title, Content)

facebook <- data %>% filter(ChannelSiteType == 'SOCIAL_NETWORKING_SITE')
twitter <- data %>% filter(ChannelSiteType == 'MICROBLOG')
forum <- data %>% filter(ChannelSiteType == 'FORUM')

cdata <- corpus(data$Content, unique_docnames = FALSE)
cfacebook <- corpus(facebook$Content, unique_docnames = FALSE)
ctwitter <- corpus(twitter$Content, unique_docnames = FALSE)
cforum <- corpus(forum$Content, unique_docnames = FALSE)

docvars(cdata, "Date") <- data$PostDate
summary(cdata)

# ----- TOKENIZATION -----
# Tokenize total raw data and by source

tokdata <- tokens(cdata, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE)
tokfacebook <- tokens(cfacebook, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE)
toktwitter <- tokens(ctwitter, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE)
tokforum <- tokens(cforum, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE)

tokdata <- tokens_select(tokdata, pattern = quanteda::stopwords("en"), min_nchar = 4, selection="remove")
tokfacebook <- tokens_select(tokfacebook, pattern = quanteda::stopwords("en"), min_nchar = 4, selection="remove")
toktwitter <- tokens_select(toktwitter, pattern = quanteda::stopwords("en"), min_nchar = 4, selection="remove")
tokforum <- tokens_select(tokforum, pattern = quanteda::stopwords("en"), min_nchar = 4, selection="remove")

# Generate 2- and 3-grams per source

tokdata_2gram <- tokens_ngrams(tokdata, n = 2, concatenator = " ")
tokdata_3gram <- tokens_ngrams(tokdata, n = 3, concatenator = " ")

tokfacebook_2gram <- tokens_ngrams(tokfacebook, n = 2, concatenator = " ")
tokfacebook_3gram <- tokens_ngrams(tokfacebook, n = 3, concatenator = " ")

toktwitter_2gram <- tokens_ngrams(toktwitter, n = 2, concatenator = " ")
toktwitter_3gram <- tokens_ngrams(toktwitter, n = 3, concatenator = " ")

tokforum_2gram <- tokens_ngrams(tokforum, n = 2, concatenator = " ")
tokforum_3gram <- tokens_ngrams(tokforum, n = 3, concatenator = " ")

# ----- DOCUMENT-FEATURE MATRIX -----

dfmdata <- dfm(tokdata) %>%
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")
dfmdata2 <- dfm(tokdata_2gram)
dfmdata3 <- dfm(tokdata_3gram)

dfmfacebook <- dfm(tokfacebook)
dfmfacebook2 <- dfm(tokfacebook_2gram)
dfmfacebook3 <- dfm(tokfacebook_3gram)

dfmtwitter <- dfm(toktwitter)
dfmtwitter2 <- dfm(toktwitter_2gram)
dfmtwitter3 <- dfm(toktwitter_3gram)

dfmforum <- dfm(tokforum)
dfmforum2 <- dfm(tokforum_2gram)
dfmforum3 <- dfm(tokforum_3gram)

dfmdata <- dfm(tokdata)
dfmdata2 <- dfm(tokdata_2gram)
dfmdata3 <- dfm(tokdata_3gram)

#topfeatures(dfmdata2,50)
write.csv(topfeatures(dfmdata2,20), 'freq.csv')
#freqPlot(dfmforum, 'Top 20 Most Frequent Words')

# ----- TOPIC MODELING -----
library(seededlda)
set.seed(2020-08-20)

ldadata <- textmodel_lda(dfmdata, k = 10)
#terms(ldadata, 10)
#table(topics(ldadata))

ldafacebook <- textmodel_lda(dfmfacebook, k = 10)
#terms(ldafacebook, 10)
#table(topics(ldafacebook))

ldatwitter <- textmodel_lda(dfmtwitter, k = 10)
#terms(ldatwitter, 10)
#table(topics(ldatwitter))

ldaforum <- textmodel_lda(dfmforum, k = 10)
#terms(ldaforum, 10)
#table(topics(ldaforum))

# ===== SENTIMENT ANALYSIS =====
dict_posneg <- data_dictionary_LSD2015[1:2]
tokdata_posneg <- tokens_lookup(tokdata, dictionary = dict_posneg)
dfmatdata_posneg <- dfm(tokdata_posneg) %>% 
  dfm_group(groups = Date)
str(dfmatdata_posneg)
#plot(dfmatdata_posneg$Date, dfmatdata_posneg[,"positive"] - dfmatdata_posneg[,"negative"], 
#     type = "l", ylab = "Sentiment", xlab = "Date", main = "Sentiment of Related Words to \"Juice\" from 2018-11 to 2019-05")
#grid()
#abline(h = 0, lty = 2)