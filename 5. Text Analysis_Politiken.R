# Generic Text Analysis - Politiken
# Maria Fernanda L F Del Ducca
# au516257  201400917

# Clean list
rm(list = ls())

# Set as working directory
setwd("~/Documents/Masters'/1.2019/R Files/Politiken")

# Install necessary packages
library(quanteda)
library(ggplot2)
library(topicmodels)

# Load data
load("~/Documents/Masters'/1.2019/R Files/Politiken/4. Complete_Politiken.RData")

#######

            ### Text Organization and Formatting ###

# Extract from my dataframe, as a value, only the texts from Politiken
quanteda_options("language_stemmer" = "danish")
articles.politiken <- gsub(":", " ", politiken$article, fixed = T) 

# Using tokens to remove symbols, punctuation, separators, urls from the words in the texts
articles.politiken <- tokens(articles.politiken, what = "word",
                      remove_punct = T,
                      remove_symbols = T,
                      remove_separators = T,
                      remove_url = T,
                      remove_hyphens = T,
                      verbose = T,
                      remove_twitter = T,
                      remove_numbers = T)

# Transforming all words to lower case
articles.politiken <- tokens_tolower(articles.politiken) 

# Removing danish stopwords
## Stopwords are words that do not bring any kind of information. For instance, "and", "for", are stopwords because we cannot infer anything from them
articles.politiken <- tokens_remove(articles.politiken, stopwords("danish")) 

# Stem words
articles.politiken <- tokens_wordstem(articles.politiken)

# Remove stopwords after stem the text
articles.politiken <- tokens_remove(articles.politiken, stopwords("danish")) 

# Creating a dfm (document feature matrix) with the tokenized articles
politiken.dfm <- dfm(articles.politiken)

#####
              ##### 1st Analysis #####

#### Activity: Analyse the text from the entire data set from Politiken
#### Goal: To observe what are the most typed words in the articles I found

## Top Features Observation

# Check the top features in the document feature matrix
# This is to observe if there is any other non-wanted object in the to be analysed dataframe
topfeatures(politiken.dfm) #The most frequent words are not important, so we need to remove them (i.e. kan, så, dansk, ved, ing)

# Create dataframe with most typed words
df <- data.frame(word = names(topfeatures(politiken.dfm)), count = topfeatures(politiken.dfm))

# Ggplot about the word count
ggplot(df,
       aes(x = reorder(df$word, df$count), y = df$count)) +
  geom_col(stat = "identity", fill = c("coral3", "steelblue", "darkgoldenrod", "forestgreen", "bisque3", "deepskyblue4", "darksalmon", "darkseagreen", "burlywood4", "brown4")) +
  coord_flip() +
  xlab("Words") + ylab("Count") +
  theme_minimal()

# Obs.: None of these words are relevant, because it means nothing to the research, so we have to exclude them
# Obs.: The exclusion process will go until we get some nice results

# Keep in the dfm only words that appear in more than 2 articles
politiken.dfm <- dfm_trim(politiken.dfm, min_termfreq = 2)

# Check top features
topfeatures(politiken.dfm) #Same result as above

# Removing words that does not mean anything to the research
# Selecting specific words (words that are not useful for my analysis) to be removed from my dataframe matrix
wordstoremove <- c("kan", "så", "dansk", "danmark", "kom", "ved", "år", "land", "sag",
                   "støjberg", "ing", "få", "tag", "fler", "dag", "gør", "tid",
                   "sid", "sam", "andr", "mer", "udlænding", "tal", "hel", "and", "giv", "to",
                   "fordi", "v", "får", "bland", "mul", "derfor", "helt", "nye", "person", "lig",
                   "send", "ifølg", "må", "først", "stor", "kun", "del")

# Removing words
politiken.dfm <- dfm_remove(politiken.dfm, wordstoremove)

# Checking the top features after removing non-wanted words
topfeatures(politiken.dfm)

# Create dataframe with most typed words after removing unwanted words
df2 <- data.frame(word = names(topfeatures(politiken.dfm)), count = topfeatures(politiken.dfm))

# Ggplot about the world count
ggplot(df2,
       aes(x = reorder(df2$word, df2$count), y = df2$count)) +
  geom_col(stat = "identity", fill = c("coral3", "steelblue", "darkgoldenrod", "forestgreen", "bisque3", "deepskyblue4", "darksalmon", "darkseagreen", "burlywood4", "brown4")) +
  coord_flip() +
  xlab("Words") + ylab("Count") +
  theme_minimal()

##

## Selecting features by importance in a document ##

# Create tf_idf-weighted dfm 
# (The Tf-idf is the frequency of a term adjusted for how rarely it is used)
politiken.tfidf <- dfm_tfidf(politiken.dfm)

# Select from main dfm using its top features
politiken.dfm <- dfm_keep(politiken.dfm, names(topfeatures(politiken.tfidf, n = 30)))

######

# Constructing feature-occurence matrix
politiken.fcm <- fcm(politiken.dfm)
top_politiken.fcm <- fcm_select(politiken.fcm, pattern = politiken.dfm)
textplot_network(top_politiken.fcm, min_freq = 0.5, edge_alpha = 0.5, edge_size = 1,
                 vertex_labelsize = 1.5 * rowSums(top_politiken.fcm/min(rowSums(top_politiken.fcm))))

# Creat wordcloud
textplot_wordcloud(politiken.dfm, min_size = 0.2, max_size = 4, min_count = 5,
                   max_words = 100, color = "black", font = NULL, 
                   adjust = 0, rotation = 0.1, random_order = FALSE, 
                   random_color = FALSE, ordered_color = FALSE)
######

## Run topic Model ##

# Goal: Want to observe the words appearing in the topic models, where I have 2 randomized clusters (thinking that there were two different governments)

# Convert 'quanteda dfm' to 'tm dtm' #better to run the topic models
politiken.dtm <- convert(politiken.dfm, to = "topicmodels")

# Using Latent Dirichlet Allocation (LDA) to run the topic model (Choice of 2 clusters)
politiken.lda <- LDA(politiken.dtm, k = 2)

# Review terms by topic
terms(politiken.lda, 20)

# Create a column with the topics in the original data frame
Topic <- data.frame(topics(politiken.lda))
Topic$row <- seq.int(nrow(Topic))
# Plot the frequencies for 2 clusters #Not important plot to the research, just to verify data
qplot(data = final.politiken, x = Topic, geom = "bar") #almost 50/50 the two topics and I didn't expect anything different

## Checking if there is better options to analyse the dataset

### Looking for the optimal k (number of clusters)

# Randomly sample test data
set.seed(5701)
select <- sample(1:nrow(politiken.dtm), size = 3990)
test <- politiken.dtm[select, ]
train <- politiken.dtm[!(1:nrow(politiken.dtm) %in% select), ]

n.tops <- 2:15 # The topics I am looking for can be divided from 2 to 15 different groups
metrics <- data.frame(topics = n.tops,
                      perplexity = NA)

# Checking for all the number of groups
for(i in n.tops) { 
  print(i)
  est <- LDA(train, k = i)
  metrics[(i - 1), "perplexity"] <- perplexity(est, newdata = test)
}


qplot(data = metrics, x = topics, y = perplexity, geom = "line",
      xlab = "Number of topics",
      ylab = "Perplexity on test data") + theme_minimal()

# Actually from the graph we see that the best way is to use 2,4,6,8,10,11,12,13,14,15 clusters
# We will use the Optimal k = 2

# Saving new file
save(final.politiken, file = "politiken.topics.RData")
