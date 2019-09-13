# Full summaries from Politiken
# Maria Fernanda Del Ducca
# au516257 201400917

# Clean list
rm(list = ls())

# Set as working directory
setwd("~/Documents/Masters'/1.2019/R Files/Politiken")

# Install necessary packages
library(rvest)
library(tidyr)
library(dplyr)

# Load RData file
load("~/Documents/Masters'/1.2019/R Files/Politiken/1. Titles_Politiken.RData")

    ## Filtering links necessary to the scraping of summaries ##

# Obs.: This filtering process is due to the file politiken.unique has a total of 326,917 observations. 
# To scrape everything is a long and not necessary process because while scraping the titles I obtained articles not interesting for the research, as sport or travel

politiken.themes <- politiken.unique %>%
  filter(grepl("(debat)|(indland)|(udland)", urls, ignore.case = TRUE))

    ## Scraping Summaries ##

# Login in the website
login <- "https://medielogin.dk/politiken/login?redirect=%2Fopenid%2Fendpoint%3Fopenid.ns%3Dhttp%3A%252F%252Fspecs.openid.net%252Fauth%252F2.0%26openid.claimed_id%3Dhttp%3A%252F%252Fspecs.openid.net%252Fauth%252F2.0%252Fidentifier_select%26openid.identity%3Dhttp%3A%252F%252Fspecs.openid.net%252Fauth%252F2.0%252Fidentifier_select%26openid.return_to%3Dhttps%3A%252F%252Fpolitiken.dk%252F%253Fpolid_return%253D1556061648%26openid.realm%3Dhttps%3A%252F%252Fpolitiken.dk%26openid.assoc_handle%3D7FNp!IAAAAJOSsCUfDPIhEzFBywNx1aXHKOZanVsMLPzmtapZJI3tQQAAAAEvGB5AgUqaWQPLeSFCYZf9FrsoqDOLz1jwhFWSebEvBo2JaUdfcjULF5tkWHI4GDSYH04oXa8S0roaQVQuJMwA%26openid.mode%3Dcheckid_setup%26openid.ns.ext1%3Dhttp%3A%252F%252Fopenid.net%252Fsrv%252Fax%252F1.0%26openid.ext1.brand%3Dpolitiken"

pgsession <- html_session(login)
pgform<-html_form(pgsession)[[1]]

filled_form <- set_values(pgform, Username="EMAIL", Password = "PASSWORD")
submit_form(pgsession, filled_form)

### Collecting summaries ###
# I had to collect with two different codes because the summaries had different nodes

# Collecting summaries 1
summaries1 <- rep("",nrow(politiken.themes))

for(i in 1:nrow(politiken.themes)){
  url <- as.character(politiken.themes$urls[i])
  final1 <- jump_to(pgsession, url)
  
  try({
    text1 <- read_html(final1) %>%
      html_nodes(".article-top .summary--large") %>% 
      html_text(trim = T) 
    summaries1[i] = paste(text1, collapse = " ")
  })
}

# Collecting summaries 2
summaries2 <- rep("",nrow(politiken.themes))

for(i in 1:nrow(politiken.themes)){
  url <- as.character(politiken.themes$urls[i])
  final2 <- jump_to(pgsession, url)
  
  try({
    text2 <- read_html(final2) %>%
      html_nodes(".article-top .summary__p") %>% 
      html_text(trim = T) 
    summaries2[i] = paste(text2, collapse = " ")
  })
}


politiken.themes$Summaries1 <- summaries1
politiken.themes$Summaries2 <- summaries2

# Arrange nicely Summaries column
politiken.themes <- unite(politiken.themes, summary, c(Summaries1,Summaries2), sep = "_", remove = TRUE)

# Remove rows with no content in summaries
politiken.summaries <- politiken.themes %>%
  filter(grepl(" ", summary, ignore.case = TRUE))

# Saving Summaries without duplicates
save(politiken.summaries, file="2. Summaries_Politiken.RData")
