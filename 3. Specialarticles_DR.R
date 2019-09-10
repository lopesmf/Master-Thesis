# Scraping specific files
# Maria Fernanda Del Ducca
# au516257 201400917

# Clean list
rm(list = ls())

# Set as working directory
setwd("~/Documents/Masters'/1.2019/R Files/DR")

#installing necessary packages
library(rvest)
library(dplyr)

# Defining the url
url <- "https://www.dr.dk/search/Result?query=brooke+harrington+udenlandske+forskere"

# Reading the url
brooke_url <- read_html(url)

# Scraping page
titles <- brooke_url %>%
  html_nodes(".heading-medium")%>%
  html_text() %>%
  gsub("^\\s+|\\s+$", "", .)

urls <- brooke_url %>%
  html_nodes(".heading-medium") %>%
  html_attr("href")

dates <- brooke_url %>%
  html_nodes("time") %>%
  html_text()

brooke.articles <- data.frame(titles = titles,
                              urls = urls,
                              dates = dates,
                              stringsAsFactors = FALSE)

# Colecting headlines
brooke.headlines <- rep("",nrow(brooke.articles))
for(i in 1:nrow(brooke.articles)){
  try({
    brooke.text <- read_html(as.character(brooke.articles$urls[i])) %>%
      html_nodes(".dre-article-title__summary") %>% 
      html_text(trim = T) 
    brooke.headlines[i] = paste(brooke.text, collapse = " ")
  })
}

brooke.articles$headlines <- brooke.headlines

# Removing rows without headlines
brooke.articles <- brooke.articles %>%
  filter(grepl(" ", headlines, ignore.case = TRUE))

##

# Collecting full texts

article <- rep("",nrow(brooke.articles))
for(i in 1:nrow(brooke.articles)){
  try({
    brooke.text <- read_html(as.character(brooke.articles$urls[i])) %>%
      html_nodes(".dre-container__content--small p") %>% 
      html_text(trim = T) 
    article[i] = paste(brooke.text, collapse = " ")
  })
}

article <- gsub('Send  Facebook(.*)dw\\.com/p/\\w+','',article)
brooke.articles$article <- article

# Filter only news from 15.09.2011 and 20.03.2019 and formatting dates
Sys.setlocale("LC_TIME", "da_DK.UTF-8") #Transforming system to danish language
date1 <- as.Date(brooke.articles$dates, "%d. %b. %Y")
date2 <- as.Date(brooke.articles$dates, "%d. %b %Y")
date1[is.na(date1)] <- date2[is.na(date1)]

brooke.articles$dates <- date1

# Output full articles
save(brooke.articles, file="3. Specialarticles_DR.RData")
