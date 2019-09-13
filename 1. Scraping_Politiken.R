# Scraping pages in Politiken
# Maria Fernanda Del Ducca
# au516257 201400917

# Clean list
rm(list = ls())

# Set as working directory
setwd("~/Documents/Masters'/1.2019/R Files/Politiken")

# Install necessary packages
library(rvest)
library(data.table)

# Defining URL
url <- "https://politiken.dk/arkiv/"
articles <- list()

# Generating dates
Enddate <- as.Date("2019/3/20", format = "%Y/%m/%d")
Startdate <- as.Date("2011/9/15", format = "%Y/%m/%d")

format.dates <- seq(Startdate, Enddate, by = "days")

format.dates <- format(format.dates, "%Y/%m/%d")

# Looping
for (page in format.dates){
  final_url <- paste0(url,page,"/")
  
  politiken.html <- read_html(final_url)
  
  titles <- politiken.html %>%
    html_nodes(".headline--xxsmall") %>%
    html_text(trim = T)
  
  urls <- politiken.html %>%
    html_nodes(".archive-article__link") %>%
    html_attr("href")
  
  dates <- politiken.html %>%
    html_nodes('.archive-article__time') %>%
    html_text(trim = T) %>%
    gsub("^\\s+|\\s+$", "", .)
  
  articles[[page]] <- data.frame(titles = titles,
                                 urls = urls,
                                 dates = dates,
                                 stringsAsFactors = FALSE)
}

# Concatenate all pages
politiken.articles <- as.data.frame(rbindlist(articles))

# Remove duplicated articles
politiken.unique <- unique(politiken.articles)

# Cleaning dates
politiken.unique$dates <- substr(politiken.unique$dates,1,13)

# Filter only news from 15.09.2011 and 20.03.2019 and formatting dates
Sys.setlocale("LC_TIME", "da_DK.UTF-8") #Transforming system to danish language
date1 <- as.Date(politiken.unique$dates, "%d. %b. %Y")
date2 <- as.Date(politiken.unique$dates, "%d. %b %Y")
date1[is.na(date1)] <- date2[is.na(date1)]

politiken.unique$dates <- date1

# Shape the final data with the desired space of time: From 15.09.2011 to 20.03.2019
politiken.unique <- politiken.unique[politiken.unique$dates >= "2011-09-15" & politiken.unique$dates <= "2019-03-20",]

# Removing NAs from dataaset
politiken.unique <- na.omit(politiken.unique)

# Output final data
save(politiken.unique, file="1. Titles_Politiken.RData")
