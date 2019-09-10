# Scraping the 'Politik' Section from DR Newspaper
# Maria Fernanda Del Ducca
# au516257 201400917

# Clean list
rm(list = ls())

# Set as working directory
setwd("~/Documents/Masters'/1.2019/R Files/DR")

# Install necessary packages
library(rvest)
library(lubridate)
library(data.table)
library(dplyr)
library(stringr)

# Defining URL
url <- "https://www.dr.dk/nyheder/allenyheder/politik/"
articles <- list()

# Generating dates
Enddate <- as.Date("20032019", format = '%d%m%Y')
Startdate <- as.Date("15092011", format = '%d%m%Y')

format.dates <- seq(Startdate, Enddate, by = "days")

format.dates <- format(format.dates, "%d%m%Y")

# Looping
for (page in format.dates){
  final_url <- paste0(url,page)
  
  dr.html <- read_html(final_url)
  
  titles <- dr.html %>%
    html_nodes("section:first-child article h3 a") %>%
    html_text()

  urls <- dr.html %>%
    html_nodes("section:first-child article h3 a") %>%
    html_attr("href")
  
  dates <- dr.html %>%
    html_nodes("section:first-child article h3+.metainfo") %>%
    html_text()
  
  articles[[page]] <- data.frame(titles = titles,
                                 urls = urls,
                                 dates = dates,
                                 stringsAsFactors = FALSE)

}

# Concatenate all pages
dr.articles <- as.data.frame(rbindlist(articles))

# Remove duplicated articles
dr.unique <- unique(dr.articles)

# Organizing URLs
dr.unique$urls <- paste0("https://www.dr.dk",dr.unique$urls)

# Cleaning dates
dr.unique$dates <- substr(dr.unique$dates,1,13)

# Filter only news from 15.09.2011 and 20.03.2019 and formatting dates
Sys.setlocale("LC_TIME", "da_DK.UTF-8") #Transforming system to danish language
date1 <- as.Date(dr.unique$dates, "%d. %b. %Y")
date2 <- as.Date(dr.unique$dates, "%d. %b %Y")
date1[is.na(date1)] <- date2[is.na(date1)]

dr.unique$dates <- date1

# Shape the final data with the desired space of time: From 15.09.2011 to 20.03.2019
dr.unique <- dr.unique[dr.unique$dates >= "2011-09-15" & dr.unique$dates <= "2019-03-20",]

# Removing NAs from dataaset
dr.unique <- na.omit(dr.unique)

# Output final data
save(dr.unique, file="1. titles_DR.RData")
