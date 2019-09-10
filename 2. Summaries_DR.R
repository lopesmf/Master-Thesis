# Full Summaries from DR
# Maria Fernanda Del Ducca
# au516257 201400917

# Clean list
rm(list = ls())

# Set as working directory
setwd("~/Documents/Masters'/1.2019/R Files/DR")

# Install necessary packages
library(dplyr)
library(rvest)

# Load RData file
load("~/Documents/Masters'/1.2019/R Files/DR/1. titles_DR.RData")

headlines <- rep("",nrow(dr.unique))
for(i in 1:nrow(dr.unique)){
  try({
    text <- read_html(as.character(dr.unique$urls[i])) %>%
      html_nodes(".dre-article-title__summary") %>% 
      html_text(trim = T) 
    headlines[i] = paste(text, collapse = " ")
  })
}

dr.unique$headlines <- headlines

# Removing rows without headlines
dr.unique <- dr.unique %>%
  filter(grepl(" ", headlines, ignore.case = TRUE))

# Saving Summaries without duplicates
save(dr.unique, file="2. Summaries_DR.RData")
