# Collection of complete articles from DR
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
load("~/Documents/Masters'/1.2019/R Files/DR/2. Summaries_DR.RData")

# Filtering with wanted themes
dr.themes1 <- dr.unique %>% ##Filtering titles #859 observations
  filter(grepl("(integration)|(arbejdslovgivning)|(migration)|(immigrant)|(Støjberg)|(flytning)|(udlænding)|
               (udlændingelov)|(udlændingestyrelsen)|(opholdstilladelse)|(tilladelse)|(udvisning)|
               (hjemsendelse)|(nydansker)|(bibeskæftigelse)|(højtuddannet)|(Brooke Harrington)|(ulovligt arbejde)|(udenlandske forskere)|
               (bijob)|(udenlandske bijobbere)|(bijobbe)", titles, ignore.case = TRUE))

dr.themes2 <- dr.unique %>% ##Filtering summaries #1076 observations
  filter(grepl("(integration)|(arbejdslovgivning)|(migration)|(immigrant)|(Støjberg)|(flytning)|(udlænding)|
               (udlændingelov)|(udlændingestyrelsen)|(opholdstilladelse)|(tilladelse)|(udvisning)|
               (hjemsendelse)|(nydansker)|(bibeskæftigelse)|(højtuddannet)|(Brooke Harrington)|(ulovligt arbejde)|(udenlandske forskere)|
               (bijob)|(udenlandske bijobbere)|(bijobbe)", headlines, ignore.case = TRUE))

dr.filter <- rbind(dr.themes1,dr.themes2) #1935 observations
dr.filter <- unique(dr.filter) # 1434 observations

# Collect full articles
article <- rep("",nrow(dr.filter))
for(i in 1:nrow(dr.filter)){
  try({
    text <- read_html(as.character(dr.filter$urls[i])) %>%
      html_nodes(".dre-article-body") %>% 
      html_text(trim = T) 
    article[i] = paste(text, collapse = " ")
  })
}

article <- gsub('Send  Facebook(.*)dw\\.com/p/\\w+','',article)

# Creating new row with full articles
dr.filter$article <- article

# Removing rows without entire articles
dr.filter <- dr.filter %>%
  filter(grepl(" ", article, ignore.case = TRUE))

## 1431 observations

# Saving Articles without duplicates
save(dr.filter, file="3. Articles_DR.RData")


