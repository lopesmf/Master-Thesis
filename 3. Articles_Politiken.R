# Collect complete articles from Politiken
# Maria Fernanda Del Ducca
# au516257 201400917

# Clean list
rm(list = ls())

# Set as working directory
setwd("~/Documents/Masters'/1.2019/R Files/Politiken")

# Install necessary packages
library(dplyr)
library(rvest)

# Load RData file
load("~/Documents/Masters'/1.2019/R Files/Politiken/2. Summaries_Politiken.RData")

####

# Filtering summaries according to the needs

# Filtering with wanted themes
politiken.themes1 <- politiken.summaries %>% #1491 observations
  filter(grepl("(integration)|(arbejdslovgivning)|(migration)|(immigrant)|(Støjberg)|(flytning)|(udlænding)|
               (udlændingelov)|(udlændingestyrelsen)|(opholdstilladelse)|(tilladelse)|(udvisning)|
               (hjemsendelse)|(nydansker)|(bibeskæftigelse)|(højtuddannet)|(Brooke Harrington)|(ulovligt arbejde)|(udenlandske forskere)|
               (bijob)|(udenlandske bijobbere)|(bijobbe)", titles, ignore.case = TRUE))

politiken.themes2 <- politiken.summaries %>% #2184 observations
  filter(grepl("(integration)|(arbejdslovgivning)|(migration)|(immigrant)|(Støjberg)|(flytning)|(udlænding)|
               (udlændingelov)|(udlændingestyrelsen)|(opholdstilladelse)|(tilladelse)|(udvisning)|
               (hjemsendelse)|(nydansker)|(bibeskæftigelse)|(højtuddannet)|(Brooke Harrington)|(ulovligt arbejde)|(udenlandske forskere)|
               (bijob)|(udenlandske bijobbere)|(bijobbe)", summary, ignore.case = TRUE))

politiken.filter <- rbind(politiken.themes1,politiken.themes2) ##3675 observations
politiken.filter <- unique(politiken.filter) ##2931 observations

##### Collecting Full Texts #####

# Login in the website
login <- "https://medielogin.dk/politiken/login?redirect=%2Fopenid%2Fendpoint%3Fopenid.ns%3Dhttp%3A%252F%252Fspecs.openid.net%252Fauth%252F2.0%26openid.claimed_id%3Dhttp%3A%252F%252Fspecs.openid.net%252Fauth%252F2.0%252Fidentifier_select%26openid.identity%3Dhttp%3A%252F%252Fspecs.openid.net%252Fauth%252F2.0%252Fidentifier_select%26openid.return_to%3Dhttps%3A%252F%252Fpolitiken.dk%252F%253Fpolid_return%253D1556061648%26openid.realm%3Dhttps%3A%252F%252Fpolitiken.dk%26openid.assoc_handle%3D7FNp!IAAAAJOSsCUfDPIhEzFBywNx1aXHKOZanVsMLPzmtapZJI3tQQAAAAEvGB5AgUqaWQPLeSFCYZf9FrsoqDOLz1jwhFWSebEvBo2JaUdfcjULF5tkWHI4GDSYH04oXa8S0roaQVQuJMwA%26openid.mode%3Dcheckid_setup%26openid.ns.ext1%3Dhttp%3A%252F%252Fopenid.net%252Fsrv%252Fax%252F1.0%26openid.ext1.brand%3Dpolitiken"

pgsession <- html_session(login)
pgform <-html_form(pgsession)[[1]]

filled_form <- set_values(pgform, Username="EMAIL", Password = "PASSWORD")
submit_form(pgsession, filled_form)

# Collect full texts
article <- rep("",nrow(politiken.filter))

for(i in 1:nrow(politiken.filter)){
  url <- as.character(politiken.filter$urls[i])
  final <- jump_to(pgsession, url)
  
  try({
    text <- read_html(final) %>%
      html_nodes(".body__h3 , .body__p") %>% 
      html_text(trim = T) 
    article[i] = paste(text, collapse = " ")
  })
}

# Creating new row with full text
politiken.filter$article <- article # 2931 observations

# Removing rows without entire articles
politiken <- politiken.filter %>%
  filter(grepl(" ", article, ignore.case = TRUE)) #2906 observations

# Output full articles
save(politiken, file="3. Articles.RData")

# 2906 observations
