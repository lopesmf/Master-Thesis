# Formatting dates and creating plots of frequency - Politiken
# Maria Fernanda L F Del Ducca
# au516257  201400917

# Clean list
rm(list = ls())

# Set as working directory
setwd("~/Documents/Masters'/1.2019/R Files/Politiken")

# Install necessary packages
library(zoo)
library(ggplot2)
library(dplyr)

# Load data
load("~/Documents/Masters'/1.2019/R Files/Politiken/3. Articles.RData")

#######

# Create an ID for each article according to the Date
politiken <- politiken[order(as.Date(politiken$dates, format="%Y-%b-%d")),]
politiken$ID <- seq.int(nrow(politiken))
politiken$ID <- as.numeric(politiken$ID)

  ### Checking for frequencies ###

# Tabulate dates
politiken_tab <- table(cut(politiken$dates, 'month'))

# Creating data frame with frequency of articles
politiken.frequency <- data.frame(Date = format(as.Date(names(politiken_tab)), '%m/%Y'), Frequency = as.vector(politiken_tab))

# Create row with month and year only (for the graph)
politiken$yearmonth <- as.yearmon(politiken$dates)
politiken$yearmonth <- as.Date(politiken$yearmonth)

# Graph: Published Articles in Politiken by month
qplot(data = politiken, x = yearmonth, 
      geom = "bar", xlab = "", 
      ylab = "Frequency") + 
  theme_minimal() + geom_bar(stat = "count", fill = "steelblue4") +
  theme(axis.text.x = element_text(angle = 0))

###

# Creating Row to differentiate the first to the second government
for (i in 1:nrow(politiken)){
  if(politiken$dates[i]>="2011-09-15" & politiken$dates[i] <= "2015-06-15"){
    politiken$Government[i] = 1
  } else if (politiken$dates[i]>="2015-06-16" & politiken$dates[i] <= "2019-03-20"){
    politiken$Government[i] = 2}
}

# Transform to number the column Government
politiken$Government <- as.numeric(politiken$Government)

# Deleting yearmonth column (not necessary anymore)
politiken$yearmonth <- NULL

save(politiken, file="4. Complete_Politiken.RData")
save(politiken.frequency, file = "4. Frequencytable_Politiken.RData")
