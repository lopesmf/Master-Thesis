# Formatting dates and creating plots of frequency - DR
# Maria Fernanda L F Del Ducca
# au516257  201400917

# Clean list
rm(list = ls())

# Set as working directory
setwd("~/Documents/Masters'/1.2019/R Files/DR")

# Install necessary packages
library(zoo)
library(ggplot2)

# Load data
load("~/Documents/Masters'/1.2019/R Files/DR/3. Specialarticles_DR.RData")
load("~/Documents/Masters'/1.2019/R Files/DR/3. Articles_DR.RData")

#######

# Merging two files
dr.complete <- rbind.data.frame(brooke.articles,dr.filter)

# Create an ID for each article according to the Date
dr.complete <- dr.complete[order(as.Date(dr.complete$dates, format="%Y-%b-%d")),]
dr.complete$ID <- seq.int(nrow(dr.complete))
dr.complete$ID <- as.numeric(dr.complete$ID)

      ### Checking for frequencies ###

# Tabulate dates
dr_tab <- table(cut(dr.complete$dates, 'month'))

# Creating data frame with frequency of articles
dr.frequency <- data.frame(Date = format(as.Date(names(dr_tab)), '%m/%Y'), Frequency = as.vector(dr_tab))

# Create row with month and year only (for the graph)
dr.complete$yearmonth <- as.yearmon(dr.complete$dates)
dr.complete$yearmonth <- as.Date(dr.complete$yearmonth)

# Graph: Published Articles in DR by month
qplot(data = dr.complete, x = yearmonth, 
      geom = "bar", xlab = "", 
      ylab = "Frequency") + 
  theme_minimal() + geom_bar(stat = "count", fill = "steelblue4") +
  theme(axis.text.x = element_text(angle = 0))

###

# Creating Row to differentiate the first to the second government

for (i in 1:nrow(dr.complete)){
  if(dr.complete$dates[i]>="2011-09-15" & dr.complete$dates[i] <= "2015-06-15"){
    dr.complete$Government[i] = 1
  } else if (dr.complete$dates[i]>="2015-06-16" & dr.complete$dates[i] <= "2019-03-20"){
    dr.complete$Government[i] = 2}
}

# Transform to number the column Government
dr.complete$Government <- as.numeric(dr.complete$Government)

# Deleting yearmonth column (not necessary anymore)
dr.complete$yearmonth <- NULL

save(dr.complete, file="4. Complete_DR.RData")
save(dr.frequency, file = "4. Frequencytable_DR.RData")
write.csv(dr.complete, file="4. Complete_DR.csv", row.names = F)
