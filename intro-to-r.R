# Intro to R (pre-registered class)
# NICAR 2018
# Saturday, March 10

# Charles Minshew, IRE-NICAR (charles@ire.org)
# Olga Pierce, ProPublica (olga.pierce@propublica.org)
# Ryann Grochowski Jones, ProPublica (ryann.jones@propublica.org)

# set working directory
setwd("C:\\Users\\RGrochowski\\Desktop\\intro-to-r")

# load packages
library(tidyverse)
library(lubridate)

## http://blog.rstudio.com/2015/04/09/readr-0-1-0/

# load data
chiCrime2017 <- read_csv("data/chicago_crime_2017.csv",
                        col_names = TRUE, na = c("", "NA"),
                        trim_ws = TRUE, guess_max = min(1000))


###################################
####### DATA VISUALIZATION ########
###################################

# load El Ridership data
elRidership <- read_csv("data/cta_ridership_12_17.csv")

# Inspect the data in a View and see what we're working with
View(elRidership)

# One of the most basic and common graphs we can do in R is the histogram.
# Before we make a histogram, let's get some stats on ridership numbers in the table.
summary(elRidership$rides)

# Since we're looking at daily ridership at each station, let's see what the most common numbers are.
hist(elRidership$rides)

# This histogram is very skewed to the left. We've got a few days where ridership is HIGH.
# Let's change the number of breaks.
hist(elRidership$rides, breaks = 5)

# OK. Now we see that most of our records show daily station ridership of less than 10,000 rides.
# Let's increase the breaks.
hist(elRidership$rides, breaks = 10)

# Let's specify the breaks this time. We're very interested in breaking rides below 10,000 down.
hist(elRidership$rides, breaks = c(0,5000,10000,15000,20000,25000,30000,35000,40000))

# It's pretty clear. Ridership in Chicago's metro system is shared across many stations. 

# Let's look at ridership over time now. 
# Our dates are not in a good format. Let's clean those using lubridate in a new column. 
elRidership <- elRidership %>%
  mutate(date_clean = as.Date(date, "%m/%d/%y"))

# This is a lot of data to work with, so let's filter for just one station near us.
# There are two stops with Grand in it's name. So we need to use grep to find them.
grandRidership <- filter(elRidership, grepl("Grand", stationname))

# Let's look at what we're left with now.
table(grandRidership$stationname) # We're going to be left with two stations here. 
                                  # Grand/Milwaukee and Grand/State

# Base R provides us with some standard plots. Not going to be useful but can provide a cursory look.
plot(grandRidership$date_clean,grandRidership$rides)

# Let's try this same plot in ggplot2.
# NOTE: aes() is for 'aesthetic mapping' This helps us standardize names.
# We have to declare the data frame, the x and y values inside aes() as well as the graph type.
ggplot(grandRidership,aes(date_clean,rides)) + geom_point() # Points
ggplot(grandRidership,aes(date_clean,rides)) + geom_line() # Lines
ggplot(grandRidership,aes(date_clean,rides)) + geom_bar(stat='identity') # Bars

# Okay, a little cleaner, but what if we color by 'stationname'
ggplot(grandRidership,aes(date_clean,rides, color=stationname)) + geom_point()
ggplot(grandRidership,aes(date_clean,rides, color=stationname)) + geom_line()
ggplot(grandRidership,aes(date_clean,rides, color=stationname)) + geom_bar(stat='identity')

# Let's facet this graph by daytype
ggplot(grandRidership,aes(date_clean,rides, color=stationname)) + geom_point() + 
  facet_grid(.~daytype)

# This can be hard to see what's going on, but we easily see yearly trends.
# Let's filter further for just one year of data - 2017.
# There are other ways to filter, but this is easier for changing to different dates.
grandRidership17 <- filter(grandRidership, date_clean >= "2017-01-01" & date_clean <= "2017-12-31")

# What does our most recent plot look like when filtered to one year?
ggplot(grandRidership17,aes(date_clean,rides, color=daytype)) + geom_point() + 
  facet_grid(.~stationname)

# We're not going to facet anymore. That was facet-nating, right? 
# But let's take a look at how we can modify our charts.
# For this chart, we're looking at Grand/State only. Filtering one more time.

grandStateRidership17 <- filter(grandRidership17, stationname == "Grand/State")

# And we'll plot.
ggplot(grandStateRidership17,aes(date_clean,rides)) + geom_point() # Just the points.

# This is a little messy but you can see the weekdays, Saturdays and Sundays.
ggplot(grandStateRidership17, aes(date_clean, rides, fill=daytype)) +
  geom_bar(stat="identity")

# This time, we'll look at lines.
ggplot(grandStateRidership17, aes(date_clean,rides)) + geom_line()

# I can also see these together. It's kind of messy but it can be done.
ggplot(grandStateRidership17, aes(date_clean,rides)) + geom_line() + geom_point()

# I really like the line for a possible graphic showing ridership at
# Grand/State throughout 2017. Let's add a headline. We're also going to store this whole block of code.

myChart <- ggplot(grandStateRidership17, aes(date_clean,rides)) + 
  geom_line() +
  labs(title="Grand/State station daily ridership in 2017") +
  labs(subtitle = "Daily ridership at the Grand/State metro station as counted by the Chicago Transit Authority") +
  labs(x = "Date") +
  labs(y = "Number of rides") +
  labs(caption = "Charles Minshew/IRE and NICAR")

# Since we stored the chart as 'myChart', we need to run this in order to show it.
myChart

# Now let's output our graphic to a file!
# First JPEG
jpeg('output/myChart.jpeg')
myChart
dev.off() # This is very important to end the export!

# Next up, EPS (for using in Adobe Illustrator)
setEPS()
postscript('output/myChart.eps')
myChart
dev.off()






