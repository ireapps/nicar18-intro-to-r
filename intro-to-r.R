# Intro to R (pre-registered class)
# NICAR 2018
# Saturday, March 10

# Charles Minshew, IRE-NICAR (charles@ire.org)
# Olga Pierce, ProPublica (olga.pierce@propublica.org)
# Ryann Grochowski Jones, ProPublica (ryann.jones@propublica.org)

# set working directory
setwd("C:/Users/Administrator/Desktop/hands_on_classes/Intro_to_R_pre_registered_attendees_only_1098")

# when you use a package on a computer for the first time, you need to install it
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("readxl")


# load packages
library(tidyverse)
library(lubridate)
library(stringr)
library(readxl)
library(lubridate)
library(tidyr)


## http://blog.rstudio.com/2015/04/09/readr-0-1-0/

# load data -----------
chiCrime2017 <- read_csv("chicago_crime_2017.csv",
                         col_names = TRUE, na = c("", "NA"),
                         trim_ws = TRUE, guess_max = min(1000))

# there's also a general purpose function for loading data that is 
# not as nicely formatted 

# we can specify column names and column types
# names <- colnames(chiCrime2017)
# we can also choose to load only some of the columns by using cols_only


# chiCrime2017_notused <- read_delim("chicago_crime_2017.csv", delim = ',' , col_names = names, 
# col_types = cols_only(ID = col_integer(), 
# `Case Number` = col_character(),
# Date = col_character(),
# Block = col_character(),
# IUCR = col_character(),
# `Primary Type` = col_character(),
# Description = col_character(),
# `Location Description` = col_character(),
# Arrest = col_logical(),
# Domestic = col_logical(),
# Beat = col_integer(),
# District = col_integer(),
# Ward = col_integer(),
# `Community Area` = col_integer(),
# `FBI Code` = col_character()
# ))

# rm(chiCrime2017_notused)

# Now we want to load a tab from an excel spreadsheet
# But we don't remember how
# We can pull up the documentation for a function using a '?' before the name

?read_excel

# The help pane shows us what arguments are needed for the function
# We want just the tab from the our excel file

IL_schools <- read_excel("practice.xlsx", sheet = "IL_schools")

View(head(IL_schools))

# We're done with IL_schools though, so let's remove it from our environment
rm(IL_schools)

# Exercise 0 ------
# Read in the cpd_ucr_codes.csv table with the name cpd_ucr
cpd_ucr <- read_csv("cpd_ucr_codes.csv")

# Now let's start manipulating our data
# In the tidyverse, there are two ways to use a function

# The first is to type out the function, complete with arguments
# Try this:
?select

# Now let's use 'select' to grab just the case numbers from our crime data

cases <- select(chiCrime2017, `Case Number`: Block)

# But there's another way to use many R functions, that can be more concise and
# easier to read

# It's called a pipe and looks like this %>% 

cases <- chiCrime2017 %>% select(-`Case Number`)

# Piping is most helpful if we are doing more than one operation
# This code selects just the case ID numbers and then sorts them

IDs_sorted <- chiCrime2017 %>% select(ID) %>% arrange(ID)

rm(IDs_sorted)

# Another useful verb is mutate, it lets us either alter an existing column,
# or create one or more new ones
# Let's use a mutate function to properly format the 'Date' field in a new column

# First we'll separate the date itself from the time
chiCrime2017 <- chiCrime2017 %>% separate(Date, sep = " ", into = c("date_short","time"))

# Then we'll use the lubridate package to date-format the date_short column
chiCrime2017 <- chiCrime2017 %>% mutate(date_fixed = mdy(date_short))

# Let's take a glance to see if it worked
View(head(chiCrime2017))  # This shows us the first 6 rows

# Or this, which lets us specify the range of rows we want to see
View(chiCrime2017 %>% slice(1:100))

# We got no errors, but it's good practice to not just assume a transformation worked
# We can use the 'count' function to find out how many times each value appears, in particular we keep an eye out for NAs

dates <- chiCrime2017 %>% count(date_fixed)

# We can find them by looking at the tail of dates (helpfully,NAs always go at the end)
View(tail(dates))
rm(dates)

# Now that we have date-formatted our column, we can use functions on it or do math like finding the difference between dates

# Let's practice this by making a column with just the month of each date
chiCrime2017 <- chiCrime2017 %>% mutate(date_month = month(date_fixed))

# Exercise 1 --------
# Find the number of cases for each Primary Type
# Hint: when there's a space in the name you must use ``


# Another use of mutate is to recode variables
# Let's make a switch for District 10
chiCrime2017 <- chiCrime2017 %>% 
  mutate(district_10 = ifelse(District == '10',1,0))

chiCrime2017 %>% count(district_10)

# Joining tables -------
# Joining tables is useful when some of our data is in one table and some is in another
# We can use the join functions to bring two tables together

# First let's view the cpd_ucr table
View(head(cpd_ucr))

# When we're doing a join, we need to figure out what column the two tables have in common
# In this case the field is the 'IUCR' field

# There are different types of join, which you can study up on later, but in this case
# we want all the rows from chiCrime2017, but only those rows from cpd_ucr that match that table, so we want a left join

chiCrime2017_joined <- left_join(chiCrime2017, cpd_ucr)

# dplyr guesses which column we want to join on, but it's good to check to make sure you agree with its choices

# We can use another useful verb, filter which lets us choose only certain rows
# Let's find just cases of BATTERY

chiCrime_battery <- chiCrime2017 %>% filter(`Primary Type` == "BATTERY")

# We can also use multiple criteria

chiCrime2017_bar_sidewalk <- chiCrime2017 %>% 
  filter(`Location Description` == 'BAR OR TAVERN' | `Location Description` == 'SIDEWALK')

# One last thing that's useful: group_by and summarize
# For this, let's use one of the built in R tables

View(mtcars)

# Now let's group by cylinders and find the mean horsepower

cylindrical_horse <- mtcars %>% group_by(cyl) %>% summarize(mean_hp = mean(hp), max_mpg = max(mpg))

###################################
####### DATA VISUALIZATION ########
###################################

# load El Ridership data
elRidership <- read_csv("cta_ridership_12_17.csv")

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
  mutate(date_clean = mdy(date))

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
ggplot(grandRidership,aes(date_clean,rides, color=daytype)) + geom_point() + 
  facet_grid(.~stationname)

# This can be hard to see what's going on, but we easily see yearly trends.
# Let's filter further for just one year of data - 2017.
# There are other ways to filter, but this is easier for changing to different dates.
grandRidership17 <- filter(grandRidership, date_clean >= "2017-01-01" & date_clean <= "2017-12-31")

# What does our most recent plot look like when filtered to one year?
ggplot(grandRidership17,aes(date_clean,rides, color=daytype)) + geom_line() + 
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
jpeg("myChart.jpeg")
myChart
dev.off() # This is very important to end the export!

# Next up, EPS (for using in Adobe Illustrator)
setEPS()
postscript('output/myChart.eps')
myChart
dev.off()

write_csv(grandStateRidership17, "grand_state.csv")
