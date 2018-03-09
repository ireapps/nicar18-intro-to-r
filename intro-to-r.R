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
chiCrime2017 <- read_csv("chicago_crime_2017.csv",
                        col_names = TRUE, na = c("", "NA"),
                        trim_ws = TRUE, guess_max = min(1000))
