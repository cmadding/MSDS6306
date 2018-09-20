# Load the needed libraries
library(dplyr)
library(ggplot2)
library(rvest)
library(stringr)
library(tidyr)
library(data.table)
library(reshape2)

# Read data from website
HotnessDirect <- read_html("https://boardgamegeek.com/")

Hotness_html <- html_nodes(HotnessDirect,'#results_hotitems .innermoduletable a')

# Converting the data to text
Hotness_data <- html_text(Hotness_html)

#Convert data to a data frame
Hotness_data = data.frame(Hotness_data)

# change class to character
Hotness_data[] <- lapply(Hotness_data, as.character)

# add column names
names(Hotness_data) = c("The Hotness")
