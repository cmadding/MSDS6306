# Load the needed libraries
library(dplyr)
library(ggplot2)
library(rvest)
library(stringr)
library(tidyr)
library(data.table)
library(reshape2)

# Read data from website
QTDirect <- read_html("https://www.imdb.com/name/nm0000233/?ref_=nv_sr_1#director")

QTDirect_html <- html_nodes(QTDirect,':nth-child(8) .filmo-row b a')
QTDirectYear_html <- html_nodes(QTDirect,':nth-child(8) .filmo-row .year_column')

# Converting the data to text
QTDirect_data <- html_text(QTDirect_html)
QTDirectYear_data <- html_text(QTDirectYear_html)

#Convert data to a data frame
QTDirect_data = data.frame(QTDirect_data)
QTDirectYear_data = data.frame(QTDirectYear_data)

# change class to character
QTDirect_data[] <- lapply(QTDirect_data, as.character)
QTDirectYear_data[] <- lapply(QTDirectYear_data, as.character)

# add column names
names(QTDirect_data) = c("Movies Directed By Quentin Tarantino")
names(QTDirectYear_data) = c("Year")

#Combine data into one table
QuentinTarantinoDirect <- cbind(QTDirect_data,QTDirectYear_data)
