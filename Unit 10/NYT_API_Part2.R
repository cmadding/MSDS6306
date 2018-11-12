NYTIMES_KEY="2f797325c5fc4d00aee61d2ae3e615d1"
Sys.setenv(NYTIMES_KEY="2f797325c5fc4d00aee61d2ae3e615d1")
install.packages("devtools")
devtools::install_github("mkearney/nytimes")
install.packages("jsonlite")
library(jsonlite)
library(devtools)

x <- fromJSON("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=mueller&api-key=2f797325c5fc4d00aee61d2ae3e615d1", flatten = TRUE) %>% data.frame()

list(name = c("persons", "organizations", "subject"), value = c("Mueller, Robert S III", "Federal Bureau of Investigation", "United States Politics and Government"), rank = 1:3, major = c("N", "N", "N"))

# Let's set some parameters
term <- "central+park+jogger" # Need to use + to string together separate words
begin_date <- "19890419"
end_date <- "19890901"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1)


