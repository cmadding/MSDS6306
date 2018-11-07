# author: Jared Lander
#
# hard coded call to API
theCall <- "http://api.nytimes.com/svc/search/v1/article?format=json&query=nytd_section_facet:[Sports]&fields=url,title,body&rank=newest&offset=0&api-key=2f797325c5fc4d00aee61d2ae3e615d1"

# we need the rjson, plyr, and RTextTools packages
require(plyr)
require(rjson)
require(RTextTools)

## first let's look at an individual call
res1 <- fromJSON(file=theCall)
# how long is the result
length(res1$results)
# look at the first item
res1$results[[1]]
# the first item's title
res1$results[[1]]$title
# the first item converted to a data.frame, Viewed in the data viewer
View(as.data.frame(res1$results[[1]]))

# convert the call results into a data.frame, should be 10 rows by 3 columns
resList1 <- ldply(res1$results, as.data.frame)
View(resList1)
## now let's build this for multiple calls
# build a string where we will substitute the section for the
first %s and offset for the second %s
theCall <- "http://api.nytimes.com/svc/search/v1/article?format=json&query=nytd_section_facet:[%s]&fields=url,title,body&rank=newest&offset=%s&api-key=2f797325c5fc4d00aee61d2ae3e615d1"
# create an empty list to hold 3 result sets
resultsSports <- vector("list", 3)
## loop through 0, 1 and 2 to call the API for each value
for(i in 0:2)
{
  # first build the query string replacing the first %s with Sport and the second %s with the current value of i
  tempCall <- sprintf(theCall, "Sports", i)
  # make the query and get the json response
  tempJson <- fromJSON(file=tempCall)
  # convert the json into a 10x3 data.frame and save it to the list
  resultsSports[[i + 1]] <- ldply(tempJson$results,
                                  as.data.frame)
}
# convert the list into a data.frame
resultsDFSports <- ldply(resultsSports)

# make a new column indicating this comes from Sports
resultsDFSports$Section <- "Sports"
## repeat that whole business for arts
## ideally you would do this in a more eloquent manner, but this is just for illustration
resultsArts <- vector("list", 3)
for(i in 0:2)
{
  tempCall <- sprintf(theCall, "Arts", i)
  tempJson <- fromJSON(file=tempCall)
  resultsArts[[i + 1]] <- ldply(tempJson$results,
                                as.data.frame)
}
resultsDFArts <- ldply(resultsArts)
resultsDFArts$Section <- "Arts"
# combine them both into one data.frame
resultBig <- rbind(resultsDFArts, resultsDFSports)
dim(resultBig)
View(resultBig)
## now time for tokenizing
# create the document-term matrix in english, removing numbers and stop words and stemming words
doc_matrix <- create_matrix(resultBig$body, language="english",
                            removeNumbers=TRUE, removeStopwords=TRUE, stemWords=TRUE)
doc_matrix
View(as.matrix(doc_matrix))
# create a training and testing set
theOrder <- sample(60)
container <- create_container(matrix=doc_matrix,
                              labels=resultBig$Section, trainSize=theOrder[1:40],
                              testSize=theOrder[41:60], virgin=FALSE)

