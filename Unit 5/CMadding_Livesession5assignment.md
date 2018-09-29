---
title: "CMadding_Livesession5assignment"
author: "Chad Madding"
date: "September 30, 2018"
output:
  html_document:
    keep_md: true
---



####Questions

   Backstory: Your client is expecting a baby soon.  However, he is not sure what to name the child.  Being out of the loop, he hires you to help him figure out popular names.  He provides for you raw data in order to help you make a decision.

####1.	Data Munging (30 points):
   
   Utilize yob2016.txt for this question. This file is a series of popular children’s names born in the year 2016 in the United States.  It consists of three columns with a first name, a gender, and the amount of children given that name.  However, the data is raw and will need cleaning to make it tidy and usable.

   a.	First, import the .txt file into R so you can process it. Keep in mind this is not a CSV file.  You might have to open the file to see what you’re dealing with.  Assign the resulting data frame to an object, df, that consists of three columns with human-readable column names for each.

```r
#Read in the 2015 data
yob2016 <- read.csv("yob2016.txt", ";", header = FALSE)
#Change the name
df <- data.frame(yob2016)
#name the columns
colnames(df) <- c("Name","Gender","Number")
```
   
   
   b.	Display the summary and structure of df

```r
str(df)
```

```
## 'data.frame':	32869 obs. of  3 variables:
##  $ Name  : Factor w/ 30295 levels "Aaban","Aabha",..: 9317 22546 3770 26409 12019 20596 6185 339 9298 11222 ...
##  $ Gender: Factor w/ 2 levels "F","M": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Number: int  19414 19246 16237 16070 14722 14366 13030 11699 10926 10733 ...
```

```r
summary(df)
```

```
##       Name       Gender        Number       
##  Aalijah:    2   F:18758   Min.   :    5.0  
##  Aaliyan:    2   M:14111   1st Qu.:    7.0  
##  Aamari :    2             Median :   12.0  
##  Aarian :    2             Mean   :  110.7  
##  Aarin  :    2             3rd Qu.:   30.0  
##  Aaris  :    2             Max.   :19414.0  
##  (Other):32857
```

   c.	Your client tells you that there is a problem with the raw file. One name was entered twice and misspelled.  The client cannot remember which name it is; there are thousands he saw! But he did mention he accidentally put three y’s at the end of the name.  Write an R command to figure out which name it is and display it.

```r
#Change name to character
df$Name <- as.character(df$Name)
#find mispelled name
yyyname <- df[grep("yyy", df$Name), ]
yyyname
```

```
##         Name Gender Number
## 212 Fionayyy      F   1547
```

   d.	Upon finding the misspelled name, please remove this particular observation, as the client says it’s redundant. Save the remaining dataset as an object: y2016

```r
#remove the yyyname and rename the df
y2016 <- df[-grep("yyy", df$Name),]
```

####2.	Data Merging (30 points):
   
   Utilize yob2015.txt for this question.  This file is similar to yob2016, but contains names, gender, and total children given that name for the year 2015.

   a.	Like 1a, please import the .txt file into R.  Look at the file before you do.  You might have to change some options to import it properly.  Again, please give the dataframe human-readable column names. Assign the dataframe to y2015.

```r
#Read in the 2015 data and set it for a comma
y2015 <- read.csv("yob2015.txt", ",", header = FALSE)
#name the columns
colnames(y2015) <- c("Name","Gender","Number")
#Change name to character to match 2016 data
y2015$Name <- as.character(y2015$Name)
```

   b.	Display the last ten rows in the dataframe. Describe something you find interesting about these 10 rows.

#####I found it insteresting that the last 10 rows contained only males and had only 5 for each name.


```r
#displat the last 10 rows
tail(y2015, 10)
```

```
##         Name Gender Number
## 33054   Ziyu      M      5
## 33055   Zoel      M      5
## 33056  Zohar      M      5
## 33057 Zolton      M      5
## 33058   Zyah      M      5
## 33059 Zykell      M      5
## 33060 Zyking      M      5
## 33061  Zykir      M      5
## 33062  Zyrus      M      5
## 33063   Zyus      M      5
```
   
   c.	Merge y2016 and y2015 by your Name column; assign it to final.  The client only cares about names that have data for both 2016 and 2015; there should be no NA values in either of your amount of children rows after merging.

```r
#merge 2015 and 2016 data by Name
final <- merge(y2016, y2015, all = FALSE, by = c("Name", "Gender"))
#renaming the columns to make them more readable
colnames(final) <- c("Name", "Gender", "Total2016", "Total2015")
#getting a total of NAs in each column
colSums(is.na.data.frame(final))
```

```
##      Name    Gender Total2016 Total2015 
##         0         0         0         0
```
   

####3.	Data Summary (30 points):

   Utilize your data frame object final for this part.

   a.	Create a new column called “Total” in final that adds the amount of children in 2015 and 2016 together.

```r
#create a Total column by adding 2015 and 2016 data
final$Total <- with(final, Total2016 + Total2015)
```
   In those two years combined, how many people were given popular names?

```r
#add together total ampunt of names
sum(final$Total)
```

```
## [1] 7239231
```
   
   b.	Sort the data by Total. What are the top 10 most popular names?

```r
#final sorted by Total
final <- final[order(-final$Total),]
#Looking at the top 10 names
head(final, 10)
```

```
##           Name Gender Total2016 Total2015 Total
## 8290      Emma      F     19414     20415 39829
## 19886   Olivia      F     19246     19638 38884
## 19594     Noah      M     19015     19594 38609
## 16114     Liam      M     18138     18330 36468
## 23273   Sophia      F     16070     17381 33451
## 3252       Ava      F     16237     16340 32577
## 17715    Mason      M     15192     16591 31783
## 25241  William      M     15668     15863 31531
## 10993    Jacob      M     14416     15914 30330
## 10682 Isabella      F     14722     15574 30296
```
   c.	The client is expecting a girl!  Omit boys and give the top 10 most popular girl’s names.

```r
#create top girls name list
topGirls <- final[final$Gender == "F",] #remove M from final data
#create top boys name list
topBoys <- final[final$Gender == "M",] #remove F from final data
#create a top 10 girls list
top10Girls <- topGirls[1:10,]
top10Girls
```

```
##            Name Gender Total2016 Total2015 Total
## 8290       Emma      F     19414     20415 39829
## 19886    Olivia      F     19246     19638 38884
## 23273    Sophia      F     16070     17381 33451
## 3252        Ava      F     16237     16340 32577
## 10682  Isabella      F     14722     15574 30296
## 18247       Mia      F     14366     14871 29237
## 5493  Charlotte      F     13030     11381 24411
## 277     Abigail      F     11699     12371 24070
## 8273      Emily      F     10926     11766 22692
## 9980     Harper      F     10733     10283 21016
```
   d.	Write these top 10 girl names and their Totals to a CSV file. Leave out the other columns entirely.

```r
#remove the id column
row.names(top10Girls) <- c()
#create a top 10 with just Name and Total
listTop10Girls <- within(top10Girls, rm("Gender","Total2016","Total2015"))
listTop10Girls
```

```
##         Name Total
## 1       Emma 39829
## 2     Olivia 38884
## 3     Sophia 33451
## 4        Ava 32577
## 5   Isabella 30296
## 6        Mia 29237
## 7  Charlotte 24411
## 8    Abigail 24070
## 9      Emily 22692
## 10    Harper 21016
```

```r
#export the top 10 to a CSV file
write.csv(listTop10Girls, "listTop10Girls.csv")
#check to make sure the file was exported
list.files()
```

```
##  [1] "CMadding_Livesession5assignment.html"
##  [2] "CMadding_Livesession5assignment.md"  
##  [3] "CMadding_Livesession5assignment.Rmd" 
##  [4] "codebook_final.Rmd"                  
##  [5] "codebook_y2015.Rmd"                  
##  [6] "codebook_y2016.pdf"                  
##  [7] "codebook_y2016.Rmd"                  
##  [8] "codebook_yob2016.Rmd"                
##  [9] "listTop10Girls.csv"                  
## [10] "Unit 05.Rproj"                       
## [11] "yob2015.txt"                         
## [12] "yob2016.txt"
```

####4.	Upload to GitHub (10 points):

   Push at minimum your RMarkdown for this homework assignment and a Codebook to one of your GitHub repositories (you might place this in a Homework repo like last week). The Codebook should contain a short definition of each object you create, and if creating multiple files, which file it is contained in. You are welcome and encouraged to add other files—just make sure you have a description and directions that are helpful for the grader.

```
## Data report generation is finished. Please wait while your output file is being rendered.
```

```
## 
##  Is codebook_y2015.docx open on your computer? Please close it as fast as possible to avoid problems!
```

```
## Data report generation is finished. Please wait while your output file is being rendered.
```

```
## 
##  Is codebook_yob2016.docx open on your computer? Please close it as fast as possible to avoid problems!
```

```
## Data report generation is finished. Please wait while your output file is being rendered.
```

```
## 
##  Is codebook_final.docx open on your computer? Please close it as fast as possible to avoid problems!
```

All files maybe accesses at:
https://github.com/cmadding/MSDS6306/tree/master/Unit%205

####Reminder

To complete this assignment, please submit one RMarkdown and matching HTML file before 11:59 pm Monday October 1. You do not need to submit a link to your GitHub: just note where the assignment is (URL) in your RMarkdown file. Make sure it is public!! Please submit all files at the same time; only one submission is granted.

Good luck!
