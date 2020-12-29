## ---- echo=TRUE, message=FALSE, warning=FALSE, include=FALSE------------------
options(warn=-1)
options(tidyverse.quiet = TRUE)
if(!require(tidyverse, warn.conflicts = FALSE)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(devtools)) install.packages("devtools")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(kableExtra)) install.packages("kableExtra")


## ---- echo=TRUE, message=FALSE, warning=FALSE, include=FALSE------------------
library(tidyverse, warn.conflicts = FALSE)
library(caret)
library(data.table)
library(lubridate)
library(devtools)
library(gridExtra)
library(kableExtra) 
library(plyr)
devtools::install_github('yihui/tinytex')
options(tinytex.verbose = TRUE)

#updating default size text of ggplots
update_geom_defaults("text", list(size = 16))

#Converting Markdown to R script.
knitr::purl("HousePricing.Rmd")

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)


## -----------------------------------------------------------------------------
read_csv <- function(file){
    path_data <- "data"
    filename <- paste(path_data,file,sep="/")
    csv__ <- read.csv(filename)
    csv__
}

test_set <-read_csv('test.csv')
train_set<- read_csv('train.csv')


#Train SET INFO
colnames_trian_set<-colnames(train_set)
memory_usage_train_set<-format(object.size(train_set),units="MB")
dim_train_set<- dim(train_set)

#TEST SET INFO
colnames_test_set<-colnames(test_set)
memory_usage_test_set<-format(object.size(test_set),units="MB")
dim_test_set <- dim(test_set)


## ---- echo=FALSE,  message=FALSE, warning=FALSE-------------------------------


#searching for the additional column in the train set
colname_diff <-setdiff(colnames_trian_set,colnames_test_set)
#Join datasets
df<- bind_rows(train_set,test_set)

#Selecting character columns
categorical_columns<-colnames(df %>% select(which(sapply(.,is.character))))
#Number of character columns
n_cat_cols <- length(categorical_columns)
#Display character columns in table

categorical_columns_tb <- matrix(categorical_columns,10,byrow=TRUE) %>%kable()%>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Categorical Columns"=5))

#Selecting numeric data
numerical_columns<-colnames(df %>% select(which(sapply(.,is.numeric))))
#Number of Numeric Columns
n_numeric_columns <- length(numerical_columns)
#Display numeric columns in Table
numerical_columns_tb <- matrix(numerical_columns,10,byrow=TRUE) %>%kable()%>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Numerical Columns"=4))


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
#Remove MSSubClass from numeric columns
numerical_columns <- numerical_columns[!(numerical_columns %in% c("MSSubClass"))]
#Append MSSubClass to categorical columns
categorical_columns<-append(categorical_columns,"MSSubClass")


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
#getting percentage of null values in each column
nan_columns <- sort(colMeans(is.na(df)))
nan_columns <- nan_columns[nan_columns>0]
nan_summary<- data.table("name"= names(nan_columns),"prc_na"=nan_columns) %>% arrange(desc(prc_na)) %>% mutate(type= ifelse(name %in% categorical_columns,"categorical","numerical"))

na_categorical <- nan_summary %>% filter(type=="categorical") %>% select(name,prc_na) %>%
  kable() %>%
  kable_styling(position = "left", full_width = FALSE) %>%
  column_spec(2, color = "white",
              background = spec_color(nan_summary$prc_na,option="B", end = 0.7))%>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Missing Categorical Columns"=2))
  
  
na_numerical <- nan_summary %>% filter(type=="numerical") %>% select(name,prc_na)%>%
  kable() %>%
  kable_styling(position = "left", full_width = FALSE) %>%
  column_spec(2, color = "white",
              background = spec_color(nan_summary$prc_na,option="B", end = 0.7))%>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Missing Numerical Columns"=2))
        
        


## -----------------------------------------------------------------------------

evaluation_quality <- data.table("quality" = c("NA","Po","Fa","TA","Gd","Ex"), "score"=c(-999999,0,1,2,3,4))
quality_columns<-names(train_set[,(grepl("Qu|Qua|QC|Cond",names(train_set)))&names(train_set) %in% categorical_columns])
for(col in quality_columns){
  train_set[,col]<- mapvalues(as.vector(train_set[,col]),evaluation_quality$quality,evaluation_quality$score)
  train_set[,col][is.na(train_set[col])]<--9999999
}

