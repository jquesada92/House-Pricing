## ---- echo=TRUE, message=FALSE, warning=FALSE, include=FALSE--------------------------------------
options(warn=-1)
options(tidyverse.quiet = TRUE)
if(!require(tidyverse, warn.conflicts = FALSE)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(devtools)) install.packages("devtools")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(ggraph)) install.packages("ggraph")
if(!require(igraph)) install.packages("igraph")


## ---- echo=TRUE, message=FALSE, warning=FALSE, include=FALSE--------------------------------------
library(tidyverse, warn.conflicts = FALSE)
library(caret)
library(data.table)
library(lubridate)
library(devtools)
library(gridExtra)
library(kableExtra) 
library(plyr)
library(Matrix)
options(tinytex.verbose = TRUE)

#updating default size text of ggplots
update_geom_defaults("text", list(size = 16))

#Converting Markdown to R script.
knitr::purl("HousePricing.Rmd")

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)



#mode function get most frequent value.
mode<- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}



#Label_quality_Conditions encoder.
Quac_Cond_decoder <- function(df,col){
evaluation_quality <- data.table("quality" = c("Po","Fa","TA","Gd","Ex"), "score"=c(1,2,3,4,5))
mapvalues(as.vector(df[,col]),evaluation_quality$quality,evaluation_quality$score)}


## -------------------------------------------------------------------------------------------------
read_csv <- function(file){
    path_data <- "data"
    filename <- paste(path_data,file,sep="/")
    csv__ <- read.csv(filename)
    csv__
}

test_set <-read_csv('test.csv')
train_set<- read_csv('train.csv')


#Join datasets, For this project we going to join train and set data for the cleansing and EDA, later we going to split again by SalesPrices not null as train set and test set is null.
df<- bind_rows(train_set,test_set)


## ---- echo=FALSE,warning=FALSE,message=FALSE------------------------------------------------------


#Train SET INFO
colnames_trian_set<-colnames(train_set)
memory_usage_train_set<-format(object.size(train_set),units="MB")
dim_train_set<- dim(train_set)

#TEST SET INFO
colnames_test_set<-colnames(test_set)
memory_usage_test_set<-format(object.size(test_set),units="MB")
dim_test_set <- dim(test_set)


## ---- echo=FALSE,  message=FALSE, warning=FALSE---------------------------------------------------

#searching for the additional column in the train set
colname_diff <-setdiff(colnames_trian_set,colnames_test_set)

#Selecting character columns
categorical_columns<- colnames(df %>% select(which(sapply(.,is.character))))
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


## ---- echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------
#Remove MSSubClass from numeric columns
numerical_columns <- numerical_columns[!(numerical_columns %in% c("MSSubClass"))]
#Append MSSubClass to categorical columns
categorical_columns<-append(categorical_columns,"MSSubClass")


## ---- echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------
#getting percentage of null values in each column
missing_values<- function(df){
nan_columns <- sort(colMeans(is.na(df)))
nan_summary<- data.table("name"= names(nan_columns),"prc_na"=nan_columns) %>% arrange(desc(prc_na)) %>% mutate(type= ifelse(name %in% categorical_columns,"categorical","numerical"))
  nan_summary}
nan_summary<- missing_values(df)  %>% filter(prc_na>0)
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




## ---- echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------
df <-df[, order(names(df))]
#By looking in the description file, i bring columns that allow NA.
add_features <- c("Alley"
,"MasVnr"
,"Bsmt"
,"Fireplace"
,"Pool"
,"Heating"
,"Fence"
,"Misc"
,"Kitchen"
,"Exterior"
,"Garage")


#Create 3 empty values for save results
name_features<-NULL
dim_features <- NULL
dtype <-NULL
#set position in our lists
n <-0
#loop through our detected features
for ( f in add_features){
  #selecting columns that contains feature in name.
  f_df = df[,grepl(f, names(df))]
  num_cols =length(names(f_df))
  #check if there is true missing data.
  
   for(add_f in names(f_df)){
    n = n+1
    if(add_f %in% categorical_columns){
      dtype[n] = "categorical"}
    else{dtype[n]="numeric"}
   
    name_features[n] = f 
    dim_features[n]= add_f
   }
  
    
}

#I created a temporal data table to display features and columns.  
temp_df <- data.table(name_features,dim_features,dtype)
temp_df %>% kable()%>% pack_rows(index = table(fct_inorder(temp_df$name_features) )) %>% 
  kable_styling() %>%
  add_header_above(c("Related Features"=3))


## ---- echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------
rm(temp_df)
df<- df %>% mutate(
          PoolQC = ifelse(is.na(PoolArea)|PoolArea==0,"No Pool",PoolQC),
          FireplaceQu = ifelse(is.na(Fireplaces)|Fireplaces==0,"No Fireplace",FireplaceQu),
          Alley = ifelse(is.na(Alley),"None Alley",Alley),
          Fence = ifelse(is.na(Fence),"None Fence",Fence),
          MasVnrType = ifelse(MasVnrArea==0|is.na(MasVnrArea), "None MasVnr",MasVnrType)
         )


## ---- message=FALSE, warning=FALSE, echo=FALSE----------------------------------------------------

MasVnr_temp <- df  %>% filter(MasVnrType!="None MasVnr")

grid.arrange(
      grid.arrange(  MasVnr_temp %>%  ggplot() +
                    geom_bar(aes(y=MasVnrType))
                   ,MasVnr_temp %>% 
                    ggplot(aes(x=MasVnrType,y=MasVnrArea)) +
                    geom_boxplot() +
                    stat_summary(fun=mean,geom="point")+
                    coord_flip()
                    ,ncol = 2, nrow = 1),
       MasVnr_temp %>% group_by(YearBuilt,MasVnrType) %>% dplyr::summarise(count_=n()) %>%
         ggplot(aes(x=YearBuilt,y=count_,col=MasVnrType)) + 
        geom_line()+
        theme(legend.justification = c(0,1),
              legend.position = c(0,1),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
        scale_x_continuous(breaks=seq(min(MasVnr_temp$YearBuilt),max(MasVnr_temp$YearBuilt),5)),
      heights=c(1/3, 2/3)
)



## ---- echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------
mode_masvnr <- MasVnr_temp   %>% group_by(YearBuilt) %>% dplyr::summarise(t_mode =mode(MasVnrType))
df<- df %>% left_join(mode_masvnr,  by="YearBuilt") %>%
  mutate(MasVnrType = ifelse(is.na(MasVnrType),t_mode,MasVnrType))%>%
  select(-t_mode) %>%
  mutate(MasVnrArea=ifelse(MasVnrType=="None MasVnr",0,MasVnrArea))
rm(MasVnr_temp)
missing_values(df[grepl("MasVnr",names(df))])  %>% kable()%>%
  kable_styling() %>%
  add_header_above(c("Mansory Veneer"=3))


## -------------------------------------------------------------------------------------------------



## ---- warning=FALSE,message=FALSE, echo=FALSE-----------------------------------------------------

df <- df %>% mutate(BsmtFinType1 = ifelse(BsmtFinSF1==0, "No Basment 1", BsmtFinType1),
             BsmtFinType1 = ifelse(BsmtFinSF2==0, "No Basment 2", BsmtFinType2),
             BsmtCond = ifelse(TotalBsmtSF==0, "No Basment", BsmtCond),
             BsmtQual = ifelse(TotalBsmtSF==0, "No Basment", BsmtQual),
             BsmtExposure = ifelse(TotalBsmtSF==0, "No Basment", BsmtExposure),
             BsmtFullBath = ifelse(TotalBsmtSF==0, 0, BsmtFullBath),
             BsmtHalfBath = ifelse(TotalBsmtSF==0, 0, BsmtHalfBath)
)

             

missing_values(df[grepl("Bsmt",names(df))]) %>% kable()%>%
  kable_styling() %>%
  add_header_above(c("Basement"=3))


