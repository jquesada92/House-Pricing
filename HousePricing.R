## ---- echo=TRUE, message=FALSE, warning=FALSE, include=FALSE------------------
options(warn=-1)
options(tidyverse.quiet = TRUE)
if(!require(tidyverse, warn.conflicts = FALSE)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(devtools)) install.packages("devtools")
if(!require(grid)) install.packages("grid")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(corrgram)) install.packages("corrgram")
if(!require(reshape2)) install.packages("reshape2")


## ---- echo=TRUE, message=FALSE, warning=FALSE, include=FALSE------------------
library(tidyverse, warn.conflicts = FALSE)
library(caret)
library(data.table)
library(lubridate)
library(devtools)
library(grid)
library(gridExtra)
library(kableExtra) 
library(plyr)
library(corrgram)
library(reshape2)

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


## -----------------------------------------------------------------------------
read_csv <- function(file){
    path_data <- "data"
    filename <- paste(path_data,file,sep="/")
    csv__ <- read.csv(filename)
    csv__
}

test_set <-read_csv('test.csv')
train_set<- read_csv('train.csv')


#Join datasets, For this project we going to join train and set data for the cleansing and EDA,
#later we going to split again by SalesPrices not null as train set and test set is null.
df<- bind_rows(train_set,test_set)


## ---- echo=FALSE,warning=FALSE,message=FALSE----------------------------------
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


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
rm(colname_diff)
rm(categorical_columns_tb)
rm(numerical_columns_tb)
#Remove MSSubClass from numeric columns
numerical_columns <- numerical_columns[!(numerical_columns %in% c("MSSubClass"))]
#Append MSSubClass to categorical columns
categorical_columns<-append(categorical_columns,"MSSubClass")


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
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




## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
rm(nan_summary)
rm(na_numerical)
rm(na_categorical)
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


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------

QC_Cond_decoder <- function(col,No_feature){
evaluation_quality <- data.table("quality" = c(No_feature,"Po","Fa","TA","Gd","Ex"), "score"=c(0,1,2,3,4,5))
mapvalues(as.vector(col),evaluation_quality$quality,evaluation_quality$score)}

df<- df %>% mutate(PoolArea= ifelse(is.na(PoolArea),0,PoolArea)) %>% mutate(PoolQC=QC_Cond_decoder(ifelse(PoolArea==0,"No Pool", PoolQC),"No Pool"))

df %>% ggplot(aes(y=PoolArea,x=as.factor(PoolQC))) + geom_boxplot()





## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
df<- df %>% mutate(PoolQC=ifelse(is.na(PoolQC),5,PoolQC))
missing_values(df[,grepl("Pool",names(df))])%>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Pool Missing Values"=3))


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
grid.arrange(df %>% ggplot(aes(x=MiscFeature)) + geom_bar()+labs(title="MiscFeature"),df %>% ggplot(aes(x=Alley)) + geom_bar()+labs(title="Alley"),df %>% ggplot(aes(x=Fence)) + geom_bar()+labs(title="Fence"),ncol=3
             )


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
df <- df %>% mutate(MiscFeature=ifelse(is.na(MiscFeature),"No Feature",MiscFeature),
                    Alley=ifelse(is.na(Alley),"No Alley",Alley),
                    Fence=ifelse(is.na(Fence),"No Fence",Fence)
                    )
missing_values(df[,c("MiscFeature","Alley","Fence")])%>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("MiscF, Alley & Fence Missing Values"=3))


## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
df<- df %>% mutate(MasVnrType = ifelse(MasVnrArea==0|is.na(MasVnrArea), "None MasVnr",MasVnrType))

MasVnr_temp <- df  %>% filter(MasVnrType!="None MasVnr")

grid.arrange(
      grid.arrange(  MasVnr_temp %>%  ggplot() +
                    geom_bar(aes(y=MasVnrType))
                   ,MasVnr_temp %>% 
                    ggplot(aes(x=MasVnrType,y=MasVnrArea)) +
                    geom_boxplot() +
                    stat_summary(fun=mean,geom="point")+
                    coord_flip()
                    ,ncol = 2, nrow = 1,top=textGrob("Mansory Veneer",gp=gpar(fontsize=20,font=3))),
       MasVnr_temp %>% group_by(YearBuilt,MasVnrType) %>% dplyr::summarise(count_=n()) %>%
         ggplot(aes(x=YearBuilt,y=count_,col=MasVnrType)) + 
        geom_line()+
        theme(legend.justification = c(0,1),
              legend.position = c(0,1),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
        scale_x_continuous(breaks=seq(min(MasVnr_temp$YearBuilt),max(MasVnr_temp$YearBuilt),5)),
      heights=c(1/3, 2/3))



## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
mode_masvnr <- MasVnr_temp   %>% group_by(YearBuilt) %>% dplyr::summarise(t_mode =mode(MasVnrType))
df<- df %>% left_join(mode_masvnr,  by="YearBuilt") %>%
  mutate(MasVnrType = ifelse(is.na(MasVnrType),t_mode,MasVnrType))%>%
  select(-t_mode) %>%
  mutate(MasVnrArea=ifelse(MasVnrType=="None MasVnr",0,MasVnrArea))
rm(MasVnr_temp)
missing_values(df[grepl("MasVnr",names(df))])  %>% kable()%>%
  kable_styling() %>%
  add_header_above(c("Mansory Veneer"=3))


## ---- warning=FALSE,message=FALSE, echo=FALSE---------------------------------
bsmt_cols <- grepl("Bsmt",names(df))
#Label_quality_Conditions encoder.
bsmt_finT <- function(col,No_feature){
evaluation_quality <- data.table("quality" = c(No_feature,"Unf","LwQ","Rec","BLQ","ALQ","GLQ"), "score"=c(0,1,2,3,4,5,6))
mapvalues(as.vector(col),evaluation_quality$quality,evaluation_quality$score)}


bsmt_Exposure <- function(col){
evaluation_quality <- data.table("quality" = c("No Basement","No","Mn","Av","Gd"), "score"=c(0,1,2,3,4))
mapvalues(as.vector(col),evaluation_quality$quality,evaluation_quality$score)}

df <- df %>% 
  mutate(BsmtExposure=ifelse(is.na(BsmtExposure),"No",BsmtExposure),
         TotalBsmtSF = ifelse(is.na(TotalBsmtSF),0,TotalBsmtSF),
         BsmtUnfSF = ifelse(is.na(BsmtUnfSF),0,BsmtUnfSF),
         BsmtFinSF1 = ifelse(is.na(BsmtFinSF1),0,BsmtFinSF1),
         BsmtFinSF2 = ifelse(is.na(BsmtFinSF2),0,BsmtFinSF2)
         )%>%
  mutate(
             BsmtFinType1 = as.numeric(bsmt_finT(ifelse(BsmtFinSF1==0|is.na(BsmtFinSF1), "No Basement 1", BsmtFinType1), "No Basement 1")),
             BsmtFinType2 = as.numeric(bsmt_finT(ifelse(BsmtFinSF2==0|is.na(BsmtFinSF2), "No Basement 2", BsmtFinType2), "No Basement 2")),
             BsmtCond = as.numeric(QC_Cond_decoder(ifelse(TotalBsmtSF+BsmtUnfSF==0, "No Basement", BsmtCond),"No Basement")),
             BsmtQual =  as.numeric(QC_Cond_decoder(ifelse(TotalBsmtSF+BsmtUnfSF==0, "No Basement", BsmtQual),"No Basement")),
             BsmtExposure =  as.numeric(bsmt_Exposure(ifelse(TotalBsmtSF==0, "No Basement", BsmtExposure))),
             BsmtFullBath = ifelse(TotalBsmtSF==0, 0, BsmtFullBath),
             BsmtHalfBath = ifelse(TotalBsmtSF==0, 0, BsmtHalfBath))

df["BsmtUnfSF"] <- ifelse(df$TotalBsmtSF>0,(df$BsmtUnfSF)/df$TotalBsmtSF,0)
missing_values(df[,bsmt_cols])  %>% kable()%>%
  kable_styling() %>%
  add_header_above(c("Mansory Veneer"=3))


## ---- warning=FALSE,message=FALSE, echo=FALSE---------------------------------
bsmt_cor<-cor(df[rowMeans(is.na(df[,grepl("Bsmt",names(df))]))==0,grepl("Bsmt",names(df))])
temp <- melt(bsmt_cor) %>% filter(value>=0.5 & Var1!=Var2) %>% arrange(-value)
temp <-  temp[seq(1,nrow(temp),2),]%>% kable()%>%
  kable_styling() %>%
  add_header_above(c("High Correlated Dim" =4))
corrgram(df[rowMeans(is.na(df[,bsmt_cols]))==0,bsmt_cols],  lower.panel=panel.shade,
  upper.panel=NULL, text.panel=panel.txt,
  main="Basement Dimensions")



## ---- echo=FALSE,message=FALSE,warning=FALSE----------------------------------
avg_sf <- mean(df$TotalBsmtSF)
sd_sf<- sd(df$TotalBsmtSF)
z<- (df$TotalBsmtSF -avg_sf)/sd_sf
histogram(z,main="TotalBsmtSF(Zscore)")


## ---- echo=FALSE,message=FALSE,warning=FALSE----------------------------------
BsmtQual_model<- lm(BsmtQual~TotalBsmtSF,data=filter(df,abs(z)<2.5))
BsmtQual_pred  <- lapply(predict(BsmtQual_model,data=df,newdata=df),as.integer)

df<- df %>% mutate(predic=as.integer(predict(lm(BsmtQual~TotalBsmtSF,data=.),newdata=.))) %>% mutate(SE_= (BsmtQual-predic)^2)
df %>% filter(abs(z)<2.5) %>%ggplot(aes(x=TotalBsmtSF,y=BsmtQual)) + geom_point() + geom_smooth() + labs(title="BsmtQual Vs TotalBsmtSF")

rm(BsmtQual_model)
rm(BsmtQual_pred)


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
bsmtCond_mode <- mode(df$BsmtCond)
bsmtFinF2_mode <- mode(df$BsmtFinType2)
df<- df %>% mutate(BsmtQual=ifelse(is.na(BsmtQual),predic,BsmtQual),
                   BsmtCond = ifelse(is.na(BsmtCond),bsmtCond_mode,BsmtCond),
                   BsmtFinType2 = ifelse(is.na(BsmtFinType2),bsmtFinF2_mode ,BsmtFinType2)) %>%
                   select(-predic,-SE_)
missing_values(df[,bsmt_cols]) %>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Bsmt Missing Values"=3))


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
df<- df %>% mutate(Fireplaces=ifelse(is.na(Fireplaces),0,Fireplaces)) %>% mutate(FireplaceQu=QC_Cond_decoder(ifelse(Fireplaces==0,"No Fireplace",FireplaceQu),"No Fireplace"))
missing_values(df[,grepl("Fire",names(df))]) %>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Fireplace Missing Values"=3)) 

