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
if(!require(moments)) install.packages("moments")
if(!require(glmnet)) install.packages("glmnet")
if(!require(randomForest)) install.packages("randomForest")


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
library(moments) 
library(glmnet)
library(randomForest)
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


cor_SalesPrice<- function(df,columns){
  df_ <- df[,names(df)%in% c(columns,"SalePrice")]
  cor(df_,use="pairwise.complete.obs")
}

high_cor_cols<- function(df){
  melt(df) %>% filter(Var1=="SalePrice" & (round(abs(value),1)>=0.5 & Var1!=Var2)) %>% arrange(-abs(value))
}


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
categorical_columns<- colnames(df %>% select(which(sapply(.,is.character))))
numerical_columns<-colnames(df %>% select(which(sapply(.,is.numeric))))
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
,"Exter"
,"Garage"
,"Lot")


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
  kable_styling(font_size = 10) %>%
  add_header_above(c("Related Features"=3))


## -----------------------------------------------------------------------------
#HERE IM STORIGN COLUMNS TO DROP 
cols_to_Drop <- NULL


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
#Replace with Average
z_score_lotfrtage <- (df$LotFrontage - mean(df$LotFrontage,na.rm=TRUE))/sd(df$LotFrontage,na.rm=TRUE)
histogram(z_score_lotfrtage,main="LotFrontage Zscore")


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
filter_mean <- df %>% filter(abs(z_score_lotfrtage)<=2.5) %>% summarise(avg_=  mean(LotFrontage,na.rm=TRUE)) %>% pull(avg_)
df$LotFrontage <- ifelse(is.na(df$LotFrontage),filter_mean ,df$LotFrontage)


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------

QC_Cond_decoder <- function(col,No_feature){
evaluation_quality <- data.table("quality" = c(No_feature,"Po","Fa","TA","Gd","Ex"), "score"=c(0,1,2,3,4,5))
mapvalues(as.vector(col),evaluation_quality$quality,evaluation_quality$score)}

df<- df %>% mutate(PoolArea= ifelse(is.na(PoolArea),0,PoolArea)) %>% mutate(PoolQC=QC_Cond_decoder(ifelse(PoolArea==0,"No Pool", PoolQC),"No Pool"))

df %>% ggplot(aes(y=PoolArea,x=as.factor(PoolQC))) + geom_boxplot()





## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
df<- df %>% mutate(PoolQC=as.numeric(ifelse(is.na(PoolQC),5,PoolQC))) 
missing_values(df[,grepl("Pool",names(df))])%>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Pool Missing Values"=3))


## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
df<- df %>% mutate(MasVnrType = ifelse(MasVnrArea==0|is.na(MasVnrArea), "None MasVnr",MasVnrType))
Mas_cols <- grepl("Mas",names(df))
MasVnr_temp <- df  %>% filter(MasVnrType!="None MasVnr")

grid.arrange(
      grid.arrange(  MasVnr_temp %>%  ggplot() +
                    geom_bar(aes(y=MasVnrType))
                   ,MasVnr_temp %>% 
                    ggplot(aes(x=MasVnrType,y=MasVnrArea)) +
                    geom_boxplot() +
                    stat_summary(fun=mean,geom="point")+
                    coord_flip()
                    ,ncol = 2, nrow = 1,top=textGrob("Mansory Veneer",gp=gpar(fontsize=15,font=3))),
       MasVnr_temp %>% group_by(YearRemodAdd,MasVnrType) %>% dplyr::summarise(count_=n()) %>%
         ggplot(aes(x=YearRemodAdd,y=count_,col=MasVnrType)) + 
        geom_line()+
        theme(legend.justification = c(0,1),
              legend.position = c(0,1),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
        scale_x_continuous(breaks=seq(min(MasVnr_temp$YearRemodAdd),max(MasVnr_temp$YearRemodAdd),5)),
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
evaluation_quality <- data.table("quality" = c("No_Basement","No","Mn","Av","Gd"), "score"=c(0,1,2,3,4))
mapvalues(as.vector(col),evaluation_quality$quality,evaluation_quality$score)}

df <- df %>% 
  mutate(BsmtExposure=ifelse(is.na(BsmtExposure),"No",BsmtExposure),
         TotalBsmtSF = ifelse(is.na(TotalBsmtSF),0,TotalBsmtSF),
         BsmtUnfSF = ifelse(is.na(BsmtUnfSF),0,BsmtUnfSF),
         BsmtFinSF1 = ifelse(is.na(BsmtFinSF1),0,BsmtFinSF1),
         BsmtFinSF2 = ifelse(is.na(BsmtFinSF2),0,BsmtFinSF2)
         )%>%
  mutate(
             BsmtFinType1 = as.numeric(bsmt_finT(ifelse(BsmtFinSF1==0|is.na(BsmtFinSF1), "No_Basement1", BsmtFinType1), "No_Basement1")),
             BsmtFinType2 = as.numeric(bsmt_finT(ifelse(BsmtFinSF2==0|is.na(BsmtFinSF2), "No_Basement2", BsmtFinType2), "No_Basement2")),
             BsmtCond = as.numeric(QC_Cond_decoder(ifelse(TotalBsmtSF==0, "No_Basement", BsmtCond),"No_Basement")),
             BsmtQual =  as.numeric(QC_Cond_decoder(ifelse(TotalBsmtSF==0, "No_Basement", BsmtQual),"No_Basement")),
             BsmtExposure =  as.numeric(bsmt_Exposure(ifelse(TotalBsmtSF==0, "No_Basement", BsmtExposure))),
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
BsmtQual_model<- lm(BsmtQual~TotalBsmtSF,data=filter(df,abs(z)<=2.5))
BsmtQual_pred  <- lapply(predict(BsmtQual_model,data=df,newdata=df),as.integer)

df<- df %>% mutate(predic=as.integer(predict(lm(BsmtQual~TotalBsmtSF,data=.),newdata=.))) %>% mutate(SE_= (BsmtQual-predic)^2)
df %>% filter(abs(z)<=2.5) %>%ggplot(aes(x=TotalBsmtSF,y=BsmtQual)) + geom_point() + geom_smooth() + labs(title="BsmtQual Vs TotalBsmtSF")

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

## -----------------------------------------------------------------------------
bsmt_cor <- cor_SalesPrice(df,names(df[grepl('Bsmt',names(df))]))
bsmt_cor<-high_cor_cols(bsmt_cor)
cols_to_Drop <- c(cols_to_Drop,names(df[,bsmt_cols&!names(df)%in% bsmt_cor$Var2]) )
bsmt_cor%>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Bsmt Columns to Keep"=3))


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
df<- df %>% mutate(Fireplaces=ifelse(is.na(Fireplaces),0,Fireplaces)) %>% 
  mutate(FireplaceQu=as.numeric(QC_Cond_decoder(ifelse(Fireplaces==0,"No_Fireplace",FireplaceQu),"No_Fireplace")))

missing_values(df[,grepl("Fire",names(df))]) %>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Fireplace Missing Values"=3)) 


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
temp<- melt(cor(df[rowMeans(is.na(df[,grepl("Garage",names(df))&names(df)%in%numerical_columns]))==0,grepl("Garage",names(df))&names(df)%in%numerical_columns]))%>%
  filter(Var1!=Var2)%>%
   arrange(-value)
temp[seq(1,nrow(temp),2),]%>%
  kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Garage (Numerical Columns Corr)"=4))



## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
df[rowMeans(is.na(df[,grepl("Garage",names(df))]))==0,grepl("Garage",names(df))] %>% ggplot(aes(x=GarageArea,y=GarageCars)) +geom_point() + geom_smooth() 


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
df<- df %>% mutate(GarageArea=ifelse(is.na(GarageArea),0,GarageArea)) %>% 
        mutate(GarageCond= ifelse(GarageArea==0,"No_Garage",GarageCond),
               GarageFinish= ifelse(GarageArea==0,"No_Garage",GarageFinish),
               GarageQual= ifelse(GarageArea==0,"No_Garage",GarageQual),
               GarageType = ifelse(GarageArea==0,"No_Garage",GarageType),
               GarageYrBlt =ifelse(GarageArea==0,0,GarageYrBlt),
               GarageCars=ifelse(GarageArea==0,0,GarageCars))

t(df[rowMeans(is.na(df[,grepl("Garage",names(df))]))>0,grepl("Garage",names(df))]) %>% kable()  %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Garage Missing Values Escenario"=2)) 


## -----------------------------------------------------------------------------
garage_fn <- c("No_Garage"=0,"Unf"=1,"RFn"=2,"Fin"=3)


## ---- echo=FALSE, message=FALSE,warning=FALSE---------------------------------
df[rowMeans(is.na(df[,grepl("Garage",names(df))]))>0.5,grepl("Garage",names(df))] <-0
df<- df %>% mutate(GarageArea=ifelse(is.na(GarageArea),0,GarageArea)) %>% 
        mutate(GarageCond= as.numeric(QC_Cond_decoder(ifelse(GarageArea==0,"No_Garage",GarageCond),"No_Garage")),
               GarageFinish=as.numeric(revalue(ifelse(GarageArea==0,"No_Garage",GarageFinish),garage_fn)),
               GarageQual= as.numeric(QC_Cond_decoder(ifelse(GarageArea==0,"No_Garage",GarageQual),"No_Garage")),
               GarageType = ifelse(GarageArea==0,"No_Garage",GarageType),
               GarageYrBlt =ifelse(GarageArea==0,0,GarageYrBlt),
               GarageCars=ifelse(GarageArea==0,0,GarageCars))
missing_values(df[,grepl("Garage",names(df))]) %>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Garage Missing Values"=3)) 


## -----------------------------------------------------------------------------
#
#First join the feautre name with column value.
#The used spread to transpose the cols values as new binary columns (1,0)
df<-df %>%
  mutate(v = 1, GarageType=gsub(" ","_",paste0("Garage_",GarageType))) %>% 
    spread(GarageType, v, fill = 0)



## -----------------------------------------------------------------------------
p1 <- df %>% 
  group_by(Exterior1st) %>%
  dplyr::summarise(N=n()/nrow(df)) %>% ggplot(aes(x= Exterior1st,y=N)) +
  geom_bar(stat="identity") + 
  labs(title="Exterior1st")+ coord_flip()
p2 <- df %>% 
  group_by(Exterior2nd) %>%
  dplyr::summarise(N=n()/nrow(df)) %>% ggplot(aes(x= Exterior2nd,y=N)) +
  geom_bar(stat="identity") + 
  labs(title="Exterior2nd")+ coord_flip()
p3 <- df %>% 
  group_by(ExterQual) %>%
  dplyr::summarise(N=n()/nrow(df)) %>% ggplot(aes(x= ExterQual,y=N)) +
  geom_bar(stat="identity") + 
  labs(title="ExterQual")+ coord_flip()
p4 <- df %>% 
  group_by(ExterCond ) %>%
  dplyr::summarise(N=n()/nrow(df)) %>% ggplot(aes(x= ExterCond ,y=N)) +
  geom_bar(stat="identity") + 
  labs(title="ExterCond")+ coord_flip()
grid.arrange(p1,p2,p3,p4,ncol=4)


## -----------------------------------------------------------------------------
replace_exterior2 <- c("Wd Shng"="WdShing", "CmentBd"="CemntBd" ,"Brk Cmn"="BrkComm")
#replaceing NA with mode
df <- df%>% mutate(Exterior1st=ifelse(is.na(Exterior1st),mode(Exterior1st),Exterior1st),Exterior2nd=ifelse(is.na(Exterior2nd),mode(Exterior2nd),Exterior2nd)) %>%
  mutate(Exterior2nd=revalue(Exterior2nd,replace_exterior2))
#unique values between Exterior1st and Exterior2nd
exteriors<- unique(c(unique(df$Exterior1st),unique(df$Exterior2nd)))


## -----------------------------------------------------------------------------
missing_values(df[,grepl("Ext",names(df))]) %>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Exterior Missing Values"=3)) 


## ---- warning=FALSE,message=FALSE---------------------------------------------
evaluate_cond_qc <- c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
df["ExterCond"] <- as.numeric(revalue(df$ExterCond,evaluate_cond_qc))
df["ExterQual"] <- as.numeric(revalue(df$ExterQual,evaluate_cond_qc))


## -----------------------------------------------------------------------------
for (ex in exteriors){
  name_ = gsub(" ","_",sprintf("Exterior_Matertial_%s",ex))
  df[,name_] <- as.numeric(0)
  df[df$Exterior1st==ex,name_]<-1
  df[df$Exterior1st!=ex,name_]<-0

}
df <- df%>% select(-Exterior1st,-Exterior2nd)


## ---- echo=FALSE, warning=FALSE, message=FALSE--------------------------------
df <- df %>% mutate(MiscFeature=ifelse(MiscVal==0,"No Feature",MiscFeature))
df %>% ggplot(aes(x=MiscFeature,y=MiscVal))+geom_point() + geom_boxplot()



## -----------------------------------------------------------------------------
df <- df %>% arrange(-MiscVal) %>% fill(MiscFeature,.direction = "downup")
missing_values(df[,grepl("Misc",names(df))]) %>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Misc Missing Values"=3)) 


## ----echo=FALSE,message=FALSE,warning=FALSE-----------------------------------
#create to dynamically create graph counting values, as use !!as.name so that the tidyverse functions can interpret the inputs as a name. 
graph_values<- function(df,feature){
  #Aggregate df by feature column calculate % of each value
  agg_df <- df %>% 
    group_by(!!as.name(feature)) %>%
    dplyr::summarise(N=n()/nrow(df))
  #Barplot get most frequent value
  p <-  agg_df %>% ggplot(aes(x= !!as.name(feature),y=N)) +
    geom_bar(stat="identity") + 
    labs(title=feature)+ coord_flip()

}


## ---- include=FALSE ,echo=FALSE,message=FALSE,warning=FALSE-------------------
p_Mszone <- graph_values(df,"MSZoning")
p_Alley<-graph_values(df,"Alley")
p_Utilities <- graph_values(df,"Utilities")
p_Functional <- graph_values(df,"Functional")
p_Fence <- graph_values(df,"Fence")
p_Air <- graph_values(df,"CentralAir")
p_PavedDrive <- graph_values(df,"PavedDrive")
p_KitchenQual <- graph_values(df,"KitchenQual")
p_Electrical <- graph_values(df,"Electrical")
p_SaleType <- graph_values(df,"SaleType")



## ----echo=FALSE,message=FALSE,warning=FALSE, fig.height=  12------------------
p<- arrangeGrob(p_Mszone,p_Alley,p_Utilities,p_Functional,p_Fence,p_Air,p_PavedDrive,p_SaleType,p_Electrical,p_KitchenQual)
grid.arrange(p)


## ----  message=FALSE,warning=FALSE--------------------------------------------

#Fence Replace NA with No Fence and Alley with no Alley
df <- df %>% mutate(Fence=ifelse(is.na(Fence),"No Fence",Fence),
                    Alley=ifelse(is.na(Alley),"No Alley",Alley))
#Fence Replace NA with No Fence
df <- df %>% mutate(Fence=ifelse(is.na(Fence),"No Fence",Fence))

#Replace NA with mode
df$Functional <- ifelse(is.na(df$Functional),mode(df$Functional),df$Functional)
df$Utilities <- ifelse(is.na(df$Utilities),mode(df$Utilities),df$Utilities)
df$MSZoning <- ifelse(is.na(df$MSZoning),mode(df$MSZoning),df$MSZoning)
df$KitchenQual <- ifelse(is.na(df$KitchenQual),mode(df$KitchenQual),df$KitchenQual)
df$SaleType <- ifelse(is.na(df$SaleType),mode(df$SaleType),df$SaleType)
df$Electrical <- ifelse(is.na(df$Electrical),mode(df$Electrical),df$Electrical)


privacy_levels <- c("No Fence"=0,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=4)
#Encode Label
df$Fence= as.numeric(revalue(df$Fence,privacy_levels))

evaluate_cond_qc <- c("Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
df["KitchenQual"] <- as.numeric(revalue(df$KitchenQual,evaluate_cond_qc))
df["HeatingQC"] <- as.numeric(revalue(df$HeatingQC,evaluate_cond_qc))
#CentraL Air(Y/N)
df$CentralAir <- as.numeric(ifelse(df$CentralAir=="Y",1,0))

#PAvedDrive Encoding
df$PavedDrive <- as.numeric(revalue(df$PavedDrive,c("N"=0,"P"=1,"Y"=2)))

missing_values(df)[1:4,]%>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Missing Values"=3)) 


## -----------------------------------------------------------------------------
df$TotalBathRooms <- df$BsmtFullBath +  (df$BsmtHalfBath * 0.5) + df$FullBath + (df$HalfBath * 0.5)
df$Second_Floor<- ifelse(df$X2ndFlrSF>0,1,0) #0=No Second Floor, 1= Second Floor



## -----------------------------------------------------------------------------
df$Remod <- ifelse(df$YearBuilt==df$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
df$Age <- as.numeric(df$YrSold)-df$YearRemodAdd
df$New <- ifelse(df$YrSold==df$YearBuilt, 1, 0)  #0=No, 1= Yes


cols_to_Drop <- c(cols_to_Drop,"Yrsold","MoSold","YearBuilt","Id")



## -----------------------------------------------------------------------------

sf_area<- setdiff(names(df[,grep("SF|Area|SalePrice",names(df))]), cols_to_Drop)
t(head((df[,sf_area]),10))


## -----------------------------------------------------------------------------
cols_to_Drop <- c(cols_to_Drop,"X1stFlrSF", "X2ndFlrSF")
df<- df[, !names(df)%in%cols_to_Drop]


## ----warning=FALSE, fig.height=7,message=FALSE--------------------------------
sf_area<- setdiff(names(df[,grep("SF|Area|SalePrice",names(df))]), cols_to_Drop)

graph_dist<- function(df,feature){
  
  p <-  df %>% filter(!!as.name(feature)>0)%>% ggplot(aes(x= !!as.name(feature))) +
    geom_histogram() +
    labs(title=feature)
  p}
p1<- graph_dist(df,sf_area[1])
p2<- graph_dist(df,sf_area[2])
p3<- graph_dist(df,sf_area[3])
p4<- graph_dist(df,sf_area[4])
p5<- graph_dist(df,sf_area[5])
p6<- graph_dist(df,sf_area[6])
p7<- graph_dist(df,sf_area[7])
p8<- graph_dist(df,sf_area[8])
p9<- graph_dist(df,sf_area[9])
p10<- graph_dist(df,sf_area[10])
p<-arrangeGrob(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
grid.arrange(p)

## -----------------------------------------------------------------------------
sf_area_Cor<-cor_SalesPrice(df,sf_area)
sf_area_Cor<-high_cor_cols(sf_area_Cor)
cols_to_Drop <- c(cols_to_Drop,names(df[,!names(df) %in% unique(sf_area_Cor$Var2) & names(df)%in%sf_area]),"X1stFlrSF")
sf_area_Cor%>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("SF & Area Columns to Keep"=3))


## -----------------------------------------------------------------------------
sf_area
feature <- NULL
skew <- NULL
kurt_<-NULL
mean_ <- NULL
median_ <- NULL
n<-0
for(col in sf_area){
  cat(col)
  df_ = df
  if(col=="SalePrice"){
    df_ = df %>% filter(!is.na(SalePrice))
  }
  n=1+n
  feature[n]<- col
  skew[n]<- skewness(df_[,col])
  kurt_[n]<- kurtosis(df_[,col])
  mean_[n]<-mean(df_[,col])
  median_[n]<-median(df_[,col])
}
dist_summary <-data.table(feature,skew,kurt_,mean_,median_)
dist_summary %>% kable() %>%
  kable_material(c("striped"))%>% 
  kable_minimal()%>%
  add_header_above(c("Missing Values"=5)) 


## -----------------------------------------------------------------------------
qqnorm(df$SalePrice,main = "Before Log Transformation")
qqline(df$SalePrice)

## -----------------------------------------------------------------------------
df$SalePrice <- log(df$SalePrice)
qqnorm(df$SalePrice,main = "After Log Transformation")
qqline(df$SalePrice)



## -----------------------------------------------------------------------------
#Selecting character columns
categorical_columns<- colnames(df %>% select(which(sapply(.,is.character))))
#Number of character columns
n_cat_cols <- length(categorical_columns)
#Display character columns in table

matrix(categorical_columns,9,byrow=TRUE) %>%kable()%>%
  kable_material(c("striped"))%>% 
  kable_minimal()



## -----------------------------------------------------------------------------
#
#First join the feautre name with column value.
#The used spread to transpose the cols values as new binary columns (1,0)
column_dummy_name <- function(x,colname_){
   gsub(" ","_",paste(colname_,x,sep="_"))
  
}

for(col in categorical_columns){
  df[col] <-apply(df[col],2,FUN=column_dummy_name,colname_=col)
  df<-df %>% mutate(v = 1) %>% 
  spread(!!as.name(col), v, fill = 0)} 
colnames(df)<- sapply(colnames(df),function(X){gsub("\\(","",X)})
colnames(df)<- sapply(colnames(df),function(X){gsub("\\)","",X)})
colnames(df)<- sapply(colnames(df),function(X){gsub("\\&","",X)})


## -----------------------------------------------------------------------------

pre_train_set<- df[!is.na(df$SalePrice),]
indx  <- createDataPartition(y = pre_train_set$SalePrice, times = 1, p = 0.25, list = FALSE)
train_set<- pre_train_set[-indx,]
X_train<- train_set[,names(train_set)!="SalePrice"]
Y_train <- train_set$SalePrice
test_set<- pre_train_set[indx,]
X_test<- test_set[,names(train_set)!="SalePrice"]
Y_test<- test_set$SalePrice



## -----------------------------------------------------------------------------

set.seed(250000)
cv <-trainControl(method="cv", number=15)
lasso<- train(x= X_train
              , y= Y_train
              , method='glmnet'
              , trControl= cv
              , tuneGrid= expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))) 
plot(lasso)


## -----------------------------------------------------------------------------
t(lasso$bestTune %>% select(alpha,lambda))%>%kable() %>% 
  kable_styling(font_size = 10) %>%
  add_header_above(c("Parameters"=2))


## ----echo=FALSE---------------------------------------------------------------
lassoVarImp <- varImp(lasso,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')


## ---- echo=FALSE--------------------------------------------------------------
LassoPred <- predict(lasso, X_test)
predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
real<-exp(Y_test)
d = real-predictions_lasso
mse = mean((d)^2)
mae = mean(abs(d))
rmse = sqrt(mse)
R2 = 1-(sum((d)^2)/sum((real-mean(real))^2))

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 

    "RMSE:", rmse, "\n", "R-squared:", R2)


## -----------------------------------------------------------------------------
set.seed(2000)

rf_model<-randomForest(
  formula = SalePrice ~ .,
  data    = train_set)

summary(rf_model)

## -----------------------------------------------------------------------------
plot(rf_model)


## -----------------------------------------------------------------------------
rfImportance <- rf_model$importance
varsSelected <- length(which(rfImportance!=0))
varsNotSelected <- length(which(rfImportance==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')


## -----------------------------------------------------------------------------
rf_mode_pred <- exp(predict(rf_model,X_test))
original<- exp(Y_test)
d = original-rf_mode_pred
mse = mean((d)^2)
mae = mean(abs(d))
rmse = sqrt(mse)
R2 = 1-(sum((d)^2)/sum((original-mean(original))^2))

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 

    "RMSE:", rmse, "\n", "R-squared:", R2)

