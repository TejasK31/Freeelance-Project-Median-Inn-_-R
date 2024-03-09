##Loading data set form all the months
data1 <- read.csv("C:/Users/tkulk/OneDrive/Desktop/BAN620/620/May.csv")
data2 <- read.csv("C:/Users/tkulk/OneDrive/Desktop/BAN620/620/April.csv")
data3 <- read.csv("C:/Users/tkulk/OneDrive/Desktop/BAN620/620/June.csv")
data4 <- read.csv("C:/Users/tkulk/OneDrive/Desktop/BAN620/620/July.csv")
data5 <- read.csv("C:/Users/tkulk/OneDrive/Desktop/BAN620/620/August.csv")
data6 <- read.csv("C:/Users/tkulk/OneDrive/Desktop/BAN620/620/September.csv")
data7 <- read.csv("C:/Users/tkulk/OneDrive/Desktop/BAN620/620/October.csv")
data8 <- read.csv("C:/Users/tkulk/OneDrive/Desktop/BAN620/620/November.csv")
library("dplyr")
#combing data from all the months
boom <- bind_rows(data1, data2,data3,data4,data5,data6,data7,data8)
View(boom)
dim(boom)
#cleaning the data
newData.df<-boom[-c(1:6,11,17,19,21,27:35,39:50)]
summary(newData.df)
dim(newData.df)
View(newData.df)
MedianInn.df<-newData.df[-c(2,3,8,9,20:30,32:50)]
View(MedianInn.df)
dim(MedianInn.df)
summary(MedianInn.df)
#Converitng categorical variables into factors
MedianInn.df$Booking.Source <- as.factor(MedianInn.df$Booking.Source)
MedianInn.df$Corporate.Name <- as.factor(MedianInn.df$Corporate.Name)
MedianInn.df$Room.No. <- as.factor(MedianInn.df$Room.No.)
MedianInn.df$Room.Type <- as.factor(MedianInn.df$Room.Type)
MedianInn.df$Pax <- as.factor(MedianInn.df$Pax)
MedianInn.df$Guest.Invoice.issued.by <- as.factor(MedianInn.df$Guest.Invoice.issued.by)
#Cleaning unnecessary variables
MedianInn1.df<-MedianInn.df[-c(8:12,16)]
summary(MedianInn1.df)
dim(MedianInn1.df)
View(MedianInn1.df)
ncol(MedianInn1.df$Total.Amount)
library("dplyr")
#Identifying customers with 0 total ammount
boom %>% count(Total.Amount)


#Making new dataframe to study the cancellations

Amount_with_zero<-MedianInn1.df[MedianInn1.df$Total.Amount==0.00,]
View(Amount_with_zero)
summary(Amount_with_zero)
dim(Amount_with_zero)



#Idetifying the 0, i.e why there are cancellations 

Workig_with_Cancellations_df <- transform(MedianInn1.df,cancellations=ifelse(MedianInn1.df$Total.Amount==0.00,0,1))
View(Workig_with_Cancellations_df)
Workig_with_Cancellations_df <- Workig_with_Cancellations_df[-8]
summary(Workig_with_Cancellations_df)

Workig_with_Cancellations_df1 <- Workig_with_Cancellations_df[-c(6,8,9)]

summary(Workig_with_Cancellations_df1)
Workig_with_Cancellations_df1$cancellations <- as.factor(Workig_with_Cancellations_df1$cancellations)

#devloping a classification tree (#tress doesnt make any sense, cant detect any predictors for cancellations)
set.seed(1)
train.rows <- sample(rownames(Workig_with_Cancellations_df1), dim(Workig_with_Cancellations_df1)[1]*0.6)
train.df <- Workig_with_Cancellations_df1[train.rows, ]
valid.rows <- setdiff(rownames(Workig_with_Cancellations_df1), train.rows) 
valid.df <- Workig_with_Cancellations_df1[valid.rows, ]

train.norm.df <- train.df
valid.norm.df <- valid.df

library(caret)
library(rpart)
library(rpart.plot)

#Devloping the tree
CancelaltionDefault <- rpart(cancellations ~ ., data = train.df, 
                              method = "class")
CancellationsTree <- rpart(cancellations ~ ., data = train.df, 
                       control = rpart.control(minsplit=2, maxdepth = 2), method = "class")

#plot the tree
prp(CancellationsTree, type = 1, extra = 1, split.font = 1, varlen = -10)

#Use it for prediction
Cancellatons.pred.valid <- predict(CancellationsTree,valid.df,type = "class")

# generate confusion matrix for valid data
confusionMatrix(Cancellatons.pred.valid, as.factor(valid.df$cancellations))

cancellations.ct <- rpart(cancellations ~ ., data = train.df, method = "class", 
                    cp = 0.00001, minsplit = 10,xval=5)
prp(cancellations.ct, type = 1, extra = 1, split.font = 1, varlen = -10)


#Use it for prediction
cancellations1.pred.valid <- predict(cancellations.ct,valid.df,type = "class")
confusionMatrix(cancellations1.pred.valid, as.factor(valid.df$cancellations))

library(randomForest)
#Random Forest tree
CancellationsForest <- randomForest(as.factor(cancellations) ~ ., data = train.df, ntree = 500, 
                               mtry = 4, nodesize = 5, importance = TRUE) 
#Use it for prediction
CancellationsForest.pred.valid <- predict(CancellationsForest,valid.df,type = "class")
confusionMatrix(CancellationsForest.pred.valid, as.factor(valid.df$cancellations))


#tryinng to run logit to understand better predictors of acceptors
Cancellations.logit <- glm(cancellations ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(Cancellations.logit)

# use predict() with type = "response" to compute predicted probabilities. 
Cancellations.reg.pred <- predict(Cancellations.logit, valid.df, type = "response") 
confusionMatrix(as.factor(ifelse(Cancellations.reg.pred>0.5,1,0)), as.factor(valid.df[, 6]))

# use step() to run stepwise regression.
library(leaps)
Cancellations.logit.step <- step(Cancellations.logit, direction = "backward")
summary(Cancellations.logit.step)  
Cancellations.lm.step.pred <- predict(Cancellations.logit.step, valid.df)

confusionMatrix(as.factor(ifelse(Cancellations.lm.step.pred>0.5,1,0)), as.factor(valid.df[, 6]))



summary(MedianInn1.df)
##########
##removing all the ROWS corresponding to the cells which contains "zero" in 'Total LODGING Amount' column 
MedianInn_withoutzeros<- MedianInn1.df %>% 
  filter(Total.Lodging.Amount != 0.00)
dim(MedianInn_withoutzeros)
summary(MedianInn_withoutzeros)
View(MedianInn_withoutzeros)
#selecting the column which is looking suspicious to determine what is fishy 
MedianInn_withoutzeros[MedianInn_withoutzeros$Paid.To.Treebo.Amount == '-2772',]
###

######################
#####################
###
plot(MedianInn_withoutzeros$Total.Lodging.Amount ~ MedianInn_withoutzeros$Booking.Source, xlab = "book_source", ylab = "amount")

plot(MedianInn_withoutzeros$Total.Lodging.Amount ~ MedianInn_withoutzeros$Corporate.Name, xlab = "book_source", ylab = "amount")

categories<-unique(MedianInn_withoutzeros$Corporate.Name)
no_categories<-length(categories)
summary(categories)
summary(MedianInn_withoutzeros$Corporate.Name)

library(ggplot2)
ggplot(MedianInn_withoutzeros) + geom_point(aes(x =Corporate.Name , y =Total.Lodging.Amount ), colour = "navy", alpha = 0.7)

#

hist(MedianInn_withoutzeros$Booking.Source, xlab = "booking source")
#
boxplot(MedianInn_withoutzeros$Booking.Source, ylab = "booking source")
#
boxplot(MedianInn_withoutzeros$Total.Lodging.Amount ~ MedianInn_withoutzeros$Booking.Source, xlab = "SOURCE", ylab = "AMOUNT")
#######################
#######################
#######################
## determining the characteristics of the columns in the new data set
sapply(MedianInn_withoutzeros,class)

head(MedianInn_withoutzeros)
# removing the redundent preditors to perform  Multivariable regression
MedianInn_withoutzerosreg<- MedianInn_withoutzeros[-c(6, 9, 10)]

set.seed(1)
train.rows <- sample(rownames(MedianInn_withoutzerosreg), dim(MedianInn_withoutzerosreg)[1]*0.6)
train2.df <- MedianInn_withoutzerosreg[train.rows, ]
valid.rows <- setdiff(rownames(MedianInn_withoutzerosreg), train.rows) 
valid2.df <- MedianInn_withoutzerosreg[valid.rows, ]

train.norm.df <- train2.df
valid.norm.df <- valid2.df

###regression analysis
library(caret)
options(scipen=999)
library(leaps)

MedianInn_withoutzerosreg1 <- lm(Total.Amount ~ ., data = train2.df) 
summary(MedianInn_withoutzerosreg1)
#running stepwise regression
MedianInn.lm.step <- step(MedianInn_withoutzerosreg1, direction = "backward")
summary(MedianInn.lm.step) 
#
MedianInn.lm.step.pred <- predict(MedianInn.lm.step, valid2.df)
#NOT ABLE TO RUN Prediction due to soo many levels......
accuracy(MedianInn.lm.step, valid.df$Total.Amount)


##working with banquet and booking data trying to fid relation with it 
BanquetData.df <- read.csv("C:/Users/tkulk/OneDrive/Desktop/BAN620/ENQURY BANQUEAT (1) (1).csv")

summary(BanquetData.df)
head(BanquetData.df)

#Trying to find commom data with MedianInn and Banquet
CommomCutomers_df<-boom[boom$Primary.Guest.Name==BanquetData.df$NAME.OF.GUEST,]
CommomCutomers_df
boom[boom$Primary.Guest.Name==BanquetData.df$NAME.OF.GUEST,]

#the common customer in banquet and hotel
boom[boom$Primary.Guest.Name == 'SWAPNIL KUBDE',]


        
install.packages("janitor")
library(janitor)        
compare_df_cols(boom,BanquetData.df, return='match')
#Preprocessing the data

BanquetData.df$PAX = as.factor(BanquetData.df$PAX)
BanquetData.df$MENU = as.factor(BanquetData.df$MENU)
BanquetData.df$MORNING = as.factor(BanquetData.df$MORNING)
BanquetData.df$EVENING = as.factor(BanquetData.df$EVENING)
BanquetData.df$AMOUNT = as.integer(BanquetData.df$AMOUNT)
summary(BanquetData.df)
library(dplyr)
inner_join(boom,BanquetData.df)

common.df <- intersect(boom$Primary.Guest.Name,BanquetData.df$NAME.OF.GUEST)
summary(common.df)
View(common.df)



