#predict the worlwide box-office revenue for the movies

#import the data
train_data<-read.csv("D:/Mactores/Boxoffice Revenue/train_modified.csv")
test_data<-read.csv("D:/Mactores/Boxoffice Revenue/test_modified.csv")


str(train_data)
summary(train_data)



#remove unwanted columns from train and test data
train_data<-subset(train_data,select = -c(prod1,prod2,prod3,country1,
                                        country2,country3,cast1,cast2,cast3,Director))

str(train_data)

test_data<-subset(test_data,select = -c(prod1,prod2,prod3,
                                      country1,country2,country3,cast1,cast2,cast3,Director))


#fit the linear regression model
#split the train dataset into sample_train and sample_test

set.seed(12345)
row.number <- sample(x=1:nrow(train_data), size=0.75*nrow(train_data))
sample_train = train_data[row.number, ]
sample_test = train_data[-row.number,]



saturated_model<-lm(revenue ~ budget + runtime + genre_number + production_company_number
                    + production_countries_number 
                    + spoken_languages_number + keywords_number + cast_number
                    + crew_number + Budget_cast_ratio + Budget_runtime_ratio
                    + mean_budget_by_year + has_poster
                    + weekday_0 + weekday_1 + weekday_2 + weekday_3
                    + weekday_4 + weekday_5 + weekday_6 + month_0 + month_1
                    + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 +
                    + month_8 + month_9 + month_10 + month_11, data = sample_train)
summary(saturated_model)




#reduced model - use step wise selection model method to get best fit model
#install leaps library to use regsubsets function:

library(leaps)

#divide the train dataset into sample train and sample test
set.seed(12345)
row.number <- sample(x=1:nrow(train_data), size=0.75*nrow(train_data))
sample_train = train_data[row.number, ]
sample_test = train_data[-row.number,]

#construct hypothetical model with all variables of interest:


fit <- lm(revenue ~ budget + runtime + genre_number + production_company_number
          + production_countries_number 
          + spoken_languages_number + keywords_number + cast_number
          + crew_number + Budget_cast_ratio + Budget_runtime_ratio
          + mean_budget_by_year + has_poster
          + weekday_0 + weekday_1 + weekday_2 + weekday_3
          + weekday_4 + weekday_5 + weekday_6 + month_0 + month_1
          + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 
          + month_8 + month_9 + month_10 + month_11, data = sample_train)
summary(fit)

#Use step function to give AIC estimates for each stepwise variable removal from the full model:
step <- step(fit, direction="backward")
step$anova # display results

#fit the data into the best fit model



#fit the data into the best fit model indentified after performing backward stepwise feature reduction method

lm_fit <- lm(revenue ~ budget + runtime + production_company_number + production_countries_number 
             + keywords_number + cast_number + crew_number + Budget_cast_ratio 
             + Budget_runtime_ratio + weekday_0 + weekday_2 + weekday_3 
             + weekday_4 + month_0 + month_3 + month_5 + month_6 + month_8,
             data=sample_train)
summary(lm_fit)

#predict the value for sample test using the best fit model
predicted_ys <- predict(lm_fit, newdata = sample_test)
predicted_ys
observed_ys <- sample_test$revenue

#plot different graphs to analyze the model
par(mfrow=c(2,2))
plot(lm_fit)


# plot observed vs predicted regression line
plot(predicted_ys,observed_ys, main = "Observed vs Predicted",
     xlab = "Predicted Revenue", ylab = "Observed Revenue",
     pch = 19, frame = FALSE)
abline(lm_fit, col = "blue")



#Compare the predicted and observed values
head(predicted_ys)
head(observed_ys)

#use different metrices to identify the error
SSE <- sum((observed_ys - predicted_ys) ^ 2)
SST <- sum((observed_ys - mean(observed_ys)) ^ 2)
r2 <- 1 - SSE/SST
rmse <- sqrt(mean((predicted_ys - observed_ys)^2))

r2 
#0.5508917
rmse 
#99162913



#using a tree model to fit the data and predict the sales
#load tree libraries
library(rpart)
library(caret)
library(rattle)
library(randomForest)
library(randomForestSRC)
library(tree)
library(e1071)
library(caTools)
library(randomForest)
library(randomForestSRC)

#divide the train dataset into sample training and sample validation dataset

set.seed(12345)
row.number <- sample(x=1:nrow(train_data), size=0.75*nrow(train_data))
sample_training = train_data[row.number, ]
Sample_validation = train_data[-row.number,]

#Let's try the data w/ Random Forest:

modfit<-randomForest(revenue ~  has_poster  + month_4 + weekday_2+  month_1 
                     + month_11 + month_10 + weekday_3 + month_9  + month_5 +  month_7 +month_8
                      + month_3 +  month_2  + weekday_5 +  weekday_4 
                     + weekday_1 + weekday_0 + 
                     + production_countries_number 
                     + spoken_languages_number + month_6 + weekday_6   + month_0  
                     + genre_number  + production_company_number
                     + mean_budget_by_year + keywords_number 
                     + runtime + Budget_cast_ratio  + crew_number
                     + cast_number + Budget_runtime_ratio + budget 
                     , data = sample_train,
                     mtry=6,ntree=500)

modfit


#predict the accuracy of the model on Validation data 
pred <- predict(modfit,Sample_validation)
observed_sales<-Sample_validation$revenue

head(pred)
head(observed_sales)

#plot feature importance
feat_imp<-as.data.frame(importance(modfit))

par(mar=c(-0.1,13,1,0.5)+.1)

barplot((feat_imp$IncNodePurity), names.arg =rownames(feat_imp), horiz=TRUE,las=1,
         xlab = NULL)
title("Feature importance based on IncNodePurity value",adj=0)

#draw feature imp table
library(gridExtra)
library(grid)

grid.table(feat_imp)

#use different metrices to identify the error
SSE <- sum((observed_sales - pred) ^ 2)
SST <- sum((observed_sales - mean(observed_sales)) ^ 2)
r2 <- 1 - SSE/SST
rmse <- sqrt(mean((pred - observed_sales)^2))

r2 
#0.5927156
rmse 
#94432748

#predict the sales for unknown data (test dataset)

pred <- predict(modfit,test_data)
head(pred)
test_data$revenue<- pred

write.csv(test_data,file = "test_predicted.csv")

