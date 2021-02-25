##################################
#Problem statement :To predict loans approval or disapprove
#Author:Saranya
#Date: 25/02/2021
#version: 1.3
##################################
##################################





##########IMPORT AND AUDIT DATA
setwd('D:\\rdata\\Imarticus\\LOAN prediction_')
getwd()
train <- read.csv('train_ctrUa4k.csv',header = TRUE,na.strings = c(""))
test <- read.csv('test_lAUu6dg.csv',header = TRUE,na.strings = c(""))
str(train)
########################DATA PREPARATION AND DATA TRANSFORMATION######

######CHANGE 3+   in dependents to 3

train$Dependents <- as.character(train$Dependents)
train$Dependents <- ifelse(train$Dependents=='3+','3',train$Dependents)
train$Dependents <- as.factor(train$Dependents)
table(train$Dependents)
sum(is.na(train))

train$Credit_History <- as.factor(train$Credit_History)

names(train)
#######MISSING VALUES

set.seed(123)
install.packages("Amelia")
library(Amelia)
newimpute <- amelia(train, m=5,
                    idvars = c('Loan_ID', 'Education', 'ApplicantIncome', 'CoapplicantIncome', 'Property_Area', 'Loan_Status'),
                    noms = c('Gender', 'Married', 'Self_Employed', 'Credit_History'),
                    ords = c('Dependents'))
                    
write.amelia(newimpute, file.stem = 'imputed_data_set')

##############IMPORT THE IMPUTED DATA SET

train_imp <- read.csv('Imputed_data_set1.csv',na.strings = c(''))
sum(is.na(train_imp))

str(train_imp)
train_imp$Credit_History <- as.factor(train_imp$Credit_History)                    
train_imp$Dependents <- as.factor(train_imp$Dependents)
sum(is.na(train_imp$Credit_History))

summary(train_imp)

library(e1071)
skewness(train_imp$ApplicantIncome)
skewness(train_imp$CoapplicantIncome)
skewness(train_imp$LoanAmount)
skewness(train_imp$Loan_Amount_Term)

##############Boxplot

boxplot(train_imp$ApplicantIncome)
boxplot(train_imp$CoapplicantIncome)
boxplot(train_imp$LoanAmount)
boxplot(train_imp$Loan_Amount_Term)


#############Bi-Variate Analysis
#library(sqldf)
#cont_vars <- sqldf('select ApplicantIncome,CoapplicantIncome,LoanAmount,
#                    Loan_Amount_term  from train_imp')

continous_vars <- subset(train_imp,
                         select = c('ApplicantIncome','CoapplicantIncome','LoanAmount','Loan_Amount_Term'))
library(Hmisc)

corr <- rcorr(as.matrix(continous_vars))
corr$r

############Chi square test between Gender and Loan_Status
############Chi square test between Married and Loan_Status

tbl_1 <- table(train_imp$Gender, train_imp$Loan_Status)
tbl_2 <- table(train_imp$Married, train_imp$Loan_Status)
tbl_1
tbl_2

library(MASS)
chisq.test(tbl_1)

#####write an SQL query to find the average income of loan status Yes and no
#####write an Sql query to find the average Loan_Amount of loan status yes and no

install.packages('sqldf')

library(sqldf)
avg_Income<-sqldf("select Loan_Status,avg(ApplicantIncome)from train_imp group by Loan_Status")
avg_Income
avg_Inc<-sqldf("select Loan_Status,avg(ApplicantIncome)from train_imp group by Loan_Status")
avg_Inc
##########Multi-Variate model

##############

train_imp <- train_imp[,-1]
names(train_imp)
summary(train_imp)
str(train_imp)
train_imp$Gender <- as.integer(train_imp$Gender)
train_imp$Married <- as.integer(train_imp$Married)
train_imp$Self_Employed <- as.integer(train_imp$Self_Employed)
train_imp$Education <- as.integer(train_imp$Education)
train_imp$Credit_History <- as.integer(train_imp$Credit_History)
train_imp$Property_Area <- as.integer(train_imp$Property_Area)
train_imp$Loan_Status <- as.integer(train_imp$Loan_Status)
str(train_imp)

#################logistic regression model#############
model1_LR <- glm(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+
                   CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area,
                 data = train_imp,family='binomial')
summary(model1_LR)
sum(is.na(train_imp))
train_imp$pred_probs <- model1_LR$fitted.values
train_imp$preds_lr <- ifelse(train_imp$pred_probs>0.5,1,0)
cm_lr <- table(train_imp$Loan_Status, train_imp$preds_lr)
cm_lr

#############DecisionTree##############

library(rpart)
library(rpart.plot)
dtree <- rpart(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome
               +CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area,data=train_imp)
rpart.plot(dtree)
train_imp$preds_tree <- predict(dtree,train_imp, type ='class')
cm_tree <-table(train_imp$Loan_Status,train_imp$preds_tree)
cm_tree

################randomforest###############
library(randomForest)
rf <- randomForest(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome
                    +CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area,data=train_imp,ntree = 500)
rf$confusion                  
                   
