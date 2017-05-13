#load libraries

libraries<-c("ggplot2","stringr","e1071","class","caret","randomForest")
lapply(libraries,require,character.only=TRUE)

#Path of the data file
pathToFile <- "F:\\Projects\\telecomAnalysis\\data\\telecom_churn_data.txt"

#Loading data into r environment
customer_data <- read.table(pathToFile, sep=",")

#checking summary of different variables
str(customer_data)
summary(customer_data)

#removing whitespace from last column
customer_data[,21]<-as.factor(trimws(customer_data[,21]))

#allot Column names
columnNames <- c("State",
                 "Account_Length",
                 "Area_Code",
                 "Phone",
                 "International_Plan",
                 "VMail_Plan",
                 "VMail_Message",
                 "Day_Mins",
                 "Day_Calls",
                 "Day-Charge",
                 "Eve_Mins",
                 "Eve_Calls",
                 "Eve_Charge",
                 "Night_Mins",
                 "Night-Calls",
                 "Night_Charge",
                 "International_Mins",
                 "International_Calls",
                 "International_Charge",
                 "CustServ_Calls","Result")
names(customer_data)<-columnNames

#remving Phone colum from dataframe as it doesn't help us for prediction
customer_data<-customer_data[-4]
#Coverting Customer Call to factor
customer_data[,19]<-factor(customer_data[,19])
customer_data[,3]<-factor(customer_data[,3])


#Checking null data and distrubution of variables
summary(customer_data)

#creating a new column customer bill
cust_bill<-rowSums(customer_data[9]+customer_data[12]+customer_data[15]+customer_data[18])
customer_data$bill<-cust_bill

#lest prepare training and test data
set.seed(101)
sample_idx<-sample(1:dim(customer_data)[1],3600)
train_data <- customer_data[sample_idx,]
test_data <- customer_data[-sample_idx,]

#Check variable wise data distribution

#frequency of Call to customer care
ggplot(train_data,aes(CustServ_Calls,fill=CustServ_Calls))+geom_bar()
#age of accounts
ggplot(train_data,aes(Account_Length))+geom_histogram(binwidth = 20)
#account length wise churn rate
ggplot(train_data,aes(Account_Length,fill=Result))+geom_histogram(binwidth = 50,position = "fill")+scale_x_continuous(breaks=seq(0,250,50))
#state wise customer
ggplot(train_data,aes(x=Area_Code))+geom_bar()
#Churn rate of customers availing Intl facility vs Non-Intl Facility
ggplot(train_data,aes(x=International_Plan,fill=Result))+geom_bar(position="fill")
#Churn rate of customers availing VMail facility vs Non Vmail Facility
ggplot(train_data,aes(x=VMail_Plan,fill=Result))+geom_bar(position="fill")
#State wise Churn rate 
ggplot(train_data,aes(x=State,fill=Result))+geom_bar(position="fill")
#Area wise Churn rate 
ggplot(train_data,aes(x=Area_Code,fill=Result))+geom_bar(position="fill")
#customer spending wise churn rate
ggplot(train_data,aes(x=bill,fill=Result))+geom_density(position = "fill")


remove_row_idx<-c(3,9,12,15,18)
#Naive Bayes starts
customer_classifier <- naiveBayes(train_data[,-remove_row_idx], train_data$Result)
class(customer_classifier)
str(customer_classifier)
summary(customer_classifier)


customer_test_pred <- predict(customer_classifier, newdata=test_data[,-c(remove_row_idx,20)])
summary(customer_test_pred)
table(customer_test_pred, test_data$Result)
#Naive Bayes Ends
