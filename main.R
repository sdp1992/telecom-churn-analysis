#load libraries

libraries<-c("ggplot2","stringr","e1071","class","caret","randomForest","gridExtra")
sapply(libraries,require,character.only=TRUE)

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
                 "Day_Charge",
                 "Eve_Mins",
                 "Eve_Calls",
                 "Eve_Charge",
                 "Night_Mins",
                 "Night_Calls",
                 "Night_Charge",
                 "International_Mins",
                 "International_Calls",
                 "International_Charge",
                 "CustServ_Calls","Result")
names(customer_data)<-columnNames

#Coverting Area Code and no of calls to customer care as factor
customer_data[,3]<-factor(customer_data[,3])
customer_data[20]<-factor(customer_data[,20])


#Checking null data and distrubution of variables
summary(customer_data) # no null value in any predictors

#creating a new feature customer bill
cust_bill<-rowSums(customer_data[9]+customer_data[12]+customer_data[15]+customer_data[18])
customer_data$bill<-cust_bill

#creating new features average min per call
customer_data$avg_day_call<-customer_data$Day_Mins/customer_data$Day_Calls
customer_data$avg_eve_call<-customer_data$Eve_Mins/customer_data$Eve_Calls
customer_data$avg_night_call<-customer_data$Night_Mins/customer_data$Night_Calls
customer_data$avg_intl_call<-customer_data$International_Mins/customer_data$International_Calls

#Creating new feature call rates
customer_data$day_call_rate<-customer_data$Day_Charge/customer_data$Day_Mins
customer_data$eve_call_rate<-customer_data$Eve_Charge/customer_data$Eve_Mins
customer_data$night_call_rate<-customer_data$Night_Charge/customer_data$Eve_Mins
customer_data$intl_call_rate<-customer_data$International_Charge/customer_data$International_Mins
  
#we are getting some na values for some new predictors
#because of for some user call minutes and no of calls are zero
customer_data$avg_day_call[is.na(customer_data$avg_day_call)]<-0
customer_data$avg_eve_call[is.na(customer_data$avg_eve_call)]<-0
customer_data$avg_night_call[is.na(customer_data$avg_night_call)]<-0
customer_data$avg_intl_call[is.na(customer_data$avg_intl_call)]<-0

#from the summary of call rates columns we can conclude that for 
#most of the users there is no such difference in call rates.
customer_data<-customer_data[,1:26]

#creating new feature phn no category
phn_no_cat<-str_split_fixed(customer_data$Phone, "-",2)

#creating new dataset removing 'Phone' and binding 'phn_no_cat'
customer_data$phn_no_cat<-phn_no_cat[,1]
customer_data<-customer_data[,-4]
customer_data$phn_no_cat<-as.factor(customer_data$phn_no_cat)

#lest prepare training and test data
set.seed(101)
sample_idx<-sample(1:nrow(customer_data),3600)
train_data <- customer_data[sample_idx,]
test_data <- customer_data[-sample_idx,]

#Check variable wise data distribution

#State wise Churn rate 
ggplot(train_data,aes(x=State,fill=Result))+geom_bar(position="fill")
#State Like California has churn rate moe than 25%

#age of accounts
ggplot(train_data,aes(x=Account_Length,fill=Result))+geom_density(position = 'fill')
#Old customers are more likely to discontinue service compared to new customer

#Area Code wise customer
ggplot(train_data,aes(x=Area_Code,fill=Result))+geom_bar(position = 'fill')

#Churn rate of customers availing Intl facility vs Non-Intl Facility
ggplot(train_data,aes(x=International_Plan,fill=Result))+geom_bar(position="fill")
#Customers who has International plan are more likely to churn compared to non-International plan based customer
#This is a serious concern for a company as  International customers generates more per capita revenue compared to others

#Churn rate of customers availing VMail facility vs Non Vmail Facility
ggplot(train_data,aes(x=VMail_Plan,fill=Result))+geom_bar(position="fill")
#Customer those wh are not availing voicemail plan are more like to churn compared to those who have

#frequency of Call to customer care
ggplot(train_data,aes(CustServ_Calls))+geom_bar(aes(fill=Result))
#From the chart we can conclude that Customer who called customer care more than 3 times are likely to 
#discontinue service compared to other customers

#customer spending wise churn rate
ggplot(train_data,aes(x=bill,fill=Result))+geom_density(position = "fill")
#customers having bill amount <250 are very likely to discontinue service

#Day Minutes wise churn rate
plot1<-ggplot(train_data,aes(x=Day_Mins,fill=Result))+geom_density(position="fill")
#Evenig Minutes wise churn rate
plot2<-ggplot(train_data,aes(x=Eve_Mins,fill=Result))+geom_density(position="fill")
#Night Minutes wise churn rate
plot3<-ggplot(train_data,aes(x=Night_Mins,fill=Result))+geom_density(position="fill")
#International Minutes wise churn rate
plot4<-ggplot(train_data,aes(x=International_Mins,fill=Result))+geom_density(position="fill")

grid.arrange(plot1, plot2,plot3,plot4 ,nrow=2, ncol=2)
#From the chart we can see that customers called more tan 250 mins in daytime are more likely to churn.
#This may be because of poor service in day time

#No of Day Calls wise churn rate
plot5<-ggplot(train_data,aes(x=Day_Calls,fill=Result))+geom_density(position="fill")
#No of Evening calls wise churn rate
plot6<-ggplot(train_data,aes(x=Eve_Calls,fill=Result))+geom_density(position="fill")
#No of Night Calls wise churn rate
plot7<-ggplot(train_data,aes(x=Night_Calls,fill=Result))+geom_density(position="fill")
#No of Internatinal calls wise churn rate
plot8<-ggplot(train_data,aes(x=International_Calls,fill=Result))+geom_density(position="fill")

grid.arrange(plot5, plot6,plot7,plot8 ,nrow=2, ncol=2)


#average day call wise churn rate
plot9<-ggplot(train_data,aes(x=avg_day_call,fill=Result))+geom_density(position="fill")
#average evening call wise churn rate
plot10<-ggplot(train_data,aes(x=avg_eve_call,fill=Result))+geom_density(position="fill")
#average night call wise churn rate
plot11<-ggplot(train_data,aes(x=avg_night_call,fill=Result))+geom_density(position="fill")
#average intl call wise churn rate
plot12<-ggplot(train_data,aes(x=avg_intl_call,fill=Result))+geom_density(position="fill")

grid.arrange(plot9, plot10,plot11,plot12 ,nrow=2, ncol=2)
#customers having higher ave minute per call in day time are more likely to churn compared to others

#From all day time graph we can make a assumption that in Daytime there maybe some problem with service quality

#customer phn_no_cat wise churn rate
ggplot(train_data,aes(x=phn_no_cat,fill=Result))+geom_bar(position="fill")



#Using svm
svm_model<-tune(svm,Result~.,data=train_data,kernal="radial",ranges=list(cost=c(0.1,1,10,100), gamma=c(0.01,0.02,0.03,0.04,0.05)))
svm_pred<-predict(svm_model$best.model,test_data)

#confusion matrix
table(test_data$Result,svm_pred)

#Model accuracy is around 92-93%