setwd("~/Desktop/Aliya")

library(dplyr)
library(ggplot2)

all_content = readLines("loanStats3a.csv")
skip_first = all_content[-1]
mydata <- read.csv(textConnection(skip_first),header=TRUE)

#remove empty rows
mydata<-mydata[(mydata2$loan_status !=""]

#names(mydata)

loanStatus <- mydata$loan_status
levels(loanStatus) <- c("","Charged Off", "Current" , "Charged Off" , "Charged Off", "Current", "Fully Paid", "In Grace Period", "Fully Paid", "In Grace Period","Late (16-30 days)", "Late (31-120days)")
issueDate  <- mydata$issue_d

# remove the columns that are not going to be used in the model 

blacklist <- c("id",
               "member_id", 
               "emp_title" ,
               "url", 
               "pymnt_plan", 
               "initial_list_status",
               "out_pmcp",
               "desc",               
               "out_pmcp_inv",
               "total_pymnt",
               "last_pymnt_d",
               "last_pymnt_amnt",
               "next_pymnt_d",
               "last_credit_pull_d",
               "recoveries",
               "collection_recovery_fee",
               "policy_code", 
               "zip_code",
               "funded_amnt",
               "funded_amnt_inv",               
               "title",
               "total_pymnt_inv",
               "total_rec_prncp",
               "total_rec_int",
               "total_rec_late_fee"
               )

ColsToKeep <- names(mydata) %in% blacklist == FALSE

mydata <- mydata[,ColsToKeep]

# plot the historical default rate vs. grade
df<- mydata %>% group_by( sub_grade , loan_status) %>% summarise(n=n()) %>% mutate(pct = 100 * n/sum(n))
qplot(x = sub_grade , y = pct , data = df[df$loan_status=="Charged Off",],xlab = "Sub Grade" , ylab = "Probability of Default(%)", stat="identity", geom = "histogram",fill = I("blue"), main = "Historical Default Rate by Grade")

# plot the inventory vs. grade
df2<- mydata %>% group_by( sub_grade ) %>% summarise(n=n()) %>% mutate(pct = 100 * n/sum(n))
qplot(x = sub_grade , y = pct , data = df2,xlab = "Sub Grade" , ylab = "Inventory(%)", stat="identity", geom = "histogram",fill = I("blue"), main = "Inventory by Grade")

#plot the inventory

mydata$sub_grade <- factor(sapply( as.character(mydata$sub_grade) , function(chr) substr(chr,2,2)))

#convert catergorical interest rate variable into numeric
mydata$int_rate <- as.numeric(sapply( as.character(mydata$int_rate), function(x) substr(x,2,6)))

#transform temporal data
mydata$issue_d <- as.Date(paste0(as.character(mydata$issue_d), "-01"),format = "%b-%Y-%d")
mydata$earliest_cr_line <- as.Date(paste0(as.character(mydata$earliest_cr_line),"-01"), format = "%b-%Y-%d")

