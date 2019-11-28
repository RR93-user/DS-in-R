# Group Project 1 - EDA Case Study with following team members
# Rohit Rajagopal, Sameer Rai, Vineeth Pydimarry and Yatin Kode

# EDA Case Study -- Group Project 1 -- Customers default on loans
# Important: In loan status, the customers labelled as 'charged-off' are the 'defaulters'. 

# Loading libraries
load.libr
aries <- c('tidyr','dplyr','lubridate','ggplot2','stringr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

# pre-set the theme
theme_loan <- function () { 
  theme_bw(base_size=9, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_rect(fill="gray80", colour=NA),
      plot.background = element_rect(fill="gray96", colour=NA), 
      legend.background = element_rect(fill="white", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      legend.position="bottom"
    )
}   

# Loading data
loan_records_master <- read.csv("loan.csv", stringsAsFactors = FALSE)

# Structure of the dataframe
str(loan_records_master)
# There are 39,717 rows(observations) in 111 columns(variables)

###################################### EDA ######################################

########################## EDA Part 1: Data Cleaning ############################
# In data cleaning, we will be looking to fix rows and columns, fix missing values, 
# standardise values, fix invalid values, etc. 

#--------------1. Fixing rows -- Removing duplicate rows and rows not required -------- 
# checking for duplicate rows
sum(duplicated(loan_records_master))  ## or loan_records_master <- unique(loan_records_master)
# There is no duplicate rows in the dataset
#Since we cannot determine anything based on current applicants we can filter them out
loan_records_master <- subset(loan_records_master, loan_records_master$loan_status %in% c("Charged Off", "Fully Paid"))

#--------------2. Fixing columns - Removing All NA columns--------
# Counting NA in all the columns (also the names of all the columns)
NAcols<-as.numeric(which(colSums(is.na(loan_records_master))==nrow(loan_records_master)))
#creating vector a containing column indexes of all columns  from  loan_records_master
colindex<-as.numeric(c(1:ncol(loan_records_master)))
#Removing columns with all NA values
loan_records_master<-loan_records_master[,setdiff(colindex,NAcols)]
# We removed 54 columns from the dataframe because they have all the entries as NA

#--------------3. Fixing columns - Removing Single Value columns ----
# We also observed that there are few columns with only single value
#getting the column indexes where  there is only single value in the whole column
SingleValuecols <- as.numeric(which(sapply(loan_records_master, function(x) length(unique(x)))==1))
#creating vector  containing column indexes of all columns  from  loan_records_master-redefined after removing NA values previously
colindex<-as.numeric(c(1:ncol(loan_records_master)))
#Removing columns with only 1 value per column 
loan_records_master <- loan_records_master[,setdiff(colindex,SingleValuecols)]

#--------------4. Fixing columns - Removing unnecessary columns ------
unique(loan_records_master$purpose) # We will keep this column as it can provide information on what type of loans default
unique(loan_records_master$title) # Since this is manually entered and may not be categorized
# We will addr_state to analyze if any particular area is defaulting
unique(loan_records_master$delinq_2yrs) # We will keep this column to check if this can provide some informaiton on which borrower is going to default
unique(loan_records_master$inq_last_6mths) # Let's keep this column for now
unique(loan_records_master$open_acc) # Does more credit line means less default, we will check that
unique(loan_records_master$pub_rec) # Should we invest more in collection public records
unique(loan_records_master$out_prncp) # Remove the column
unique(loan_records_master$out_prncp_inv) # Remove the column
unique(loan_records_master$next_pymnt_d) # Only empty value hence can be removed

# We will also remove some other unnecessary columns or rows that may not add value to analysis
loan_records_master <- select(loan_records_master, -c(member_id,url,desc,emp_title,mths_since_last_record,mths_since_last_delinq))
# We are removing columns that are post-loan information about loans and will not add value to our analysis
loan_records_master <- select(loan_records_master, -c(title, sub_grade,total_pymnt,total_pymnt_inv,total_rec_prncp, total_rec_int, total_rec_late_fee, recoveries, 
                                                      collection_recovery_fee, last_pymnt_amnt, collections_12_mths_ex_med,chargeoff_within_12_mths, zip_code,last_pymnt_d))

# Dataframe for our analysis after fixing columns
str(loan_records_master)
# We are now left with 38,577 rows(observations) of 28 columns(variables or features)

#---------------4. Standardize columns - Removing unnecessary symbols and texts after numbers ------------------
#Removing '%' sign after interest rate
loan_records_master$int_rate = as.numeric(gsub("\\%", "", loan_records_master$int_rate))
#Removing '%' sign after revol_util
loan_records_master$revol_util = as.numeric(gsub("\\%", "", loan_records_master$revol_util))

#---------------5. Standardize columns - Standardising date formats ---------------------------------------------
# We have four columns(issue_d,earliest_cr_line, last_payment_d & last_credit_pull_d)with dates. We will convert them to date format                              

loan_records_master$issue_d <- parse_date_time(loan_records_master$issue_d , orders = 'b-y')
loan_records_master$issue_d <- format(loan_records_master$issue_d,format = "%B-%Y")
loan_records_master$earliest_cr_line <- parse_date_time(loan_records_master$earliest_cr_line , orders = 'b-y')
loan_records_master$earliest_cr_line<- format(loan_records_master$earliest_cr_line,"%b-%Y")
loan_records_master$last_credit_pull_d <- parse_date_time(loan_records_master$last_credit_pull_d , orders = 'b-y')
loan_records_master$last_credit_pull_d<- format(loan_records_master$last_credit_pull_d,"%b-%Y")
str(loan_records_master)
# uber_request$Request.timestamp <- as.POSIXct(uber_request$Request.timestamp, format = "%d-%m-%Y %H:%M")

#---------------5. Fix missing values  ------------------------------------------
sapply(loan_records_master, function(x) sum(is.na(x)))

# Now we have only three columns with missing values pub_rec_bankruptcies,tax_liens, revol_util
# Those columns have missing values because value is not applicable 

#---------------6. Standardize columns - Coverting relevant columns to factors ---
# Coverting relevant columns to factors
loan_records_master <- mutate_if(loan_records_master, is.character, as.factor)
str(loan_records_master)




##################### Univariate and Bivariate Analysis #####################
# "Correlation does not mean causation", so we will stay from drawing conclusions. 

# Plotting the records of loan status
ggplot(loan_records_master, aes(x = loan_status )) + # Identify the extent of default
  geom_bar(fill = "steelblue4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Loan Status",
       y = "Count",
       title = "Fig : Loan Request Status")

prop.table(table(loan_records_master$loan_status))*100
# Approximately 15% loans have defaulted 

# Range of loan amount, funded amount and funded amount committed by investors
sum(loan_records_master$loan_amnt) #426,161,100
sum(loan_records_master$funded_amnt) #416,016,625
sum(loan_records_master$funded_amnt_inv) #394,352,654

# Since all the three values are close, any one column will provide a good proxy for other two columns
ggplot(loan_records_master, aes(x = loan_amnt)) +
  geom_histogram(bins = 30, fill = "steelblue4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Loan amount",
       y = "USD",
       title = "Fig : Loan amount values")

ggplot (loan_records_master, aes(y = loan_amnt, x = loan_status)) + # Adding loan status
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "USD",
        title = "Fig : Loan amount values")
max(loan_records_master$loan_amnt)
min(loan_records_master$loan_amnt)
# The loan value varies from 500 to 35,000 with median falling around 10,000
# Plotting the box plot of loan values against default, it is not possible to identify any 
# trend in loan default

# Plotting term of loans
ggplot(loan_records_master, aes(x = term)) +
  geom_bar() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Loan amount",
       y = "Count",
       title = "Fig : Term of Loan")




# We will find proportion of loan status versus term of loan. For that we create proportion table and then plot graph on that 

prop_term_loan_status<-prop.table(table(loan_records_master$loan_status,loan_records_master$term),2)

ggplot(as.data.frame(prop_term_loan_status), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Term",y = "Percent of Loan Status",
       title = "Fig : Proportion of Loan Status vs Terms" ) +
  theme_loan()+
  guides(fill=guide_legend(title="Loan Status"))+
  scale_y_continuous(labels = scales::percent)

#Here We can see that around 25% loans of 60 months have been defaulted and around 12% of loans have been defaulted for 36 months
#Also we have seen that around 75% loans of 60 months have fully paid and around 88% of loans fully paid of 36 months
#We observe that lesser the term period, higher the chances of full payment



# Int rate and installament pending

# Plotting grade of the loan
ggplot(loan_records_master, aes(x = grade)) +
  geom_bar() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Loan Grade",
       y = "Count",
       title = "Fig : Grade of loan")
# Majority of the loans are in Grade A and Grade B




# We find out the percentage of defaulting and paying loan for better understanding
prop_grade_loan_status<-prop.table(table(loan_records_master$loan_status,loan_records_master$grade),2)

ggplot(as.data.frame(prop_grade_loan_status), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Grade",y = "Percent of Loan Status",
       title = "Fig : Proportion of Loan Status vs Grade" ) +
  theme_loan()+
  guides(fill=guide_legend(title="Loan Status"))+
  scale_y_continuous(labels = scales::percent)
## As you can see in the above bar plot there is an increasing trend of defaulters along grades in ascending order fromA to G respactivel

# Let's check the customer profile basis employment duration
ggplot(loan_records_master, aes(x = emp_length)) +
  geom_bar() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Employement Duration",
       y = "Count",
       title = "Fig : Duration of Employement")


#Plotting proportion of Employment length on loan status
prop_loan_status_emp_length<-prop.table(table(loan_records_master$loan_status,loan_records_master$emp_length),2)

ggplot(as.data.frame(prop_loan_status_emp_length), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Employment Duration",y = "Percent of loan Status",
       title = "Fig : Proportion of Loan Status vs Job Experience" ) +
  theme_loan()+
  guides(fill=guide_legend(title="Loan Status"))+
  scale_y_continuous(labels = scales::percent)

#There is no disproportion in defaulting and making full payment of the loan based on employee length


# Plotting home ownership
ggplot(loan_records_master, aes(x = home_ownership)) +
  geom_bar() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Home Ownership",
       y = "Count",
       title = "Fig :Home Ownership")


#Plotting proportion of home ownership on loan status
prop_loan_status_home_own<-prop.table(table(loan_records_master$loan_status,loan_records_master$home_ownership),2)


ggplot(as.data.frame(prop_loan_status_home_own), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Home Ownership",y = "Percent of loan Status",
       title = "Fig : Proportion of Loan Status vs Home Ownership" ) +
  theme_loan()+
  guides(fill=guide_legend(title="Loan Status"))+
  scale_y_continuous(labels = scales::percent)

#There is no trend visible here, for Home ownership as none since there is no data available so the fully paid is showing as 100%



# Can verifying the source income improve full loan payment
ggplot(loan_records_master, aes(x = verification_status)) +
  geom_bar() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Income Verification Status",
       y = "Count",
       title = "Fig :Verification Status")


#Plotting proportion of verification status on loan status
prop_loan_status_veri_st<-prop.table(table(loan_records_master$loan_status,loan_records_master$verification_status),2)


ggplot(as.data.frame(prop_loan_status_veri_st), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Home Ownership",y = "Percent of loan Status",
       title = "Fig : Proportion of Loan Status vs Verification Status" ) +
  theme_loan()+
  guides(fill=guide_legend(title="Verification Status"))+
  scale_y_continuous(labels = scales::percent)
#There is no specific pattern visible here


#lets look according to states
prop_loan_status_state<-prop.table(table(loan_records_master$loan_status,loan_records_master$addr_state),2)


ggplot(as.data.frame(prop_loan_status_state), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "State Code",y = "Percent of loan Status",
       title = "Fig : Proportion of Loan Status vs States" ) +
  theme_loan()+
  guides(fill=guide_legend(title="Loan Status vs State"))+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x=element_text(angle=90))
#There is high proportion for defaulting for state NE in comparision to other states


#Lets look according to the purpose for which the loan was taken
prop_loan_status_purpose<-prop.table(table(loan_records_master$loan_status,loan_records_master$purpose),2)

ggplot(as.data.frame(prop_loan_status_purpose), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Purpose",y = "Percent of loan Status",
       title = "Fig : Proportion of Loan Status vs Purpose of Loan" ) +
  theme_loan()+
  guides(fill=guide_legend(title="Loan Status"))+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x=element_text(angle=90))
#We can observe that loan for small business are highest defaulters


#Lets look according to number of open accounts
ggplot(loan_records_master, aes(x = open_acc,fill=loan_status)) +
  geom_bar(position="dodge") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Number of open Accounts",
       y = "Count",
       title = "Fig : Number Of open Acc vs loan status")
#Number of open accounts does not affect the defaulting of loans

#Lets look according to number of total accounts
ggplot(loan_records_master, aes(x = total_acc,fill=loan_status)) +
  geom_bar(position="dodge") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Number of Total Accounts",
       y = "Count",
       title = "Fig : Number Of Total Acc vs loan status")
#Number of total accounts does not affect the defaulting of loans

#Lets look according to number of revol util
ggplot(loan_records_master, aes(x = revol_util,fill=loan_status)) +  geom_area(stat="bin") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Revol Util",
       y = "Count",
       title = "Fig : Number Of Revol util vs loan status")
#Revol Util does not affect the defaulting of loans


#Lets look according to revolving balance
ggplot(loan_records_master, aes(x = revol_bal,fill=loan_status)) +
  geom_histogram(binwidth=10000) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Revolving balance",
       y = "Count",
       title = "Fig : Revolving Balance vs loan status")
#Revolving does not affect the defaulting of loans

prop_loan_status_inq_6<-prop.table(table(loan_records_master$loan_status,loan_records_master$inq_last_6mths),2)

ggplot(as.data.frame(prop_loan_status_inq_6), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Inquiry in last 6 months",y = "Percent of loan Status",
       title = "Fig : Proportion of Loan Status vs Inquiry in 6 months" ) +
  theme_loan()+
  guides(fill=guide_legend(title="Loan Status"))+
  scale_y_continuous(labels = scales::percent)



#############################################################Bivariate Analysis##################################################################

# We will now analyze the default on the basis of annual income of the loanee
ggplot (loan_records_master, aes(y = annual_inc)) +
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "USD", x = "Employement Length",
        title = "Fig 1: Annual income of the loanees")
# We see there are few outliers in the annual income category
# Though it is recommened that outliers not be removed, we will remove 1% 
# for better representation on the graph
newdata <- subset(loan_records_master,!(loan_records_master$annual_inc > quantile(loan_records_master$annual_inc, probs=c(.00, .99))[2] | loan_records_master$annual_inc < quantile(loan_records_master$annual_inc, probs=c(.00, .99))[1]) ) 
ggplot (newdata, aes(y = annual_inc)) +
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "USD", x = "Employement Length",
        title = "Fig 1: Annual income of the loanee( removing 1% outliers)")

ggplot (newdata, aes(y = annual_inc, x = emp_length)) + # Adding employement lenght
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "USD",x = "Employement Length",
        title = "Fig 1: Annual income of the loanees")
# As expected, as employement length increase there is a increase in median income

# We will check the hypothesis if the median income of defaulters in lesser 

ggplot (newdata, aes(y = annual_inc, x = emp_length)) + # Adding employement lenght
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~loan_status) + # Additional variable
  labs( y = "USD", x = "Employement Length",
        title = "Fig 1: Annual income of the loanees")+theme(axis.text.x=element_text(angle=90))
# As we see annual income of people defaulting on loans is visibly less than the one
# who are paying back fully. 
# Annual income shuld be considered as one of the variable to decide on the eligibility
# and interest rate of the loans

# Let's check if the revolving utilization provide early warning on the default
str(loan_records_master$revol_util)
# Since it is in percentage the value will lie between 0 and 100
# We will draw a histogram to see the typical value of revolving credit line
ggplot(loan_records_master, aes(x = revol_util)) +
  geom_histogram(binwidth = 5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Percentage", x = "Revolving Utilization",
       title = "Revolving Line Utilization Rate")
# No one dominating value
ggplot (newdata, aes(y = revol_util, x = loan_status)) + # Loan status
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "Percentage", x = "Loan Status",
        title = "Fig 1: Revolving Line Utilization Rate")
# We see from the chart that Fully Paid loanees have lower revolving utilization than
# defaulters. Since this is a data for any one instance of time, we may not conclude.
# Though an increase in revolving utlization should be considered as one of the variable
# that can predict upcoming default. Appropriate action should be planned to avoid default.

# Another possible indicator for upcoming default can be more than 30+ days deliquency in last 24 months
length(unique(loan_records_master$delinq_2yrs))
# There are only 11 unique values in delinq_2yrs. we will convert this column 
# to character for further analysis
loan_records_master$delinq_2yrs <- as.character(loan_records_master$delinq_2yrs)

ggplot(loan_records_master, aes(x = delinq_2yrs )) + # Identify the extent of default
  geom_bar(fill = "steelblue4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Number of 30+ days past-due indicidence of delinquency",
       y = "Count",
       title = "Fig 1: 30+ days past-due indicidence of delinquency")
# Let's remove "0" for analysis
ggplot(subset(loan_records_master, delinq_2yrs!= "0" ), aes(x = delinq_2yrs )) + # Identify the extent of default
  geom_bar(fill = "steelblue4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~loan_status) +
  labs(x = "Number of 30+ days past-due indicidence of delinquency",
       y = "Count",
       title = "Fig 1: 30+ days past-due indicidence of delinquency")
# We cannot infer based on 30+ days past-due indicidence of delinquency that a loanee
# may default on loan 

ggplot(loan_records_master,aes(x=dti,y=revol_util,col=loan_status))+geom_smooth()+theme_loan()+
  labs(x = "debt-to-income ratio",
       y = "debt-to-limit-ratio",
       title = "Fig 12: DTI vs DTL plot")

# Against number of accounts
ggplot(loan_records_master, aes(y = total_acc)) +
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~loan_status) + # Additional variable
  labs( y = "USD", x = "Employement Length",
        title = "Fig 1: Annual income of the loanees")
# No visible trend

# Let's see if investing more in collecting information about customers can assist us 
# in identifying the upcoming default
length(unique(loan_records_master$pub_rec_bankruptcies))
loan_records_master$pub_rec_bankruptcies <- as.character(loan_records_master$pub_rec_bankruptcies)

ggplot(loan_records_master, aes(x = pub_rec_bankruptcies )) + # Identify the extent of default
  geom_bar(fill = "steelblue4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Public records of bankrupties",
       y = "Count",
       title = "Public record of bankrupticies")
# Let's remove "0" and "NA" for analysis
ggplot(subset(loan_records_master, pub_rec_bankruptcies!= "0" & pub_rec_bankruptcies!= "NA"), aes(x = pub_rec_bankruptcies)) + # Identify the extent of default
  geom_bar(fill = "steelblue4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~loan_status) +
  labs(x = "Public records of bankrupties",
       y = "Count",
       title = "Public records of bankrupties")
# Unfortunately we still cannot infer any statement from this chart

# Conclusion: We have identified few parameters that can provider proxies for the 
# default of the loans such as term of loans, one state of NC that is defaulting more
# than others and small businesses

# Also, as the revolving utilization increase, we see a trend of more defaults. 
# It can act as proxy to check the status of account before the default is made. 



