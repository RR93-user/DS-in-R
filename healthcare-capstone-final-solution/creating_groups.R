library("tidyr")

# Importing data corresponding to the 7 groups and organising into 7 separate data frames

####################################  Mortality  ###############################

raw_input_file <- read.csv("Readmissions and Deaths - Hospital.csv")

# removing measure name, estimates, dates
raw_input_file <- raw_input_file[,-c(9, 14:18)]
str(raw_input_file)

# Score and Denominator are currently character (factor) type variables and 
# contain 'Not Available'
length(which(raw_input_file$Score == "Not Available")) # 25k NAs 
length(which(raw_input_file$Denominator == "Not Available")) # Same
length(which(raw_input_file$Compared.to.National == "Not Available")) # Same

# replace 'Not Available' with NA
# since this needs to be done multiple times, lets write a function
f_removes_missing <- function(a_vector, replace_str, replace_by){
  a_vector[which(a_vector == replace_str)] <- replace_by
  return(a_vector)
}


raw_input_file$Score <- as.numeric(f_removes_missing(raw_input_file$Score,
                                                     "Not Available", 
                                                     NA))
raw_input_file$Denominator <- as.numeric(f_removes_missing(raw_input_file$Denominator,
                                                     "Not Available", 
                                                     NA))
raw_input_file$Compared.to.National <- f_removes_missing(raw_input_file$Compared.to.National,
                                                     "Not Available", 
                                                     NA)

# storing all hospital related info in table
table <- raw_input_file[, 1:8]
table <- table[ which(!duplicated(table)),]

# storing scores and denominators in two separate dfs
# a higher denominator means that a larger no. of data points were taken to measure
# the measure for the respective hospital
# High denominator => more important the variable; so we may need it later 
score <- raw_input_file[, c(1, 9, 12)]
denominator <- raw_input_file[,c(1, 9, 11)]
compared_to_national <- raw_input_file[, c(1, 9, 10)]

# converting score and denominator to wide format
score_wide <- spread(score, Measure.ID, Score)
denominator_wide <- spread(denominator, Measure.ID, Denominator)
compared_to_national_wide <- spread(compared_to_national, Measure.ID, Compared.to.National)

# merging table, score and denominator
data <- merge(denominator_wide, score_wide, 
              by="Provider.ID",
              suffixes=c("_denominator","_score"))


data <- merge(table, data, by="Provider.ID")

# extracting all readmissions columns from the dataframe
data_for_readmission <- data
readmission_data <- data_for_readmission[,c(1:8,15:22,29:36)]
readmission_score <- readmission_data[,c(1,17:24)]
write.csv(readmission_score, "group_data\\readmission.csv")

# retaining all the mortality columns only 
data_for_mortality <- data[,-c(15:22, 29:36)]


# Complications file 
raw_input_file_1 <- read.csv("Complications - Hospital.csv")
raw_input_file_1 <- raw_input_file_1[, c(1,10,12,13)]

raw_input_file_1$Score <- as.numeric(f_removes_missing(raw_input_file_1$Score,
                                                     "Not Available", 
                                                     NA))
raw_input_file_1$Denominator <- as.numeric(f_removes_missing(raw_input_file_1$Denominator,
                                                     "Not Available", 
                                                     NA))

# The variable "PSI_4_SURG_COMP" is also in the group mortality
mydf <- raw_input_file_1[which(raw_input_file_1$Measure.ID == "PSI_4_SURG_COMP"), ]
names(mydf)=c("Provider.ID","Measure.ID","PSI_4_SURG_COMP_Denominator","PSI_4_SURG_COMP_Score")
mydf <- mydf[,-2]

mortality_data <- merge(data_for_mortality,mydf,by="Provider.ID")
mortality_score <- mortality_data[, c(1,15:20,22)]
write.csv(mortality_score, "group_data\\mortality.csv")

######################## Safety of Care ###########################

safety_input <- read.csv("Healthcare Associated Infections - Hospital.csv")
safety_input <- safety_input[, -c(9, 11, 13, 14, 15)]

safety_input$Score <- f_removes_missing(safety_input$Score, "Not Available", NA)

table <- safety_input[,1:8]
table <- table[which(!duplicated(table)),]

score <- safety_input[,c(1,9,10)]
score_wide <- spread(score,Measure.ID,Score)
score_wide <- score_wide[,c(1,6,7,18,19,30,31,36,37,42,43,48,49)]
safety_data_1 <- merge(table,score_wide,by="Provider.ID")

safety_input_1 <- read.csv("Complications - Hospital.csv")
safety_input_1 <- safety_input_1[,c(1,10,12,13)]
safety_input_1$Score <- f_removes_missing(safety_input_1$Score, "Not Available", NA)
safety_input_1$Denominator <- f_removes_missing(safety_input_1$Denominator, "Not Available", NA)

score_1 <- safety_input_1[,c(1,2,4)]
Denominator_1 <- safety_input_1[,c(1,2,3)]
score_1_wide <- spread(score_1,Measure.ID,Score)
score_1_wide <- score_1_wide[,c(1,2,12)]
Denominator_1_wide <- spread(Denominator_1,Measure.ID,Denominator)
Denominator_1_wide <- Denominator_1_wide[,c(1,2,12)]
data_1 <- merge(score_1_wide,Denominator_1_wide,by="Provider.ID", suffixes = c("_Score","_Denominator"))
safety_data <- merge(safety_data_1,data_1,by="Provider.ID")

safety_score<-safety_data[,c(1,10,12,14,16,18,20:22)]
write.csv(safety_score, "group_data\\safety.csv")

############################# Patient Experience #########################################3

input_data <- read.csv("HCAHPS - Hospital.csv")
input_data <- input_data[,c(1:9,16,17)]


input_data$HCAHPS.Linear.Mean.Value <- f_removes_missing(input_data$HCAHPS.Linear.Mean.Value, "Not Applicable", NA)
input_data$Number.of.Completed.Surveys <- f_removes_missing(input_data$Number.of.Completed.Surveys, 
                                                            "Not Available", NA)

table <- input_data[,1:8]
table <- table[which(!duplicated(table)),]
score_mean <- input_data[,c(1,9,10)]
denominator_survey <- input_data[,c(1,9,11)]

score_mean_wide <- spread(score_mean,HCAHPS.Measure.ID,HCAHPS.Linear.Mean.Value)
denominator_survey_wide <- spread(denominator_survey, HCAHPS.Measure.ID, Number.of.Completed.Surveys)

score_mean_wide <- score_mean_wide[,c(1,5,8,13,18,23,28,32,38,44,49,53)]
denominator_survey_wide <- denominator_survey_wide[,c(1,5,8,13,18,23,28,32,38,44,49,53)]

data_1 <- merge(score_mean_wide,denominator_survey_wide,by="Provider.ID",suffixes=c("_mean","_total"))
experience_data <- merge(table,data_1,by="Provider.ID")
experience_score<-experience_data[,c(1,9:19)]

write.csv(experience_score, "group_data\\experience.csv")

#################### Medical Imaging ##########################

input_data <- read.csv("Outpatient Imaging Efficiency - Hospital.csv")

input_data <- input_data[,-c(12,13,14)]
input_data$Score <- f_removes_missing(input_data$Score, "Not Available", NA)

table <- input_data[,1:8]
score <- input_data[,c(1,9,11)]

score_wide <- spread(score,Measure.ID,Score)
score_wide <- score_wide[,-c(7)]

data <- merge(table, score_wide,by="Provider.ID",suffixes=c("_Score"))
medical_data <- data[which(!duplicated(data)),]
medical_score<-medical_data[,-c(2:8)]
write.csv(medical_score, "group_data\\medical.csv")

########################Timeliness#######################

timely_input <- read.csv("Timely and Effective Care - Hospital.csv")
timely_input <- timely_input[,-c(9,11,14:16)]

levels(factor(timely_input$Score))
levels(factor(timely_input$Sample))

timely_input$Sample <- f_removes_missing(timely_input$Sample, "Not Available", NA)

table <- timely_input[,1:8]
table <- table[which(!duplicated(table)),]
score <- timely_input[,c(1,9,10)]
score_wide <- spread(score,Measure.ID,Score)
score_wide <- score_wide[,c(1,5,6,12,14,15,20,22)]
sample <- timely_input[,c(1,9,11)]
sample_wide <- spread(sample,Measure.ID,Sample)
sample_wide <- sample_wide[,c(1,5,6,12,14,15,20,22)]
data <- merge(score_wide,sample_wide,by="Provider.ID",suffixes=c("_Score","_Sample"))
timely_data <- merge(table,data,by="Provider.ID")

timely_score<-timely_data[,c(1,9:15)]
write.csv(timely_score, "group_data\\timeliness.csv")

############################Effectiveness########################

input_data <- read.csv("Timely and Effective Care - Hospital.csv")

input_data <- input_data[,-c(14,15,16)]

input_data$Sample <- f_removes_missing(input_data$Sample, "Not Available", NA)

table <- input_data[,1:8]
score <- input_data[,c(1,10,12)]
denominator <- input_data[,c(1,10,13)]

score_wide <- spread(score,Measure.ID,Score)
score_wide <- score_wide[,c(1,4,9,10,16,17,18,19,21,23,35:41,43,44)]
denominator_wide <- spread(denominator,Measure.ID,Sample)
denominator_wide <- denominator_wide[,c(1,4,9,10,16,17,18,19,21,23,35:41,43,44)]

data <- merge(denominator_wide,score_wide,by="Provider.ID",suffixes=c("_denominator","_Score"))
data <- merge(table,data,by="Provider.ID")

effectiveness_data <- data[which(!duplicated(data)),]

effectiveness_score<-effectiveness_data[,c(1,27:44)]
write.csv(effectiveness_score, "group_data\\effectiveness.csv")

#####################################Score Calculation#######################################################

masterfile<-merge(mortality_score,readmission_score,by="Provider.ID",all=TRUE)
masterfile<-merge(masterfile, safety_score, by="Provider.ID",all=TRUE)
masterfile<-merge(masterfile, experience_score, by="Provider.ID",all=TRUE)
masterfile<-merge(masterfile, medical_score, by="Provider.ID",all=TRUE)
masterfile<-merge(masterfile, timely_score, by="Provider.ID",all=TRUE)
masterfile<-merge(masterfile, effectiveness_score, by="Provider.ID",all=TRUE)

write.csv(masterfile, "group_data\\master.csv")





