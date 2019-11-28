library(dplyr)
library(ggplot2)

# After creating separate files for groups, 
# Cleaning involves: 1) Standardising the variables to convert to a common scale, 
# 2) Accounting for the 'direction' of the measure (acc to whether lower or higher 
# is better) such that higher is always better and 3) Outlier imputation

# Note: The file 'Hospital.pdf' contains the data dictionary 

# 1) mortality
mortality <- read.csv("group_data\\mortality.csv")
# removing the redundant row numbers
mortality <- mortality[ ,-1]

# All measures are 'lower is better' since they indicate the death rate

# Thus, we need to scale in the reverse direction such that +1 indicates good and
# -1 indicates bad, i.e. (mu - x) / sigma
standardise_negative <- function(some_vector){
  return( (mean(some_vector, na.rm = T) -  some_vector)/(sd(some_vector, na.rm = T)) )
}

mortality[,2:ncol(mortality)] <- sapply(mortality[, -1], standardise_negative)

# Outlier imputation: According to the CMS documentation, they've trimmed the 
# measures at the 0.125th and the 99.875th percentiles

# example calculation
q = quantile(mortality$MORT_30_AMI_score, probs = seq(0, 1, 0.00001), na.rm = T)
length(q)
q[0.00126*length(q)]
q[0.99876*length(q)]

mortality$MORT_30_AMI_score[which(mortality$MORT_30_AMI_score <= q[0.00126*length(q)])] <- q[0.00126*length(q)]
mortality$MORT_30_AMI_score[which(mortality$MORT_30_AMI_score >= q[0.99876*length(q)])] <- q[0.99876*length(q)]

# let's write a function to do this for all measures

impute_outliers <- function(a_dataframe){
  for (col in 2:(ncol(a_dataframe))){
    q = quantile(a_dataframe[, col], probs = seq(0, 1, 0.00001), na.rm = T)
    a_dataframe[, col][which(a_dataframe[, col] <= q[0.00126*length(q)])] <- q[0.00126*length(q)]
    a_dataframe[, col][which(a_dataframe[, col] >= q[0.99876*length(q)])] <- q[0.99876*length(q)]
  }
  return(a_dataframe)
}

mortality <- impute_outliers(mortality)
write.csv(mortality, "group_data\\mortality_clean.csv")

# 2) readmission: all the measures are currently 'lower is better'
readmission <- read.csv("group_data\\readmission.csv")
# removing the redundant row numbers
readmission <- readmission[ ,-1]

readmission[, 2:ncol(readmission)] <- sapply(readmission[, -1], standardise_negative)
readmission <- impute_outliers(readmission)


write.csv(readmission, "group_data\\readmission_clean.csv")

# 3) medical imaging
medical <- read.csv("group_data\\medical.csv")
medical <- medical[, -1]

# Meaning of variables: The data dictionary states that 
# 'The measures on the use of medical imaging show how often a hospital 
# provides specific imaging tests for Medicare beneficiaries under circumstances where 
# they may not be medically appropriate.' Thus , lower percentages suggest more efficient use of 
# medical imaging

medical[, 2:ncol(medical)] <- sapply(medical[, -1], standardise_negative)
medical <- impute_outliers(medical)

write.csv(medical, "group_data\\medical_clean.csv")

# 4) safety
safety <- read.csv("group_data\\safety.csv")
safety <- safety[, -1]
  
# The HAI measures show how often patients in a particular hospital contract certain infections during the course 
# of their medical treatment, when compared to like hospitals.Thus, lower is better.
safety[, 2:ncol(safety)] <- sapply(safety[, -1], standardise_negative)
safety <- impute_outliers(safety)
write.csv(safety, "group_data\\safety_clean.csv")

# 5) timeliness
timeliness <- read.csv("group_data\\timeliness.csv")
timeliness <- timeliness[, -1]


# All the measures in timeliness indicate the median time the patient had to wait
# before being attended. Thus, all the measures are 'lower is better', i.e. negative.

# timeliness contains "Not Available" values, lets fix them
f_removes_missing <- function(a_vector, replace_str, replace_by){
  a_vector[which(a_vector == replace_str)] <- replace_by
  return(a_vector)
}


sapply(timeliness, function(x) length(which(x == "Not Available")))
for (col in 1:(ncol(timeliness))) {
  timeliness[, col] <- as.numeric(f_removes_missing(timeliness[, col],
                                            "Not Available", NA))
}

str(timeliness)
timeliness[, 2:ncol(timeliness)] <- sapply(timeliness[, -1], standardise_negative)

timeliness <- impute_outliers(timeliness)
write.csv(timeliness, "group_data\\timeliness_clean.csv")

# 6) experience: It measures cleanliness, how well the nurses/doctors communicate,
# quietness etc. Thus, higher score is better here.
experience <- read.csv("group_data\\experience.csv")
experience <- experience[, -1]

sapply(experience, function(x) length(which(x == "Not Available")))

for (col in 1:(ncol(experience))) {
  experience[, col] <- as.numeric(f_removes_missing(experience[, col],
                                            "Not Available", NA))
}
str(experience)

# create a function to standardise such that higher score indicates
# better quality
standardise_positive <- function(some_vector){
  return( ( some_vector - mean(some_vector, na.rm = T) )/(sd(some_vector, na.rm = T)) )
}

experience[, 2:ncol(experience)] <- sapply(experience[, -1], standardise_positive)

experience <- impute_outliers(experience)
write.csv(experience, "group_data\\experience_clean.csv")

# 7)  effectiveness
effectiveness <- read.csv("group_data\\effectiveness.csv")
effectiveness <- effectiveness[, -1]

sapply(effectiveness, function(x) length(which(x == "Not Available")))

for (col in 1:(ncol(effectiveness))) {
  effectiveness[, col] <- as.numeric(f_removes_missing(effectiveness[, col],
                                            "Not Available", NA))
}

# In effectiveness, some measures are positive while some are negative
# This information is mentioned in the files 'Timely and Effective care' in the measure name column
timely_effective <- read.csv("Timely and Effective Care - National.csv")
View(timely_effective)

positive_measures <- c(2, 3, 4, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18)
negative_measures <- c(5, 10, 19)

effectiveness[, positive_measures] <- sapply(effectiveness[, positive_measures], 
                                             standardise_positive)

effectiveness[, negative_measures] <- sapply(effectiveness[, negative_measures], 
                                             standardise_negative)

effectiveness <- impute_outliers(effectiveness)
write.csv(effectiveness, "group_data\\effectiveness_clean.csv")

# merging all standardised group data frames
masterfile <- merge(mortality, readmission, by="Provider.ID",all=TRUE)
masterfile <- merge(masterfile, safety, by="Provider.ID",all=TRUE)
masterfile <- merge(masterfile, experience, by="Provider.ID",all=TRUE)
masterfile <- merge(masterfile, medical, by="Provider.ID",all=TRUE)
masterfile <- merge(masterfile, timeliness, by="Provider.ID",all=TRUE)
masterfile <- merge(masterfile, effectiveness, by="Provider.ID",all=TRUE)


# Converting fields to 3 decimal places
masterfile[, 2:ncol(masterfile)] <- round(masterfile[, 2:ncol(masterfile)], 3)
write.csv(masterfile, "group_data\\masterfile_clean.csv")






