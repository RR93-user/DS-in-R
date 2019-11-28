library(ggplot2)
library(corrplot)
library(dplyr)
library(Information)
library(gridExtra)

# --  Univariate analysis - identifying and comparing group importance --- 
# Hospital star rating analysis

ratings <- read.csv("Hospital General Information.csv")
str(ratings)
summary(ratings)



# Hospital Type - Most are acute care type, which are the ones included in star ratings
summary(ratings$Hospital.Type) 
table(ratings$Hospital.Type, ratings$Hospital.overall.rating)
# Most acute care hospitals are given ratings, none of the children care hospitals 
# are given ratings, and most critical care hospitals are not given ratings

# Let's thus subset the data to only the acute care hospitals
ratings <- filter(ratings, Hospital.Type == "Acute Care Hospitals")
ratings$Hospital.Type <- factor(as.character(ratings$Hospital.Type))
table(ratings$Hospital.Type, ratings$Hospital.overall.rating)


# Ownership - most are private
summary(ratings$Hospital.Ownership)

# Overall rating
summary(ratings$Hospital.overall.rating)
ratings$Hospital.overall.rating[which(ratings$Hospital.overall.rating == "Not Available")] <- NA 
ratings$Hospital.overall.rating <- as.numeric(ratings$Hospital.overall.rating)

# Segmented univariate analysis - segmenting across different 'group' categorical variables 
# and comparing the average overall ratings

avg_by_ownership <- ratings %>% group_by(Hospital.Ownership) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_ownership
# low ratings for tribal, local government; high for physician, voluntary non-profit etc.

# mortality
avg_by_mortality <- ratings %>% group_by(Mortality.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n()) %>%
  arrange(desc(avg_rating))

avg_by_mortality

# looking at the difference between the avg ratings of 'above' and 'below' the national
# average
avg_by_mortality$avg_rating[1] - avg_by_mortality$avg_rating[nrow(avg_by_mortality)]

# Thus, the average star rating varies by about 1.15 in different levels of mortality
# We can similarly compare the avg rating across other groups and observe which ones affect the rating more


# a function to do the same procedure for all group categorical variables
plot_rating <- function(cat_var, var_name){
  a <- aggregate(Hospital.overall.rating~cat_var, ratings, mean)
  b <- aggregate(Hospital.overall.rating~cat_var, ratings, length)
  
  colnames(a) <- var_name
  colnames(b)[1] <- var_name
  agg_response <- merge(a, b, by = var_name)
 
  colnames(agg_response) <- c(var_name, "avg_rating","count")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  agg_response <- arrange(agg_response, desc(avg_rating))
  agg_response[, 2] <- as.numeric(agg_response[, 2])
  
  p.plot <- ggplot(agg_response, aes(agg_response[, 1], avg_rating, label = count)) +
    geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
  return(list(agg_response[1, 2] - agg_response[nrow(agg_response), 2], p.plot, agg_response))
  
}

diff_mortality = plot_rating(ratings$Mortality.national.comparison, "mortality")
diff_mortality[[1]]
diff_mortality[[2]]
diff_mortality[[3]]

# safety of care
diff_safety = plot_rating(ratings$Safety.of.care.national.comparison, "safety")
diff_safety[[1]]
diff_safety[[2]]
diff_safety[[3]]

##
avg_by_safety <- ratings %>% group_by(Safety.of.care.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_safety
ggplot(ratings, aes(x=factor(Safety.of.care.national.comparison), 
                    fill=factor(Hospital.overall.rating))) + geom_bar(position = "dodge")

# readmission

diff_readmission = plot_rating(ratings$Readmission.national.comparison, "readmission")
diff_readmission[[1]]
diff_readmission[[2]]

#
avg_by_readmission <- ratings %>% group_by(Readmission.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_readmission

# patient experience
diff_experience = plot_rating(ratings$Patient.experience.national.comparison, "experience")
diff_experience[[1]]
diff_experience[[2]]

#
avg_by_experience <- ratings %>% group_by(Patient.experience.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_experience

# effectiveness
diff_effectiveness = plot_rating(ratings$Effectiveness.of.care.national.comparison, "experience")
diff_effectiveness[[1]]
diff_effectiveness[[2]]


avg_by_effectiveness <- ratings %>% group_by(Effectiveness.of.care.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_effectiveness

# medical
diff_medical = plot_rating(ratings$Efficient.use.of.medical.imaging.national.comparison, "experience")
diff_medical[[1]]
diff_medical[[2]]


avg_by_medical <- ratings %>% group_by(Efficient.use.of.medical.imaging.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_medical

# there's not much difference between the average ratings of above and same as 
# national average hospitals by medical group variables

# timeliness
diff_timeliness = plot_rating(ratings$Timeliness.of.care.national.comparison, "experience")
diff_timeliness[[1]]
diff_timeliness[[2]]

avg_by_timeliness <- ratings %>% group_by(Timeliness.of.care.national.comparison) %>%
  summarise(avg_rating = mean(Hospital.overall.rating, na.rm = T), count = n())

avg_by_timeliness
# timeliness seems to affect avg rating significantly (from 2.62 to 3.24 )


# Effect of groups on avg star rating - summary
group_effect <- c(mortality = diff_mortality[[1]], timeliness = diff_timeliness[[1]], 
                  medical = diff_medical[[1]], effectiveness = diff_effectiveness[[1]], 
                  experience = diff_experience[[1]], safety = diff_safety[[1]], 
                  readmission = diff_readmission[[1]])

group_effect[order(group_effect, decreasing=T)]

# This is also expected - the top 4 groups have been assigned an importance of 22% each, 
# while the bottom three have 4% weightage

# let's store the imporance of groups in a named vector group_importance
group_importance <- group_effect[order(group_effect, decreasing=T)]


# --------- Bivariate analysis -----------

# mortality
mortality <- read.csv("group_data\\mortality_clean.csv")

# looking at correlations between variables
mortality_cor <- round(cor(mortality[, -c(1, 2)], use="pairwise.complete.obs"), 2)
write.csv(mortality_cor, "group_data\\correlations\\mortality_cor.csv")

# looking at the csv file after conditional formatting, COPD score and HF score are
# negatively correlated to almost every other variable
# Also, the absolute value of correlation of COPD and HF with other measures is quite small 
# It implies that hospitals who perform better (in managing mortality) do so for PN, STK COMP, CABG 
# Hence these 4 variables seem to be important mortality measures

# Let's see how measure scores affect overall star rating
ratings <- read.csv("Hospital General Information.csv")
mortality <- mortality[, -1]
mort_rating <- merge(mortality, ratings[, c("Provider.ID", "Hospital.overall.rating")], by="Provider.ID")

mort_summary <- mort_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

write.csv(mort_summary, "group_data\\directions\\mortality_directions.csv")

# Can see that the rating increases as each measure increases
# but not for COPD and HF 
# This indicates that MORT_30_COPD_score and MORT_30_HF_score may be 
# the lesser important variables

# 2. readmission
readmission <- read.csv("group_data\\readmission_clean.csv")
readmission_cor <- round(cor(readmission[, -c(1, 2)], use="pairwise.complete.obs"), 2)
corrplot(readmission_cor)
write.csv(readmission_cor, "group_data\\correlations\\readmission_cor.csv")

# Observations: Hip-Knee is uncorrelated and thus is probably not an important measure
# Similarly, CABG (readm due to Coronary Artery Bypass Graft) patients and Stroke seem less important
# Hospital wide readmission seems the most important measure, as expected, since it measures 
# readmissions across all types of treatments

# Let's see how measure scores affect overall star rating
readmission <- readmission[, -1]
read_rating <- merge(readmission, ratings[, c("Provider.ID", "Hospital.overall.rating")], by="Provider.ID")

read_summary <- read_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

write.csv(read_summary, "group_data\\directions\\readmission_directions.csv")
# Can see that the ratings go up as all measures of readmission increase
# Thus, all readmission measures seem to be important

# 3. timeliness
timeliness <- read.csv("group_data\\timeliness_clean.csv")
timeliness_cor <- round(cor(timeliness[, -c(1, 2)], use="pairwise.complete.obs"), 2)
corrplot(timeliness_cor)
write.csv(timeliness_cor, "group_data\\correlations\\timeliness_cor.csv")

# All OP_x variables (median time patients spent before a treatment/being attended) seem either 
# positively correlated or non-correlated, but not negative - which is expected since hospitals that 
# are punctual would tend to be punctual across treatment types

# ED-1b and ED-2b are negatively correlated only with each other, but not with other measures
# This may be negative because hospitals who tend to make the patients wait in emergency dept 
# before admission may tend to avoid any furtehr delays after admitting the patient

# How measures affect star ratings
timeliness <- timeliness[, -1]

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
timeliness_rating <- merge(timeliness, ratings[, c("Provider.ID", "Hospital.overall.rating")], by="Provider.ID")

timeliness_summary <- timeliness_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

write.csv(timeliness_summary, "group_data\\directions\\timeliness_directions.csv")

# Indicates that ED_1b and OP_21 are the most important measures
# OP 18b and OP 3b also seem important, but the overall weightage of the group 
# is only 4%, so that will reduce the importance

# 4. effectiveness
effectiveness <- read.csv("group_data\\effectiveness_clean.csv")
effectiveness_cor <- round(cor(effectiveness[, -c(1, 2)], use="pairwise.complete.obs"), 2)
write.csv(effectiveness_cor, "group_data\\correlations\\effectiveness_cor.csv")

# How measures affect star ratings
effectiveness <- effectiveness[, -1]

for (col in 1:(ncol(effectiveness))) {
  effectiveness[, col] <- as.numeric(f_removes_missing(effectiveness[, col],
                                            "Not Available", NA))
}

effectiveness_rating <- merge(effectiveness, ratings[, c("Provider.ID", "Hospital.overall.rating")], by="Provider.ID")

effectiveness_summary <- effectiveness_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

write.csv(effectiveness_summary, "group_data\\directions\\effectiveness_directions.csv")

# Ratings reduce as all VTE measures increase (apart from VTE_6)
# this indicates that VTE (1, 2, 3, 5) measures are correlated to each other 
# and thus may be important predictors
# Same for STK measures


# 5. experience
experience <- read.csv("group_data\\experience_clean.csv")
experience_cor <- round(cor(experience[, -c(1, 2)], use="pairwise.complete.obs"), 2)
min(experience_cor)
write.csv(experience_cor, "group_data\\correlations\\experience_cor.csv")

# all variables are positively correlated with each other, such as nurse communication,
# doctor communication, overall rating etc., indicating that hospitals who do well 
# in communication and hospitality generally do well in all such areas
# One hypothesis may be that the overall culture of hospitals impacts all these measures

# How measures affect star ratings
experience <- experience[, -1]

for (col in 1:(ncol(experience))) {
  experience[, col] <- as.numeric(f_removes_missing(experience[, col],
                                            "Not Available", NA))
}

experience_rating <- merge(experience, ratings[, c("Provider.ID", "Hospital.overall.rating")], by="Provider.ID")

experience_summary <- experience_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

write.csv(experience_summary, "group_data\\directions\\experience_directions.csv")
# Indicates that all measures in experience are important measures 

# 6. medical
medical <- read.csv("group_data\\medical_clean.csv")
medical_cor <- round(cor(medical[, -c(1, 2)], use="pairwise.complete.obs"), 2)
write.csv(medical_cor, "group_data\\correlations\\medical_cor.csv")

# seems like there's no significance pattern here, apart from OP-10 and OP_11
#(unnecessary double scan of abdomen and chest respectively)

# How measures affect star ratings
medical <- medical[, -1]

for (col in 1:(ncol(medical))) {
  medical[, col] <- as.numeric(f_removes_missing(medical[, col],
                                            "Not Available", NA))
}

medical_rating <- merge(medical, ratings[, c("Provider.ID", "Hospital.overall.rating")], by="Provider.ID")

medical_summary <- medical_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

write.csv(medical_summary, "group_data\\directions\\medical_directions.csv")
# Only OP_14 seems significant, but the weight of medical group itself is quite less

# 7. safety
safety <- read.csv("group_data\\safety_clean.csv")
safety_cor <- round(cor(safety[, -c(1, 2)], use="pairwise.complete.obs"), 2)
write.csv(safety_cor, "group_data\\correlations\\safety_cor.csv")

# HAI-1, 2 and 3 seem the most correlated ones, and almost all are uncorrelated 
# with HAI-5, HAI-6 and COMP-HIP-KNEE score

# How measures affect star ratings
safety <- safety[, -1]

for (col in 1:(ncol(safety))) {
  safety[, col] <- as.numeric(f_removes_missing(safety[, col],
                                            "Not Available", NA))
}

safety_rating <- merge(safety, ratings[, c("Provider.ID", "Hospital.overall.rating")], by="Provider.ID")

safety_summary <- safety_rating[, -1] %>% group_by(Hospital.overall.rating) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

write.csv(safety_summary, "group_data\\directions\\safety_directions.csv")

# Thus, HAI 1, 2, 4, 5, comp_hip_knee and PSI_safety seem important predictors   

# ------ Understanding the importance of measures in star rating prediction for each group---
star_ratings <- ratings[, c(1, 13)]

# removing rows with NA values of star rating
star_ratings[, 2] <- f_removes_missing(star_ratings[, 2], "Not Available", NA)
star_ratings[, 2] <- as.numeric(star_ratings[, 2])
summary(star_ratings$Hospital.overall.rating)


# merging group level data with star ratings
mortality_rating  = merge(mortality, star_ratings, by="Provider.ID")
readmission_rating  = merge(readmission, star_ratings, by="Provider.ID")
effectiveness_rating  = merge(effectiveness, star_ratings, by="Provider.ID")
timeliness_rating  = merge(timeliness, star_ratings, by="Provider.ID")
medical_rating  = merge(medical, star_ratings, by="Provider.ID")
experience_rating  = merge(experience, star_ratings, by="Provider.ID")
safety_rating  = merge(safety, star_ratings, by="Provider.ID")



###############Optional - Ignore below this ###############3
# Information Value of measures
# The aim is to understand how each measure influences the star rating and identify the 
# important ones

# Since all measures are continuous variables, we can bin them into categorical and 
# compare the average rating for each category

# To convert continuous measures into categorical, i.e. to do binning, we can use the 'Information Value 
# (IV)' of each measure. A high IV implies that the measure is a significant predictor and vice versa. 

# To simplify the analysis, let's divide the star ratings into two levels
star_ratings$Hospital.overall.rating <- ifelse(star_ratings$Hospital.overall.rating <= 3, 0, 1)
star_ratings <- star_ratings[-which(is.na(star_ratings$Hospital.overall.rating)),]

mortality_rating  = merge(mortality, star_ratings, by="Provider.ID")
readmission_rating  = merge(readmission, star_ratings, by="Provider.ID")
effectiveness_rating  = merge(effectiveness, star_ratings, by="Provider.ID")
timeliness_rating  = merge(timeliness, star_ratings, by="Provider.ID")
medical_rating  = merge(medical, star_ratings, by="Provider.ID")
experience_rating  = merge(experience, star_ratings, by="Provider.ID")
safety_rating  = merge(safety, star_ratings, by="Provider.ID")


# create infotables from the Information package does that
?create_infotables

# also, the target variable has to be binary
IV_mortality <- create_infotables(mortality_rating[,-1], 
                        y = "Hospital.overall.rating")

arrange(IV_mortality[[2]], desc(IV))

# Following the thumb rule, a variable is:
# Useless if IV is < 0.02
# Weak if IV is [0.02, 0.1)
# Medium if IV is [0.1, 0.3)
# Strong if IV is[0.3, 0.5) and suspicious thereafter

# Thus, in mortality, mort due to pneumonia is extremely important,
# AMI, COPD, HF and COMP seem to be of medium importance, rest are weak
# Note that the ultimate importance also depends on the importance of the mortality group itself

# readmission
IV_readmission <- create_infotables(readmission_rating[,-1], 
                        y = "Hospital.overall.rating")

arrange(IV_readmission[[2]], desc(IV))
# readmission measures seem to be way more important than expected, even suspicious

# This is expected since readmission seems to be the most important group (analysed before)
group_importance

# experience
IV_experience <- create_infotables(experience_rating[,-1], 
                        y = "Hospital.overall.rating")

arrange(IV_experience[[2]], desc(IV))
# well, all measures in experience seem suspiciously important predictors of star rating
# This is because we've clubbed ratings 4 and 5 to 1 and 1,2, 3 to 0. 
# However, this table succesfully shows the relative importance of measures in the group 
# It implies that, for e.g., if a hospital's HSP_RATING_linear score is high, it is very likely 
# to have a rating of 4 or 5

# let's visualise how the values of measures affect rating
ggplot(experience_rating, aes(x=H_CLEAN_LINEAR_SCORE_mean, fill=factor(Hospital.overall.rating)))+
  geom_histogram()


# safety
IV_safety <- create_infotables(safety_rating[,-1], 
                        y = "Hospital.overall.rating")

arrange(IV_safety[[2]], desc(IV))
# PSI_90 seems important, all the rest are medium to weak

# timeliness
IV_timeliness <- create_infotables(timeliness_rating[,-1], 
                        y = "Hospital.overall.rating")

arrange(IV_timeliness[[2]], desc(IV))
# all measures are medium - weak, as expected because of lower importance of timeliness group

# effectiveness
IV_effectiveness <- create_infotables(effectiveness_rating[,-1], 
                        y = "Hospital.overall.rating")

arrange(IV_effectiveness[[2]], desc(IV))
# all measures are medium - weak

# medical
IV_medical <- create_infotables(medical_rating[,-1], 
                        y = "Hospital.overall.rating")

arrange(IV_medical[[2]], desc(IV))
# all measures are weak