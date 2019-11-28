# Factor analysis for each of the seven groups separately
library(psych)
library(Information)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(caret)
library(gridExtra)
library(cowplot)

# effectiveness
effectiveness <- read.csv("group_data\\effectiveness_clean.csv") 
str(effectiveness)

# removing row numbers and ID 
eff <- effectiveness[, -c(1, 2)]
str(eff)
sum(is.na(eff))


# observing correlations in the effectiveness measures
eff_cor <- lowerCor(eff)

# Some of the correlated variables are CAC_3_score-IMM_2_score, STK_6, 
# STK_8 etc. These should appear in the loading coefficeints as well. 

# Using fa from the psych package to find the loadings
eff.fa <- fa(eff, fm="ml", scores = "tenBerge")
eff_loadings <- eff.fa$loadings
eff_loadings
# All VTE scores, STK scores and IMM_2 score have high factor loadings


eff_loads <- vector()

for (i in 1:(length(eff_loadings))){
  print(eff_loadings[i])
  eff_loads[i] <- eff_loadings[i]
}

round(eff_loads, 2) 

# comparing with correlations
eff_cor <- read.csv("group_data\\correlations\\effectiveness_cor.csv")
eff_cor <- data.frame(sapply(eff_cor[, -1], function(x) round(mean(x), 2)))
eff_cor <- cbind(eff_cor, round(eff_loads, 2))
colnames(eff_cor) <- c("avg_correlation", "factor loading")
View(eff_cor)

# You can see that, roughly, the factor loadings are high for the more correlated variables
# Thus, the important (i.e. high weight) measures should be VTE_1, VTE_2, STK_8, IMM_2, VTE_5,
# STK_6, STK_5, VTE_3, STK_4 (based on loading values)

# factanal also returns a list of weights
# weights
eff_weights <- eff.fa$weights
eff_weights
sum(eff_weights)
# Note that the sum of weights is not one
# We can normalise the weights simply by dividing each weight by the sum of weights
eff_weights = eff_weights/sum(eff_weights)
sum(eff_weights)

eff_cor <- cbind(eff_cor, round(eff_weights, 3))
colnames(eff_cor)[3] <- "weights"
View(eff_cor)
# Can see that weights are almost proportional to factor loadings
# Using the weights, we can calculate the group score for each hospital

# Calculating group scores for each hospital
# Acc to CMS, a hospital needs to have at least 3 measures per group 
# lets find the hospitals which do not meet this criteria
# finding rows with less than 3 measures 
eff$invalid <- FALSE

for (row in 1:nrow(eff)){
  if (sum(!is.na(eff[row, ])) <= 3) {eff[row, c("invalid")] = TRUE}
  else {eff[row, c("invalid")] = FALSE}
}

sum(eff$invalid)/nrow(eff)
# Thus, about 22% hospitals are not valid to get effectiveness score  
valid_indices <- which(!eff$invalid)
valid_indices

eff <- eff[which(!eff$invalid), ]
sum(eff$invalid)
eff <- eff[, -ncol(eff)]

# to calculate scores, we need to take care of missing values
# replacing them with the median value of the measure
f_na <- function(measure){
  measure[which(is.na(measure))] <- median(measure, na.rm=T)
  return(measure)
}

eff <- data.frame(sapply(eff, f_na))

# calculating group scores
eff_weights
n <- ncol(eff) 
eff$score <- 0

for (row in 1:nrow(eff)){
  eff[row, c("score")] <- round(sum(eff[row, 1:n]*eff_weights)/length(eff_weights), 3)
}

effectiveness <- effectiveness[valid_indices, ]
effectiveness$score <- eff$score
effectiveness_scores <- effectiveness[, c("Provider.ID", "score")]
colnames(effectiveness_scores) <- c("Provider.ID", "eff_score")
View(effectiveness_scores)

######### effectiveness scores end ############

# we need to do this for 6 other groups, so
# let's put this entire code in a function
# it takes in the main group measures file and the measure correlation file
# and returns the group scores for each hospital

f_group_scores <- function(main = measures_file, measures_cor){
  
  
  # removing row numbers and ID 
  measures <- main[, -c(1, 2)]
  str(measures)
  sum(is.na(measures))
  
  
  # Using fa from the psych package to find the loadings
  measures.fa <- fa(measures, fm="ml", scores = "tenBerge")
  measures_loadings <- measures.fa$loadings
  measures_loadings
  
  
  measures_loads <- vector(mode="numeric", length=length(measures_loadings))
  
  for (i in 1:(length(measures_loadings))){
    print(measures_loadings[i])
    measures_loads[i] <- measures_loadings[i]
  }
  
  round(measures_loads, 2) 
  
  # comparing with correlations
  measures_cor <- data.frame(sapply(measures_cor[, -1], function(x) round(mean(x), 2)))
  measures_cor <- cbind(measures_cor, round(measures_loads, 2))
  colnames(measures_cor) <- c("avg_correlation", "factor loading")

  # You can see that, roughly, the factor loadings are high for the more correlated variables
  
  # factanal also returns a list of weights
  # weights
  measures_weights <- measures.fa$weights
  measures_weights <- measures_weights/sum(measures_weights)
  measures_cor <- cbind(measures_cor, round(measures_weights, 3))
  colnames(measures_cor)[2] <- "factor_loading"
  colnames(measures_cor)[3] <- "weight"
  View(measures_cor)
  # Can see that weights are almost proportional to factor loadings
  # Using the weights, we can calculate the group score for each hospital
  
  # Calculating group scores for each hospital
  # Acc to CMS, a hospital needs to have at least 3 measures per group 
  # lets find the hospitals which do not meet this criteria
  # finding rows with less than 3 measures 
  measures$invalid <- FALSE
  
  for (row in 1:nrow(measures)){
    if (sum(!is.na(measures[row, ])) <= 3) {measures[row, c("invalid")] = TRUE}
    else {measures[row, c("invalid")] = FALSE}
  }
  
  sum(measures$invalid)/nrow(measures)
  # Thus, about 27% hospitals are not valid to get effectiveness score  
  valid_indices <- which(!measures$invalid)
  valid_indices
  
  measures <- measures[which(!measures$invalid), ]
  sum(measures$invalid)
  measures <- measures[, -ncol(measures)]
  
  # to calculate scores, we need to take care of missing values
  # replacing them with the median value of the measure
  f_na <- function(measure){
    measure[which(is.na(measure))] <- median(measure, na.rm=T)
    return(measure)
  }
  
  measures <- data.frame(sapply(measures, f_na))
  
  # calculating group scores
  measures_weights
  n <- ncol(measures) 
  measures$score <- 0
  
  for (row in 1:nrow(measures)){
    measures[row, c("score")] <- round(sum(measures[row, 1:n]*measures_weights)/length(measures_weights), 3)
  }
  
  main <- main[valid_indices, ]
  main$score <- measures$score
  main_scores <- main[, c("Provider.ID", "score")]
  colnames(main_scores) <- c("Provider.ID", "group_score")
  group_scores <- main_scores

  return(list(group_scores, measures_cor))
  
}

# effectiveness
main <- read.csv("group_data\\effectiveness_clean.csv")
measures_cor <- read.csv("group_data\\correlations\\effectiveness_cor.csv")
eff_scores <- f_group_scores(main, measures_cor) 
colnames(eff_scores[[1]])[2] <- "eff_score"
View(eff_scores[[2]])
write.csv(eff_scores[[2]], "measure_importance\\effectiveness_importance.csv")



# experience
main <- read.csv("group_data\\experience_clean.csv") 
measures_cor <- read.csv("group_data\\correlations\\experience_cor.csv")
exp_scores <- f_group_scores(main, measures_cor)
colnames(exp_scores[[1]])[2] <- "exp_score"
View(exp_scores[[2]])
write.csv(exp_scores[[2]], "measure_importance\\experience_importance.csv")

# mortality
main <- read.csv("group_data\\mortality_clean.csv") 
measures_cor <- read.csv("group_data\\correlations\\mortality_cor.csv")
mortality_scores <- f_group_scores(main, measures_cor)
colnames(mortality_scores[[1]])[2] <- "mort_score"
View(mortality_scores[[2]])
write.csv(mortality_scores[[2]], "measure_importance\\mortality_importance.csv")

# safety
main <- read.csv("group_data\\safety_clean.csv") 
measures_cor <- read.csv("group_data\\correlations\\safety_cor.csv")
safety_scores <- f_group_scores(main, measures_cor)
colnames(safety_scores[[1]])[2] <- "safety_score"
View(safety_scores[[2]])
write.csv(safety_scores[[2]], "measure_importance\\safety_importance.csv")

# timeliness
main <- read.csv("group_data\\timeliness_clean.csv") 
measures_cor <- read.csv("group_data\\correlations\\timeliness_cor.csv")
timeliness_scores <- f_group_scores(main, measures_cor)
colnames(timeliness_scores[[1]])[2] <- "timeliness_score"
View(timeliness_scores[[2]])
write.csv(timeliness_scores[[2]], "measure_importance\\timeliness_importance.csv")

# readmission
main <- read.csv("group_data\\readmission_clean.csv") 
measures_cor <- read.csv("group_data\\correlations\\readmission_cor.csv")
readmission_scores <- f_group_scores(main, measures_cor)
colnames(readmission_scores[[1]])[2] <- "readm_score"
View(readmission_scores[[2]])
write.csv(readmission_scores[[2]], "measure_importance\\readmission_importance.csv")

# medical
main <- read.csv("group_data\\medical_clean.csv") 
measures_cor <- read.csv("group_data\\correlations\\medical_cor.csv")
medical_scores <- f_group_scores(main, measures_cor)
colnames(medical_scores[[1]])[2] <- "medical_score"
View(medical_scores[[2]])
write.csv(medical_scores[[2]], "measure_importance\\medical_importance.csv")

all_groups_importance <- rbind(eff_scores[[2]], exp_scores[[2]], mortality_scores[[2]],
                              medical_scores[[2]], readmission_scores[[2]], 
                              timeliness_scores[[2]], safety_scores[[2]])
write.csv(all_groups_importance, "measure_importance\\all_groups_importance.csv")

#after making changes in Excel
all_weights <- read.csv("measure_importance\\cumulative.csv")
all_weights$rank <- 1:nrow(all_weights)
ggplot(all_weights, aes(x=rank, y=Cumulative.Weight, col=factor(Group)))+
  geom_point()+geom_text(aes(label=Group))+xlab("Rank")+ylab("Cumulative Weight")+
  theme_minimal()+ggtitle("Cumulative Weights of Measures")+
  scale_fill_discrete(name = "New Legend Title")

############  score calculation complete  ###################

# merging all group score files
medical_scores <- medical_scores[[1]]
mortality_scores <- mortality_scores[[1]]
readmission_scores <- readmission_scores[[1]]
timeliness_scores <- timeliness_scores[[1]]
safety_scores <- safety_scores[[1]]
eff_scores <- eff_scores[[1]]
exp_scores<- exp_scores[[1]]

m1 <- merge(medical_scores, mortality_scores, by="Provider.ID", all=T)
m2 <- merge(m1, safety_scores, by="Provider.ID", all=T)
m3 <- merge(m2, eff_scores, by="Provider.ID", all=T)
m4 <- merge(m3, exp_scores, by="Provider.ID", all=T)
m5 <- merge(m4, readmission_scores, by="Provider.ID", all=T)
group_scores <- merge(m5, timeliness_scores, by="Provider.ID", all=T) 

######## calculating final scores #############

# If certain group scores are missing, the weightage of the other groups
# needs to be reproportioned

weights <- c(medical = 0.04, mortality = 0.22, safety = 0.22, effectiveness = 0.04,
             experience = 0.22, readmission = 0.22, timeliness = 0.04)
  
# For example, for hospital ID 10008, medical, safety and experience scores are missing
# the remaining weights add up to 52%. Thus, the reproportioned weights will be 
# c(mortality = 0.22/0.52, effectivenss = 0.04/0.52, readmission = 0.22/0.52, 
# timeliness = 0.04/0.52)
group_scores$final_score <- 100
  
for (row in 1:nrow(group_scores)){
  if ( sum(is.na(group_scores[row, ])) > 0){
    invalid_indices = which(is.na(group_scores[row, ])) - 1
    s = sum(weights[-invalid_indices])
    reproportioned_weights <- weights[-invalid_indices]/s
    # print(weights[-invalid_indices])
    # print(reproportioned_weights)
    print(sum(weights[-invalid_indices]*reproportioned_weights))
    group_scores$final_score[row] <- sum(group_scores[row, 2:8][-invalid_indices]*reproportioned_weights)
  }
  
  else {
    group_scores$final_score[row] <- sum(group_scores[row, 2:8]*weights)
  }
  

}

final_scores <- group_scores[, c(1, ncol(group_scores))] 
summary(final_scores$final_score)
####### final scores end  ###################


# clustering
score_cluster <- kmeans(final_scores$final_score, 5, nstart = 100)
summary(score_cluster)
summary(factor(score_cluster$cluster))
final_scores$cluster_id <- score_cluster$cluster

f = final_scores %>% group_by(cluster_id) %>% 
  summarise(avg_score = mean(final_score)) %>%
  arrange(desc(avg_score))

f
# we now need to reassign the cluster ratings according to the average rating
str(f)

# swap the 1st element with 5, 2nd with 4, 3rd with 3, 4th with 2, 5th with 1
# In general, in 1:5, swap the ith element with (5-i + 1) 

# 5 - which(f$cluster_id == 3) + 1
# 5 - which(f$cluster_id == 1) + 1

for (row in 1:nrow(final_scores)){
  # swap x with 5 - which(f$cluster_id == x) + 1
  id = final_scores$cluster_id[row]
  final_scores$newcluster_id[row] = 5 - which(f$cluster_id == id) + 1
  
 
}


final_scores %>% group_by(newcluster_id) %>% 
  summarise(avg_score = mean(final_score)) %>%
  arrange(desc(avg_score))

final_scores$newcluster_id <- as.factor(final_scores$newcluster_id)
summary(final_scores$newcluster_id)

## comparing scores across star ratings ####
View(final_scores)
ggplot(final_scores, aes(x=factor(as.character(newcluster_id)), y=final_score*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("final score") + theme_minimal() + 
  scale_y_continuous(breaks=100*seq(-0.4, 0.4, 0.05))

# plotting group level scores with rating
group_scores <- merge(group_scores, final_scores, by="Provider.ID")

# mortality
plot.m <- ggplot(group_scores, aes(x=factor(as.character(newcluster_id)), y=mort_score*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("mortality score") + theme_minimal() + 
  scale_y_continuous(breaks=100*seq(-0.4, 0.4, 0.05))
plot.m

# readmission
plot.r <- ggplot(group_scores, aes(x=factor(as.character(newcluster_id)), y=readm_score*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("readmission score") + theme_minimal() + 
  scale_y_continuous(breaks=100*seq(-0.4, 0.4, 0.05))
plot.r

# safety
plot.s <- ggplot(group_scores, aes(x=factor(as.character(newcluster_id)), y=safety_score*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("safety score") + theme_minimal() + 
  scale_y_continuous(breaks=100*seq(-0.4, 0.4, 0.05))
plot.s

# effectiveness
plot.eff <- ggplot(group_scores, aes(x=factor(as.character(newcluster_id)), y=eff_score*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("effectiveness score") + theme_minimal() + 
  scale_y_continuous(breaks=100*seq(-0.4, 0.4, 0.05))
plot.eff

# experience
plot.exp <- ggplot(group_scores, aes(x=factor(as.character(newcluster_id)), y=exp_score*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("experience score") + theme_minimal() + 
  scale_y_continuous(breaks=100*seq(-0.4, 0.4, 0.05))
plot.exp

# timeliness
plot.t <- ggplot(group_scores, aes(x=factor(as.character(newcluster_id)), y=timeliness_score*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("timeliness score") + theme_minimal() + 
  scale_y_continuous(breaks=100*seq(-0.4, 0.4, 0.05))
plot.t

# medical
plot.med <- ggplot(group_scores, aes(x=factor(as.character(newcluster_id)), y=medical_score*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("medical score") + theme_minimal() + 
  scale_y_continuous(breaks=100*seq(-0.4, 0.4, 0.05))
plot.med

p.grid <- plot_grid(plot.r, plot.exp, plot.m, plot.s, 
          plot.t, plot.eff, plot.med,
          labels=c("readmission", "experience", "mortality", "safety",  "timeliness", 
                   "effectiveness", "medical"), 
          ncol = 3, nrow = 3)

p.grid


# Comparing median ratings for different groups against star ratings
median_scores <- group_scores %>% group_by(newcluster_id) %>%
  summarise(median(readm_score, na.rm=T), median(safety_score, na.rm=T),
            median(exp_score, na.rm=T), median(mort_score, na.rm=T), 
            median(eff_score, na.rm=T), median(timeliness_score, na.rm=T), 
            median(medical_score, na.rm=T))

View(median_scores)
write.csv(median_scores, "group_data\\median_group_scores.csv")


# comparing weights with correlations across groups (plot)
correl <- read.csv("measure_importance\\all_groups_importance.csv")
ggplot(correl, aes(x=Average.Correlation, y=Weight, col=Group))+geom_point(size=2)+
  facet_wrap(~Group, ncol=4)

# ######################star_rating_validation######################  
rating_file <- read.csv("Hospital General Information.csv")
ratings <- rating_file[, c(1, 13)]
ratings$Hospital.overall.rating <- factor(ratings$Hospital.overall.rating)

final_scores <- merge(final_scores, ratings, by="Provider.ID", all=T)
final_scores$newcluster_id <- as.character(final_scores$newcluster_id)
final_scores$newcluster_id[which(is.na(final_scores$newcluster_id))] <- "Not Available"
final_scores$newcluster_id <- factor(final_scores$newcluster_id)
summary(final_scores$newcluster_id)

t = table(final_scores$newcluster_id, final_scores$Hospital.overall.rating)
t
str(t)

for (i in 1:6){
  print(t[i,i] / sum(t[i, ]))
}

confusionMatrix(t)
