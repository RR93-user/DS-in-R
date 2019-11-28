library(dplyr)
library(ggplot2)
library(ggthemes)
library(cowplot)

attach(group_scores)

# analysis of provider ID = 140010; current rating is 3

str(group_scores)

# comapring provider's overall score with
plot.box <- ggplot(group_scores, aes(x=1, y=final_score.x))+geom_boxplot()

# provider's score
group_scores[which(group_scores$Provider.ID == 140010),]


plot.box + ylab("Final Score") + xlab(NULL)+geom_hline(yintercept=0.0676, col="red")+ggtitle("Provider Score")

# grouped_by_rating <- group_scores %>% group_by(newcluster_id) %>%
#   summarise(mean(final_score.x))
# 
# View(grouped_by_rating)

ggplot(group_scores, aes(x=newcluster_id, y=final_score.x))+geom_boxplot()+
  geom_hline(yintercept=0.0676, col="red")+theme_minimal()+xlab("Star Rating")+
  ylab("Final Score")+ggtitle("Final Score with Star Rating")

g <- group_scores %>% group_by(newcluster_id) %>%
  summarise(mean(final_score.x, na.rm = T), mean(mort_score, na.rm = T), 
            mean(safety_score, na.rm = T), mean(eff_score, na.rm = T), 
            mean(readm_score, na.rm = T), mean(timeliness_score, na.rm = T), 
            mean(medical_score, na.rm=T), mean(exp_score, na.rm=T))
g[, -1] <- round(g[, -1], 3)
colnames(g) <- c("Rating", "avg_final", "avg_mort", "avg_safety", "avg_effectiveness", 
                 "avg_readmission", "avg_timeliness", "avg_medical", "avg_experience")
View(g)
write.csv(g, "measure_importance\\avg_scores_by_rating.csv")

# Viewing scores of the given provider
group_scores[which(group_scores$Provider.ID == 140010),]

provider.grid <- plot_grid(plot.r + geom_hline(yintercept=6, col="red"), 
                    plot.s + geom_hline(yintercept=-4.4, col="red"),
                    plot.exp + geom_hline(yintercept=-1.5, col="red"),
                    plot.m + geom_hline(yintercept=26.9, col="red"),
                    plot.t + geom_hline(yintercept=4.2, col="red"),
                    plot.eff + geom_hline(yintercept=3.8, col="red"),
                    plot.med + geom_hline(yintercept=4.6, col="red"),
          labels=c("readmission", "safety", "experience", "mortality", "timeliness", 
                   "effectiveness", "medical"), 
          ncol = 3, nrow = 3)


provider.grid 

grid_low <- plot_grid(plot.s + geom_hline(yintercept=-4.4, col="red")+ggtitle("Safety"),
                    plot.exp + geom_hline(yintercept=-1.5, col="red")+ggtitle("Experience"))
grid_low 

grid_med <- plot_grid(plot.r + geom_hline(yintercept=6, col="red")+ggtitle("Readmission"),
                      plot.m + geom_hline(yintercept=26.9, col="red")+ggtitle("Mortality"),
                      plot.t + geom_hline(yintercept=4.2, col="red")+ggtitle("Timeliness"),
                    plot.eff + geom_hline(yintercept=3.8, col="red")+ggtitle("Effectiveness"),
                    plot.med + geom_hline(yintercept=4.6, col="red")+ggtitle("Medical"),
          ncol = 3)
grid_med
# On comparing the average scores of groups with the provider's score, we find that:
# 1. The final score is higher than the average score for rating=4 but lower than for rating=5
# 2. Mortality score is better than average for rating=5
# 3. Safety score is between the avg score for rating = 2 and 1
# 4. Effectiveness score is better than average for rating=5
# 5. Readmissions score is between avg of ratings 3 and 4
# 6. Timeliness score is better than average for rating=5
# 7. Medical score is better than average for rating=5
# 8. Experience score is between avg of ratings 2 and 3

# Overall, the rating is low because of a very low safety score (lesser than avg for rating=2), 
# a low experience score and (between the avg for ratings 2 and 3) and 
# (both are 22% weightage groups)

# lets compare the provider's score with the overall averages
mean_scores <- round(sapply(group_scores[2:9], function(x) mean(x, na.rm = T)), 2)
mean_scores
group_scores[which(group_scores$Provider.ID == 140010),][2:9]

# Can see that the safety and experience scores of the provider are lesser than the respective averages
plot.s + geom_hline(yintercept=-4.4, col="red")
plot.exp + geom_hline(yintercept=-1.5, col="red")

# let's drill down further into safety and experience scores (measure wise)
# 1. safety
# 
safety_scores <- read.csv("group_data\\safety_clean.csv") 

# safety scores of the provider
avg_safety = round(safety_scores[which(safety_scores$Provider.ID == 140010),][3:ncol(safety_scores)], 3)

# comparing with the average safety scores
provider_safety = round(sapply(safety_scores[, 3:10], function(x) mean(x, na.rm = T)), 3)

View(rbind(avg_safety, provider_safety))



# HAI_2, HAI_1, HAI_3 scores of the provider are better than avg
# But HAI_4 and HAI_5 are lower than average
# HAI_4 is Surgical Site Infection from abdominal hysterectomy (SSI: Hysterectomy)
# HAI_ 5 is Methicillin-resistant Staphylococcus Aureus (MRSA) Blood Laboratory-identified Events (Bloodstream infections)

# merging safety scores with hospital ratings
safety_scores <- merge(safety_scores, final_scores, by="Provider.ID")

plot.hai_4 <- ggplot(safety_scores, aes(x=factor(as.character(newcluster_id)), y=HAI_4_SIR*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("HAI_4 score") + theme_minimal() 
plot.hai_4

# Observation: HAI_4 score of the provider (0.00) is lower than the median scores for ratings 2, 3, 4, 5

plot.hai_5 <- ggplot(safety_scores, aes(x=factor(as.character(newcluster_id)), y=HAI_5_SIR*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("HAI_5 score") + theme_minimal() 
plot.hai_5


# let's drill down further into safety and experience scores (measure wise)
# 2. experience
# 
experience_scores <- read.csv("group_data\\experience_clean.csv") 

# safety scores of the provider
avg_experience = round(experience_scores[which(experience_scores$Provider.ID == 140010),][3:ncol(experience_scores)], 3)

# comparing with the average safety scores
provider_experience = round(sapply(experience_scores[, 3:10], function(x) mean(x, na.rm = T)), 3)

View(rbind(avg_experience, provider_experience))

# we  know that within experience, H_COMP_1_LINEAR_SCORE_mean, H_HSP_RATING_LINEAR_SCORE_mean,
# H_COMP_7_LINEAR_SCORE_mean, H_COMP_3_LINEAR_SCORE_mean, H_COMP_4_LINEAR_SCORE_mean, 
# H_COMP_5_LINEAR_SCORE_mean, H_RECMND_LINEAR_SCORE_mean are the most imp measures


# Observations:
# H_HSP_RATING_LINEAR_SCORE_mean, H_RECMND_LINEAR_SCORE_mean, H_CLEAN_LINEAR_SCORE_mean
# are worse than avg

# H_HSP_RATING_LINEAR_SCORE_mean is overall hospital rating linear mean score
# H_RECMND_LINEAR_SCORE_mean is Recommend hospital - linear mean score
# H_CLEAN_LINEAR_SCORE_mean is Cleanliness - linear mean score


# merging safety scores with hospital ratings
experience_scores <- merge(experience_scores, final_scores, by="Provider.ID")

plot.hsp <- ggplot(experience_scores, aes(x=factor(as.character(newcluster_id)), y=H_HSP_RATING_LINEAR_SCORE_mean*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("H_HSP_RATING_LINEAR_SCORE_mean score") + theme_minimal() 
plot.hsp

# Observation: HSP_RATING score of the provider (0.00) is lower than the 25th percentile of rating=4

plot.recmd <- ggplot(experience_scores, aes(x=factor(as.character(newcluster_id)), y=H_RECMND_LINEAR_SCORE_mean*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("H_RECMND_LINEAR_SCORE_mean score") + theme_minimal() 
plot.recmd
# H_RECMND_LINEAR_SCORE_mean is near the 25th percentile of rating=4

plot.clean <- ggplot(experience_scores, aes(x=factor(as.character(newcluster_id)), y=H_CLEAN_LINEAR_SCORE_mean*100)) + geom_boxplot() + 
  xlab("star rating") + ylab("H_CLEAN_LINEAR_SCORE_mean score") + theme_minimal() 
plot.clean

# H_CLEAN_LINEAR_SCORE_mean of 0 is comparable to the median cleanliness score for rating=3 

# readmission
readm_scores <- read.csv("group_data\\readmission_clean.csv") 

# readm scores of the provider
avg_readm = round(readm_scores[which(readm_scores$Provider.ID == 140010),][3:ncol(readm_scores)], 3)

# comparing with the average readm scores
provider_readm = round(sapply(readm_scores[, 3:10], function(x) mean(x, na.rm = T)), 3)

View(rbind(avg_readm, provider_readm))


# Summary of provider analysis:
## The provider has low scores in the groups safety and patient-experience 
provider.grid

# The safety score of provider is -4.4 (on the scale of this plot)
plot.s + geom_hline(yintercept=-4.4, col="red")

# The experience score is -1.5 (on this scale)
plot.exp + geom_hline(yintercept=-1.5, col="red")

# within safety, HAI_4 and HAI_5 scores are lower than the average scores of these measures
# within experience, H_HSP_RATING_LINEAR_SCORE_mean, H_RECMND_LINEAR_SCORE_mean, H_CLEAN_LINEAR_SCORE_mean
# are worse than avg

