#-------------INSTALLING & LOADING IMP PACKAGES---------#
# Function for installing and loading multiple 
install.mpkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("dplyr", "MASS", "car", "tidyr", "rccmisc")
install.mpkgs(packages)
library(dplyr)
library(MASS)
library(car)
library(tidyr)
library(rccmisc)

#-----------LOADING THE DATA--------#
CarPrice <- read.csv("CarPrice_Assignment.csv")
View(CarPrice)
class(CarPrice)
str(CarPrice)

#----------------DATA PREPARATION-------------#
#standardizing case of dataset
CarPrice<- lownames(CarPrice)
CarPrice$carname <- tolower(CarPrice$carname)

# De-duplicating rows
CarPrice<- unique(CarPrice)

#checking for NA
sum(is.na(CarPrice)) # sum is 0 i.e no NA's present
# checking for outliers
v.quantile<-data.frame(sapply(CarPrice[,-c(1:9,15,16,18)], quantile, probs= seq(0,1,0.01))) # as you can see from v.quantile there is no significant jump in quantiles by which it can be concluded outliers are less

# seperating CarName into C_Name(Company Name), Mod_name(Model Name)
CarPrice1 <- separate(CarPrice, carname,into = c("C_Name", "Mod_Name"),sep = " ")
View(CarPrice1)

## correcting spelling mistakes in C_Name (company name)
CarPrice1$C_Name <-recode(CarPrice1$C_Name, "'maxda'='mazda'; 'porcshce'='porsche';'vokswagen'='volkswagen'; 'vw'='volkswagen'; 'toyouta'='toyota'")

CarPrice1<- CarPrice1[,-4] # removing model name as we don't require it

# Converting 2 levels factors to numeric for modelling
levels(CarPrice1$fueltype) <- c(0,1) # diesel (0), gas(1)
CarPrice1$fueltype <- as.numeric(levels(CarPrice1$fueltype))[CarPrice1$fueltype]

levels(CarPrice1$aspiration) <- c(0,1) # std (0), turbo(1)
CarPrice1$aspiration <- as.numeric(levels(CarPrice1$aspiration))[CarPrice1$aspiration]

levels(CarPrice1$doornumber) <- c(0,1) # four (0), two (1)
CarPrice1$doornumber <- as.numeric(levels(CarPrice1$doornumber))[CarPrice1$doornumber]

levels(CarPrice1$enginelocation) <- c(0,1) # front (0), rear(1)
CarPrice1$enginelocation <- as.numeric(levels(CarPrice1$enginelocation))[CarPrice1$enginelocation]

# # Creating dummy variables for multilevel variables 

#1. carbody
dum_cbdy <- data.frame(model.matrix( ~carbody, data = CarPrice1))
View(dum_cbdy)
dum_cbdy <- dum_cbdy[,-1]

#2. drivewheel
dum_dwl <- data.frame(model.matrix( ~drivewheel, data = CarPrice1))
View(dum_dwl)
dum_dwl <- dum_dwl[,-1]

#3. enginetype
dum_etype <- data.frame(model.matrix( ~enginetype, data = CarPrice1))
View(dum_etype)
dum_etype <- dum_etype[,-1]

#4. cylindernumber
dum_cyln <- data.frame(model.matrix( ~cylindernumber, data = CarPrice1))
View(dum_cyln)
dum_cyln <- dum_cyln[,-1]

#5. fuelsystem
dum_fsys <- data.frame(model.matrix( ~fuelsystem, data = CarPrice1))
View(dum_fsys)
dum_fsys <- dum_fsys[,-1]

#6. Company name
dum_comp <- data.frame(model.matrix( ~C_Name, data = CarPrice1))
View(dum_comp)
dum_comp <- dum_comp[,-1]

# combing dummy variables to CarPrice2 & storing in in CarPrice2
CarPrice2 <-cbind(CarPrice1[,-c(3,7,8,15,16,18)], dum_cbdy,dum_dwl,dum_etype,dum_cyln,dum_fsys,dum_comp)
View(CarPrice2)
str(CarPrice2)

# Deriving combined mpg by adding citympg and highway mpg and taking mean
CarPrice2$cmpg <- round((CarPrice2$citympg + CarPrice2$highwaympg)/2)

#--------------Modelling------------------#

#set seed for reproducing same results
set.seed(100)

# Creating training(70%) and test data sets (30%)
trainindices= sample(1:nrow(CarPrice2), 0.7*nrow(CarPrice2))
train_set = CarPrice2[trainindices,]
test = CarPrice2[-trainindices,]

View(train_set)
str(train_set)
View(test)
str(test)

# model 1 containing all variables 
# given price as dependent variable
model1 <-lm(price~ .,data= train_set)
summary(model1)


# Variable selection using step-wise function
# using stepAIC function
step<-stepAIC(model1, direction="both")
step

# Storing output of step in model2
model2<-lm(formula = price ~ car_id + fueltype + aspiration + enginelocation + 
             carwidth + curbweight + enginesize + boreratio + stroke + 
             compressionratio + horsepower + peakrpm + citympg + highwaympg + 
             carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
             drivewheelrwd + enginetypel + enginetypeohc + enginetypeohcf + 
             enginetyperotor + cylindernumberfive + cylindernumberthree + 
             fuelsystem2bbl + fuelsystemmpfi + C_Namebmw + C_Namedodge + 
             C_Namehonda + C_Nameisuzu + C_Namemazda + C_Namemercury + 
             C_Namemitsubishi + C_Namenissan + C_Nameplymouth + C_Namerenault + 
             C_Namesaab + C_Nametoyota + C_Namevolkswagen + C_Namevolvo + 
             cmpg, data = train_set)

summary(model2)
vif(model2) #checking multicollinearity (VIF>2 to be dropped if statistically insignificant)

## checking multicollinearity 
#Dropping cmpg,citympg,fueltype,compressionratio,car_id,carbodyhatchback,carbodysedan
#fuelsystemmpfi,enginetypeohc,carbodywagon
model3<-lm(formula = price ~ aspiration + enginelocation + 
             carwidth + curbweight + enginesize + boreratio + stroke + horsepower + peakrpm + 
             carbodyhardtop + drivewheelrwd + enginetypel + enginetypeohcf + 
             enginetyperotor + cylindernumberfive + cylindernumberthree + 
             fuelsystem2bbl + C_Namebmw + C_Namedodge + 
             C_Namehonda + C_Nameisuzu + C_Namemazda + C_Namemercury + 
             C_Namemitsubishi + C_Namenissan + C_Nameplymouth + C_Namerenault + 
             C_Namesaab + C_Nametoyota + C_Namevolkswagen + C_Namevolvo, data = train_set)

summary(model3)
vif(model3) #checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
#Dropping drivewheelrwd,boreratio,carbodyhardtop

model4<- lm(formula = price ~ aspiration + enginelocation + carwidth + curbweight 
            + enginesize + stroke + horsepower + peakrpm + enginetypel
            + enginetypeohcf +  enginetyperotor + cylindernumberfive + cylindernumberthree 
            + fuelsystem2bbl + C_Namebmw + C_Namedodge + C_Namehonda + C_Nameisuzu 
            + C_Namemazda+ C_Namemercury + C_Namemitsubishi + C_Namenissan 
            + C_Nameplymouth + C_Namerenault + C_Namesaab + C_Nametoyota + C_Namevolkswagen 
            + C_Namevolvo, data = train_set)
summary(model4)
vif(model4) #checking multicollinearity (VIF>2 to be dropped if statistically insignificant)

# Notice that the variables enginesize and curbweight still
# have the highest VIFs with very high significance
# checking corelation b/w enginesize and curbweight still
cor(train_set$enginesize,train_set$curbweight) #corelation is ~85%

#removing lower significant variable among the two

#Dropping curbweight

model5<-lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + stroke 
           + horsepower + peakrpm + enginetypel + enginetypeohcf +  enginetyperotor 
           + cylindernumberfive + cylindernumberthree + fuelsystem2bbl + C_Namebmw 
           + C_Namedodge + C_Namehonda + C_Nameisuzu + C_Namemazda+ C_Namemercury 
           + C_Namemitsubishi + C_Namenissan + C_Nameplymouth + C_Namerenault + C_Namesaab
           + C_Nametoyota + C_Namevolkswagen + C_Namevolvo, data = train_set)
summary(model5)
vif(model5) #checking multicollinearity (VIF>2 to be dropped if statistically insignificant)

#Dropping horsepower, cylindernumberfive, fuelsystem2bbl
model6<-lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + stroke 
           + peakrpm + enginetypel + enginetypeohcf +  enginetyperotor 
           + cylindernumberthree + C_Namebmw 
           + C_Namedodge + C_Namehonda + C_Nameisuzu + C_Namemazda+ C_Namemercury 
           + C_Namemitsubishi + C_Namenissan + C_Nameplymouth + C_Namerenault + C_Namesaab
           + C_Nametoyota + C_Namevolkswagen + C_Namevolvo, data = train_set)
summary(model6)
vif(model6) #checking multicollinearity (VIF>2 to be dropped if statistically insignificant)

#Checking corelation b/w carwidth & enginesize
cor(train_set$carwidth,train_set$enginesize)

#dropping carwidth, peakrpm

model7<-lm(formula = price ~ aspiration + enginelocation + enginesize + stroke 
           +  enginetypel + enginetypeohcf +  enginetyperotor 
           + cylindernumberthree + C_Namebmw 
           + C_Namedodge + C_Namehonda + C_Nameisuzu + C_Namemazda+ C_Namemercury 
           + C_Namemitsubishi + C_Namenissan + C_Nameplymouth + C_Namerenault + C_Namesaab
           + C_Nametoyota + C_Namevolkswagen + C_Namevolvo, data = train_set)
summary(model7) 
vif(model7) #checking multicollinearity (VIF>2 to be dropped if statistically insignificant)

model8<- lm(formula = price ~ aspiration + enginelocation + enginesize  
            +  enginetypel + enginetypeohcf +  enginetyperotor 
            + cylindernumberthree + C_Namebmw 
            + C_Namedodge + C_Namehonda + C_Nameisuzu + C_Namemazda+ C_Namemercury 
            + C_Namemitsubishi + C_Namenissan + C_Nameplymouth + C_Namerenault + C_Namesaab
            + C_Nametoyota + C_Namevolkswagen + C_Namevolvo, data = train_set)
summary(model8)
vif(model8) #checking multicollinearity (VIF>2 to be dropped if statistically insignificant)

#Dropping cylindernumberthree, C_Namesaab, C_Namebmw, C_Namemercury, C_Namevolvo
model9<-lm(formula = price ~ aspiration + enginelocation + enginesize  
           +  enginetypel + enginetypeohcf +  enginetyperotor + C_Namedodge + C_Namehonda
           + C_Nameisuzu + C_Namemazda+ C_Namemitsubishi + C_Namenissan + C_Nameplymouth 
           + C_Namerenault + C_Nametoyota + C_Namevolkswagen, data = train_set)
summary(model9)
vif(model9) #checking multicollinearity (VIF>2 to be dropped if statistically insignificant)


#As you can see  model9 has only significant parameters apart from company name 
#So we can say model9 is our final model & contains all the variablessignificant in predicting price of cars
# We can deduce from our model that pricces are more brand specific then actual attributes of the car
#Engine type, location size play a role in pricing of car 
#------------------Extra------------------#
# predicting the results in test dataset
Predict_1 <- predict(model9,test[-10])
test$test_price <- Predict_1

#Lets test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- (cor(test$price,test$test_price))^2
rsquared
