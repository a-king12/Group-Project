# install.packages("caTools")
# install.packages("ROCR")
# install.packages("dplyr")
# install.packages("caret")
# install.packages('rpart')

library(caTools)
library(ROCR)
library(dplyr)
library(caret)
library(rpart)

contact_survey <- read.csv('contact_survey.csv')
contact_service <- read.csv('contact_service.csv')
contact_hire <- read.csv('contact_hire.csv')

str(contact_service) #linear reg
str(contact_hire) #logistic reg
str(contact_survey) #logistic reg

#turning necessary columns into factors 
contact_survey[,7:19]<-lapply(contact_survey[,7:19],factor)
contact_hire[,7:19]<-lapply(contact_hire[,7:19],factor)
contact_service[,7:18]<-lapply(contact_service[,7:18],factor)

##################  quesiton 3 #####################
set.seed(123)

trainIndex <- createDataPartition(contact_survey$complete_survey,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)

survey_train <- contact_survey[trainIndex,]
survey_valid <-contact_survey[-trainIndex,]


# Logistic regression model
survey.model <- glm(formula=complete_survey ~  Gender__c__Female+Service_Branch__c__Navy +Service_Branch__c__Marines
                      + Service_Branch__c__Coast.Guard+Service_Branch__c__Army+Service_Branch__c__Air.Force
                      +Highest_Level_of_Education_Completed__c__4.Year.Degree..BA..BS..etc..
                      +Highest_Level_of_Education_Completed__c__2.Year.Degree..AA..AS..etc..
                      + Highest_Level_of_Education_Completed__c__Doctorate..PhD..MD..etc..
                      +Highest_Level_of_Education_Completed__c__High.School.GED
                      +Highest_Level_of_Education_Completed__c__Post.Graduate.Degree..MA..MS..JD..etc..
                      +length_of_service+Disability_Rating__c +Number_of_dependents__c , family = binomial(link='logit'),data = survey_train)
summary(survey.model)

#Evaluation model performance using the validation dataset
#Predict the default probabilities based on the validation dataset
pred.probabilities <- predict(survey.model,newdata=survey_valid,type='response')

#Turn the default probabilities to binary
pred.results <- ifelse(pred.probabilities > 0.5,1,0)

# Create the confusion matrix
print("The confusion matrix is:")
print(table(pred.results, survey_valid$complete_survey))


######################### Question 5 #################
set.seed(1234)

trainIndex2 <- createDataPartition(contact_hire$confirm_hire,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)

hired_train <- contact_hire[trainIndex2,]
hired_valid <-contact_hire[-trainIndex2,]

services_model <- glm(formula=confirm_hire ~ Gender__c__Female+Service_Branch__c__Navy +Service_Branch__c__Marines
                      + Service_Branch__c__Coast.Guard+Service_Branch__c__Army+Service_Branch__c__Air.Force
                      +Highest_Level_of_Education_Completed__c__4.Year.Degree..BA..BS..etc..
                      +Highest_Level_of_Education_Completed__c__2.Year.Degree..AA..AS..etc..
                      + Highest_Level_of_Education_Completed__c__Doctorate..PhD..MD..etc..
                      +Highest_Level_of_Education_Completed__c__High.School.GED
                      +Highest_Level_of_Education_Completed__c__Post.Graduate.Degree..MA..MS..JD..etc..
                      +length_of_service+Disability_Rating__c +Number_of_dependents__c, family = binomial(link='logit'),data = hired_train)
summary(services_model)


#Evaluation model performance using the validation dataset
#Predict the default probabilities based on the validation dataset
pred.probabilities2 <- predict(services_model,newdata=contact_valid,type='response')

#Turn the default probabilities to binary
pred.results2 <- ifelse(pred.probabilities2 > 0.5,1,0)

# Create the confusion matrix
print("The confusion matrix is:")
print(table(pred.results2, contact_valid$confirm_hire))

################## Question 2 ###################
linearMod2 <- lm(days_seek_services ~Gender__c__Male + Gender__c__Female+Service_Branch__c__Navy +Service_Branch__c__Marines
                + Service_Branch__c__Coast.Guard+Service_Branch__c__Army+Service_Branch__c__Air.Force
                +Highest_Level_of_Education_Completed__c__4.Year.Degree..BA..BS..etc..
                +Highest_Level_of_Education_Completed__c__2.Year.Degree..AA..AS..etc..
                + Highest_Level_of_Education_Completed__c__Doctorate..PhD..MD..etc..
                +Highest_Level_of_Education_Completed__c__High.School.GED
                +Highest_Level_of_Education_Completed__c__Post.Graduate.Degree..MA..MS..JD..etc..
                +length_of_service+Disability_Rating__c +Number_of_dependents__c, data=contact_service) 
summary(linearMod2)
