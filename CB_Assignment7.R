# Project: Assignment 7, Rigor and Reproducibility 
# Name: Catharine Bowman
# Due Date: May 20, 2024

# Question Prompt: The analysis should include at least one table describing the variables, 
# a regression-based approach (e.g., lm(), glm(), randomForest(), etc), at least one figure, 
# and a brief summary of your findings in the README. Include a link to your repository in the text box below.

### File Read In: 
RR_Assignment7 <- read.csv(file="./raw-data/cohort.csv", header=TRUE, stringsAsFactors = FALSE)
RR_Assignment7_copy<-RR_Assignment7

### Packages ### 
library(table1)
library(arsenal)
library(tidyverse)
library(dplyr)
library(rmeta)

### Study Question: Are higher annual out-of-pocket costs of care associated with adverse cardiac outcomes?
## Assumptions: 
# I am assuming the 'care' variable represents annual cost of out-of-pocket care and 'cardiac' represents an adverse cardiac event
# I am assuming that annual cost of out-of-pocket care represents a time period prior to the beginning of outcome measurement 
# I recognize several confounding factors are not accounted for in this analysis 

### Explore Individual Variables ###   
summary(RR_Assignment7$age,addNA=TRUE) #Ages 18-65 yrs 
summary(RR_Assignment7$cost,addNA=TRUE) #Costs $8478-$11326 
summary(freqlist(~smoke,data=RR_Assignment7),addNA=TRUE) 
summary(freqlist(~female,data=RR_Assignment7),addNA=TRUE) 
summary(freqlist(~cardiac,data=RR_Assignment7),addNA=TRUE) 

### Creating Descriptive Table ### 
# Re-Label Variables for Table Presentation 
RR_Assignment7_copy$CardiacEvent[RR_Assignment7_copy$cardiac==0]<-"No Cardiac Outcome"
RR_Assignment7_copy$CardiacEvent[RR_Assignment7_copy$cardiac==1]<-"Cardiac Outcome"

#Assessing Distribution Visually
hist(RR_Assignment7_copy$cost) #Normal
hist(RR_Assignment7_copy$age) #Uniform

#Integrating Factors into Categorical Variables
RR_Assignment7_copy$Sex <- factor(RR_Assignment7_copy$female, levels=c(0,1),
         labels=c("Male", 
                  "Female"))

RR_Assignment7_copy$Smoking <- factor(RR_Assignment7_copy$smoke, levels=c(0,1),
                                  labels=c("Non-Smoker", 
                                           "Smoker"))

RR_Assignment7_copy$CardiacEvent <- factor(RR_Assignment7_copy$cardiac, levels=c(0,1),
                                      labels=c("No Cardiac Outcome", 
                                               "Cardiac Outcome"))

RR_Assignment7_copy$Age <- RR_Assignment7_copy$age

# Transforming Cost into Categories
RR_Assignment7_copy$CostCategories[RR_Assignment7_copy$cost>0 & RR_Assignment7_copy$cost<=9800]<-"<$9800"
RR_Assignment7_copy$CostCategories[RR_Assignment7_copy$cost>9800 & RR_Assignment7_copy$cost<=9950]<-"$9801-$9950"
RR_Assignment7_copy$CostCategories[RR_Assignment7_copy$cost>9950]<-">$9950"

# Converting to Factor
RR_Assignment7_copy$CostCategories <- factor(RR_Assignment7_copy$CostCategories)

# Generating Table
table1(~ CostCategories + Smoking + Sex + Age | CardiacEvent, data=RR_Assignment7_copy)

### Using Logistic Regression to Examine Association Between Cost of Care and Cardiac Event ###
# Crude
Unadjustedmodel_A7<-glm(CardiacEvent~relevel(CostCategories, "<$9800"), data=RR_Assignment7_copy, family=binomial)
exp(cbind(OR=coef(Unadjustedmodel_A7),confint(Unadjustedmodel_A7)))

# Adjusted for Age, Smoking Status, and Sex
AdjustedModel_A7<-glm(CardiacEvent~relevel(CostCategories, "<$9800")+relevel(Smoking, "Non-Smoker")+age+relevel(Sex, "Male"), data=RR_Assignment7_copy, family=binomial)
exp(cbind(OR=coef(AdjustedModel_A7),confint(AdjustedModel_A7)))

### Creating a Figure ###
#Storing ORs and CIs together for multivariable analysis 
visual <- data.frame(
  ID = c(1,2),
  label=c("$9801-$9950", ">$9950"),
  ORs=c(3.5656701,10.4567673),
  LowerBound = c(2.01386401, 6.18683784),
  UpperBound = c(6.1879656, 17.8745406),
  Confidence_Interval=c("2.01386401,6.18683784","6.1879656, 17.8745406"))

#Generating Forest Plot
forestplot <- ggplot(data=visual, aes(x=label, y=ORs, ymin=LowerBound, ymax=UpperBound)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() + 
  xlab("Annual Out-of-Pocket Care Expenditure (CAD)") + ylab("Adjusted OR (95% CI)") +
  theme_bw()+
  theme(axis.text=element_text(size=12,colour="black"),
         axis.title=element_text(size=12,,colour="black",face="bold"))+
  ggtitle("Adjusted Odds Ratios Comparing Odds of Adverse Cardiac Event by Annual Out-of-Pocket Care Expenditures")
print(forestplot)

#Method Learned from Dayimu A. (2024), "Introduction to forestploter", cran.r-project



