library(readxl)
Data <- read_excel("Desktop/Second Sem/Data Mining/Assignment 4/UV6696-XLS-ENG.xlsx", sheet = "Case Data")
View(Data) 

library(funModeling)
df_status(Data)
#No null values
#Converting data type of Churn to factor
Data$`Churn (1 = Yes, 0 = No)` <- as.factor(Data$`Churn (1 = Yes, 0 = No)`)

#Univariate analysis

#Churn
pie(table(Data$`Churn (1 = Yes, 0 = No)`), main="Distribution of Churn variable")
#Age
library(psych)
describe(Data$`Customer Age (in months)`)
summary(Data$`Customer Age (in months)`)

#Bivariate Analysis 
boxplot(Data$`Customer Age (in months)`~Data$`Churn (1 = Yes, 0 = No)`, col= c("coral", "steelblue"), 
        main= "Customer Age v/s Churn", xlab= "Churn", ylab= "Customer Age")
#Hypothesis in the case: Customers with lower age churn out more than cuatomers with a high age
#From the boxplot: The median age of customer churning is slightly greater than the median age of customer staying
#The range and spread of the customer age for customers staying is more spread out
#Above the customer age 40; cutomers are more likely to stay than churn 
#Else the difference in churn with respect to age is not much significant 

#Checking the same after converting customer age into factors: 
#Categories of 'less than 6', '6-14', 'more than 14' 

Data$agecat <- rep(0, nrow(Data))
Data$agecat[Data$`Customer Age (in months)`<=6] <- 1
Data$agecat[Data$`Customer Age (in months)`> 6 & Data$`Customer Age (in months)` <= 14] <- 2
Data$agecat[Data$`Customer Age (in months)`> 14] <- 3

#Univariate of the new variable:
table(Data$agecat)

#Bivariate of the new variable with target variable: 
t1 <- xtabs(~Data$agecat+Data$`Churn (1 = Yes, 0 = No)`)
prop.table(t1)*100

#Hypothesis and observation comparison:
#Hypothesis: Customers with an age of more than 14 months are less likely to leave than 6-14
#Observation: 40% (2.07/5) population churning out belongs to this category 
#Hypothesis: Customers with an age between 6 to 14 months are most likely to leave
#Observation: 50% (2.31/5) population churning out belongs to this category 
#Customers with less than 6 month age are the least likely to leave: 14% (0.7/5)



#-----------------------------------------------------------------------------------------

#Question 2


#Statistical testing before model building
options(scipen=99)
chisq.test(Data$agecat, Data$`Churn (1 = Yes, 0 = No)`)   #p-value = 0.000000000000007198
t.test(Data$`Customer Age (in months)`~Data$`Churn (1 = Yes, 0 = No)`)  #p-value = 0.003057
#Choosing customer agecat over Customer Age 

#Significant 
t.test(Data$`CHI Score Month 0`~Data$`Churn (1 = Yes, 0 = No)`) #p-value = 0.0000000000002097
t.test(Data$`CHI Score 0-1`~Data$`Churn (1 = Yes, 0 = No)`)  #p-value = 0.00000001571
t.test(Data$`Support Cases Month 0`~Data$`Churn (1 = Yes, 0 = No)`)  #p-value = 0.00000006281
t.test(Data$`SP Month 0`~Data$`Churn (1 = Yes, 0 = No)`)  #p-value = 0.0000004381
t.test(Data$`Logins 0-1`~Data$`Churn (1 = Yes, 0 = No)`)   #p-value = 0.0004037
t.test(Data$`Days Since Last Login 0-1`~Data$`Churn (1 = Yes, 0 = No)`)  #p-value = 0.00005215
t.test(Data$`Blog Articles 0-1`~Data$`Churn (1 = Yes, 0 = No)`) #p-value = 0.01158
#Insignificant
t.test(Data$`Support Cases 0-1`~Data$`Churn (1 = Yes, 0 = No)`)  #p-value = 0.5278
t.test(Data$`SP 0-1`~Data$`Churn (1 = Yes, 0 = No)`)   #p-value = 0.5218
t.test(Data$`Views 0-1`~Data$`Churn (1 = Yes, 0 = No)`) #p-value = 0.05631

#Selecting significant variables for model: 
Data <- subset(Data, select= -c(`Customer Age (in months)`))
ModelData <- subset(Data, select= -c(`Support Cases 0-1`, `SP 0-1`, `Views 0-1`))



#Logistic regresion model 1: 
mod <- glm(ModelData$`Churn (1 = Yes, 0 = No)`~., data = ModelData, family = 'binomial')
summary(mod)

#672 observation
a <- ModelData[672, ]
a1 <- predict(mod, newdata = a, type = "response")
a2 <- ifelse(a1>=0.5, "1", "0")      
a2                                     #Predicted: 0
a$`Churn (1 = Yes, 0 = No)`          #Target: 0

#354 and 5203 observation
b <- ModelData[c(354, 5203), ]
b1 <- predict(mod, newdata = b, type = "response")
b2 <- ifelse(b1>=0.5, "1", "0")      
b2                                   #Predicted: 0, 0
b$`Churn (1 = Yes, 0 = No)`          #Target: 0, 0

p <- predict(mod, data = Data, type = "response")
p1 <- ifelse(p>=0.5, "1", "0")   
p1<- as.factor(p1)
data.frame(ModelData$`Churn (1 = Yes, 0 = No)`, p1)


Target <- ModelData$`Churn (1 = Yes, 0 = No)`
Target <- as.factor(Target)
confusionMatrix(p1,Target)

#Therefore, the technique performed is incorrect



#Logistic Regression Final Model:

# Install package
install.packages("ROCR")
# Computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
library(ROCR)
# Calculating the values for ROC curve
pred = prediction( p, ModelData$`Churn (1 = Yes, 0 = No)`)
perf = performance(pred,"tpr","fpr")
# Plotting the ROC curve
plot(perf, col = 'black', lty = 3, lwd = 3)


opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)}
print(opt.cut(perf, pred))
cutoff <- 0.05354738

mod2 <- glm(ModelData$`Churn (1 = Yes, 0 = No)`~., data = ModelData, family = 'binomial')
summary(mod2)
p <- predict(mod2, data= ModelData, type= "response")
predicted_class <- ifelse(p>=0.05354738, "1", "0") 
sum(predicted_class!=ModelData$`Churn (1 = Yes, 0 = No)`)

xtabs(~ModelData$`Churn (1 = Yes, 0 = No)`+predicted_class)

predicted_class <- as.factor(predicted_class)
library(caret)
confusionMatrix(predicted_class, ModelData$`Churn (1 = Yes, 0 = No)`)


#672 observation
a <- ModelData[672, ]
a1 <- predict(mod, newdata = a, type = "response")
anew2 <- ifelse(a1>=0.05354738, "1", "0")      
anew2                                     #Predicted: 1
a$`Churn (1 = Yes, 0 = No)`          #Target: 0

#354 and 5203 observation
b <- ModelData[c(354, 5203), ]
b1 <- predict(mod, newdata = b, type = "response")
bnew2 <- ifelse(b1>=0.05354738, "1", "0")      
bnew2                                   #Predicted: 0, 0
b$`Churn (1 = Yes, 0 = No)`          #Target: 0, 0




#Question 3
ModelData <- cbind(ModelData, p)
sort <- ModelData[with(ModelData, order(-p)), ]
sorted <- sort[1:100, ]
sortednew <- subset(sorted, select= -p)
sortednew$`CHI Score Month 0`
library(randomForest)
fit = randomForest(sortednew$`Churn (1 = Yes, 0 = No)`~ ., data=sortednew, 
                   importance=TRUE, proximity=TRUE)
library(rpart.plot)
t <- rpart(`Churn (1 = Yes, 0 = No)`~ ., data= sortednew, minsplit= 0)
t
rpart.plot(t)
#CHI Score 0-1< -0.5
#CHI Score Month 0< 30
#Logins 0-1>=1
#Days Since Last Login 0-1>=30.5








