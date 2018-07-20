## Import Libraries
library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)
library(modeest)

## Load the dataset and explore
dis_pred <- read.csv("C:\\Users\\s1112\\Desktop\\Hackathon 2018\\Diseases_Pred1.csv", header = TRUE)

head(dis_pred)

summary(dis_pred)

str(dis_pred)

## Check the number of missing values in the dataset for different variables
colSums(is.na(disease))

## Check the mean and median value of age, bp, sg and hemo variable
mean(dis_pred$age, na.rm = T)
median(dis_pred$age, na.rm = T)

mean(dis_pred$bp, na.rm = T)
median(dis_pred$bp, na.rm = T)

mean(dis_pred$sg, na.rm = T)
median(dis_pred$sg, na.rm = T)

mean(dis_pred$hemo, na.rm = T)
median(dis_pred$hemo, na.rm = T)

## Impute the median value for missing age, bp, sg, al and hemo variable values
dis_pred$age[is.na(dis_pred$age)] <- median(dis_pred$age, na.rm = T)
dis_pred$bp[is.na(dis_pred$bp)] <- median(dis_pred$bp, na.rm = T)
dis_pred$sg[is.na(dis_pred$sg)] <- median(dis_pred$sg, na.rm = T)
dis_pred$hemo[is.na(dis_pred$hemo)] <- median(dis_pred$hemo, na.rm = T)

## Convert al variable to factor (only 4 levels)
dis_pred$al <- as.factor(dis_pred$al)

## Find the MODE value for al variable and Impute it for missing values
mode <- mlv(dis_pred$al, method = "mfv", na.rm = T)[1]
dis_pred$al[is.na(dis_pred$al)] <- mode

## Drop the rbc variable from the datset
dis_pred1 <- dis_pred[,-5]

## Check the new dataset
str(dis_pred1)

## Divide the train and test dataset 
## Training set : Test set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(dis_pred1), 0.7*nrow(dis_pred1), replace = F)
Trainset <- dis_pred1[train, ]
Testset <- dis_pred1[-train, ]

Trainset <- Trainset[,-1]

## Create a Random Forest Model 
rf_model <- randomForest(Diseases ~ ., data = Trainset, ntree = 10, importance = TRUE)

## Predict on Train dataset
predTrain <- predict(rf_model, Trainset, type = "class")

## Check accuracy on Train dataset
table(predTrain, Trainset$Diseases)
mean(predTrain == Trainset$Diseases) ## Accuracy - approx 100%

## Prediction on Test dataset
predTest <- predict(rf_model, Testset, type = "class")

## Check accuracy on Test dataset
mean(predTest == Testset$Diseases) ## Accuracy - approx 97%
table(predTest, Testset$Diseases)
#dotplot(factor(predTest) ~ Testset$id)
#axis(side = 1, at = Testset$id, las = 2)

## Check the important variables
varImp(rf_model)

## Model1 variable Importance Plot
 # Get importance
 importance    <- importance(rf_model)
 varImportance <- data.frame(Variables = row.names(importance), 
                             Importance = round(importance[ ,'MeanDecreaseGini'],2))

 # Create a rank variable based on importance
 rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

 # Use ggplot2 to visualize the relative importance of variables
 ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                            y = Importance, fill = Importance)) +
   geom_bar(stat='identity') + 
   geom_text(aes(x = Variables, y = 0.5, label = Rank),
             hjust=0, vjust=0.55, size = 4, colour = 'red') +
   labs(x = 'Variables') +
   coord_flip() + 
   theme_bw()
 
 
## Compare the RF model with Decision Tree
library(rpart)
model_dt = train(Diseases ~ ., data = Trainset, method = "rpart")

## Predict on train dataset and check accuracy
model_dt_1 = predict(model_dt, data = Trainset)
table(model_dt_1, Trainset$Diseases)
mean(model_dt_1 == Trainset$Diseases) ## Accuracy - approx 72%

## Predict on test dataset and check accuracy
pred_dt <- predict(model_dt, Testset)
table(pred_dt, Testset$Diseases)
mean(pred_dt == Testset$Diseases) ## Accuracy - approx 73%
