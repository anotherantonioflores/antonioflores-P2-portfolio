###############################

#required libraries
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(earth)
library(tidyr)
library(dplyr)
library(caret)
library(knitr)
library(randomForest)
library(Metrics)
library(pROC)


#loading data. 
data_location <- here::here("data","processed-data","processeddata4.rds")
mydata <- readRDS(data_location)
summary(mydata)

#Basic Statistical Analysis
#Examining a logistic regression test with all variables of interest

logitFull = glm(y_termSubscribed~., data = mydata, family = "binomial")
summary(logitFull)

lrtable <- broom::tidy(logitFull)
fullmodel = here("results", "tables", "fullmodel.rds")
saveRDS(lrtable, file = fullmodel)

#All Predictors are Significant

#Full Data Analysis


######################################
#Machine Learning Models Part 1
######################################
#Using Numeric Unfactored Variables
############################


#Preprocessing 2 True Numeric Variables
set.seed(24)
mydataPP = preProcess(mydata[7:8], method = c("scale", "center", "BoxCox"))
mydata[7:8] = predict(mydataPP, mydata[7:8])


#Splitting the data. 80% for training, 20% test.

set.seed(21)
split1 = sample(c(rep(0, 0.8 * nrow(mydata)), rep(1, 0.2*nrow(mydata))))


train_data = mydata[split1 == 0, ]
test_data = mydata[split1 == 1, ]

train_y = train_data$y_termSubscribed
test_y = test_data$y_termSubscribed


train_x = train_data %>% 
  select(!y_termSubscribed)

test_x = test_data %>% 
  select(!y_termSubscribed)



#Setting Tran Control
ctrl <- trainControl(method = "cv")

#################
#Modeling Section

# MARS model
set.seed(21)
marsmodel = train(y_termSubscribed~., data=train_data, 
                  method = "earth",
                  tuneGrid = expand.grid(degree=1, nprune=2:15),
                  trControl = ctrl)

marsmodel


plot(marsmodel)

marsImp = varImp(marsmodel, scale = FALSE)
plot(marsImp)


file_path <- here("results", "figures","importantM1.png")
png(filename = file_path, width = 800, height = 600)
plot(marsImp)
dev.off()




### KNN 

set.seed(21)


knnModel = train(y_termSubscribed~., data=train_data, 
                 method = "knn",
                 tuneGrid = data.frame(k=1:20),
                 trControl= ctrl)


knnModel

plot(knnModel)



KNNImp = varImp(knnModel, scale = FALSE)
plot(KNNImp)


file_path <- here("results", "figures","importantKNN1.png")
png(filename = file_path, width = 800, height = 600)
plot(KNNImp)
dev.off()

#Logistic Regression Model

logregmodel = glm(y_termSubscribed~., data=train_data, family = "binomial")


LOGimp = varImp(logregmodel, scale = FALSE)

LOGimp$Names = row.names(LOGimp)

LOGimp %>% 
  top_n(10, Overall) %>% 
  ggplot()+
  geom_bar(mapping= aes(x = reorder(Names, Overall), y = Overall), stat = "identity")+ 
  coord_flip()

ggsave(here("results", "figures","importantLog1.png"))


############Random Forest

set.seed(222)

rfmodel = randomForest(y_termSubscribed~., data = train_data,
                importance = TRUE,
                preProcess = "pca",
                trControl = ctrl)

rfmodel


rfIMP = varImp(rfmodel)

rfIMP$Names = row.names(rfIMP)

rfIMP %>% 
  top_n(10, `1`) %>% 
  ggplot()+
  geom_bar(mapping= aes(x = reorder(Names, `1`), y = `1`), stat = "identity")+ 
  coord_flip()


ggsave(here("results", "figures","importantRF1.png"))

###############RESULTS


testResults = data.frame(obs=test_y)
testResults$MARS = predict(marsmodel, test_x)
testResults$KNN = predict(knnModel, test_x)
logpred = predict(logregmodel, test_x, type = "response")
logpred1 = ifelse(logpred>0.5, 1, 0)
testResults$Logreg = logpred1
testResults$RF = predict(rfmodel, test_x)


head(testResults, n=10)



MARSCM = confusionMatrix(testResults$MARS, testResults$obs, mode = 'everything', positive = "1")
KNNCM = confusionMatrix(testResults$KNN, testResults$obs, mode = 'everything',  positive = "1")
LogRegCM = confusionMatrix(as.factor(testResults$Logreg), testResults$obs, mode = 'everything')
RFCM =confusionMatrix(testResults$RF, testResults$obs, mode = 'everything',  positive = "1")

cleanMARSCM = tidy(MARSCM)
cleanKNNCM = tidy(KNNCM)
cleanLogRegCM = tidy(LogRegCM)
cleanRFCM = tidy(RFCM)

Term = cleanKNNCM$term
MARS = round(cleanMARSCM$estimate, 3)
KNN = round(cleanKNNCM$estimate, 3)
LogReg = round(cleanLogRegCM$estimate, 3)
RF = round(cleanRFCM$estimate, 3)

finalresults = data.frame(
  cbind(Term, MARS, KNN, LogReg, RF))


finalresults = finalresults %>% 
  filter(Term != "mcnemar" & Term != "pos_pred_value" & Term != "neg_pred_value"
         & Term != "prevalence" & Term != "detection_rate" & Term != "detection_prevalence"
         & Term != "balanced_accuracy")


table_file1 = here("results", "tables", "finaltable1.rds")
saveRDS(finalresults, file = table_file1)



testResults$MARS = as.numeric(testResults$MARS)
testResults$KNN = as.numeric(testResults$KNN)
testResults$Logreg = as.numeric(testResults$Logreg)
testResults$RF = as.numeric(testResults$RF)


MARSROC = roc(testResults$obs, testResults$MARS)
KNNROC = roc(testResults$obs, testResults$KNN)
LOGROC = roc(testResults$obs, testResults$Logreg)
RFROC = roc(testResults$RF, testResults$RF)


file_path <- here("results", "figures","ROC1.png")
png(filename = file_path, width = 800, height = 600)


plot(MARSROC, col= 2, lty = 1)
lines(KNNROC, col=2, lty=2)
lines(LOGROC, col=3, lty=3)
lines(RFROC, col=4, lty=4)
legend('bottomright', c('MARS','KNN','LOG', 'RF'), col=1:4, lty=1:4,lwd=2)
dev.off()




######################################
#Machine Learning Models Part 2
######################################
#Using Numeric FACTORED Variables
############################

data_location <- here::here("data","processed-data","processeddata3.rds")

#loading data. 
mydata2 <- readRDS(data_location)
summary(mydata2)


set.seed(24)
mydata2PP = preProcess(mydata2[7:8], method = c("scale", "center", "BoxCox"))
mydata2[7:8] = predict(mydata2PP, mydata2[7:8])


#Splitting the data. 80% for training, 20% test.

set.seed(21)
split2 = sample(c(rep(0, 0.8 * nrow(mydata2)), rep(1, 0.2*nrow(mydata2))))


train_data2 = mydata2[split2 == 0, ]
test_data2 = mydata2[split2 == 1, ]

train_y2 = train_data2$y_termSubscribed
test_y2 = test_data2$y_termSubscribed


train_x2 = train_data2 %>% 
  select(!y_termSubscribed)

test_x2 = test_data2 %>% 
  select(!y_termSubscribed)


#################
#Modeling Section

# MARS model
set.seed(21)
marsmodel2 = train(y_termSubscribed~., data=train_data2, 
                  method = "earth",
                  tuneGrid = expand.grid(degree=1, nprune=2:15),
                  trControl = ctrl)

marsmodel2

plot(marsmodel2)

marsImp2 = varImp(marsmodel2, scale = FALSE)
plot(marsImp2)

file_path <- here("results", "figures","importantM2.png")
png(filename = file_path, width = 800, height = 600)
plot(marsImp2)
dev.off()


### KNN 
set.seed(21)


knnModel2 = train(y_termSubscribed~., data=train_data2, 
                 method = "knn",
                 tuneGrid = data.frame(k=1:20),
                 trControl= ctrl)


knnModel2
plot(knnModel2)


KNNImp2 = varImp(knnModel2, scale = FALSE)
plot(KNNImp2)

file_path <- here("results", "figures","importantKNN2.png")
png(filename = file_path, width = 800, height = 600)
plot(KNNImp2)
dev.off()


#Logistic Regression Model

logregmodel2 = glm(y_termSubscribed~., data=train_data2, family = "binomial")

summary(logregmodel2)


LOGimp2 = varImp(logregmodel2, scale = FALSE)

LOGimp2$Names = row.names(LOGimp2)

LOGimp2 %>% 
  top_n(10, Overall) %>% 
  ggplot()+
  geom_bar(mapping= aes(x = reorder(Names, Overall), y = Overall), stat = "identity")+ 
  coord_flip()


ggsave(here("results", "figures","importantLog2.png"))



# Random Forest

rfmodel2 = randomForest(y_termSubscribed~., data = train_data2,
                       importance = TRUE,
                       preProcess = "pca",
                       trControl = ctrl)

rfmodel2


rfIMP2 = varImp(rfmodel2)

rfIMP2$Names = row.names(rfIMP2)

rfIMP2 %>% 
  top_n(10, `1`) %>% 
  ggplot()+
  geom_bar(mapping= aes(x = reorder(Names, `1`), y = `1`), stat = "identity")+ 
  coord_flip()


ggsave(here("results", "figures","importantRF2.png"))



###### RESULTS

## Predictions
testResults1 = data.frame(obs=test_y2)
testResults1$MARS = predict(marsmodel2, test_x2)
testResults1$KNN = predict(knnModel2, test_x2)
logpred3 = predict(logregmodel2, test_x2, type = "response")
logpred2 = ifelse(logpred3>0.5, 1, 0)
testResults1$Logreg = logpred2
testResults1$RF = predict(rfmodel2, test_x2)

head(testResults1, n=10)

##Confusion Matrix
MARSCM1 = confusionMatrix(testResults1$MARS, testResults1$obs, mode = 'everything', positive = "1")
KNNCM1 = confusionMatrix(testResults1$KNN, testResults1$obs, mode = 'everything',  positive = "1")
LogRegCM1 = confusionMatrix(as.factor(testResults1$Logreg), testResults1$obs, mode = 'everything',  positive = "1")
RFCM1 =confusionMatrix(testResults1$RF, testResults1$obs, mode = 'everything',  positive = "1")

cleanMARSCM1 = tidy(MARSCM1)
cleanKNNCM1 = tidy(KNNCM1)
cleanLogRegCM1 = tidy(LogRegCM1)
cleanRFCM1 = tidy(RFCM1)

Term1 = cleanKNNCM1$term
MARS1 = round(cleanMARSCM1$estimate, 3)
KNN1 = round(cleanKNNCM1$estimate, 3)
LogReg1 = round(cleanLogRegCM1$estimate, 3)

RF1 = round(cleanRFCM1$estimate, 3)

finalresults1 = data.frame(
  cbind(Term1, MARS1, KNN1, LogReg1, RF1))

finalresults1 = finalresults1 %>% 
  filter(Term1 != "mcnemar" & Term1 != "pos_pred_value" & Term1 != "neg_pred_value"
         & Term1 != "prevalence" & Term1 != "detection_rate" & Term1 != "detection_prevalence"
         & Term1 != "balanced_accuracy")


table_file2 = here("results", "tables", "finaltable2.rds")
saveRDS(finalresults1, file = table_file2)


## ROC Curve
testResults1$MARS = as.numeric(testResults1$MARS)
testResults1$KNN = as.numeric(testResults1$KNN)
testResults1$Logreg = as.numeric(testResults1$Logreg)
testResults1$RF = as.numeric(testResults1$RF)
MARSROC1 = roc(testResults1$obs, testResults1$MARS)
KNNROC1 = roc(testResults1$obs, testResults1$KNN)
LOGROC1 = roc(testResults1$obs, testResults1$Logreg)
RFROC1 = roc(testResults1$RF, testResults1$RF)


plot(MARSROC1, col= 2, lty = 1)
lines(KNNROC1, col=2, lty=2)
lines(LOGROC1, col=3, lty=3)
lines(RFROC1, col=4, lty=4)
legend('bottomright', c('MARS','KNN','LOG', 'RF'), col=1:4, lty=1:4,lwd=2)

