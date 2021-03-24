setwd("/Users/hussainabuwala/Desktop/dataAnalytics-asgn2")
getwd()


# clean up the environment before starting
rm(list = ls())
install.packages("tree")
library(tree)
install.packages("e1071")
library(e1071)
install.packages(("ROCR"))
library(ROCR)
install.packages("randomForest")
library(randomForest)
install.packages("adabag")
library(adabag)
install.packages("rpart")
library(rpart)


install.packages("neuralnet")
install.packages("car")
library(neuralnet)
library(car)

options(digits=4)

# reading file
GCD <- read.csv("GCD2018.csv")

#converting class column to factors
GCD$Class <- as.factor(GCD$Class)

#removing NA'S from class column
GCD <- na.omit(GCD, cols = "Class")

# --------------- Question - 1 - Descriptive Analysis ------------#
good <- subset(GCD, Class == '1')
bad <- subset(GCD,Class == '2')

hist(GCD$Age)
hist(good$Age)
hist(bad$Age)
                  
summary(GCD)
summary(good)
summary(bad)

# --------------------------------------------------#


# ---------------- Question - 2 --------------------#
#splitting data in 70% training and 30% testing
set.seed(27502333) #random seed
train.row = sample(1:nrow(GCD), 0.7*nrow(GCD))
GCD.train = GCD[train.row,]
GCD.test = GCD[-train.row,]
# --------------------------------------------------#



# ---------------------------- Question - 3,4,5 ----------------------------------------#

#########################################################################

# Calculate a decision tree
GCD.tree = tree(Class ~., data = GCD.train)
print(summary(GCD.tree))
plot(GCD.tree)
text(GCD.tree, pretty = 0)

# do predictions as classes and draw a table
GCD.predtree = predict(GCD.tree, GCD.test, type = "class")
t1=table(Predicted_Class = GCD.predtree, Actual_Class = GCD.test$Class)
cat("\n#Decision Tree Confusion\n")
print(t1)

# do predictions as probabilities and draw ROC
GCD.pred.tree = predict(GCD.tree, GCD.test, type = "vector")
# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class
GCDDpred <- prediction( GCD.pred.tree[,2], GCD.test$Class)
GCDDperf <- performance(GCDDpred,"tpr","fpr")
plot(GCDDperf)
abline(0,1)

#find AUC
cauc =	performance(GCDDpred,	"auc")
print(as.numeric(cauc@y.values))

#########################################################################


# Calculate naive bayes
GCD.bayes = naiveBayes(Class ~. , data = GCD.train)
GCD.predbayes = predict(GCD.bayes, GCD.test)
t2=table(Predicted_Class = GCD.predbayes, Actual_Class = GCD.test$Class)
cat("\n#NaiveBayes Confusion\n")
print(t2)

# outputs as confidence levels
GCDpred.bayes = predict(GCD.bayes, GCD.test, type = 'raw')
GCDBpred <- prediction( GCDpred.bayes[,2], GCD.test$Class)
GCDBperf <- performance(GCDBpred,"tpr","fpr")
plot(GCDBperf, add=TRUE, col = "blueviolet")

#find AUC
cauc =	performance(GCDBpred,	"auc")
print(as.numeric(cauc@y.values))

#########################################################################

# Calculate Bagging
GCD.bag <- bagging(Class ~. , data = GCD.train, mfinal=10)
GCDpred.bag <- predict.bagging(GCD.bag, GCD.test)

# draw ROC
GCDBagpred <- prediction( GCDpred.bag$prob[,2], GCD.test$Class)
GCDBagperf <- performance(GCDBagpred,"tpr","fpr")
plot(GCDBagperf, add=TRUE,  col = "red")
cat("\n#Bagging Confusion\n")
print(GCDpred.bag$confusion)

#find AUC
cauc =	performance(GCDBagpred,	"auc")
print(as.numeric(cauc@y.values))

#########################################################################

#Calculate Boosting
GCD.Boost <- boosting(Class ~. , data = GCD.train, mfinal=10)
GCDpred.boost <- predict.boosting(GCD.Boost, newdata=GCD.test)


# Draw ROC
GCDBoostpred <- prediction( GCDpred.boost$prob[,2], GCD.test$Class)
GCDBoostperf <- performance(GCDBoostpred,"tpr","fpr")
plot(GCDBoostperf, add=TRUE, col = "green")
cat("\n#Boosting Confusion\n")
print(GCDpred.boost$confusion)


#find AUC
cauc =	performance(GCDBoostpred,	"auc")
print(as.numeric(cauc@y.values))
#########################################################################

# Calculate Random Forest
GCD.rf <- randomForest(Class ~. , data = GCD.train, na.action = na.exclude)
GCDpredrf <- predict(GCD.rf, GCD.test)
t3=table(Predicted_Class = GCDpredrf, Actual_Class = GCD.test$Class)
cat("\n#Random Forest Confusion\n")
print(t3)
GCDpred.rf <- predict(GCD.rf, GCD.test, type="prob")


# draw ROC
GCDFpred <- prediction( GCDpred.rf[,2], GCD.test$Class)
GCDFperf <- performance(GCDFpred,"tpr","fpr")
plot(GCDFperf, add=TRUE, col = "pink")

#find AUC
cauc =	performance(GCDFpred,	"auc")
print(as.numeric(cauc@y.values))

##########################################################################




#----------------------------------------------------------------------------#



#----------------------------- Question - 7 ---------------------------------#

#Attribute importance
cat("\n#Decision Tree Attribute Importance\n")
print(summary(GCD.tree))
cat("\n#Baging Attribute Importance\n")
print(GCD.bag$importance)
cat("\n#Boosting Attribute Importance\n")
print(GCD.Boost$importance)
cat("\n#Random Forest Attribute Importance\n")
print(GCD.rf$importance)


#----------------------------------------------------------------------------#






#-----------------------------Question 8 - Improving Random Forest--------------

# Random Forest
GCD.rf1 <- randomForest(Class ~. , data = GCD.train,ntree=35, norm.votes = FALSE,na.action = na.exclude)
GCD.rf2 <- randomForest(Class ~. , data = GCD.train,ntree=35, norm.votes = FALSE,na.action = na.exclude)
GCD.rf3 <- randomForest(Class ~. , data = GCD.train,ntree=35, norm.votes = FALSE, na.action = na.exclude)

GCD.rf <- combine(GCD.rf1, GCD.rf2, GCD.rf3)

#print confusion matrix
GCDpredrf <- predict(GCD.rf, GCD.test)
t3=table(Predicted_Class = GCDpredrf, Actual_Class = GCD.test$Class)
cat("\n#Random Forest Confusion\n")
print(t3)
GCDpred.rf <- predict(GCD.rf, GCD.test, type="prob")


# print ROC and AUC
GCDFpred <- prediction( GCDpred.rf[,2], GCD.test$Class)
GCDFperf <- performance(GCDFpred,"tpr","fpr")
plot(GCDFperf,col = "blue")
abline(0,1)
cauc =	performance(GCDFpred,	"auc")
print(as.numeric(cauc@y.values))


# -----------------------------------------------------------------------------------





#------------------------------------------Question 9----------------------------------

GCD <- read.csv("GCD2018.csv")

# delete rows with missing values
GCD = GCD[complete.cases(GCD),]

#converting the class variable to numeric
GCD$Class = as.numeric(GCD$Class)


#############################################
#augmenting the input in Status coloumn

GCD$A11 = GCD$Status == 'A11'
GCD$A12 = GCD$Status == 'A12'
GCD$A13 = GCD$Status == 'A13'
GCD$A14 = GCD$Status == 'A14'


#############################################
#augment history column


GCD$A30 = GCD$History == 'A30'
GCD$A31 = GCD$History == 'A31'
GCD$A32 = GCD$History == 'A32'
GCD$A33 = GCD$History == 'A33'
GCD$A34 = GCD$History == 'A34'


#############################################
#augment Purpose Column

GCD$A40 = GCD$Purpose == 'A40'
GCD$A41 = GCD$Purpose == 'A41'
GCD$A410 = GCD$Purpose == 'A410'
GCD$A42 = GCD$Purpose == 'A42'
GCD$A43 = GCD$Purpose == 'A43'
GCD$A44 = GCD$Purpose == 'A44'
GCD$A45 = GCD$Purpose == 'A45'
GCD$A46 = GCD$Purpose == 'A46'
GCD$A48 = GCD$Purpose == 'A48'
GCD$A49 = GCD$Purpose == 'A49'

#############################################

#normalize function (min-max)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#normalize duration,age and credit
GCD$Duration = normalize(GCD$Duration)
GCD$Age = normalize(GCD$Age)
GCD$Credit = normalize(GCD$Credit)

#split training and testing
set.seed(27502333) #random seed
train.row = sample(1:nrow(GCD), 0.7*nrow(GCD))
GCD.train = GCD[train.row,]
GCD.test = GCD[-train.row,]

#create neural network
GCD.nn = neuralnet(Class ~ Duration +
                     Age +
                     Credit +
                     A11 + A12 + A13 + A14 +   #Status
                     A30 + A31 + A32 + A33 + A34 + #History
                     A40 + A41 + A410 + A42 + A43 + A44 + A45 + A46 + A48 + A49   #Purpose
                   , GCD.train, hidden=c(5,3))

#testing
GCD.pred = compute(GCD.nn, GCD.test[c(2,13,18,seq(22,40,by=1))])

plot(GCD.nn)

# now round these down to integers
GCD.pred = as.data.frame(round(GCD.pred$net.result,0))

# plot confusion matrix
table(observed = GCD.test$Class, predicted = GCD.pred$V1)


#--------------------------------------------------------------------------------------

