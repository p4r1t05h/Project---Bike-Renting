rm(list=ls())

setwd("D:/Data Science/Project - Bike Renting")
getwd()

#loading Data set

bike<-read.csv("day.csv", header = T)
head(bike)

str(bike)
bike1<-bike


bike1$season<-as.factor(bike$season)
bike1$yr<-as.factor(bike$yr)
bike1$mnth<-as.factor(bike$mnth)
bike1$holiday<-as.factor(bike$holiday)
bike1$weekday<-as.factor(bike$weekday)
bike1$workingday<-as.factor(bike$workingday)
bike1$weathersit<-as.factor(bike$weathersit)

str(bike1)

#Feature Selection

library(Boruta)
library(glmnet)
library(tree)
library(ISLR)
library(boot)
library(class)
library(C50)
library(randomForest)
library(caret)

impVar<-Boruta(cnt~., data = bike1)
print(impVar)

#cross checking with bivariate analysis

#Dependency test Q->Q
#cnt->instant

qq1<-cor(bike1$instant, bike1$cnt)
qq1

#Visualization
plot(bike1$instant, bike1$cnt)

#Test
qq1.test<-lm(cnt~instant, data = bike1)
summary(qq1.test)
abline(qq1.test)

#DEPENDENT

#cnt->temp

qq2<-cor(bike1$temp, bike1$cnt)
qq2

#Visualization
plot(bike1$temp, bike1$cnt)

#Test
qq2.test<-lm(cnt~temp, data = bike1)
summary(qq2.test)
abline(qq2.test)

#DEPENDENT

#cnt->atemp

qq3<-cor(bike1$atemp, bike1$cnt)
qq3

#Visualization
plot(bike1$atemp, bike1$cnt)

#Test
qq3.test<-lm(cnt~atemp, data = bike1)
summary(qq3.test)
abline(qq3.test)

#DEPENDENT

#cnt->hum

qq4<-cor(bike1$hum, bike1$cnt)
qq4

#Visualization
plot(bike1$hum, bike1$cnt)

#Test
qq4.test<-lm(cnt~hum, data = bike1)
summary(qq4.test)
abline(qq4.test)

#DEPENDENT

#cnt->windspeed

qq5<-cor(bike1$windspeed, bike1$cnt)
qq5

#Visualization
plot(bike1$windspeed, bike1$cnt)

#Test
qq5.test<-lm(cnt~windspeed, data = bike1)
summary(qq5.test)
abline(qq5.test)

#DEPENDENT

#cnt->casual

qq6<-cor(bike1$casual, bike1$cnt)
qq6

#Visualization
plot(bike1$casual, bike1$cnt)

#Test
qq6.test<-lm(cnt~casual, data = bike1)
summary(qq6.test)
abline(qq6.test)

#DEPENDENT

#cnt->registered

qq7<-cor(bike1$registered, bike1$cnt)
qq7

#Visualization
plot(bike1$registered, bike1$cnt)

#Test
qq7.test<-lm(cnt~registered, data = bike1)
summary(qq7.test)
abline(qq7.test)

#DEPENDENT

#C->Q Analysis

str(bike1)

#Numerical Analysis season->cnt

cq1<-boxplot(bike1$cnt~bike1$season)
cq1$stats

#Dependency test

cq1.test<-aov(bike1$cnt~bike1$season)
summary(cq1.test)

#DEPENDENT

str(bike1)

#Numerical analysis 

cq2<-boxplot(bike1$cnt~bike1$yr)
cq2$stats

#dependency test
cq2.test<-t.test(bike1$cnt~bike1$yr, alternative = "t")
cq2.test

#DEPENDENT

#Numerical Analysis

cq3<-boxplot(bike1$cnt~bike1$mnth)
cq3$stats

#Dependency Test
cq3.test<-aov(bike1$cnt~bike1$mnth)
summary(cq3.test)

#DEPENDENT

#Numerical Analysis

cq4<-boxplot(bike1$cnt~bike1$holiday)
cq4$stats

#Dependency Test
cq4.test<-t.test(bike1$cnt~bike1$holiday, alternative = "t")
cq4.test

#INDEPENDENT

#Numerial Analysis cnt~weekday

str(bike1)

cq5<-boxplot(bike1$cnt~bike1$weekday)

#Dependency Test
cq5.test<-aov(bike$cnt~bike1$weekday)
summary(cq5.test)

#INDEPENDENT

#Numerical Analysis cnt~workingday

cq6<-boxplot(bike1$cnt~bike1$workingday)

#Dependency Test

cq6.test<-t.test(bike1$cnt~bike1$workingday, alternative="t")
cq6.test

#INDEPENDENT

#Numerical Analysis cnt~weathersit

cq7<-boxplot(bike1$cnt~bike1$weathersit)

#Dependency Test

cq7.test<-aov(bike1$cnt~bike1$weathersit)
summary(cq7.test)

#DEPENDENT

#Removing Unimportant Xs

bike1<-bike1[,c(-2,-6,-7,-8)]
str(bike1)

#Model Building

set.seed(123)
ind<- sample(1:nrow(bike1), 500)
train<-bike1[ind,]
test<-bike1[-ind,]
x.train<-train[,-12]
y.train<-train[,12]
x.test<-test[,-12]
y.test<-test[,12]

##======Ridge Regression===============

#cv.out<-cv.glmnet(x.train, y.train, alpha=0)

?cv.glmnet()

##======Lasso Regression==============

#cv.out.lasso<-cv.glmnet(x.train, y.train, alpha = 1)

##======KNN===========================

knn.model<-knn(x.train,x.test,y.train, k=10)
knn.model<-as.numeric(knn.model)

rss.knn<-sum((y.test-knn.model)^2)
rss.knn
#4920865802

##========Decision Trees==============

tree.model<-tree(cnt~., data = train)
plot(tree.model)
summary(tree.model)
tree.model
text(tree.model)

predict.tree.model<-predict(tree.model, newdata = test[,-12])

rss.tree.model<-sum((predict.tree.model-test[,12])^2)
#68476852

##=========Random Forest==============

forest.model<-randomForest(cnt~., data = train, ntree=500)

forest.model

predict.forest.model<-predict(forest.model, newdata = test[,-12])

predict.forest.model

rss.forest.model<-sum((predict.forest.model-test[,12])^2)

#28875261
varImpPlot(forest.model)

View(predict.forest.model)

##============Bagging==================

randomForest
bagging.model<-randomForest(cnt~., data = train, ntree=500, mtry=19, importance=T)

predict.bagging.model<-predict(bagging.model, newdata = test[,-12])

rss.bagging.model<-sum((predict.bagging.model-test[,12])^2)

View(predict.bagging.model)
#3441805