
#NaiveBayes

library(naivebayes)
library(e1071)
iris ##load data
str(iris)

head(iris) ## see the studcture
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa
##Generate a random number that is 90% of the total number of rows in dataset.


ran <- sample(1:nrow(iris), 0.9 * nrow(iris)) 
summary(iris)

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))

summary(iris_norm)

##extract training set
iris_train <- iris_norm[ran,] 
nrow(iris_train)
##extract testing set
iris_test <- iris_norm[-ran,] 
nrow(iris_test)
##extract 5th column of dataset because it will be used in label prediction.
iris_target_category <- iris[ran,5]
##extract 5th column if test dataset to measure the accuracy
iris_test_category <- iris[-ran,5]
#model<-naive_bayes(iris$Species ~ .,data=iris)
#model
#plot(model)
model<-naive_bayes(iris_target_category ~ .,data=iris_train,type='prob')
#To check the efficiency of the model, we are now going to run the testing data set on the model, after which 
#we will evaluate the accuracy of the model by using a Confusion matrix.
#Model Evaluation
#Predict testing set
p1<-predict(model,iris_test)
#Get the confusion matrix to see accuracy value and other parameter values
tab <- table(p1,iris_test_category)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

# PREDICTION

NEW=data.frame(Sepal.Length=0.5,Sepal.Width=0.8,Petal.Length=0.2,Petal.Width=0.4 )
#summary(NEW)
model<-naive_bayes(iris_target_category ~ .,data=iris_train,type='prob')
model
p1<-predict(model,NEW)
p1

