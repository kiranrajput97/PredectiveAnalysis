
#install.packages("missForest")
install.packages('missForest', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(missForest)
View(iris)
summary(iris)
iris.mis <- prodNA(iris, noNA = 0.1)

summary(iris.mis)

iris.imp <- missForest(iris.mis)


completeDataForest=as.data.frame(iris.imp[[1]])
str(completeDataForest)
summary(completeDataForest)
