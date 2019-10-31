iris


nrow(iris)
ncol(iris)


str(iris)
class(iris$Species)
nlevels(iris$Species)
levels(iris$Species)

nrow(subset(iris,iris$Species=="setosa"))
nrow(subset(iris,iris$Species=="versicolor"))
nrow(subset(iris,iris$Species=="virginica"))

table(iris$Species)

# divide iris dataset in two sample of 75 rows each and name themm sample1 and sample2 

sample1=iris[1:75,]
sample2=iris[75:150,]


# check number o species in each sample 


nlevels(sample1$Species)
levels(sample1$Species)
table(sample1$Species)
sample1[,5]


nlevels(sample2$Species)
levels(sample2$Species)
table(sample2$Species)
sample2[,5]


# divide dataset into 2 samples with random values and name them as Train and Test
# class(iris$Species)
ind=sample(2,nrow(iris),replace = T,prob=c(0.8,0.2))
ind
Train=iris[ind==1,]
Test=iris[ind==2,]

table(Train$Species)
table(Test$Species)

#divide dataset in 2 samples with random values 
# but random function should be asme and name them as Trsin and Test



set.seed(1234)    # this mixes the data/take random data

ind=sample(2,nrow(iris),replace = T,prob=c(0.8,0.2))
ind
Train=iris[ind==1,]
Test=iris[ind==2,]

table(Train$Species)
table(Test$Species)


# finding vakidation and accuracy

checktest=data.frame(learn=c("class1","class2","class1","class1","class2"))
as.factor(checktest$learn)
levels(checktest)

checkmodel=data.frame(learn=c("class1","class2","class1","class1","class2"))
as.factor(checkmodel$learn)
levels(checkmodel$learn)


tab=table(TEST=checktest$learn,MODEL=checkmodel$learn)
tab
Accuracy=sum(diag(tab))/sum(tab)
Accuracypercent=Accuracy*100
Accuracypercent
Misclassification=1-Accuracy
Mispercent=Misclassification*100
Mispercent

  
  
  
  
  
  
  
  
  
  
  



