# Load the party package. It will automatically load other
# dependent packages.

library(party)

#prepare data
data<-readingSkills
str(readingSkills)
summary(readingSkills)
#nativeSpeaker: a factor with levels no and yes, where yes indicates that the child is a native speaker of the language of the reading test.

#age:age of the child in years.
#shoeSize: shoe size of the child in cm.
#score: raw score on the reading test.
# Create the input data frame.
#data <- readingSkills[c(1:105),]
# partitioning data into training and testing datasets

set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
training <- data[ind == 1,]
testing <- data[ind == 2,]
nrow(testing)

# Create the decision tree with party
tree <- ctree(nativeSpeaker ~ age + shoeSize + score, data = training)
#tree pruning (compact tree)
#tree <- ctree(nativeSpeaker ~ age + shoeSize + score, data = training, controls=ctree_control(mincriterion =0.9,minsplit = 100 ))
tree
# Plot the tree.
plot(tree)

#predict

pr<-predict(tree,testing,type="prob")

predict(tree,testing)
#testing[,1]
# CHECKING ACCURACY AND CROSS VALIDATION
#tab={table}(predict(tree), training[,1])
#tab
tab=table(predict(tree,testing), testing[,1])
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

##################################################################

# PREDICTION
NEW=data.frame(age=10,shoeSize=17.4,score=45.12)
summary(NEW)

check=ctree(nativeSpeaker ~ age + shoeSize + score, data = training)

