# Amber Garner
# Nov. 2, 2016
# Decision Tree on cars data

#*********Setup*************************************** 

install.packages("party")
library(party)

# Load cars dataset
cars <- read.csv("cars.csv", header = T, sep = ",")

# View structure & summary
str(cars)
summary(cars)

# remove id
cars$Id <- NULL

# factor symboling
cars$symboling <- factor(cars$symboling)

# verify changes
summary(cars$symboling)

# divide into training (70%) and test (30%) sets
set.seed(1234)
ind <- sample(2, nrow(cars), replace = TRUE, prob = c(0.7, 0.3))
train.data <- cars[ind == 1, ]
test.data <- cars[ind == 2, ]

# build decision tree with ctree
myFormula <- symboling~.
cars_ctree <- ctree(myFormula, data=train.data)

# print cars decision tree model
print(cars_ctree)

# view nodes starting at the 2nd node
nodes(cars_ctree, 2)

# plot decision tree model
plot(cars_ctree)

# plot simple decision tree model
plot(cars_ctree, type="simple")

# build confusion matrix on training data
table(predict(cars_ctree), train.data$symboling)

# table of probabilities
prop.table(table(predict(cars_ctree), train.data$symboling))

# confusion matrix on test data
testPred <- predict(cars_ctree, newdata = test.data)
table (testPred, test.data$symboling)

