library(randomForest)
library(foreach)
library(doParallel)

# Parallel stuff
cl <- makeCluster(4)
registerDoParallel(cl)

# Reading Data
test = read.csv("test.csv")
train = read.csv("train.csv")

# Data Cleaning
print("Cleaning")
train$LotFrontage[is.na(train$LotFrontage)] <- median(train$LotFrontage, na.rm = TRUE)
levels(train$Alley) <- c(levels(train$Alley), "None")
train$Alley[is.na(train$Alley)] <- "None"

test$LotFrontage[is.na(test$LotFrontage)] <- median(test$LotFrontage, na.rm = TRUE)
levels(test$Alley) <- c(levels(test$Alley), "None")
levels(test$Condition2) <- levels(train$Condition2)
test$Alley[is.na(test$Alley)] <- "None"
test$Utilities[is.na(test$Utilities)] <- "AllPub" # Missing Utilities is just AllPub
test$MSZoning[is.na(test$MSZoning)] <- "RL" # Set missing MSZoning to RL


# RFing
print("Random Forest-ing")
#my_forest <- randomForest(as.factor(SalePrice) ~ MSSubClass + MSZoning + LotArea + LotConfig + Neighborhood + Condition1 + Condition2 + OverallQual + OverallCond + YearRemodAdd, data = train, importance = TRUE, ntree = 200)
my_forest <- foreach(ntree=rep(50, 4), .combine=combine, .packages="randomForest") %dopar%  randomForest(as.factor(SalePrice) ~ MSSubClass + MSZoning + LotArea + LotConfig + Neighborhood + Condition1 + Condition2 + OverallQual + OverallCond + YearRemodAdd, data = train, importance = TRUE, ntree = ntree)

print("Predicting")
my_prediction <- predict(my_forest, test)

print("Graphing")
varImpPlot(my_forest)

my_solution <- data.frame(Id = test$Id, SalePrice = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
