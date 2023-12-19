library(readr)
eBayAuctions <- read_csv("eBayAuctions.csv")
View(eBayAuctions)

unique(eBayAuctions$sellerRating)



#xx____________DATA_CLEANING______________________xx

# Define bin edges
bin_edges <- c(0, 4, 8, 10)  # Adjust the bin edges based on your data

# Define bin labels
bin_labels <- c( "Short", "Medium", "Long")

# Use cut() to create a categorical variable
categorical_variable <- cut(eBayAuctions$Duration, breaks = bin_edges, labels = bin_labels, include.lowest = TRUE)

# Create a data frame for display
df <- data.frame(eBayAuctions$Duration, categorical_variable)
eBayAuctions$Duration <- categorical_variable

df
unique(eBayAuctions$Duration)



#Set Seed
#To ensure getting same result each time with same seed
set.seed(60)
shuffle_index <- sample(1:nrow(eBayAuctions))
head(shuffle_index)
eBayAuctions <- eBayAuctions[shuffle_index, ]
head(eBayAuctions)
tail(eBayAuctions)

# Creating random samples for data preparation
#Sampling training data with 60% of the total
train.rows <- sample(rownames(eBayAuctions), dim(eBayAuctions)[1]*0.6)
#Sampling validation data with 40% of the total
valid.rows <- sample(setdiff(rownames(eBayAuctions), train.rows),dim(eBayAuctions)[1]*0.4)
#Populating samples
train.eBayAuctions <- eBayAuctions[train.rows, ]
valid.eBayAuctions <- eBayAuctions[valid.rows, ]

names(valid.eBayAuctions)[names(valid.eBayAuctions) == "Competitive?"] <- "Competitive"
names(train.eBayAuctions)[names(train.eBayAuctions) == "Competitive?"] <- "Competitive"
names(eBayAuctions)[names(eBayAuctions) == "Competitive?"] <- "Competitive"
#xx____________TREE______________________xx

#Deep tree with all predictors that can be used
fit.deeper <- rpart(Competitive~., data = train.eBayAuctions, method = 'class', cp = 0, minsplit =1)
prp(fit.deeper, type = 2, extra = 1, under = TRUE, split.font = 1, varlen = -10)


#Best-pruned decision tree according to the assignment document.
fit <- rpart(Competitive~., data = train.eBayAuctions, method = 'class', minbucket =50, maxdepth = 7)
prp(fit, type = 2, extra = 101, main = "Customized Decision Tree Display")





  #To test our decision tree we use a confusion matrix
  default.ct.point.pred.train <- predict(fit,train.eBayAuctions, type= "class")
  deeper.ct.point.pred.train <- predict(fit.deeper,train.eBayAuctions, type= "class")
  
  train.eBayAuctions$Competitive <- factor(train.eBayAuctions$Competitive, levels = c(0,1), labels = c("0", "1"))
  
  levels(default.ct.point.pred.train)
  levels(train.eBayAuctions$Competitive)
  
  library(caret)
  confusionMatrix(default.ct.point.pred.train, train.eBayAuctions$Competitive)
  confusionMatrix(deeper.ct.point.pred.train, train.eBayAuctions$Competitive)

  valid.eBayAuctions$Competitive <- factor(valid.eBayAuctions$Competitive, levels = c(0,1), labels = c("0", "1"))
  
  #To test our decision tree using the validation data set
  default.ct.point.pred.valid <- predict(fit,valid.eBayAuctions, type= "class")
  deeper.ct.point.pred.valid <- predict(fit.deeper,valid.eBayAuctions, type= "class")
  
  
  library(caret)
  confusionMatrix(default.ct.point.pred.valid, valid.eBayAuctions$Competitive)
  confusionMatrix(deeper.ct.point.pred.valid, valid.eBayAuctions$Competitive)


#Testing for best-pruned tree with different complexity parameters  
tree_model  <- rpart(Competitive~., data = train.eBayAuctions, method = 'class',  minbucket =50, maxdepth = 7)

cv_results <- prune(tree_model, cp = seq(0.01))
cv_results2 <- prune(tree_model, cp = seq(0.0001))
cv_results3 <- prune(tree_model, cp = seq(0.5 , 0.01, by = 0.01))

best_pruned_tree <- cv_results$cptable[which.min(cv_results$cptable[, "xerror"]), "nsplit"]
best_pruned_model <- prune(tree_model, best_pruned_tree)

best_pruned_tree2 <- cv_results2$cptable[which.min(cv_results$cptable[, "xerror"]), "nsplit"]
best_pruned_model2 <- prune(tree_model, best_pruned_tree2)

best_pruned_tree3 <- cv_results2$cptable[which.min(cv_results$cptable[, "xerror"]), "nsplit"]
best_pruned_model3 <- prune(tree_model, best_pruned_tree3)

prp(best_pruned_model, type = 2, extra = 101, main = "Best-Pruned Decision Tree #1")
prp(best_pruned_model2, type = 2, extra = 101, main = "Best-Pruned Decision Tree #2")
prp(best_pruned_model3, type = 2, extra = 101, main = "Best-Pruned Decision Tree #3")


test.valid <- predict(best_pruned_model,valid.eBayAuctions, type= "class")
test.train <- predict(best_pruned_model,train.eBayAuctions, type= "class")

confusionMatrix(test.valid, valid.eBayAuctions$Competitive)
confusionMatrix(test.train, train.eBayAuctions$Competitive)



#______________________________1.C______________________________#

valid.eBayMin$Competitive <- as.numeric(as.character(valid.eBayMin$Competitive))

library(gains)
gain <- gains(train.eBayAuctions$Competitive,test.train, groups = 200 )

plot(c(0,gain$cume.pct.of.total*sum(valid.eBayMin$Competitive))~
       c(0,gain$cume.obs),
     xlab="Number of Observations", ylab = "Cumulative Percentage of Total", main = "Lift Chart for Decision Tree Classifier" )
lines(c(0,sum(valid.eBayMin$Competitive))~c(0,dim(valid.eBayMin)[1]),lty=2)



predictions <- predict(best_pruned_model, newdata = valid.eBayAuctions, type = "class")

# Create gains chart
gain <- gains(valid.eBayAuctions$Competitive,predictions, groups = 200)

# Plot gains chart
plot(
  c(0, gain$cume.pct.of.total * sum(valid.eBayAuctions$Competitive)) ~ c(0, gain$cume.obs),
  xlab = "Number of Observations",
  ylab = "Cumulative Percentage of Total",
  main = "Gains Chart for Decision Tree Classifier"
)
lines(c(0, sum(valid.eBayAuctions$Competitive)) ~ c(0, length(valid.eBayAuctions)[1]), lty = 2)



