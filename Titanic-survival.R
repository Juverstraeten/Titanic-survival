setwd("~/OneDrive/Documents/Data Science Specialization/Titanic-survival")

# Packages
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(randomForest)

# Loading Data
training <- read.csv("training.csv")
test <- read.csv("test.csv")

# Explore the training set
str(training)

# Create factors
training$Survived <- factor(training$Survived, levels = c(1,0), 
                            labels = c("Survived","Died"))
training$Pclass <- factor(training$Pclass, levels = c(1,2,3), 
                          labels = c("1st Class","2nd Class","3rd Class"))
test$Pclass <- factor(test$Pclass, levels = c(1,2,3), 
                          labels = c("1st Class","2nd Class","3rd Class"))

# How many people survived?
prop.table(table(training$Survived))
# 0.38

# Plot survival according to some variables

# 1. Class
tableClass <- prop.table(table(training$Survived,training$Pclass),2)

png("survival_by_class.png")
barplot(tableClass,
        main= "Passenger Survival by Class", 
        xlab="", ylab="", col=c("orange","gray"), 
        legend.text = c("Survived","Died"))
dev.off()

## Passengers were more likely to survive if they were in a better class.

# 2. Sex
tableSex <- prop.table(table(training$Survived,training$Sex),2)

png("survival_by_sex.png")
barplot(tableSex, 
        names.arg = c("Female","Male"), 
        main= "Passenger Survival by Sex", 
        xlab="", ylab="", col=c("#8DD3C7","gray"), 
        legend.text = c("Survived","Died"))
dev.off()

## Women were more likely to survive then men.

# 3. Age
png("survival_by_age.png")
boxplot(Age ~ Survived, data = training,
        main= "Passenger Survival by Age", 
        xlab="", ylab="Age", col=c("blueviolet","gray"))
dev.off()

## Age does not seem to have an impact on survival.

# 4. Fare
png("survival_by_fare.png")
boxplot(Fare ~ Survived, data = training,
        main= "Passenger Survival by fare", 
        xlab="", ylab="Fare", col=c("darkolivegreen3","gray"))
dev.off()

## There seem to be a relation between Survival and Fare. 
## Create groups within Fare to have a better idea.
## With the test and training data.

## Add variable Survived with NA Values to test
test$Survived <- NA

## Combine the 2 datasets
all_data <- rbind(training,test)

## Create groups
all_data$fare_group <- "Fare1"
all_data$fare_group[all_data$Fare >=5 & all_data$Fare <10] <- "Fare2"
all_data$fare_group[all_data$Fare >=10 & all_data$Fare <15] <- "Fare3"
all_data$fare_group[all_data$Fare >=15 & all_data$Fare <20] <- "Fare4"
all_data$fare_group[all_data$Fare >=20 & all_data$Fare <30] <- "Fare5"
all_data$fare_group[all_data$Fare >=30 & all_data$Fare <100] <- "Fare6"
all_data$fare_group[all_data$Fare >=100] <- "Fare7"
 
## Separate training and test data
training <- all_data[1:891,]
test <- all_data[892:1309,]

## Create table and plot
tableFare <- prop.table(table(training$Survived,training$fare_group),2)

png("survival_by_fare_group.png")
barplot(tableFare, 
        main= "Passenger Survival by Fare Group", 
        xlab="", ylab="", col=c("#8DD3C7","gray"), 
        legend.text = c("Survived","Died"))
dev.off()

## The higher the fare, the more likely passengers survived.

# 5. Name

## Extract the title from the name and create a new variable.
## With the test and training data.

## Combine the 2 datasets
all_data <- rbind(training,test)

## Convert Name to a character variable and take out the interesting data
all_data$Name <- as.character(all_data$Name)

## Extract title
all_data$Title <- sapply(all_data$Name,
                         FUN=function(x){strsplit(x,split="[,.]")[[1]][2]})
all_data$Title <- sub(" ","",all_data$Title)
all_data$Title[all_data$Title %in% c('Capt', 'Col', 'Don', 'Dr', 'Jonkheer', 'Major', 
                                     'Master', 'Rev', 'Sir')] <- 'Hon_male'
all_data$Title[all_data$Title %in% c('Dona', 'Lady', 'the Countess')]  <- 'Hon_fem'
all_data$Title[all_data$Title %in% c('Miss', 'Mlle', 'Mme', 'Mrs', 'Ms')] <- 'Non_Hon_fem'
all_data$Title[all_data$Title %in% c('Mr')] <- 'Non_Hon_male'
all_data$Title <- factor(all_data$Title)

## Separate training and test data
training <- all_data[1:891,]
test <- all_data[892:1309,]

## Create table and plot.
tableTitle <- prop.table(table(training$Survived,training$Title),2)

png("survival_by_title.png")
barplot(tableTitle, 
        main= "Passenger Survival by Title", 
        xlab="", ylab="", col=c("gold","gray"), 
        legend.text = c("Survived","Died"))
dev.off()

## Honorific titles were more likely to survive.

# 6. Number of Siblings/Spouses Aboard

tableSibSp <- prop.table(table(training$Survived,training$SibSp),2)

png("survival_by_SibSp.png")
barplot(tableSibSp, 
        main= "Passenger Survival by SibSp", 
        xlab="", ylab="", col=c("gold","gray"), 
        legend.text = c("Survived","Died"))
dev.off()

## Passengers with one SibSp aboard were more likely to survive than the others.

# 7. Number of Parents/Children Aboard

tableParch <- prop.table(table(training$Survived,training$Parch),2)

png("survival_by_Parch.png")
barplot(tableParch, 
        main= "Passenger Survival by Parch", 
        xlab="", ylab="", col=c("gold","gray"), 
        legend.text = c("Survived","Died"))
dev.off()

## Passengers with one or three Parch aboard were more likely to survive. 
## Passengers with zero or more than 4 Parch aboard were more likely to die.

# 8. Cabin

## Combine the 2 datasets
all_data <- rbind(training,test)

## Create groups
all_data$CabinGroup <- sapply(all_data$Cabin, FUN= function(x){substr(x,1,1)})
all_data$CabinGroup[all_data$CabinGroup==''] <- 'no_data'
all_data$CabinGroup <- factor(all_data$CabinGroup)

## Separate training and test data
training <- all_data[1:891,]
test <- all_data[892:1309,]

## Create table and plot
tableCabin <- prop.table(table(training$Survived,training$CabinGroup),2)

png("survival_by_CabinGroup.png")
barplot(tableCabin, 
        main= "Passenger Survival by Cabin Group", 
        xlab="", ylab="", col=c("#8DD3C7","gray"), 
        legend.text = c("Survived","Died"))
dev.off()

## The higher the fare, the more likely passengers survived.
  
# Conclusion: We found variables that have an impact on survival.

# Aggregate some variables. 
training$Survived_inverse <- as.numeric(factor(training$Survived,
                                               levels(training$Survived)[c(2,1)]))-1

aggregation <- aggregate(Survived_inverse ~ Parch + fare_group + Sex, 
                       data = training, FUN= function(x) {sum(x)/length(x)})

# 1st model : my predictions.
# According to the study of the data, apply predictions.
# Start with the "worst"

test$Survived <- rep(0,418)

# Passengers with one SibSp aboard survived (0.54)
test$Survived[test$SibSp == 1] <- 1

# Passengers in the 1st class survived (0.63)
test$Survived[test$Pclass == "1st Class"] <- 1

# Passengers withe zero Parch aboard died (0.66)
test$Survived[test$Parch == 0] <- 0

# All women survived (0.74)
test$Survived[test$Sex == "female"] <- 1

# Passengers in Fare7 group survived (0.74)
test$Survived[test$fare_group == "Fare7"] <- 1

# Passengers in the 2nd class died (0.76)
test$Survived[test$Pclass == "2nd Class"] <- 0

# Passengers in Fare2 group died (0.79)
test$Survived[test$fare_group == "Fare2"] <- 0

# All men died (0.81)
test$Survived[test$Sex == "male"] <- 0

# Passengers in Fare1 group died (0.94)
test$Survived[test$fare_group == "Fare1"] <- 0

# Honorific Female survived (1.00)
test$Survived[test$Title == "Hon_fem"] <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "my_predictions.csv", row.names = FALSE)

# Score on Kaggle : 0.75120

# 2nd model : Decision Tree
training$Survived <- as.numeric(factor(training$Survived,
                                       levels(training$Survived)[c(2,1)]))-1
titanic_tree <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Title + CabinGroup + Age, 
              data=training, method = "class", control=rpart.control(minsplit = 5))

# Visualisation of the tree
png("Titanic_Tree.png")
prp(titanic_tree, extra = 1, box.col = "wheat")
dev.off()

# Use this tree to predict the outcome of the test data
titanic_tree_prediction <- predict(titanic_tree, test, type="class")

# Create the csv file for submission
submit2 <- data.frame(PassengerId = test$PassengerId, Survived = titanic_tree_prediction)
write.csv(submit2, file="titanic_tree.csv", row.names=FALSE)

# Score on Kaggle : 0.77033

# 3rd model : Random Forest

# Create a tree to fill the NAs in Age
tree_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title, 
                  data=training[!is.na(training$Age),], method = "anova", control=rpart.control(minsplit = 10))
all_data$Age[is.na(all_data$Age)] <- predict(tree_age, all_data[is.na(all_data$Age),])

# Remove NAs in Fare
all_data$Fare[is.na(all_data$Fare)] <- median(all_data$Fare, na.rm=TRUE)

# Create a Random Forest for predicting survival on test data
training <- all_data[1:891,]
test <- all_data[892:1309,]

set.seed(333)

training$Survived <- as.numeric(factor(training$Survived,
                                       levels(training$Survived)[c(2,1)]))-1

randomForest <- randomForest(as.factor(Survived) ~ Pclass + Sex + SibSp + Parch + Fare + Title + CabinGroup + Age,
                        data = training, ntree = 2000, importance = TRUE)

rf_prediction <- predict(randomForest,test)

# Create the csv file for submission
submit3 <- data.frame(PassengerId = test$PassengerId, Survived = rf_prediction)
write.csv(submit3, file="titanic_rf.csv", row.names=FALSE)

# Score on Kaggle : 0.78469