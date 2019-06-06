install.packages("randomForest")
library(randomForest)

studentPerf <- read.csv("StudentsPerformance.csv")

head(studentPerf)

#studentPerf$reading.score = cut(studentPerf[,"reading.score"],breaks=c(0,50,60,70,80,90,100), labels = c("F","E","D","C","B","A"))
studentPerf$reading.score = cut(studentPerf[,"reading.score"],breaks=c(0,70,100), labels = c("F","PASS"))

head(studentPerf)


# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(studentPerf), 0.7*nrow(studentPerf), replace = FALSE)
TrainSet <- studentPerf[train,]
ValidSet <- studentPerf[-train,]

summary(TrainSet)

summary(ValidSet)

studentPerf = studentPerf[,-c(6,8)]
TrainSet = TrainSet[,-c(6,8)]


# Create a Random Forest model with default parameters
model1 <- randomForest(reading.score ~ . , data = studentPerf,subset = train, importance = TRUE, ntree = 500)
model1

importance(model1) 

plot(model1)

pred = predict(model1, newdata=ValidSet[,-c(6,7,8)])

length(pred[pred==studentPerf[-train,"reading.score"]])/length(pred==studentPerf[-train,"reading.score"])

