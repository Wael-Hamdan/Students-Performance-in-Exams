studentPerf <- read.csv("StudentsPerformance.csv")
quantitativeCol <- c("math.score","reading.score", "writing.score")


summary(studentPerf)


### Normality test using shapiro wilks method (h0 = the data are gaussian)
# the p-values obtained are all below 0.05, meaning we reject the H0 hypothesis
# the distribution of the scores are not gaussian
tmp <- rnorm(1000, 0, 1)
shapiro.test(tmp)

shapiro.test(studentPerf$math.score)
shapiro.test(studentPerf$writing.score)
shapiro.test(studentPerf$reading.score)
hist(studentPerf$math.score)

### plotting the quantitative variables 
plot(studentPerf$math.score, studentPerf$reading.score)
plot(studentPerf$writing.score, studentPerf$reading.score)
plot(studentPerf$writing.score, studentPerf$math.score)

## seeing how the variables are close to the diagonale on the plot, we can presume that there will be a great correlation between those variables
## basically, kids tend to have the same results in every field

# since the data are not gaussian, should avoid pearson's test and use kendall instead for a correct p-value 
# (althought it doesnt seem to make much of a difference here)
cor.test(studentPerf$math.score, studentPerf$math.score, method = "kendall")

cor.test(studentPerf$math.score, studentPerf$reading.score, method = "kendall")
cor.test(studentPerf$math.score, studentPerf$writing.score, method = "kendall")
cor.test(studentPerf$reading.score, studentPerf$writing.score, method = "kendall")

cor(as.matrix(studentPerf[,quantitativeCol]))
plot(studentPerf[,6:8])

######################## since the quantitative data are related to each other, perhaps an ACP could help simplify our dataset:

studentPerf.acp = princomp(studentPerf[,quantitativeCol])

studentPerf.acp$loadings

summary(studentPerf.acp)

### we can see that the 2 first components are responsible for 98% of the inertia.
plot(studentPerf.acp$scores)

######################## we'll use the acp for a quicker exploration of the data

postacp <- cbind(studentPerf[1:5], studentPerf.acp$scores)

## we plot the student's score for each qualitative variable

plot(postacp$Comp.1, postacp$Comp.2, col = postacp$gender)
legend('topleft', legend = levels(postacp$gender), col = 1:10, cex = 0.8, pch = 1)

plot(postacp$Comp.1, postacp$Comp.2, col = postacp$race.ethnicity)
legend('topleft', legend = levels(postacp$race.ethnicity), col = 1:10, cex = 0.8, pch = 1)

plot(postacp$Comp.1, postacp$Comp.2, col = postacp$parental.level.of.education)
legend('topleft', legend = levels(postacp$parental.level.of.education), col = 1:10, cex = 0.8, pch = 1)

plot(postacp$Comp.1, postacp$Comp.2, col = postacp$lunch)
legend('topleft', legend = levels(postacp$lunch), col = 1:10, cex = 0.8, pch = 1)

plot(postacp$Comp.1, postacp$Comp.2, col = postacp$test.preparation.course)
legend('topleft', legend = levels(postacp$test.preparation.course), col = 1:10, cex = 0.8, pch = 1)

## we plot the scores for each qualitative variable, but with the original data this time

plot(studentPerf$math.score, studentPerf$reading.score, col = studentPerf$test.preparation.course)
plot(studentPerf$writing.score, studentPerf$reading.score, col = studentPerf$test.preparation.course)
plot(studentPerf$writing.score, studentPerf$math.score, col = studentPerf$test.preparation.course)

plot(studentPerf$math.score, studentPerf$reading.score, col = studentPerf$lunch)
plot(studentPerf$writing.score, studentPerf$reading.score, col = studentPerf$lunch)
plot(studentPerf$writing.score, studentPerf$math.score, col = studentPerf$lunch)

plot(studentPerf$math.score, studentPerf$reading.score, col = studentPerf$parental.level.of.education)
plot(studentPerf$writing.score, studentPerf$reading.score, col = studentPerf$parental.level.of.education)
plot(studentPerf$writing.score, studentPerf$math.score, col = studentPerf$parental.level.of.education)

plot(studentPerf$math.score, studentPerf$reading.score, col = studentPerf$race.ethnicity)
plot(studentPerf$writing.score, studentPerf$reading.score, col = studentPerf$race.ethnicity)
plot(studentPerf$writing.score, studentPerf$math.score, col = studentPerf$race.ethnicity)

plot(studentPerf$math.score, studentPerf$reading.score, col = studentPerf$gender)
plot(studentPerf$writing.score, studentPerf$reading.score, col = studentPerf$gender)
plot(studentPerf$writing.score, studentPerf$math.score, col = studentPerf$gender)


plot(studentPerf[,6:8], col = c("red","blue")[studentPerf$gender])

### We can see that while most qualitative variables do not seem to have much impact on their own, the gender gap is easy to see
### male students tend to do better in math according to the data, whereas female tend to score better in reading/writing exams


################################################ 


## change the qualitative into quantitative


for(col in colnames(studentPerf)){
  if(class(studentPerf[,col]) == "factor"){
    for(lvl in levels(studentPerf[,col])){
      print(lvl)
    }
  }
}

## we will turn the binary factors into boolean data (1 or 0)
## the race.ethnicity will have to be turned into quantitative without changing the information, so the distance from any group to another should be the same
## the parent.education can be turned into numeric with an explicit order : the higher the studies, the higher the numeric

studentQuant <- studentPerf[, c("math.score", "reading.score", "writing.score")]
studentQuant$gender <- as.numeric(studentPerf[, "gender"] == "male")      ## 0 == female here
studentQuant$lunch <- as.numeric(studentPerf[, "lunch"] == "standard")    ## 0 == reduced lunch
studentQuant$test.preparation.course <- as.numeric(studentPerf[, "test.preparation.course"] == "completed")    ## 0 == not completed

### parent's education here
levels(studentPerf$parental.level.of.education)
studentQuant$parental.level.of.education <- studentPerf[, "parental.level.of.education"]

fact <- c("master's degree","bachelor's degree", "associate's degree", "some college", "high school", "some high school") 
orderedfact <- factor(studentQuant$parental.level.of.education , levels = fact, ordered = TRUE)
studentQuant$parental.level.of.education <- as.numeric(orderedfact)

## we basically turn the parental education into an ordered numeric : master is 1, bachelor is 2... 

summary(studentQuant)

cor(studentQuant)
plot(studentQuant)


##TO DO : change the qualitative into quantitative variables and check acp + correlation matrix
## next : k-nearest, (k-means?), analyse dicriminante, linear regression.... 
## end goal : create a predictif model able to estimate the exam score depending on the attribute of the student
##          AND discuss on the impact of each variable on the grades.

studentQuant$parental.level.of.education <- studentQuant$parental.level.of.education/(max(studentQuant$parental.level.of.education)-1)
studentQuant.acp <- princomp(studentQuant)
summary(studentQuant.acp)

studentQuant.acp$loadings
plot(studentQuant.acp$scores)
plot(studentPerf.acp$scores)

## there seems to be no differences between the previous values and the one we have now

#####THERE IS NOT ENOUGH DATA TO MAKE A GOOD PREDICTIVE MODEL, SO IN ORDER TO SIMPLIFY WE'LL MAKE THE SCORES INTO FACTOR

studentQuant$math.score <- cut(studentQuant$math.score, breaks = c(-1,50,60,90,101), labels = c("fail","bad","average","first"))
studentQuant$writing.score <- cut(studentQuant$writing.score, breaks = c(-1,50,60,90,101), labels = c("fail","bad","average","first"))
studentQuant$reading.score <- cut(studentQuant$reading.score, breaks = c(-1,50,60,90,101), labels = c("fail","bad","average","first"))


summary(studentQuant[4:7])
studentQuant.acp2 <- princomp(studentQuant[4:7])
studentQuant.acp2$loadings
summary(studentQuant.acp2)

postacp2 <- cbind(studentQuant.acp2$scores, studentQuant[1:3])
plot(postacp2$Comp.1, postacp2$Comp.2, col = c("red", "blue", "green", "purple")[postacp2$math.score])

#### one-hunt encoding is shit here
