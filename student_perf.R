### SY09 PROJET STUDENT PERFORMANCE ###
setwd("/Users/Vincent/Desktop/GI04/SY09/Projet")

## Read the data ##
raw_data = read.csv(file = "StudentsPerformance.csv", header = T)
raw_data = as.data.frame(raw_data)

## Summarize the data ##
summary(raw_data)

## Check if there is NA value ##
apply(raw_data, 2, function(x) any(is.na(x)))

## Check if there is dirty data in categorical columns ##
table(raw_data$gender)
table(raw_data$race.ethnicity)
table(raw_data$parental.level.of.education)
table(raw_data$lunch)
table(raw_data$test.preparation.course)
###Categories are uniform throughout the dataset###

colnames(raw_data) <- c("gender","race","parental_level_edu","lunch","test_prep_course","math_score","reading_score","writing_score")
#--------------------------------------------------------------------#
## Correlation heatmap ##
#@description Compute correlations of columns of a dataframe of mixed types. 
#'   The dataframe is allowed to have columns of these four classes: integer, 
#'   numeric, factor and character. The character column is considered as 
#'   categorical variable.
#'  
#' The correlation is computed as follows: 
#'   
#'   integer/numeric pair: pearson correlation using `cor` function. The 
#'   valuelies between -1 and 1.
#'   
#'   integer/numeric - factor/categorical pair: correlation coefficient or
#'   squared root of R^2 coefficient of linear regression of integer/numeric
#'   variable over factor/categorical variable using `lm` function. The value
#'   lies between 0 and 1. 
#'   
#'   factor/categorical pair: cramersV value is
#'   computed based on chisq test using `lsr::cramersV` function. The value lies
#'   between 0 and 1.
#'   
cor2 = function(df){
  
  stopifnot(inherits(df, "data.frame"))
  stopifnot(sapply(df, class) %in% c("integer"
                                     , "numeric"
                                     , "factor"
                                     , "character"))
  
  cor_fun <- function(pos_1, pos_2){
    
    # both are numeric
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("integer", "numeric")){
      r <- stats::cor(df[[pos_1]]
                      , df[[pos_2]]
                      , use = "pairwise.complete.obs"
      )
    }
    
    # one is numeric and other is a factor/character
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_1]] ~ as.factor(df[[pos_2]])))[["r.squared"]])
    }
    
    if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
       class(df[[pos_1]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_2]] ~ as.factor(df[[pos_1]])))[["r.squared"]])
    }
    
    # both are factor/character
    if(class(df[[pos_1]]) %in% c("factor", "character") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- lsr::cramersV(df[[pos_1]], df[[pos_2]], simulate.p.value = TRUE)
    }
    
    return(r)
  } 
  
  cor_fun <- Vectorize(cor_fun)
  
  # now compute corr matrix
  corrmat <- outer(1:ncol(df)
                   , 1:ncol(df)
                   , function(x, y) cor_fun(x, y)
  )
  
  rownames(corrmat) <- colnames(df)
  colnames(corrmat) <- colnames(df)
  
  return(corrmat)
}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

correlation_matrix_raw = cor2(raw_data)
upper_tri <- get_upper_tri(correlation_matrix_raw)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# To predict math_score, we see that lunch, race and reading/writing scores are related to the result. We use them in the linear model.
#--------------------------------------------------------------------#
# Descriptive statistic
attach(raw_data)
summary(raw_data)

hist(math_score, freq = TRUE, angle = 30, density = 30, col = 'blue', border = "purple",
     xlab = "Math score", ylab = "Frequence", main = "Histogramme de frequence pour math score")

describe(math_score)
#--------------------------------------------------------------------#
# Max min

raw_data[which.max(math_score),]
raw_data[which.min(math_score),]

win.graph(width=10, height=8,pointsize=8)

par(fig = c(0, 0.8, 0, 0.8)) 
plot(reading_score, math_score, xlab = "reading_score", ylab = "math_score", pch = 20)
par(fig = c(0, 0.8, 0.55, 1), new = TRUE) 
boxplot(reading_score, horizontal = TRUE, axes = FALSE, col = "gold")
par(fig = c(0.65, 1, 0, 0.8), new = TRUE)
boxplot(math_score, axes = FALSE, col = "gold")
mtext("Nuage de points: reading - math score", side = 3, outer = TRUE,  line = -3)
par(opar)
#--------------------------------------------------------------------#
# Hypothesis of Gaussien distribution -------
dagoTest(math_score)

ks.test(jitter(math_score),'pnorm')$p.value 
ks.test(jitter(math_score),'pnorm')$p.value < 0.05 
#TRUE: Rejette l'hyp de Gaussien

ks.test(reading_score,'pnorm')$p.value 
ks.test(reading_score,'pnorm')$p.value < 0.05 
#TRUE: Rejette l'hyp de Gaussien

ks.test(writing_score,'pnorm')$p.value 
ks.test(writing_score,'pnorm')$p.value < 0.05 
#TRUE: Rejette l'hyp de Gaussien
#--------------------------------------------------------------------#
# Histogramme de math_score
hist(math_score, freq = FALSE, breaks = seq(min(math_score),max(math_score), length = 20), col = "lightgray",
     main = "test d'hypothèse de gaussienne: Math_score", xlab = "Math_score", ylab = "Estimation par noyau: Math_score")
lines(density(math_score), col = 'red', lty = 1, lwd = 2)
MATH_SCORE <- math_score[order(math_score)] 
lines(MATH_SCORE, dnorm(MATH_SCORE, mean(MATH_SCORE), sd(MATH_SCORE)), col = 'blue', lty = 2, lwd = 2.5)
legend("topright",legend = c("Courbe d'Estimation par noyau: Math_score","Courbe de Gaussienne distribution"),
       col = c("red","blue"), lty = c(1,5), lwd = c(2,2.5), bty = 'n')

# Histogramme de reading_score
hist(reading_score, freq = FALSE, breaks = seq(min(reading_score),max(reading_score), length = 20), col = "lightgray",
     main = "test d'hypothèse de gaussienne: Reading_score", xlab = "Reading_score", ylab = "Estimation par noyau: Reading_score")
lines(density(reading_score), col = 'red', lty = 1, lwd = 2)
READING_SCORE <- reading_score[order(reading_score)] 
lines(READING_SCORE, dnorm(READING_SCORE, mean(READING_SCORE), sd(READING_SCORE)), col = 'blue', lty = 2, lwd = 2.5)
legend("topleft",legend = c("Courbe d'Estimation par noyau: Reading_score","Courbe de Gaussienne distribution"),
       col = c("red","blue"), lty = c(1,5), lwd = c(2,2.5), bty = 'n')

# Histogramme de math_score
hist(writing_score, freq = FALSE, breaks = seq(min(writing_score),max(writing_score), length = 20), col = "lightgray",
     main = "test d'hypothèse de gaussienne: Writing_score", xlab = "Writing_score", ylab = "Estimation par noyau: Writing_score")
lines(density(writing_score), col = 'red', lty = 1, lwd = 2)
WRITING_SCORE <- writing_score[order(writing_score)] 
lines(WRITING_SCORE, dnorm(WRITING_SCORE, mean(WRITING_SCORE), sd(WRITING_SCORE)), col = 'blue', lty = 2, lwd = 2.5)
legend("topleft",legend = c("Courbe d'Estimation par noyau: Writing_score","Courbe de Gaussienne distribution"),
       col = c("red","blue"), lty = c(1,5), lwd = c(2,2.5), bty = 'n')

# Graphe Q-Q et P-P pour maths_score
par(mfrow = c(1,2))
qqnorm(math_score, xlab = "Vraie distribution", ylab = "Gaussienne distribution",
       main = "Q-Q plot pour Math_score", col = "blue")
qqline(math_score)

P <- pnorm(math_score, mean(math_score), sd(math_score))
cdf <- 0
for(i in 1:length(math_score)){cdf[i] <- sum(math_score <= math_score[i])/length(math_score)}
plot(cdf, P, xlab = 'Vraie distribution', ylab = 'Gaussienne distribution',
     main = 'P-P plot pour Math_score', xlim = c(0,1), ylim = c(0,1), col = 'blue')
abline(a = 0, b = 1)
par(opar)

# Graphe Q-Q et P-P pour reading_score
par(mfrow = c(1,2))
qqnorm(reading_score, xlab = "Vraie distribution", ylab = "Gaussienne distribution",
       main = "Q-Q plot pour Reading_score", col = "blue")
qqline(reading_score)

P <- pnorm(reading_score, mean(reading_score), sd(reading_score))
cdf <- 0
for(i in 1:length(reading_score)){cdf[i] <- sum(reading_score <= reading_score[i])/length(reading_score)}
plot(cdf, P, xlab = 'Vraie distribution', ylab = 'Gaussienne distribution',
     main = 'P-P plot pour Reading_score', xlim = c(0,1), ylim = c(0,1), col = 'blue')
abline(a = 0, b = 1)
par(opar)
# Graphe Q-Q et P-P pour writing_score
par(mfrow = c(1,2))
qqnorm(writing_score, xlab = "Vraie distribution", ylab = "Gaussienne distribution",
       main = "Q-Q plot pour Writing_score", col = "blue")
qqline(writing_score)

P <- pnorm(writing_score, mean(writing_score), sd(writing_score))
cdf <- 0
for(i in 1:length(writing_score)){cdf[i] <- sum(writing_score <= writing_score[i])/length(writing_score)}
plot(cdf, P, xlab = 'Vraie distribution', ylab = 'Gaussienne distribution',
     main = 'P-P plot pour Reading_score', xlim = c(0,1), ylim = c(0,1), col = 'blue')
abline(a = 0, b = 1)
par(opar)
#--------------------------------------------------------------------#
# collinearity diagnostics spss ---------------------------
corxx <- cbind(raw_data[,c(6:8)],CT,sch,sub,FC)
str(corxx)
XX <- cor(corxx)
kappa(XX,exact=TRUE) #exact=TRUE表示精确计算条件数

vif(lm_sol_log)

#Linear regression to predict math/reading/writing scores#

##Split the data into trainset and testset##

### 75% of the sample size
smp_size <- floor(0.75 * nrow(raw_data))

### set the seed to make the partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(raw_data)), size = smp_size)

train <- raw_data[train_ind, ]
test <- raw_data[-train_ind, ]


model_math <- lm(math_score ~ writing_score + reading_score + gender + race + lunch + parental_level_edu + test_prep_course,data=train)
summary(model_math) # p_value < 0.05???R-squared = 0.87 the higher the R-squared, the better the model fits the data.(0-1)

Pred_math <- predict(model_math,test,interval = "prediction", level=0.95)
summary(Pred_math)

test1 <- cbind(test, Pred_math)
test1
