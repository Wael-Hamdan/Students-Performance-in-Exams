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
