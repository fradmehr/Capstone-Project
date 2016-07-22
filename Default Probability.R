### SlideRule Foundations of Data  Science
### Machine Learning (Classification)
library(ISLR)
library(MASS)
Default <- read.csv(file.choose())
attach(Default)
Default$X2 <- as.factor(Default$X2)
Default$X3 <- as.factor(Default$X3)
Default$X4 <- as.factor(Default$X4)
Default$X6 <- as.factor(Default$X6)
Default$X7 <- as.factor(Default$X7)
Default$X8 <- as.factor(Default$X8)
Default$X9 <- as.factor(Default$X9)
Default$X10 <- as.factor(Default$X10)
Default$X11 <- as.factor(Default$X11)
Default$Y <- as.factor(Default$Y)

dim(Default)
str(Default)
summary(Default)

set.seed(999) 
test <- sample(nrow(Default),0.3*nrow(Default))  
data.train <- Default[-test,]  
data.test <- Default[test,]   

nrow(data.train)
nrow(data.test)
# we can remove X10 or exclude (not remove) it from training data. the result will be same
glm.1 <- glm(Y~., data= data.train[,!colnames(data.train) %in% c("X10")],family=binomial)
glm.probs <- predict(glm.1,data.test, type="response") 

# Convert probabilities into predicted class:
glm.pred <- rep(0, 9000)
glm.pred[glm.probs > 0.5] = 1 # Confusion Matrix
table(data.test$Y,glm.pred)

#     0    1
#  0 6656  336
#  1 1294  714
#         predicted
# actual   No  Yes  |
#  No      TN   FP  | N
#  Yes     FN   TP  | P
#         _________ |
#          N*   P*  |

# False Positive Rate: FP/N; Type I Error; 1-specificity
# True Positive Rate: TP/P;  1-Type II Error; power/sensitivity;  recall
# Positive Pred value: TP/P*; Precision
# Negative Pred value: TN/N*
# Accuracy: (TN+TP)/(N+P)

# FP Rate: 336/(336+6656) = 0.048
# TP Rate: 714/(714+1294) = 0.356
# Pos. Pred value = 714/(714+336) = 0.68
# Neg. Pred value = 6656/(6656+1294) = 0.84
# Accuracy = (6656+714)/9000 = 0.82

# the code to remove X10 from training data
# glm.1 <- glm(Y~.-X10, data= data.train,family=binomial)
# glm.1$xlevels[["X10"]] <- union(glm.1$xlevels[["X10"]], levels(data.test$X10))


