library(dplyr)
library(pROC)
library(class)

set.seed(10)

dat.infarc <- read.table("./SlidesCode_Simon/code/infarc-data/infarcts.txt", header = T)

dat.infarc.use <- dat.infarc %>% 
  select(infarcts, age, educ, income, weight, height, packyrs, alcoh, chd, claud, htn, diabetes, ldl, crt) %>% 
  na.omit()

train.ind <- sample(1:nrow(dat.infarc.use), floor(nrow(dat.infarc.use)/2))

dat.train <- dat.infarc.use[train.ind,]
dat.test <- dat.infarc.use[-train.ind,]

fit <- glm(infarcts~ .,
           family = binomial(),
           data = dat.train)

#### Evaluating Fit

preds <- predict(fit, dat.test, type = "response")

roc.glm <- roc(dat.test$infarcts, preds)

plot(roc.glm, legacy.axes = TRUE, print.auc = TRUE)

### Evaluating calibration

smoother <- loess(dat.test$infarcts ~ preds, span = 0.6, degree = 1)

plot(preds, predict(smoother), xlim = c(0,1), ylim = c(0,1), xlab = "predicted probs", ylab = "smoothed prob")
abline(0,1, col = "red")

#### Fitting a knn model

preds.35 <- knn(dat.train %>% select(-infarcts),
    dat.test %>% select(-infarcts),
    as.factor(dat.train$infarcts),
    k = 35, prob = TRUE)

#### Evaluating Fit

roc.knn35 <- roc(dat.test$infarcts, attributes(preds.35)$prob)

plot(roc.knn35, legacy.axes = TRUE, print.auc = TRUE)

### Evaluating calibration

knn35.use <- 1-attributes(preds.35)$prob

smoother <- loess(dat.test$infarcts ~ knn35.use, span = 0.6, degree = 1)

plot(knn35.use, predict(smoother), xlim = c(0,1), ylim = c(0,1), xlab = "predicted probs", ylab = "smoothed prob")
abline(0,1, col = "red")

### Selecting the number of neighbors

eval.knn <- list()

for(num.k in 1:20){
  preds.knn <- knn(dat.train %>% select(-infarcts),
                  dat.test %>% select(-infarcts),
                  as.factor(dat.train$infarcts),
                  k = 50 + num.k*20, prob = TRUE)
  eval.knn[num.k] <- roc(dat.test$infarcts, attributes(preds.knn)$prob)$auc
}

unlist(eval.knn)

num.k.use <- which.min(unlist(eval.knn)) * 20 + 50

preds.knn <- knn(dat.train %>% select(-infarcts),
                 dat.test %>% select(-infarcts),
                 as.factor(dat.train$infarcts),
                 k = num.k.use, prob = TRUE)

## Evaluating calibration

preds.knn.use <- 1-attributes(preds.knn)$prob ## flipping things because knn flipped class label

smoother <- loess(dat.test$infarcts ~ preds.knn.use, span = 0.6, degree = 1)

plot(preds.knn.use, predict(smoother), xlim = c(0,1), ylim = c(0,1), xlab = "predicted probs", ylab = "smoothed prob")
abline(0,1, col = "red")
