library(dplyr)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
#install.packages("randomForest")
library(randomForest)
library(ranger)
#install.packages("gbm")
library(gbm)

dat <- read.csv("./SlidesCode_Simon/code/Cancer_Registry.csv")
dat <- dat %>% select(-PctSomeCol18_24, -PctEmployed16_Over, -PctPrivateCoverageAlone)
colnames(dat)
hist(dat$TARGET_deathRate)

train.ind <- sample(1:nrow(dat), floor(nrow(dat)/2))

dat.train <- dat[train.ind,]
dat.test <- dat[-train.ind,]

### Fitting a single tree

fit <- rpart(formula = TARGET_deathRate ~ ., 
             data = dat.train %>% select(-Geography),
             control = rpart.control(cp = 0.005))

cpTab <- printcp(fit)
plotcp(fit)

minErr <- which.min(cpTab[,4])
tree.best <- prune(fit, cp = cpTab[minErr,1])

good_inds <- which(cpTab[,4]<cpTab[minErr,4]+cpTab[minErr,5])
min_complexity_ind <- good_inds[1]
tree.1se <- prune(fit, cp = cpTab[min_complexity_ind,1])

rpart.plot(tree.1se)

rpart.plot(tree.best)

preds.1se <- predict(tree.1se, dat.test)
preds.best <- predict(tree.best, dat.test)

(MSE.1tree.1se <- mean((preds.1se - dat.test$TARGET_deathRate)^2))
(MSE.1tree.1best <- mean((preds.best - dat.test$TARGET_deathRate)^2))

#### Fitting a random forest

fit.rf <- randomForest(TARGET_deathRate ~ ., 
                       data = dat.train %>% select(-Geography),
                       mtry = 5)
fit.ranger <- ranger(TARGET_deathRate ~ ., 
                       data = dat.train %>% select(-Geography),
                       mtry = 5)

preds.rf <- predict(fit.rf, dat.test)
preds.ranger<- predict(fit.ranger, dat.test)$predictions

(MSE.rf <- mean((preds.rf - dat.test$TARGET_deathRate)^2))
(MSE.ranger <- mean((preds.ranger - dat.test$TARGET_deathRate)^2))


##### Boosting
set.seed(1)
fit.gbm.1 <- gbm(TARGET_deathRate ~ ., 
               data = dat.train %>% select(-Geography),
               distribution = "gaussian",
               n.trees = 10000,
               interaction.depth = 1,
               shrinkage = 0.01,
               cv.folds = 10)

fit.gbm.3 <- gbm(TARGET_deathRate ~ ., 
                 data = dat.train %>% select(-Geography),
                 distribution = "gaussian",
                 n.trees = 10000,
                 interaction.depth = 1,
                 shrinkage = 0.01,
                 cv.folds = 10)

best.fit.gbm.1 <- gbm.perf(fit.gbm.1, method = "cv")
best.fit.gbm.3 <- gbm.perf(fit.gbm.3, method = "cv")

preds.gbm.1 <- predict(fit.gbm.1, dat.test, ntree = best.fit.gbm.1)
preds.gbm.3 <- predict(fit.gbm.3, dat.test, ntree = best.fit.gbm.3)

(MSE.gbm.1 <- mean((preds.gbm.1 - dat.test$TARGET_deathRate)^2))
(MSE.gbm.3 <- mean((preds.gbm.3 - dat.test$TARGET_deathRate)^2))


##### Trying boosting on the infarct data from before!
library(pROC)
set.seed(10)

dat.infarc <- read.table("./SlidesCode_Simon/code/infarc-data/infarcts.txt", header = T)

dat.infarc.use <- dat.infarc %>% 
  select(infarcts, age, educ, income, weight, height, packyrs, alcoh,
         chd, claud, htn, diabetes, ldl, crt) %>% 
  na.omit()

train.ind <- sample(1:nrow(dat.infarc.use), floor(nrow(dat.infarc.use)/2))

dat.train <- dat.infarc.use[train.ind,]
dat.test <- dat.infarc.use[-train.ind,]

fit.gbm.1 <- gbm(infarcts ~ ., 
                 data = dat.train,
                 distribution = "bernoulli",
                 n.trees = 1000,
                 interaction.depth = 1,
                 shrinkage = 0.01,
                 cv.folds = 10)

best.fit.gbm.1 <- gbm.perf(fit.gbm.1, method = "cv")

preds.gbm.1 <- predict(fit.gbm.1, dat.test, ntree = best.fit.gbm.1)

roc.gbm <- roc(dat.test$infarcts, preds.gbm.1)

plot(roc.gbm, legacy.axes = TRUE, print.auc = TRUE)

