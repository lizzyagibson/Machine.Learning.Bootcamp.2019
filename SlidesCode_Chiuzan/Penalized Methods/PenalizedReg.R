
######################################################################
#                     Machine Learning BootCamp                      #
#            Penalized Methods: Ridge, Lasso, Elastic Net            #
######################################################################



rm(list = ls())

library('dplyr')
library('reshape2')
library('ggplot2')
library('tidyverse')
library('corrplot')                    # For correlation matrices and plots
library('choroplethrMaps')             # For state regions

library('glmnet')
library('caret')


# Data Description

# The data for this project were aggregated from multiple sources including American Community Survey census.gov, clinicaltrials.gov, and cancer.gov. 
# The dataset includes several variables per county (USA) and the goal is to build a regression model that 'best' predicts cancer mortality (target_DeathRate). 

# Variable Description
# (a): Years 2010-2016
# (b): 2013 Census Estimates

# TARGET_deathRate: Dependent variable. Mean per capita (100,000) cancer mortalities (a)
# avgAnnCount: Mean number of reported cases of cancer diagnosed annually (a)
# avgDeathsPerYear: Mean number of reported mortalities due to cancer (a)
# incidenceRate: Mean per capita (100,000) cancer diagnoses (a)
# medianIncome: Median income per county (b)
# popEst2015: Population of county (b)  
# povertyPercent: Percent of population in poverty (b)
# studyPerCap: Per capita number of cancer-related clinical trials per county (a)
# binnedInc: Median income per capita binned by decile (b)
# MedianAge: Median age of county residents (b)
# MedianAgeMale: Median age of male county residents (b)
# MedianAgeFemale: Median age of female county residents (b)
# Geography: County name (b)
# AvgHouseholdSize: Mean household size of county (b)
# PercentMarried: Percent of county residents who are married (b)
# PctNoHS18_24: Percent of county residents ages 18-24 highest education attained: less than high school (b)
# PctHS18_24: Percent of county residents ages 18-24 highest education attained: high school diploma (b)
# PctSomeCol18_24: Percent of county residents ages 18-24 highest education attained: some college (b)
# PctBachDeg18_24: Percent of county residents ages 18-24 highest education attained: bachelor's degree (b)
# PctHS25_Over: Percent of county residents ages 25 and over highest education attained: high school diploma (b)
# PctBachDeg25_Over: Percent of county residents ages 25 and over highest education attained: bachelor's degree (b)
# PctEmployed16_Over: Percent of county residents ages 16 and over employed (b)
# PctUnemployed16_Over: Percent of county residents ages 16 and over unemployed (b)
# PctPrivateCoverage: Percent of county residents with private health coverage (b)
# PctPrivateCoverageAlone: Percent of county residents with private health coverage alone (no public assistance) (b)
# PctEmpPrivCoverage: Percent of county residents with employee-provided private health coverage(b)
# PctPublicCoverage: Percent of county residents with government-provided health coverage (b)
# PctPubliceCoverageAlone: Percent of county residents with government-provided health coverage alone (b)
# PctWhite: Percent of county residents who identify as White (b)
# PctBlack: Percent of county residents who identify as Black (b)
# PctAsian: Percent of county residents who identify as Asian (b)
# PctOtherRace: Percent of county residents who identify in a category which is not White, Black, or Asian (b)
# PctMarriedHouseholds: Percent of married households (b)
# BirthRate: Number of live births relative to number of women in county (b)


# Read-in data

cancer_reg<-read.csv("O:\\Projects\\BERD_EDU\\MiniCourses\\MLWorkshop\\Slides_Cody\\Penalized\\Cancer_Registry.csv")
            
names(cancer_reg)

# 3047 rows associated with different counties and 34 variables
dim(cancer_reg)

# Summary statistics and missing values
cancer_reg %>%
  dplyr::select(TARGET_deathRate, everything(), -Geography, -binnedInc)%>%
  skimr::skim_to_wide() %>%
  knitr::kable(digits = 2)

# There are three variables with missing values: 
# PctEmployed16_Over, PctPrivateCoverageAlone, PctSomeCol18_24

# Data vizualization
# Outcome (TARGET_deathRate) distribution for all counties

cancer_reg %>% 
  ggplot(aes(x = TARGET_deathRate)) + 
  geom_histogram(aes(y = ..density..),  
                 binwidth = 2, colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "purple") +
  geom_vline(aes(xintercept = mean(TARGET_deathRate, na.rm = T)),   # Vertical red line for the mean death rate; ignore NA values 
             color = "red", linetype = "dashed", size = 1) +        # Overlay density plot
  labs(x = "Per Capita Deaths from Cancer (All Causes)",
       y = "Density",
   title = "Distribution of Per Capita Deaths (US Counties 2010-2016)"
  )

# Summarize and vizualize death rates by state
cancer_reg_state = cancer_reg %>% 
  # add state
  mutate(county_state = Geography) %>% 
  # separate an expression or vector of characters into multiple columns
  separate(Geography, c("county", "state"), ", ") 

# Box-plots by state
cancer_reg_state %>% 
  group_by(state, TARGET_deathRate) %>% 
  #arrange(mean) %>% 
  ggplot(aes(x = state, y = TARGET_deathRate)) + 
  geom_boxplot() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Cancer Mortality Rates by State",
    x = "US State",
    y = "Deaths per Capita (100,000)"
  )

# Group counties by region: Northeast, South, North Central, West
# States and regions are can be found in package choroplethrMaps ()

regions <- tibble(state = state.name, region = state.region)

cancer_reg_region <- cancer_reg %>%
  separate(Geography, into = c("county", "state"), sep = ", ") %>%
  left_join(regions, by = "state") %>%
  #Coding DC in Northeast
  mutate(region = replace_na(region, "Northeast"))

# Graphical display by region
cancer_reg_region %>%
  ggplot(aes(x = incidenceRate, y = TARGET_deathRate, color = region)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Incidence rate per capita (100,000)",
       y = "Cancer mortality per capita (100,000)")

# Box-plots by region
cancer_reg_region %>%
  ggplot(aes(y = TARGET_deathRate, x = region)) +
  geom_boxplot(color=c("purple", "blue", "green","orange")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "US Region",
       y = "Cancer mortality per capita (100,000)")


# Some data cleaning
# Remove non-informative variables or with lots of missing data 

# Median age has max values of >600yrs
summary(cancer_reg_region$MedianAge)
summary(cancer_reg_region$MedianAgeMale)
summary(cancer_reg_region$MedianAgeFemale)

cancer_reg_clean <- cancer_reg_region %>%  
                    dplyr::select(-c(PctEmployed16_Over, PctPrivateCoverageAlone, PctSomeCol18_24,
                                     binnedInc, MedianAge, avgDeathsPerYear, popEst2015, state, region, county))


# Examine and vizualize correlation values for all the remaining variables
# Notice redundancy and several large correlations (>0.80) b/w some variables: 
# PctMarried/PctMarriedHousehold, PovertyPercent/PublicCoverageAlone

par(mar=c(4,5,1,1))
cor(cancer_reg_clean)%>%  
   corrplot(method = "circle", type = "upper", diag=FALSE)

# Remove other redundant variables (same info) captured from different data sources
cancer_reg_reduced <-cancer_reg_clean %>%
                     dplyr::select(-c(PctMarriedHouseholds, PctPrivateCoverage,
                                      PctPublicCoverage, studyPerCap, avgAnnCount))

# Re-do the correlation matrix
par(mar=c(4,5,1,1))
cor(cancer_reg_reduced)%>%  
  corrplot(method = "circle", type = "upper", diag=FALSE)

#####################################################
#                 Ridge Regression                  #
#####################################################

# Use package glmnet() to fit Ridge regression

# Select the outcome/dependent variable
Y <- cancer_reg_reduced[, 1]

# Select the set of covariates
X <- as.matrix(cancer_reg_reduced[,-1])

# Try a grid of values for lambda: from 10^-2 to 10^10
lambda <- 10^seq(10,-2, length=100)

# Penalty term: Ridge alpha=0 (default); Lasso alpha=1 (default)

ridge_try<-glmnet(X, Y, alpha=0, lambda=lambda)
dim(coef(ridge_try))

# How to examine model coeff estimates for different values of lambda
# For example, get the lambda value and its corresponding coeffs on position 50

ridge_try$lambda[50]          
coef(ridge_try)[,50]

###########################################################
#          Choice of lambda: Cross Validation (CV)        #
###########################################################

# Split the training and testing sample: 50:50

set.seed(1)

train <- cancer_reg_reduced %>%
         sample_frac(0.5)

test <- cancer_reg_reduced %>%
        setdiff(train)

#dim(train)
#dim(test)

X_train <- model.matrix(TARGET_deathRate ~., train)[,-1]
 X_test <- model.matrix(TARGET_deathRate ~., test)[,-1]

Y_train <- train[,1]
 Y_test <- test[,1]


# Use build-in CV function; performs a 10-fold validation by default
# glmnet() generates it's own lambda sequence or you can declare your own

set.seed(2)
cv.out <- cv.glmnet(X_train,Y_train, alpha=0)

# Plot of training MSE as a function of lambda
par(mar=c(4,5,1,1))
plot(cv.out)

# cv.glmnet() object contains the mean cross-validation error (cvm),
# lambda min that gives the minimum cvm, etc.
cv.out
best.lambda <- cv.out$lambda.min
best.lambda                 # 2.078005

# Identify the MSE corresponding the 'best' lambda
cbind(cv.out$lambda, cv.out$cvm)
# This lambda corresponds to an MSE=388.866


# Next we fit a ridge regression model on the training set, and evaluate its MSE on the test set. 
ridge_mod<-glmnet(X_train, Y_train, alpha=0, lambda=lambda)
plot(ridge_mod)

# We use predict() function to get predictions for a test set, by replacing type="coefficients" with the newx argument.

ridge.pred <- predict(ridge_mod, s=best.lambda, newx=X_test)

#MSE
MSE_ridge <- mean((ridge.pred-Y_test)^2) # MSE from test data = 393.736
MSE_ridge

# Fit a Ridge regression using all observations and 'best' lambda value
# As expected, several small coeffs, but none are actually set to zero.

ridge_final<-glmnet(X, Y, alpha=0, lambda=best.lambda)
coef(ridge_final)

# Fraction of deviance explained
ridge_final$dev.ratio

# dev.ratio Ridge = 0.502
# Similar interpretation to R-squared: % variance explained in the outcome by the set of covariates.

#####################################################
#                 Lasso Regression                  #
#####################################################

# Use package glmnet() to fit Lasso regression: alpha=1

# Fit Lasso on the training set
lasso_mod <- glmnet(X_train, Y_train, alpha = 1, lambda = lambda)
plot(lasso_mod)

###########################################################
#          Choice of lambda: Cross Validation (CV)        #
###########################################################

# Split the training and testing sample: 50:50

set.seed(2)
cv.out <- cv.glmnet(X_train,Y_train, alpha=1)

# Plot of training MSE as a function of lambda
par(mar=c(4,5,1,1))
plot(cv.out)

best.lambda <- cv.out$lambda.min
best.lambda                 # 0.181

# Identify the MSE corresponding the 'best' lambda
cbind(cv.out$lambda, cv.out$cvm)
# This lambda corresponds to an MSE=388.876

# Next we fit a Lasso regression model on the training set, and evaluate its MSE on the test set. 
lasso_mod<-glmnet(X_train, Y_train, alpha=1, lambda=lambda)

# We use predict() function to get predictions for a test set, by replacing type="coefficients" with the newx argument.

lasso.pred <- predict(lasso_mod, s=best.lambda, newx=X_test)

#MSE
MSE_lasso <- mean((lasso.pred-Y_test)^2) # MSE from test data = 393.495
MSE_lasso

# Fit a Lasso regression using all observations and 'best' lambda value
lasso_final<-glmnet(X, Y, alpha=1, lambda=best.lambda)
coef(lasso_final)

# Fraction of deviance explained
# dev.ratio Laso = 0.503
# Similar interpretation to R-squared: % variance explained by non-zero variables variables
lasso_final$dev.ratio

# Ridge and Lasso generated similar MSEs and fractions of deviation explained.


#####################################################
#             Elastic Net Regression                #
#####################################################


# Use package caret() to automatically select the best tunning parameters alpha and lambda,
# after testing a range of possible values for both.

# Putting training and testing variables together
train_data <- cbind(X_train, TARGET_deathRate=Y_train)
test_data <- cbind(X_test, TARGET_deathRate=Y_test)

set.seed(3)
elasticnet <- train(TARGET_deathRate ~., data = train_data, method = 'glmnet',
              trControl = trainControl('cv', number = 10), tuneLength = 10)

plot(elasticnet$finalModel , xlab='Elastic Net Regularization')
coef(elasticnet$finalModel, elasticnet$bestTune$lambda)

# 'Get the 'best' tuning parameters
elasticnet$bestTune                                    # alpha=0.5   lambda=0.4353902

# MSE Elastic
prediction_elastic <- predict(elasticnet, test_data)
MSE_elastic <- mean((prediction_elastic - Y_test)^2)   # MSE from test data = 393.051
MSE_elastic

# Compare Ridge, Lasso, Elastic Net models
# Coefficients
res_ridge_lasso_elastic<- cbind(coef(ridge_final),coef(lasso_final), coef(elasticnet$finalModel, elasticnet$bestTune$lambda))
colnames(res_ridge_lasso_elastic) <- c("Ridge", "Lasso", "Elastic Net")
res_ridge_lasso_elastic

# Acccuracy
MSE <- c(MSE_ridge, MSE_lasso, MSE_elastic)
MSE

# Seem that elastic net provides the best accuracy, but overall the differences are smal b/w all three methods.
# Lasso regression performs better than ridge in scenarios with many noise predictors, but worse in the presence of correlated predictors.
# Elastic net, is a hybrid of the two, and performs well in all these scenarios.








