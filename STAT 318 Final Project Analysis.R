### STAT 318 FINAL PROJECT
### LAN DAU, ELIZABETH KIEL, TORI STIEGMAN

# --------------------------------------------------------------
## LOAD DATA & FIT FULL MODEL

equality.dat = read.csv('/Users/toristiegman/Documents/Wellesley/2022-2023/spring2023/stat318/project/finalProject.csv')
equality.dat$EmployDiscrimination = as.factor(equality.dat$EmployDiscrimination)
attach(equality.dat)

# Full model
equality.mod.noLog = lm(HappinessScore~GDI+LifeExDiff+FemaleParliament+MaternalMort+SchoolingDiff+EmployDiscrimination)
summary(equality.mod.noLog)

# --------------------------------------------------------------
## CHECK FOR MULTI-COLLINEARITY
library('car')
vif(equality.mod)

# No values are above 10 -> no multicolinearity!

# --------------------------------------------------------------
## CHECK LINEAR REGRESSION ASSUMPTIONS

# Linear Regression Assumptions
par(mfrow=c(4,2))

# 1) Linearity of response
plot(x=GDI, y=HappinessScore)
plot(x=LifeExDiff, y=HappinessScore)
plot(x=FemaleParliament, y=HappinessScore)
plot(x=MaternalMort, y=HappinessScore) # not linear... log it!
plot(x=log(MaternalMort), y=HappinessScore)
plot(x=SchoolingDiff, y=HappinessScore)
plot(x=EmployDiscrimination, y=HappinessScore)

# refit equality.mod to reflect logging maternal mortality
equality.mod = lm(HappinessScore~GDI+LifeExDiff+FemaleParliament+log(MaternalMort)+SchoolingDiff+EmployDiscrimination)
summary(equality.mod)


# 2) Independence of error

# 3) Normality of error
e = resid(equality.mod) # estimated residuals, yi - y_hat
y.hat = fitted(equality.mod)
par(mfrow=c(1,1))
hist(e, main='Histogram of the Residuals')

# 4) Homoskedasticity
# Residual plot: Residual vs Predicted Response
par(mfrow=c(1,1))
plot(e~y.hat,
     ylab='Residuals', xlab='Predicted Happiness',
     main = 'Residuals vs Predicted Happiness',
     pch=19, cex=0.25)
abline(h=0, col='red')

# --------------------------------------------------------------
## VARIABLE SELECTION

## SUBJECTIVE PROCEDURES
library(leaps)

# Create explanatory and response variables
x = cbind(GDI,LifeExDiff,FemaleParliament,log(MaternalMort),SchoolingDiff,EmployDiscrimination)
y = HappinessScore

# Subjective procedures
# Mallow's Cp
results_Cp = leaps(x, y, int = TRUE, method=c('Cp'))
min_Cp = which.min(results_Cp$Cp)
results_Cp$which[min_Cp,]
# R2
results_r2 = leaps(x, y, int = TRUE, method=c('r2'))
max_r2 = which.max(results_r2$r2)
results_r2$which[max_r2,]
# Adjusted R2
results_adjr2 = leaps(x, y, int = TRUE, method=c('adjr2'))
max_adjr2 = which.max(results_adjr2$adjr2)
results_adjr2$which[max_adjr2,]

# Step-wise Regression
step.mod = step(equality.mod, direction="both")
summary(step.mod)

## ----- nLOOCV to choose a model ----- ##
library(caret) ; library(ModelMetrics)

set.seed(2110) # set seed for reproducability
k = 5 # specify number of folds
y = HappinessScore
folds = createFolds(y, k = k)

## Initialize results vectors for each type of model
R2.results = c()
adjR2.results = c()
Cp.results = c()
both.results = c()

# Loop
for(i in 1:k){
  # Split the data into training and validation sets based on folds
  train = equality.dat[-folds[[i]],] 		# training data
  validate = equality.dat[folds[[i]],] 		# test data
  
  # Fit the four models on training set
  R2.fitmod = lm(HappinessScore ~ GDI + LifeExDiff + FemaleParliament + log(MaternalMort) + SchoolingDiff + EmployDiscrimination)  
  adjR2.fitmod = lm(HappinessScore ~ GDI + LifeExDiff + FemaleParliament + log(MaternalMort) + SchoolingDiff)  
  Cp.fitmod = lm(HappinessScore ~ GDI + LifeExDiff + log(MaternalMort) + SchoolingDiff + EmployDiscrimination)
  both.fitmod = lm(HappinessScore ~ GDI + LifeExDiff + log(MaternalMort) + SchoolingDiff)
  
  # Make predictions using validation set (predict() function)
  R2.predictions = predict(R2.fitmod, newdata = validate)
  adjR2.predictions = predict(adjR2.fitmod, newdata = validate)
  Cp.predictions = predict(Cp.fitmod, newdata = validate)
  both.predictions = predict(both.fitmod, newdata = validate)
  
  # Calculate accuracy
  R2.pred.err = (1/length(folds[[i]]))*sum((HappinessScore[folds[[i]]] - R2.predictions)^2)
  R2.sqrt.pred.err = sqrt(R2.pred.err)
  
  adjR2.pred.err = (1/length(folds[[i]]))*sum((HappinessScore[folds[[i]]] - adjR2.predictions)^2)
  adjR2.sqrt.pred.err = sqrt(adjR2.pred.err)
  
  
  Cp.pred.err = (1/length(folds[[i]]))*sum((HappinessScore[folds[[i]]] - Cp.predictions)^2)
  Cp.sqrt.pred.err = sqrt(Cp.pred.err)
  
  
  both.pred.err = (1/length(folds[[i]]))*sum((HappinessScore[folds[[i]]] - both.predictions)^2)
  both.sqrt.pred.err = sqrt(both.pred.err)
  
  # Store results
  R2.results = c(R2.results, R2.sqrt.pred.err)
  adjR2.results = c(adjR2.results, adjR2.sqrt.pred.err)
  Cp.results = c(Cp.results, Cp.sqrt.pred.err)
  both.results = c(both.results, both.sqrt.pred.err)
} 

# Calculate the average root mean squared error
print("R2 avg root mean square error: " ); mean(R2.results) # 0.6546928
print("adjR2 avg root mean square error: "); mean(adjR2.results) # 0.654709
print("Cp avg root mean square error: "); mean(Cp.results) # 0.6604927
print("both avg root mean square error: "); mean(both.results) # 0.6605547

# The lowest RMSE is from the R2 model meaning that we will choose that model to continue forward with. 
final.mod = lm(HappinessScore ~ GDI + LifeExDiff + FemaleParliament + log(MaternalMort) + SchoolingDiff + EmployDiscrimination)    

# --------------------------------------------------------------
##  OUTLIERS, LEVERAGE, AND INFLUENTIAL POINTS
# Diagnostic plots to identify outliers/influential points
par(mfrow=c(2,2))
plot(final.mod)

# Remove influential points
equality.dat.trim = equality.dat[-c(1, 2, 22, 112),]

# Refit model
refit.mod = lm(HappinessScore~GDI+LifeExDiff+FemaleParliament+MaternalMort+
                 SchoolingDiff+EmployDiscrimination, dat = equality.dat.trim)

summary(refit.mod)
