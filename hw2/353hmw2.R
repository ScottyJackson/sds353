uv = read.csv('Documents/homework/sds353/data/uv.csv')

# Preprocessing
uv$year = as.factor(uv$year)
uv$log_gdp = log(uv$gdp)

#----------------
#Question 1
#----------------
lm1 = lm(growth ~ underval + log_gdp, data = uv)
summary(lm1)

#----------------
#Question 2
#----------------
lm2 = lm(growth ~ underval + log_gdp + country + year, data = uv)
summary(lm2)

yearcoefs = coef(summary(lm2))[182:190,"Estimate"]

years = as.vector(sort(unique(uv$year))[-1])

plot(yearcoefs~years, type='p', pch=20, cex=0.9, col="cornflowerblue",
     main="Coefficients vs Year", xlab="Coefficient", ylab="Year") + 
grid()

#----------------
#Question 3
#----------------

summary(lm1)
summary(lm2)

cv.lm <- function(data, formulae, nfolds = 5) {
  data <- na.omit(data)
  formulae <- sapply(formulae, as.formula)
  n <- nrow(data)
  fold.labels <- sample(rep(1:nfolds, length.out = n))
  mses <- matrix(NA, nrow = nfolds, ncol = length(formulae))
  colnames <- as.character(formulae)
  for (fold in 1:nfolds) {
    test.rows <- which(fold.labels == fold)
    train <- data[-test.rows, ]
    test <- data[test.rows, ]
    for (form in 1:length(formulae)) {
      current.model <- lm(formula = formulae[[form]], data = train)
      predictions <- predict(current.model, newdata = test)
      test.responses <- eval(formulae[[form]][[2]], envir = test)
      test.errors <- test.responses - predictions
      mses[fold, form] <- mean(test.errors^2)
    }
  }
  return(colMeans(mses))
}

cv.lm(uv, c(lm1$call[[2]], lm2$call[[2]]), nfolds = nrow(uv))

#----------------
#Question 4
#----------------
library(np)

# (a)
kreg = npreg(growth ~ underval + log_gdp + country + year, 
             tol=1e-3, 
             ftol=1e-4, 
             data=uv)

# (b)
plot(x=predict(lm2), y=predict(kreg),
     col='cornflowerblue', pch=20, 
     main="Linear vs Kernel Regression Predictions", 
     xlab="Linear", ylab="Kernel") + 
abline(0,1) + grid()

# (c) 
plot(x=predict(kreg), y=resid(kreg),
     col='cornflowerblue', pch=20,
     main="Residuals vs Fitted Values", 
     xlab="Fitted", ylab="Residuals") +
grid() + 
abline(0,0) + 
abline(2*sd(resid(kreg)), 0, col='red') + 
abline(-2*sd(resid(kreg)), 0, col='red')

# (d)
kreg.MSE = kreg$MSE

#----------------
#Question 5
#----------------

make.preds = function(underval, gdp) {
  return(predict(kreg, 
    newdata=data.frame(underval=rep(underval,10), 
                       log_gdp=rep(log(gdp), 10), 
                       country=rep("TUR", 10), 
                       year=sapply(seq(1955,2000,5), as.factor))))
}

plot.preds = function(preds) {
  plot(x=seq(1955,2000,5), y=preds, col='cornflowerblue', pch=20,
       main="Predicted Growth Rate vs Year", 
       xlab="Year", ylab="Predicted Growth")
  grid()
}

# underval, log_gdp, country, year
# (a)
preds.a = make.preds(underval=0, gdp=1e4)
plot.preds(preds.a)

# (b)
preds.b = make.preds(underval=0.5, gdp=1e4)
plot.preds(preds.b)

# (c)
preds.c = make.preds(underval=0, gdp=1e3)
plot.preds(preds.c)

# (d)
preds.d = make.preds(underval=0.5, gdp=1e3)
plot.preds(preds.d)

# all in one plot

x = seq(1955,2000,5)
jitter = 0.5

leg.text = c("UVI: 0,    GDP: 1e4", "UVI: 0.5, GDP: 1e4", 
             "UVI: 0,    GDP: 1e3", "UVI: 0.5, GDP: 1e3")

plot(x=x+rnorm(10, sd=jitter), y=preds.a, pch=20, 
     main="Growth Predictions for Different Predictor Values",
     xlab="Year", ylab="Predicted Growth") +
points(x=x+rnorm(10, sd=jitter), y=preds.b, pch=20, col='red') +
points(x=x+rnorm(10, sd=jitter), y=preds.c, pch=20, 
       col='cornflowerblue') +
points(x=x+rnorm(10, sd=jitter), y=preds.d, pch=20, col='coral') +
grid() +
legend('bottomleft', 'groups', leg.text, pch=rep(20,4), 
       col=c('black', 'red', 'cornflowerblue', 'coral'), cex=0.5)

