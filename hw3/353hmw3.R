sh = read.csv('Documents/homework/sds353/data/stock_history.csv')

#----------------
#Question 1
#----------------

model1 = lm(Return_10_fwd ~ Price, data = sh)
model2 = lm(Return_10_fwd ~ Earnings, data = sh)
model3 = lm(Return_10_fwd ~ Price + Earnings, data = sh)
model4 = lm(Return_10_fwd ~ Price + Earnings + Price:Earnings, data = sh)

summary(model1)
summary(model2)
summary(model3)
summary(model4)




library(stargazer)

stargazer(model1, model2, model3, model4, title="Initial Model Evaluation", column.labels = c("First Model", "Second Model", "Third Model", "Fourth Model"),
          model.numbers = TRUE, single.row=TRUE, header=FALSE, omit.stat="ser")


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

cv.lm(sh, c(model1$call[[2]], model2$call[[2]], model3$call[[2]], model4$call[[2]]), nfolds = 5)


#----------------
#Question 2
#----------------

sh$MAPE = sh$Price/sh$Earnings_10MA_back

fivenum(sh$MAPE)


model5 = lm(Return_10_fwd ~ MAPE, data = sh)
summary(model5)

cv.lm(sh, c(model1$call[[2]], model2$call[[2]], model3$call[[2]], model4$call[[2]], model5$call[[2]]), nfolds = 5)

#----------------
#Question 3
#----------------

model6 = lm(Return_10_fwd ~ 1/MAPE, data = sh)
summary(model6)

cv.lm(sh, c(model1$call[[2]], model2$call[[2]], model3$call[[2]], model4$call[[2]], model5$call[[2]], model6$call[[2]]), nfolds = 5)

#----------------
#Question 4
#----------------
sh$MAPEinv = 1/sh$MAPE

sh.na = na.omit(sh)

model.np = npreg(Return_10_fwd ~ MAPE,
                 regtype="ll",
                 bwmethod="cv.aic",
                 gradients=TRUE,
                 data=sh.na)

plot(Return_10_fwd ~ MAPE, data = sh, pch = 20,
     main="Returns vs MAPE", ylab="Returns", xlab="MAPE")
abline(model5, col='cornflowerblue', lwd = 2)
abline(model6, col='coral', lwd=2)
lines(sh$MAPEinv ~ sh$MAPE, col = 'seagreen3', lwd = 2)
lines(sh.na$MAPE[order(sh.na$MAPE)], 
      fitted(model.np)[order(sh.na$MAPE)], 
      col='darkorchid3', lwd=2)
legend('topright', 
       c("LM: MAPE", "LM: 1/MAPE", "Simple Model: 1/MAPE", "NP: MAPE"),
       col=c('cornflowerblue', 'coral', 'seagreen3', 'darkorchid3'),
       pch=rep(20,4), cex=0.75)


#----------------
#Question 5
#----------------

model7 = lm(Return_10_fwd ~ MAPE + MAPEinv, data = sh)
summary(model7)

model8 = lm(Return_10_fwd ~ MAPE + MAPEinv + sqrt(MAPE), data = sh)
summary(model8)

#----------------
#Question 6
#----------------

# (a)
confint(model6, level=0.9)

#(b)
lm.coef = function(formula, data, idx) {
  d = data[idx,]
  fit = lm(formula, data=d)
  return(coef(fit))
}

model6.boot = boot(data=sh, 
                   statistic=lm.coef, 
                   R=2000, 
                   formula=Return_10_fwd~MAPE)

boot.ci(model6.boot, type="bca", conf=0.9)

#----------------
#Question 7
#----------------
cv_bws_npreg <- function(x,y,bandwidths=(1:50)/50,nfolds=10) {
  require(np)
  n <- length(x)
  stopifnot(n > 1, length(y) == n)
  stopifnot(length(bandwidths) > 1)
  stopifnot(nfolds > 0, nfolds==trunc(nfolds))
  fold_MSEs <- matrix(0,nrow=nfolds,ncol=length(bandwidths))
  colnames(fold_MSEs) = bandwidths
  case.folds <- sample(rep(1:nfolds,length.out=n))
  for (fold in 1:nfolds) {
    train.rows = which(case.folds!=fold)
    x.train = x[train.rows]
    y.train = y[train.rows]
    x.test = x[-train.rows]
    y.test = y[-train.rows]
    for (bw in bandwidths) {
      fit <- npreg(txdat=x.train,tydat=y.train,
                   exdat=x.test,eydat=y.test,bws=bw)
      fold_MSEs[fold,paste(bw)] <- fit$MSE
    }
  }
  CV_MSEs = colMeans(fold_MSEs)
  best.bw = bandwidths[which.min(CV_MSEs)]
  return(list(best.bw=best.bw,CV_MSEs=CV_MSEs,fold_MSEs=fold_MSEs))
}

x = sh$MAPE
y = sh$Return_10_fwd
 rbws = cv_bws_npreg(x,y,bandwidths = (1:100)/200)
 
plot(x,y)
rhat = npreg(bws=rbws$best.bw, txdat = x, tydat = y)
lines(x[order(x)], fitted(rhat)[order(x)], col = 'red')
library(np)
kreg = npreg(Return_10_fwd ~ MAPE,data=sh)
MAPE.ord = order(sh$MAPE)
lines(sh$MAPE[MAPE.ord],fitted(kreg)[MAPE.ord], col='red')
plot(predict(kreg))
kreg
kreg.MSE = kreg$MSE
kreg.MSE



plot(Return_10_fwd ~ MAPE, data = sh, pch = 20)
lines(predict(model5), col = 'cornflowerblue', lwd = 2)
lines(predict(model6), col = 'coral', lwd = 2)
lines(sh$MAPEinv ~ sh$MAPE, col = 'seagreen3', lwd = 2)
lines(predict(kreg), col = 'darkorchid3', lwd = 2)

predict(model5)

plot(Return_10_fwd~resid(model6), data = sh)
resid(model6)
