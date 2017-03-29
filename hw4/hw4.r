nampd <- read.csv('Documents/homework/sds353/data/nampd.csv')
nampd$mass <- exp(nampd$ln_mass)
nampd$old_mass <- exp(nampd$ln_old_mass)

nampd.na <- na.omit(nampd)

# Question 1.
lm.fit <- lm(ln_mass ~ ln_old_mass, data=nampd.na)
preds <- predict(lm.fit, se=T)

png('q1.png', 1000, 1000, type='cairo')
par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
title('Linear Regression of new mass on mass', outer=T)

plot(nampd$ln_old_mass, nampd$ln_mass, 
     cex=.5,
     col='darkgrey',
     xlab='log(Ancestral)',
     ylab='log(New)')
lines(nampd.na$ln_old_mass, preds$fit, lwd=2, col='coral')
grid()

plot(nampd$old_mass, nampd$mass, 
     cex=.5,
     col='darkgrey',
     xlab='Ancestral',
     ylab='New',
     ylim=c(0,5e6))
lines(exp(nampd.na$ln_old_mass), exp(preds$fit), lwd=2, col='coral')
grid()
dev.off()


# Question 2.
x <- nampd.na$ln_old_mass
y <- nampd.na$ln_mass

spl.fit <- smooth.spline(x, y, cv=T)
main.curve <- eval.spl(nampd.spl)

mass.lims <- range(nampd.na$ln_old_mass)
mass.grid <- seq(from=masslims[1], to=masslims[2], by=0.3)

png('q2.png', 1000, 1000, type='cairo')
par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(nampd.na$ln_old_mass, nampd.na$ln_mass, 
     cex=.5,
     col='darkgrey')
lines(spl.fit, col='forestgreen', lwd=2)

plot(nampd.na$old_mass, nampd.na$mass, 
     cex=.5,
     col='darkgrey')
lines(exp(spl.fit$x), exp(spl.fit$y), col='forestgreen', lwd=2)
dev.off()

resample <- function(x) {
  sample(x, size=length(x), replace=TRUE)
}

boot.ci <- function(B, t.hat, alpha, simulator, statistic) {
  t.boot <- replicate(B, statistic(simulator()))
  ci.lower <- 2*t.hat - apply(t.boot, 1, quantile, probs=1-alpha/2)
  ci.upper <- 2*t.hat - apply(t.boot, 1, quantile, probs=alpha/2)
  cis <- rbind(ci.lower, ci.upper)
  return(cis)
}

boot.sd <- function(B, simulator, statistic) {
  sd.boot <- replicate(B, statistic(simulator()))
  sd.points <- apply(sd.boot, 1, sd)
  return(sd.points)
}

resample.residuals <- function() {
  new.frame <- nampd.na
  new.ln_mass <- fitted(nampd.spl) + resample(resid(nampd.spl))
  new.frame$ln_mass <- new.ln_mass
  return(new.frame)
}

resample.cases <- function() {
  sample.rows <- resample(1:nrow(nampd.na))
  return(nampd.na[sample.rows,])
}

nampd.spl.fit <- function(data) {
  fit <- smooth.spline(data$ln_old_mass, data$ln_mass)
  return(fit)
}

eval.spl <- function(spl) {
  return(predict(spl, x=nampd.na$ln_old_mass)$y)
}

nampd.spl.stat <- function(data) {
  return(eval.spl(nampd.spl.fit(data)))
}

spl.resid.ci <- boot.ci(B=1000, 
                        t.hat=main.curve,
                        alpha=.05,
                        simulator=resample.residuals,
                        statistic=nampd.spl.stat)

df <- data.frame(cbind(nampd.na$old_mass, 
                       exp(spl.resid.ci[1,]), 
                       exp(spl.resid.ci[2,])))
colnames(df) <- c('old_mass', 'ci_lower', 'ci_upper')
df <- df[order(df$old_mass),]

plot(nampd.na$old_mass, nampd.na$mass, 
     cex=.5,
     col='darkgrey')
lines(exp(nampd.spl$x), exp(nampd.spl$y), col='forestgreen', lwd=2)
lines(df$old_mass, df$ci_upper, col='forestgreen', lty=3)
lines(df$old_mass, df$ci_lower, col='forestgreen', lty=3)

spl.cases.se <- boot.sd(B=1000, 
                        simulator=resample.cases, 
                        statistic=nampd.spl.stat)

df <- data.frame(cbind(nampd.na$old_mass, 
                       exp(main.curve - 2*spl.cases.se), 
                       exp(main.curve + 2*spl.cases.se)))
colnames(df) <- c('old_mass', 'se_lower', 'se_upper')
df <- df[order(df$old_mass),]

plot(nampd.na$old_mass, nampd.na$mass, 
     cex=.5,
     col='darkgrey')
lines(exp(nampd.spl$x), exp(nampd.spl$y), col='forestgreen', lwd=2)
lines(df$old_mass, df$se_lower, col='forestgreen', lty=3, lwd=2)
lines(df$old_mass, df$se_upper, col='forestgreen', lty=3, lwd=2)

# Question 3
x.min <- 1.8
x.max <- 1e15

rmass <- function(Xa, r, sigma) {
  interp <- predict(r, data.frame(x=log(Xa)))$y
  Xd <- exp(interp + rnorm(1, 0, sigma.2))
  
  while (Xd > x.max || Xd < x.min) {
    Xd <- exp(interp + rnorm(1, 0, sigma))
  }
  
  return(Xd)
}

test.grid <- seq(x.min, x.max, length.out=150)
plot(data$old_mass, data$mass, 
     cex=.5,
     col='darkgrey')
lines(test.grid, exp(predict(spl.fit, data.frame(t=log(test.grid)))$y$t), col='forestgreen', lwd=2)
lines(test.grid, rmass(test.grid, spl.fit, 1)$x, col='pink')

