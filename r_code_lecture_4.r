# code based on the files "Code regarding section 3.2 (copyright 2003, 2004, 2012 A.Azzalini and B.Scarpa)"
# and "Code regarding section 3.3-3.5" (copyright 2003, 2004, 2012 A.Azzalini and B.Scarpa)

# read data 
dataset <- read.table("http://azzalini.stat.unipd.it/Book-DM/yesterday.dat", header = TRUE)
attach(dataset)
n <- nrow(dataset)
y.range <- range(c(y.yesterday, y.tomorrow))

# define f(x) as polynomials
polinomio <- function(p) if(p == 0) lm(y.yesterday ~ 1) else lm(y.yesterday ~ poly(x, p))
polinomi <- apply(cbind(0:23), 1, polinomio)

# grid to plot curves
x0 <- data.frame(x = sort(unique(c(x, seq(min(x), max(x), length = 200)))))


### training set ###

# figure 3.1
plot(x, y.yesterday, ylim = y.range, pch = 16, ylab = "y", cex = 2)

# figure 3.2
# a
plot(x, y.yesterday, ylim = y.range, col = 2, pch = 16, xlab = "",
     main = "data and 3rd degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[4]], newdata = x0), lwd = 3)

# b
plot(x, y.yesterday, ylim = y.range, col = 2, pch = 16, xlab = "",
     main = "data and 6th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[7]], newdata = x0), lwd = 3)

# c
plot(x, y.yesterday, ylim = y.range, col = 2, pch = 16, xlab = "",
     main = "data and 12th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[13]], newdata = x0), lwd = 3)

# d
plot(x, y.yesterday, ylim = y.range, col = 2, pch = 16,  xlab = "",
     main = "data and 18th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[19]], newdata = x0), lwd = 3)

# e
plot(x, y.yesterday, ylim = y.range, col = 2, pch = 16,  xlab = "",
     main = "data and 24th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[24]], newdata = x0), lwd = 3)


# f
# plot(x, y.yesterday ,ylim = y.range, col = 2, pch = 16, 
#      main = "Data and (n-1)th Degree Polynomial")
# lines(x0$x, predict(polinomi[[n]], newdata = x0))
# x.est <- sapply(1:n, function(x, data) data^x, data = x)
# it does not work, look at the picture in the book instead (taken with an older version of R)

# all in one
par(mfrow = c(2, 2))
plot(x, y.yesterday, ylim = y.range, col = 2, pch = 16, xlab = "",
     main = "data and 3rd degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[4]], newdata = x0), lwd = 3)
plot(x, y.yesterday, ylim = y.range, col = 2, pch = 16, xlab = "",
     main = "data and 6th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[7]], newdata = x0), lwd = 3)
plot(x, y.yesterday, ylim = y.range, col = 2, pch = 16, xlab = "",
     main = "data and 12th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[13]], newdata = x0), lwd = 3)
plot(x, y.yesterday, ylim = y.range, col = 2, pch = 16,  xlab = "",
     main = "data and 24th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[24]], newdata = x0), lwd = 3)
par(mfrow = c(1, 1))


# figure 3.3a
dev <- as.numeric(lapply(polinomi, deviance))
plot(0:(23), dev, type = "b", xlab = "p", ylab = "Deviance", ylim = c(0,max(dev)), cex = 2, lwd = 2)

# figure 3.3b
R2 <- function(obj) summary(obj)$r.squared
r2 <-  as.numeric(lapply( polinomi, R2))
plot(0:(23), r2, type="b", xlab="p", ylab=expression(R^2), cex = 2, lwd = 2)



### test set ###

# figure 3.4a
plot(x, y.tomorrow, ylim = y.range, col = 4, pch = 17, xlab = "",
     main = "data and 3rd degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[4]], newdata = x0), lwd = 3)

# figure 3.4b
plot(x, y.tomorrow, ylim = y.range, col = 4, pch = 17, xlab = "",
     main = "data and 6th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[7]], newdata = x0), lwd = 3)

# figure 3.4c
plot(x, y.tomorrow, ylim = y.range, col = 4, pch = 17, xlab = "",
     main = "data and 12th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[13]], newdata = x0), lwd = 3)

# figure 3.4d
plot(x, y.tomorrow, ylim = y.range, col = 4, pch = 17, xlab = "",
     main = "data and 18th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[19]], newdata=x0), lwd = 3)

# figure 3.4e
plot(x, y.tomorrow, ylim = y.range, col = 4, pch = 17, xlab = "",
     main = "data and 24th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[25]], newdata = x0), lwd = 3)

# figure 3.4f
# plot(x, y.tomorrow, ylim = y.range, col = 4, pch = 17, xlab = "",
#      main="data and (n-1)th degree polynomial", font.main=1, cex = 2)
# lines(x0$x, predict(polinomi[[n]], newdata=x0), lwd = 3)
# it does not work, look at the picture in the book instead (taken with an older version of R)


# all in one
par(mfrow = c(2, 2))
plot(x, y.tomorrow, ylim = y.range, col = 4, pch = 17, xlab = "",
     main = "data and 3rd degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[4]], newdata = x0), lwd = 3)
plot(x, y.tomorrow, ylim = y.range, col = 4, pch = 17, xlab = "",
     main = "data and 6th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[7]], newdata = x0), lwd = 3)
plot(x, y.tomorrow, ylim = y.range, col = 4, pch = 17, xlab = "",
     main = "data and 12th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[13]], newdata = x0), lwd = 3)
plot(x, y.tomorrow, ylim = y.range, col = 4, pch = 17, xlab = "",
     main = "data and 24th degree polynomial", font.main = 1, cex = 2)
lines(x0$x, predict(polinomi[[25]], newdata = x0), lwd = 3)
par(mfrow = c(1, 1))


# figure 3.5a
y.hat <- lapply( polinomi, predict)
devianza <- function( y.hat) sum((y.tomorrow - y.hat)^2)
dev2 <- as.numeric(lapply(y.hat, devianza))
plot(0:23, dev2, type = "b", xlab = "p", ylab = "deviance", ylim=c(0,max(dev)), cex = 2, lwd = 2)


# figure 3.5b
r2 <- 1 - dev2 / dev[1]
plot(0:23, r2, type = "b", xlab = "p", ylab = expression(R^2), ylim = c(0, 1), cex = 2, lwd = 2)


polynomial <- function(p) lm(y.yesterday ~ poly(x,p))
#it only works until polynomial of degree 23 (up to 29)
polynomials <- apply(cbind(1:23), 1, polynomial)
error.train <- sapply(polynomials, deviance)

rss <- function(p) sum((y.tomorrow - predict(polynomials[[p]]))^2)
error.test <- apply(cbind(1:23), 1, rss)

out <- cbind(1:23, error.train, error.test)
colnames(out) <- c('complexity', 'train.error', 'test.error')
out


# bias-variance trade-off
rss60<- function(p) sum((y.tomorrow - predict(polynomials[[p]]))^2)+
  sum((y.yesterday - predict(polynomials[[p]]))^2)
Error <- apply(cbind(1:23), 1, rss)

source("f_true.R")

polo.f.true <- function(p) lm(f.true ~ poly(x,p))
poli.f.true <- apply(cbind(1:23), 1, polo.f.true)

bias2 <- function(p) mean((fitted(poli.f.true[[p]]) - f.true)^2)
bias2.oss <- apply(cbind(1:23), 1, bias2)
var.oss <- sqm.true^2 * (1:23)/30

err <- bias2.oss + var.oss


# figure 3.6
plot(1:23, err, type = "b", cex = 2, pch = 16,
     xlab = "Model complexity", ylab = "Error")

# figure 3.7
plot(bias2.oss, type = "b", col = 4, ylab = "Error",
     xlab = "Model complexity", cex = 2, pch = 16)
lines(var.oss, type = "b", col = 2, cex = 2, pch = 17)
lines(err, type = "b", cex = 2, pch = 18)
legend('topright', legend = c("Bias^2", "Variance", "Total"), 
       col = c(4, 2, 1), pch = c(16, 17, 18))


# figure 3.8
plot(error.train, type = "b", col = 2, 
     xlab = "Model complexity", ylab = "Deviance", cex = 2, pch = 16)
lines(error.test, type = "b",col = 4, cex = 2, pch = 17)
text(x = c(19, 19), y = c(0.0015, 0.0065), c("Yesterday data", "Tomorrow data"))



### cross-validation ###
# it only works until polynomial of degree 23 (up to 29)

# figure 3.9
y <- c(y.yesterday, y.tomorrow)
xx <- c(x, x)

RSS <- numeric(23) 
for (p in 0:22) {
  m <- lm(y ~ poly(xx, p+1), x = TRUE)
  X <- m$x
  P <- X %*% solve(t(X)%*% X) %*% t(X) # projection matrix
  p.ii <- diag(P)
  RSS.pp <- sum(residuals(m)^2 / (1 - p.ii)^2)
  RSS.p <- 0
  RSS[p + 1] <- RSS.pp
}
plot(0:22, RSS, type = "b", xlab = "Model complexity", pch = 16,
     ylab = "Error", cex = 2, lwd = 3)

