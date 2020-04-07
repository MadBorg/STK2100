### logistic regression ###
bank <- read.csv("http://azzalini.stat.unipd.it/Book-DM/brazil.csv", header = TRUE)
summary(bank)
y <- ifelse(bank$satisfaction < 2.5, 'low', 'high')
# age.d for age dichotomous
age.d <- ifelse(bank$age <= 45, 'young', 'old')
n <- length(y)
table(age.d, y, dnn = c('age', 'satisfaction'))

# transform y in a 0-1 variable
y <- ifelse(bank$satisfaction < 2.5, 0, 1)

# compute manually coefficients
# separately
pi.old.hat <- sum(y[age.d == 'old']) / sum(age.d == 'old')
pi.old.hat
pi.young.hat <- sum(y[age.d == 'young']) / sum(age.d == 'young')
pi.young.hat
# under H_0: pi_old = pi.young
pi.common.hat <- sum(y) / n
pi.common.hat

# standard errors
se.pi.old.hat <- sqrt(pi.old.hat * (1 - pi.old.hat) / sum(age.d == 'old'))
se.pi.old.hat
se.pi.young.hat <- sqrt(pi.young.hat * (1 - pi.young.hat) / sum(age.d == 'young'))
se.pi.young.hat
# under H_0: pi_old = pi.young
se.pi.common.hat <- sqrt(pi.common.hat * (1 - pi.common.hat) / n)
se.pi.common.hat

# compute the likelihood ratio statistics test
llik_pi.old.hat_pi.young.hat <- sum(dbinom(y[age.d == 'old'], 1, pi.old.hat, log = TRUE)) +
                                sum(dbinom(y[age.d == 'young'], 1, pi.young.hat, log = TRUE))
llik_pi.old.hat_pi.young.hat
llik_pi.common.hat <- sum(dbinom(y, 1, pi.common.hat, log = TRUE))
llik_pi.common.hat
w <- 2 * (llik_pi.old.hat_pi.young.hat - llik_pi.common.hat)
w

# compute the p-value
1 - pchisq(w, df = 1)



# fit the logistic model
mod <- glm(y ~ age.d, family = 'binomial')
summary(mod)
beta <- mod$coefficients
beta

# recover the probability of high satisfaction
pi.old = exp(beta[1]) / (1 + exp(beta[1]))
pi.old
pi.young = exp(beta[1] + beta[2]) / (1 + exp(beta[1] + beta[2]))
pi.young

# odds
odds.old <- pi.old.hat / (1 - pi.old.hat)
log(odds.old) # baseline odds in the model
odds.young <- pi.young.hat / (1 - pi.young.hat)
log(odds.young)
# log odds ratio
log(odds.young / odds.old)




### figure 2.12 (left) ###
# age.c for age continuous
age.c <- bank$age
n_age <- apply(table(y, age.c), 2, sum)
y_age <- as.numeric(table(y, age.c)[2,] / n_age)
plot(sort(unique(age.c)), y_age, ylim = c(0.5, 1), xlim = c(15, 70),
     ylab = "Pr[ Y = high | age ]", xlab = "age", pch = 16, cex = 2)


# fit the model with age as a continuous variable
# with a quadratic effect
mod_tab25up <- glm(y ~ age.c + I(age.c^2), family = 'binomial')
summary(mod_tab25up)

# without a quadratic effect
mod_tab25down <- glm(y ~ age.c, family = 'binomial')
summary(mod_tab25down)

# difference in deviance
diff_deviance <- mod_tab25down$deviance - mod_tab25up$deviance
1 - pchisq(q = diff_deviance, df = 3 - 2) # not significative at level 0.05


# continure figure 2.12 (left) #
x <- seq(15, 70, length = 200)
lines(x, predict(mod_tab25down, newdata = data.frame(age.c = x), type = "response"),
      col = 2, lty = 1, lwd = 2.5)
lines(x, predict(mod_tab25up, newdata = data.frame(age.c = x), type = "response"),
      col = 4, lty = 2, lwd = 2.5)
legend('bottomright', legend = c("Linear model", "Quadratic model"),
       lty = c(1, 2), col = c(2, 4), cex = 2, lwd = 2)


### figure 2.12 (b) ###
plot(sort(unique(age.c)), y_age, ylim = c(0.5, 1), xlim = c(15, 70),
     ylab = "Pr[ Y = high | age ]", xlab = "age", pch = 16, cex = sqrt(n_age)/2)
lines(x, predict(mod_tab25down, newdata = data.frame(age.c = x), type = "response"),
      col = 2, lty = 1, lwd = 3)
lines(x, predict(mod_tab25up, newdata = data.frame(age.c = x), type = "response"),
      col = 4, lty = 2, lwd = 3)
legend('bottomright', legend = c("Linear model", "Quadratic model"),
       lty = c(1, 2), col = c(2, 4), cex = 2, lwd = 3)




