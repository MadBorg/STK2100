res_bodyfat <- read.csv("res_bodyfat/res_bodyfat.csv")
attach(res_bodyfat)



fit.log <- lm(pbfm ~ log(bmi))
print(summary(fit.log))
beta = coef(fit.log)

x = log(seq(min(bmi), max(bmi), length = 200))
plot(bmi, pbfm)
lines(exp(x),( beta[1] + beta[2]* x), col = 2) 


plot(exp(x),( beta[1] + beta[2]* x), col = 2)
