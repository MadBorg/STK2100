# Getting the data
auto <- read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header = TRUE)
summary(auto)
attach(auto)

# Variables setup 
# sample size
n <- nrow(auto)
# create a dummy variable for fuel: diesel = FALSE, gasoline = TRUE
d <- fuel == "gas"

### ---

#plot
title = "linear linear model"
#setting up the plot
plot(curb.weight, highway.distance, type= "n", main = title, ylab = "Higway distance (km/L)", xlab = "curb.weight (kg)")
#plotting gas points
points(curb.weight[d], highway.distance[d], col = 4, pch = 1)
#plotting diesel points
points(curb.weight[!d], highway.distance[!d], col = 2, pch = 2)

# lm
fit.lin.lin = lm(highway.distance ~ I(curb.weight) + fuel)
print(summary(fit.lin.lin))

#adding data to plot
x = seq(min(engine.size), max(engine.size), length = 200)
beta = coef(fit.lin.lin)
lines(x, beta[1] + beta[2]*x, col = 4, lty = 2)
lines(x, beta[1] + beta[2]*x + beta[3], col = 2, lty = 2)
legend('topright', pch = c(1, 2), col = c(4, 2),
       legend = c("Gasoline  ","Diesel"))
