# Getting the data
auto <- read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header = TRUE)
summary(auto)
#attach(auto)

# Variables setup 
# sample size
n <- nrow(auto)
# create a dummy variable for fuel: diesel = FALSE, gasoline = TRUE
d <- fuel == "gas"

plot.engine.size.highway.distance <- function(title){
  #setting up the plot
  plot(engine.size, highway.distance, type= "n", main = title, ylab = "Higway distance (km/L)", xlab = "Engine size (L)")
  #plotting gas points
  points(engine.size[d], highway.distance[d], col = 4, pch = 1)
  #plotting diesel points
  points(engine.size[!d], highway.distance[!d], col = 2, pch = 2)
  legend('topright', pch = c(1, 2), col = c(4, 2),
         legend = c("Gasoline  ","Diesel"))
}

X = cbind(engine.size^0, engine.size^1, engine.size^2)
y = highway.distance
beta = (t(X) %*% X)^(-1) %*% t(X) %*% y


fit = lm(highway.distance ~ engine.size + I(engine.size^2) + I(engine.size))
print(summary(fit))

plot.engine.size.highway.distance("tmp")

x <- (seq(min(engine.size), max(engine.size), length = 200))
beta.fit = coef(fit)

lines(x, beta.fit[1] + beta.fit[2]*x + beta.fit[3]*x^2, col = 4, lty = 2)
lines(x, beta[1] + beta[2]*x + beta[3]*x^2, col = 2, lty = 2)
legend('topright', pch = c(1, 2), col = c(4, 2),
       legend = c("Gasoline", "Diesel"))
