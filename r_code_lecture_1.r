# code based on the file "Code regarding section 2.1 (copyright 2003, 2004, 2012 A.Azzalini and B.Scarpa)"

auto <- read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header = TRUE)
summary(auto)
attach(auto)

# sample size
n <- nrow(auto)
# create a dummy variable for fuel: diesel = FALSE, gasoline = TRUE
d <- fuel == "gas"


### figure 2.1 ###
pairs(auto[ , c("city.distance", "engine.size","n.cylinders","curb.weight")],     
      labels = c("City\ndistance", "Engine\nsize","Number of\ncylinders", "Curb\nweight"),
      col = ifelse(d, 'blue', 'red'), pch = ifelse(d, 1, 2), # 1 is a circle, 2 is a triangle
      cex = 10/sqrt(n))


### figure 2.2 ###
plot(engine.size, city.distance, type = "n", ylab = "City distance (km/L)",
     xlab = "Engine size (L)", xlim = c(1, 5.5))
points(engine.size[d], city.distance[d], col = 4, pch = 1)
points(engine.size[!d], city.distance[!d], col = 2, pch = 2)
legend('topright', pch = c(1, 2), col = c(4, 2),
       legend = c("Gasoline  ","Diesel"))


### additional figure ###
plot(engine.size, city.distance, type = "n", ylab = "City distance (km/L)",
     xlab = "Engine size (L)", xlim = c(1, 5.5))
points(engine.size[d], city.distance[d], col = 1, pch = 1)
points(engine.size[!d], city.distance[!d], col = 1, pch = 1)
fitO <- lm(city.distance ~ engine.size)
print(summary(fitO))
x <- (seq(min(engine.size), max(engine.size), length = 200))
x <- seq(1, 5.5, length = 200)
beta <- coef(fitO)
lines(x, beta[1] + beta[2]*x, col = 2, lty = 1, lwd = 2)


### additional figure 2  ###
plot(engine.size, city.distance, type = "n", ylab = "City distance (km/L)",
     xlab = "Engine size (L)", xlim = c(1, 5.5))
points(engine.size[d], city.distance[d], col = 1, pch = 1)
points(engine.size[!d], city.distance[!d], col = 1, pch = 1)
fitO2 <- lm(city.distance ~ engine.size + I(engine.size^2)  + I(engine.size^3))
print(summary(fitO2))
x <- (seq(min(engine.size), max(engine.size), length = 200))
x <- seq(1, 5.5, length = 200)
beta <- coef(fitO2)
lines(x, beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3, col = 2, lty = 1, lwd = 2)


### table 2.1 ###
fuel1 <- factor(fuel, levels = c("gas", "diesel"))
fit3 <- lm(city.distance ~ engine.size + I(engine.size^2) + I(engine.size^3) + fuel1)
print(summary(fit3))


### figure 2.3 ### 
plot(engine.size, city.distance, type = "n", ylab = "City distance",
     xlab="Engine size",  xlim = c(1, 5.5))
points(engine.size[d], (city.distance[d]), col = 4, pch = 1)
points(engine.size[!d], (city.distance[!d]), col = 2, pch = 2)
x <- (seq(min(engine.size), max(engine.size), length = 200))
x <- seq(1, 5.5, length = 200)
beta <- coef(fit3)
lines(x, beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3, col = 4, lty = 2)
lines(x,  beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3 + beta[5], col = 2, lty = 2)
legend('topright', pch = c(1, 2), col = c(4, 2), legend = c("Gasoline", "Diesel"))

detach(auto)
