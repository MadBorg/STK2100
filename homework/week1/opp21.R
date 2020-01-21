auto <- read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header = TRUE)
summary(auto)
attach(auto)

# Sample size
n = nrow(auto)

# Create dummy variable for fuel: disel = False, gasoline = TRUE
d = fuel == "gas"


# Scater-plot matrix
pairs(auto[ , c("city.distance", "engine.size","n.cylinders","curb.weight")],     
      labels = c("City\ndistance", "Engine\nsize","Number of\ncylinders", "Curb\nweight"),
      col = ifelse(d, 'blue', 'red'), pch = ifelse(d, 1, 2), # 1 is a circle, 2 is a triangle
      cex = 10/sqrt(n))




# 2.14 model:

# Create plot
plot(engine.size, city.distance, type="n", ylab="City Distance (km/L)", xlab="Engine size (L)", xlim= c(1,5.5))
# Adding points to the plot, disel and as is seperated
points(engine.size[d], (city.distance[d]), col = 4, pch = 1)
points(engine.size[!d], (city.distance[!d]), col = 2, pch = 2)
# factor.fuel = factor(fuel, levels= c("gas", "diesel"))
# The linear model
y = lm(city.distance ~ engine.size + I(engine.size^2) + I(engine.size^3) + fuel)
# The coeffs from the linear model
beta = coef(y)
# x values for plot
x <- (seq(min(engine.size), max(engine.size), length = 200))
# add lines to plot
lines(x, beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3, col = 4, lty = 2)
lines(x,  beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3 + beta[5], col = 2, lty = 2)
legend('topright', pch = c(1, 2), col = c(4, 2), legend = c("Gasoline", "Diesel"))


# 2.14 without qube

# Create plot
plot(engine.size, city.distance, type="n", ylab="City Distance (km/L)", xlab="Engine size (L)", xlim= c(1,5.5))
# Adding points to the plot, disel and as is seperated
points(engine.size[d], (city.distance[d]), col = 4, pch = 1)
points(engine.size[!d], (city.distance[!d]), col = 2, pch = 2)
# factor.fuel = factor(fuel, levels= c("gas", "diesel"))
# The linear model
y2 = lm(city.distance ~ engine.size + I(engine.size^2) + fuel)
# The coeffs from the linear model
beta2 = coef(y2)
# x values for plot
x2 <- x
# add lines to plot
lines(x2, beta2[1] + beta2[2]*x + beta2[3]*x^2, col = 4, lty = 2)
lines(x2,  beta2[1] + beta2[2]*x + beta2[3]*x^2 + beta2[4], col = 2, lty = 2)
legend('topright', pch = c(1, 2), col = c(4, 2), legend = c("Gasoline", "Diesel"))
