library("plotly")
library("recipes")

data("iris")

View(iris)

# Prepare design matrix by adding intercept column and performing one-hot
# coding of the outcome
rec <- recipe(Species ~ Sepal.Width + Petal.Length, iris) %>%
  step_intercept() %>%
  step_dummy(Species, role = "outcome", one_hot = TRUE) %>%
  prep()

# Juice out the design matrix and response matrix
X <- as.matrix( juice(rec, all_predictors()) )
Y <- as.matrix( juice(rec, all_outcomes()) )

# Perform regression on the indicator outcomes
theta <- solve(t(X) %*% X) %*% t(X) %*% Y

# Compare to fitting the linear models directly
lm(I(Species == "setosa") ~ Sepal.Width + Petal.Length, iris)
lm(I(Species == "versicolor") ~ Sepal.Width + Petal.Length, iris)
lm(I(Species == "virginica") ~ Sepal.Width + Petal.Length, iris)

# Make a grid of values over which to plot
x <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), len = 200)
y <- seq(min(iris$Petal.Length), max(iris$Petal.Length), len = 100)

# Get hyperplanes for each species
z.setosa      <- t(outer(x, y, function(x, y) { cbind(1, x, y) %*% theta[,1] }))
z.versicolor  <- t(outer(x, y, function(x, y) { cbind(1, x, y) %*% theta[,2] }))
z.virginica   <- t(outer(x, y, function(x, y) { cbind(1, x, y) %*% theta[,3] }))

# Colour the planes
c.setosa <- ifelse(z.setosa > z.versicolor & z.setosa > z.virginica, 2, 1)
c.versicolor <- ifelse(z.versicolor > z.setosa & z.versicolor > z.virginica, 4, 3)
c.virginica <- ifelse(z.virginica > z.versicolor & z.virginica > z.setosa, 5, 6)

# Plot
plot_ly(colors = c('red', 'darkred', 'green', 'darkgreen', 'darkblue', 'blue')) %>%
  add_surface(x = ~x, y = ~y, z = ~z.setosa,
              opacity = 1, surfacecolor = c.setosa, cauto = FALSE, cmax=6, cmin=1) %>%
  add_surface(x = ~x, y = ~y, z = ~z.versicolor,
              opacity = 1, surfacecolor = c.versicolor, cauto = FALSE, cmax=6, cmin=1) %>%
  add_surface(x = ~x, y = ~y, z = ~z.virginica,
              opacity = 1, surfacecolor = c.virginica, cauto = FALSE, cmax=6, cmin=1) %>%
  add_trace(x = ~X[,2], y = ~X[,3], z = apply(X%*%theta, 1, max)+0.1, color = rep(c(1,3,6), each = 50),
            type = "scatter3d", mode = "markers",
            opacity = 1)


