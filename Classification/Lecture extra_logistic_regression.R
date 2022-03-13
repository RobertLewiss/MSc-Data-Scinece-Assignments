library("plotly")
library("nnet")

data("iris")

View(iris)

# Make sure setosa is the base case
iris$Species <- relevel(iris$Species, ref = "setosa")

# Fit multinomial logistic regression
fit <- multinom(Species ~ Sepal.Width + Petal.Length, iris)

# Make a grid of values over which to plot
x <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), len = 200)
y <- seq(min(iris$Petal.Length), max(iris$Petal.Length), len = 100)

# Get hyperplanes for each species
z.versicolor  <- t(outer(x, y, function(x, y) { predict(fit, data.frame(Sepal.Width = x, Petal.Length = y), type = "probs")[,"versicolor"] }))
z.virginica   <- t(outer(x, y, function(x, y) { predict(fit, data.frame(Sepal.Width = x, Petal.Length = y), type = "probs")[,"virginica"] }))
z.setosa      <- ceiling(z.versicolor)-z.versicolor-z.virginica

# Colour the planes
c.versicolor <- ifelse(z.versicolor, 4, 3)
c.virginica  <- ifelse(z.virginica, 5, 6)
c.setosa     <- ifelse(z.setosa, 2, 1)

# Plot
plot_ly(colors = c('red', 'darkred', 'green', 'darkgreen', 'darkblue', 'blue')) %>%
  add_surface(x = ~x, y = ~y, z = ~z.setosa,
              opacity = 1, surfacecolor = c.setosa, cauto = FALSE, cmax=6, cmin=1) %>%
  add_surface(x = ~x, y = ~y, z = ~z.versicolor,
              opacity = 1, surfacecolor = c.versicolor, cauto = FALSE, cmax=6, cmin=1) %>%
  add_surface(x = ~x, y = ~y, z = ~z.virginica,
              opacity = 1, surfacecolor = c.virginica, cauto = FALSE, cmax=6, cmin=1) %>%
  add_trace(x = ~iris$Sepal.Width, y = ~iris$Petal.Length, z = 0, color = rep(c(1,3,6), each = 50),
            type = "scatter3d", mode = "markers",
            opacity = 1)


