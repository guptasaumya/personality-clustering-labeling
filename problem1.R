## AMI22T Home Exercise 3, Problem 1
## Linear Regression Concepts
## Saumya Gupta, DS


# load required packages
library(ggplot2)
library(GGally)


# 2. ----
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100) / 10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

# create a data frame for ease
df = data.frame(x1 = x1, x2 = x2, y = y)

# check randomness
plot(x1)

plot(x2)

plot(y)

# check distributions
ggplot(df, aes(x1)) + geom_density()

ggplot(df, aes(x2)) + geom_density()

ggplot(df, aes(y)) + geom_density()

# check correlations
ggcorr(df, nbreaks = 10)

cor(df)

# fit models
## Model a) ----
ols.fit.1 <- lm(y ~ x1,
                data = df)

summary(ols.fit.1)

## Model b) ----
ols.fit.2 <- lm(y ~ x2,
                data = df)

summary(ols.fit.2)

## Model c) ----
ols.fit.3 <- lm(y ~ x1 + x2,
                data = df)

summary(ols.fit.3)

