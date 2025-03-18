# task 32

x <- runif(500, 3, 5)
# hist(x)
hist(x, probability = T)
curve( dunif(x, 3, 5), from = 3, to = 5, add = T, lwd = 2)

# task 33

x <- rexp(500, 1/7)
hist(x, probability = T)
curve( dexp(x, 1/7), from = 0, to = max(x), add = T, lwd = 2)

# task 34

x <- rnorm(500, 0, 1)
hist(x, probability = T)
curve( dnorm(x, 0, 1), add = T, lwd = 2)

# task 35

n <- 200
x <- runif(n, 7, 9)
plot.ecdf(x, do.points = F)
curve( punif(x, 7, 9), add = T, col = "red")

# task 36

n <- 200
x <- rexp(n, 3)
plot.ecdf(x, do.points = F)
curve( pexp(x, 3), add = T, col = "red")

# task 37

n <- 200
x <- rnorm(n, 4, 1.2)
plot.ecdf(x, do.points = F)
curve( pnorm(x, 4, 1.2), add = T, col = "red")

# task 38

par(mfrow = c(1, 3))
curve( dunif(x, 7, 9), from = 7, to = 9)
curve( punif(x, 7, 9), from = 5, to = 11)
curve( qunif(x, 7, 9), from = 0, to = 1)
par(mfrow = c(1, 3))

# task 39

par(mfrow = c(1, 3))
curve( dexp(x, 3), from = 0, to = 4)
curve( pexp(x, 3), from = -1, to = 4)
curve( qexp(x, 3), from = 0, to = 1)

# task 40

par(mfrow = c(1, 3))
curve( dnorm(x, 4, 1.2), from = 0, to = 8)
curve( pnorm(x, 4, 1.2), from = 0, to = 8)
curve( qnorm(x, 4, 1.2), from = 0, to = 1)
par(mfrow = c(1, 1))

# task 41

# X = количество на портокалов сок в случайно избрана бутилка
# X ~ U(495, 502)

# a)
# P(X > 500) = 1 - P(X <= 500)
1 - punif(500, 495, 502)

# b)
# v = ? P(X > v) = 0.8
# P(X <= v) = 0.2
v <- qunif(0.2, 495, 502)
v
punif(v, 495, 502)

# task 42
# X = време до повреда на пералня
# X ~ Exp(1/4)

# a)
# P(X > 3) = 1 - P(X <= 3)
1 - pexp(3, 1/4)

# b)
# P(X <= 2)
pexp(2, 1/4)

# v)
# P(X > 6 | X > 3) = P(X > 6)/P(X > 3)
(1 - pexp(6, 1/4)) / (1 - pexp(3, 1/4))

# g)
# t = ? P(X <= t) = 0.9
t <- qexp(0.9, 1/4)
t
pexp(t, 1/4)

# task 43
# X = изразходвано количество кашкавал за седмица
# X ~ N(mu = 41, sigma = 5)

# a)
# P(X > 51) = 1 - P(X <= 51)
1 - pnorm(51, 41, 5)

# b)
# P(45 < X < 50) = P(X < 50) - P(X < 45)
pnorm(50, 41, 5) - pnorm(45, 41, 5)

# v)
# t = ? P(X <= t) = 0.99
t <- qnorm(0.99, 41, 5)
t
pnorm(t, 41, 5)







