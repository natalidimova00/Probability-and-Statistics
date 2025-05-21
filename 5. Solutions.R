# task 60

Nrep <- 1000
# Nrep <- 10000

for(n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate( Nrep, sum( rexp(n, 1/5 ) ) )
  hist( xsum, main = paste("n = ", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate( Nrep, sum( rexp(n, 1/5) ) )
  plot.ecdf( xsum, do.points = FALSE, col = "blue", lwd = 2, main = paste("n = ", n))
  curve( pnorm(x, 5*n, 5*sqrt(n)), add = T, col = "coral", lty = "longdash", lwd = 2)
}

# task 61

for(n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate( Nrep, mean( rexp(n, 1/5 ) ) )
  hist( xmean, main = paste("n = ", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate( Nrep, mean( rexp(n, 1/5) ) )
  plot.ecdf( xmean, do.points = FALSE, col = "blue", lwd = 2, main = paste("n = ", n))
  curve( pnorm(x, 5, 5/sqrt(n)), add = T, col = "coral", lty = "longdash", lwd = 2)
}

# task 62

for(n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate( Nrep, sum( rpois(n, 3 ) ) )
  hist( xsum, main = paste("n = ", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate( Nrep, sum( rpois(n, 3) ) )
  plot.ecdf( xsum, do.points = FALSE, col = "blue", lwd = 2, main = paste("n = ", n))
  curve( pnorm(x, 3*n, sqrt(3*n)), add = T, col = "coral", lty = "longdash", lwd = 2)
}  
  
# task 63

for(n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate( Nrep, mean( rpois(n, 3 ) ) )
  hist( xmean, main = paste("n = ", n))
}
  
for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate( Nrep, mean( rpois(n, 3) ) )
  plot.ecdf( xmean, do.points = FALSE, col = "blue", lwd = 2, main = paste("n = ", n))
  curve( pnorm(x, 3, sqrt(3)/sqrt(n)), add = T, col = "coral", lty = "longdash", lwd = 2)
}    

# task 65

# с използване на ЦГТ (централната гранична теорема)
a <- (980-900)/(900/sqrt(100))
1 - pnorm(a)

# със симулации
mean.vals <- replicate(100000, mean(rexp(100, 1/900)))
sum(mean.vals > 980)/length(mean.vals)
hist(mean.vals)

# task 66

# с използване на ЦГТ
b <- (35-30)/(60/(sqrt(12)*sqrt(50)))
a <- (25-30)/(60/(sqrt(12)*sqrt(50)))
pnorm(b) - pnorm(a)

# със симулации
mean.vals <- replicate(100000, mean(runif(50, 0, 60)))
sum(mean.vals > 25 & mean.vals < 35)/length(mean.vals)
hist(mean.vals)

# task 67

# пресмятаме mu, sigma
x <- c(4:7)
p <- c(0.2,0.4,0.3,0.1)
mu <- sum(x*p)
sigma <- sqrt( sum((x^2)*p) - mu^2 )

# с използване на ЦГТ
a <- (5.5-mu)/(sigma/sqrt(49))
1 - pnorm(a)

# със симулации
mean.vals <- replicate(100000, mean(sample(x, 49, replace=T, prob=p)))
sum(mean.vals > 5.5)/length(mean.vals)

# task 68

# с използване на ЦГТ
a <- (4000 - 160*24)/(7*sqrt(160))
1 - pnorm(a)

# Не можем да решим задачата със симулации, 
# понеже не знаем разпределението на X_1,..., X_n.