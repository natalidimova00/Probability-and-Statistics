# task 1

sim.balls1 <- function(){
  x <- sample(c(1:8), 2, replace = T)
  x[1] == x[2]
}

result <- replicate(100000, sim.balls1())
sum(result)/length(result)

# task 2

sim.socks <- function(){
  x <- sample(c(1, 1, 2, 2, 3, 3), 2, replace = F)
  x[1] == x[2]
}

result <- replicate(100000, sim.socks())
sum(result)/length(result)

# task 3

sim.keys <- function(){
  x <- sample(c(1:4), 4, replace = F)
  x[4] == 1
}

result <- replicate(100000, sim.keys())
sum(result)/length(result)

# task 4

sim.exam <- function(){
  x <- sample(c(rep(0, 3), rep(1, 17)), 2, replace = F)
  sum(x) == 1
}

result <- replicate(100000, sim.exam())
sum(result)/length(result)

# task 5

sim.bday <- function(k) {
  days <- c(1:365)
  x <- sample(days, k, replace = T)
  any(duplicated(x))
}

prob.bday <- function(Nrep, k){
  res <- replicate(Nrep, sim.bday(k))
  sum(res)/length(res)
}

prob.bday(100000, 25)

#task 6

sim.gifts <- function(k){
  x <- sample(c(1:k), k, replace = F)
  d <- x - c(1:k)
  any(d==0)
}

prob.gifts <- function(Nrep, k){
  res <- replicate(Nrep, sim.gifts(k))
  sum(res)/length(res)
}

prob.gifts(100000, 20)

# task 7

sim.ants <- function(){
  a1 <- sample( c(2,3), 1)
  a2 <- sample( c(1,3), 1)
  a3 <- sample( c(1,2), 1)
  a <- c(a1,a2,a3)
  length(unique(a))==3
}

prob.ants <- function(Nrep){
  res <- replicate(Nrep, sim.ants())
  sum(res)/length(res)
}

prob.ants(100000)

#

# task 8

sim.eggs <- function(){
  eggs <- c(rep("b", 2), rep("r",6))
  draws <- sample(eggs, 8, replace = F)
  player1 <- draws[seq(1,7,2)]
  player2 <- draws[seq(2,8,2)]
  b1 <- sum(player1=="b")
  b2 <- sum(player2=="b")
  c(b1,b2)
}

Nrep <- 100000
res <- replicate(Nrep, sim.eggs())
res[,1:10]

# P(A)
( sum(res[1,] == 2) + sum(res[2,] == 2)) / Nrep

# P(B)
sum(res[1,] == 1) / Nrep
sum(res[2,] == 1) / Nrep

# P(C)
sum(res[1,] == 2) / Nrep

# P(D)
sum(res[2,] == 2) / Nrep

# task 9

sim.test.ans <- function(){
  x <- sample(c(0,1), 10, replace = T, prob = c(0.75, 0.25))
  sum(x)
}

res <- replicate(100000, sim.test.ans())
sum(res >= 5)/length(res)

# task 10

sim.airplane <- function(){
  x <- sample(c(0,1), 143, replace = T, prob = c(0.08, 0.92))
  sum(x)
}

res <- replicate(100000, sim.airplane())
# a)
sum(res <= 138)/length(res)
# b)
sum(res == 137)/length(res)











