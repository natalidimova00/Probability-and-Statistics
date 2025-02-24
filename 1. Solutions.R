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
