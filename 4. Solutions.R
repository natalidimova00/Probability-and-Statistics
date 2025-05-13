# task 49

library(MASS)
data(survey)
?survey

table(survey$Exer)
attach(survey)
table(Exer)

sort( table(Exer), decreasing = T )
100*table(Exer)/length(Exer)   # option 1
100*prop.table( table(Exer) )  # option 2

barplot( table(Exer) )                             # option 1
barplot( sort( table(Exer), decreasing = T ))      # option 2
barplot( 100*table(Exer)/length(Exer) )            # option 3
pie( table(Exer) )                                 # option 4
pie( table(Exer), col=c("red", "yellow", "blue"))  # option 5

# task 50

table(Pulse)
table(Pulse, useNA = "ifany")

pulse.interval <- cut(Pulse, breaks = seq(30, 110, 10))
pulse.interval
table(pulse.interval)

barplot(table(pulse.interval))
hist(Pulse)
hist(Pulse, breaks = seq(30, 110, 5))

stripchart(Pulse, method="stack", pch=20)
stripchart(Pulse, method="stack", pch=15)
stripchart(Pulse, method="stack", pch=1)
stripchart(Pulse, method="stack", pch="*")

# task 51

table(Age)

age.interval <- cut(Age, breaks = seq(15, 75, 10))
table(age.interval)

barplot(table(age.interval))

hist(Age)

stripchart(Age, method = "stack", pch = "*")

# task 52

my.summary <- function(x) {
  res <- c( median(x, na.rm = T), mean(x, na.rm = T), sd(x, na.rm = T) )
  names(res) <- c("Median", "Mean", "StDev")
  res
}

v1 <- rep(4, 30)
v2 <- rep(c(3.5,4.5), 15)
v3 <- rep(c(3,5), 15)
v4 <- rep(c(2:6), 6)
v5 <- rep(c(2,6), 15)

my.summary(v1)
my.summary(v2)
my.summary(v3)
my.summary(v4)
my.summary(v5)

par(mfrow=c(2,3))
stripchart(v1, pch = "*", method = "stack", xlim = c(2,6), ylim = c(0, 10))
stripchart(v2, pch = "*", method = "stack", xlim = c(2,6), ylim = c(0, 10))
stripchart(v3, pch = "*", method = "stack", xlim = c(2,6), ylim = c(0, 10))
stripchart(v4, pch = "*", method = "stack", xlim = c(2,6), ylim = c(0, 10))
stripchart(v5, pch = "*", method = "stack", xlim = c(2,6), ylim = c(0, 10))
par(mfrow=c(1, 1))

# task 53

load("cereals.RData")

summary(cereals)
attach(cereals)

summary(carbo)
mean(carbo, na.rm = T)
sd(carbo, na.rm = T)
my.summary(carbo)
hist(carbo)
boxplot(carbo, horizontal = T)

summary(sodium)
mean(sodium, na.rm = T)
sd(sodium, na.rm = T)
my.summary(sodium)
hist(sodium)
boxplot(sodium, horizontal = T)

summary(potass)
mean(potass, na.rm = T)
sd(potass, na.rm = T)
my.summary(potass)
hist(potass)
boxplot(potass, horizontal = T)

# task 54

boxplot(Pulse ~ W.Hnd)
boxplot( Pulse[W.Hnd=="Left"], Pulse[W.Hnd=="Right"] )

my.summary( Pulse[W.Hnd == "Left"] )
my.summary( Pulse[W.Hnd == "Right"] )

# task 55

# a)
median( Pulse, na.rm=T )
mean( Pulse, na.rm=T )
sd( Pulse, na.rm=T )
my.summary( Pulse )

# b)
median( Pulse[Sex == "Female"], na.rm=T )
mean( Pulse[Sex == "Female"], na.rm=T )
sd( Pulse[Sex == "Female"], na.rm=T )
my.summary( Pulse[Sex == "Female"] )

# c)
median( Pulse[Age <= 25], na.rm=T )
mean( Pulse[Age <= 25], na.rm=T )
sd( Pulse[Age <= 25], na.rm=T )
my.summary( Pulse[Age <= 25] )

# d)
median( Pulse[Exer == "Freq"], na.rm=T )
mean( Pulse[Exer == "Freq"], na.rm=T )
sd( Pulse[Exer == "Freq"], na.rm=T )
my.summary( Pulse[Exer == "Freq"] )

# e)
median( Pulse[Smoke == "Never" & Exer == "Freq"], na.rm=T )
mean( Pulse[Smoke == "Never" & Exer == "Freq"], na.rm=T )
sd( Pulse[Smoke == "Never" & Exer == "Freq"], na.rm=T )
my.summary( Pulse[Smoke == "Never" & Exer == "Freq"] )