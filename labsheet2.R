#Lab sheet 2

#Problem 1 
# a)
# X
set.seed(123)
X <- rnorm(400, mean = 10, sd = 5)

#Histogram
hist(X, breaks = 15, prob = TRUE, main = "X PMF", xlab = "X values")

# Comparison with normal curve
curve(dnorm(x, mean = 10, sd = 5), add = TRUE, lwd = 2)


# b)
# Y
Y <- rnorm(400, mean = 24, sd = 12)

#Computing S
S <- Y - X

#Histogram of S
hist(S, breaks = 15, prob = TRUE, main = "Histogram of S", xlab = "S values")


# c)
#Mean and std dev of S
Mean.of.S <- 24 - 10
Stddev.of.S <- sqrt(12^2 + 5^2)

#Comparison with updated histogram of S with curve of PMF
hist(S, breaks = 15, prob = TRUE, main = "Updated histogram of S", xlab = "Values of S")
curve(dnorm(x, mean = mu_S, sd = sigma_S), add = TRUE, lwd = 2)

# d)
#chi squared test? Maybw use GBT for this one?

#Problem 2 
# a)
#Setup of the expirement, as explained in the Problem
n <- 5
P <- runif(1, 0, 1)
B <- rbinom(1, size = n, prob = P)

#Outcome of the expirement X
X <- B + 1

#P and X sample vectors
Sample.of.P <- numeric(1000)
Sample.of.X <- numeric(1000)

#Completing the experiment 1000 times
for(i in 1:1000) {
  Sample.of.P[i] <- runif(1, 0, 1) # Generate P
  Sample.of.X[i] <- rbinom(1, size = n, prob = Sample.of.P[i]) + 1 # Generate X
}

#Histogram for Sample.of.P
hist(Sample.of.P, main="Histogram of sample of P", xlab="P values")

#histogram for sample of x with breaks
hist(Sample.of.X, breaks=c(0.5:6.5), main="Histogram of sample of X", xlab="X values")



# b)
# Computing the frequences of each outcome in sample of X
frequences.observed <- table(Xsample) 
proportions <- frequences.observed / length(Xsample)
proportions



# c)
#Assuming we already have the proportions
#Check if all proportions are approximately equal to 1/6
uniform <- all(abs(proportions - 1/6) < 0.05) #Allow some tolerance
uniform
#true so yeah it is a uniform 

# d)
#what about different values of n
claimtest <- function(n, num_trials = 1000) {
  Sample.of.P <- runif(num_trials, 0, 1)
  Sample.of.X <- rbinom(num_trials, size = n, prob = Sample.of.P) + 1
  proportions <- table(Sample.of.X) / length(Sample.of.X)
  hist(Sample.of.X, breaks = seq(0.5, max(Sample.of.X)+0.5, by = 1), main = paste("n =", n))
  return(all(abs(proportions - 1/(n+1)) < 0.05))
}


#n smaller than 5
claimtest(n = 3)
#Again is uniform 

#n greater than 5
claimtest(n = 20)
#now for n > 5 is uniform 
