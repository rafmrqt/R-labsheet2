#Lab sheet 2
#Assessment
#Ex1 (a)
#vector x
set.seed(123)
X <- rnorm(400, mean = 10, sd = 5)

#histogram
hist(X, breaks = 15, prob = TRUE, main = "X PMF", xlab = "X values")

#actual normal curve for comparision
curve(dnorm(x, mean = 10, sd = 5), add = TRUE, lwd = 2)


#Ex1 (b)
#vector y
Y <- rnorm(400, mean = 24, sd = 12)

#calculating S
S <- Y - X

#hist of S
hist(S, breaks = 15, prob = TRUE, main = "Histogram of S", xlab = "S values")


#Ex1 (c)
#mean and std dev
S_mean <- 24 - 10
S_stddev <- sqrt(12^2 + 5^2)

#updated histogram of S with curve of pmf for comparision 
hist(S, breaks = 15, prob = TRUE, main = "Histogram of S", xlab = "S values")
curve(dnorm(x, mean = mu_S, sd = sigma_S), add = TRUE, lwd = 2)

#Ex1 (d)
#chi squared test? Maybw use GBT for this one?

#Ex2 (a)
#setup like as stated in problem
n <- 5
P <- runif(1, 0, 1)
B <- rbinom(1, size = n, prob = P)

#declaring X
X <- B + 1

#P & X sample vectors
Psample <- numeric(1000)
Xsample <- numeric(1000)

#running the experiment 1000 times
for(i in 1:1000) {
  Psample[i] <- runif(1, 0, 1) # Generate P
  Xsample[i] <- rbinom(1, size = n, prob = Psample[i]) + 1 # Generate X
}

#Histogram for psample
hist(Psample, main="Histogram of Psample", xlab="Values of P")

#histogram for xsample with breaks
hist(Xsample, breaks=c(0.5:6.5), main="Histogram of Xsample", xlab="Values of X")



#Ex2 (b)
# Calculate the proportions of each outcome in Xsample
opbservedfreq <- table(Xsample) 
proportions <- opbservedfreq / length(Xsample)
proportions



#Ex2 (c)
# Assuming we already have the proportions from part (b)
# Check if all proportions are approximately equal to 1/6
is_uniform <- all(abs(proportions - 1/6) < 0.05) #Allow some tolerance
is_uniform
#returns true so yeah it is a uniform 

#Ex2 (d)
#what about different values of n
claimtest <- function(n, num_trials = 1000) {
  Psample <- runif(num_trials, 0, 1)
  Xsample <- rbinom(num_trials, size = n, prob = Psample) + 1
  proportions <- table(Xsample) / length(Xsample)
  hist(Xsample, breaks = seq(0.5, max(Xsample)+0.5, by = 1), main = paste("n =", n))
  return(all(abs(proportions - 1/(n+1)) < 0.05))
}


#n < 5
claimtest(n = 3)
#Again is uniform 

#n > 5
claimtest(n = 20)
#now for n > 5 is uniform 
