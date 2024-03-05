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
set.seed(456)
Y <- rnorm(400, mean = 24, sd = 12)

#calculating S
set.seed(789)
S <- Y - X

#hist of S
hist(S, breaks = 15, prob = TRUE, main = "Histogram of S", xlab = "S values")


#Ex1 (c)
#mean and std dev
S_mean <- 24 - 10
S_stddev <- sqrt(12^2 + 5^2)

#updated histogram of S
hist(S, breaks = 15, prob = TRUE, main = "Histogram of S", xlab = "S values")
#*Guessing the distribution*
# Due to the fact the overall shape of the histrogram that ressembles a bell curve
# Also there appears to be a central peak around the mean.

#actual PDF curve for comp again
curve(dnorm(x, mean = mu_S, sd = sigma_S), add = TRUE, lwd = 2)
#*Comparision after superimposition*
# After superimposing the PMF the curves appears to match the histogram bars at various intervals.



#Ex1 (d)
#chi squared test??

#Null
chiintervals <- seq(min(S), max(S), length.out = 16)
observed <- hist(S, breaks = chiintervals, plot = FALSE)[["counts"]]
expectedval <- diff(pnorm(chiintervals, mean = S_mean, sd= S_stddev)) * length(S)

chisqtesting <- chisq.test(x = observed, p = expectedval/sum(expectedval), rescale.p = TRUE)
chisqtesting

# Null hypothesis - The PDF is valid and a normal distribution is appropiate, 
# Alternative hypothesis - The PDF is not valid and a normal distribution is not appropiate.
#
#Since the p-value is greater than 0.05 we have insufficient evidence to reject the null 
#hypothesis therefore it can be concluded that the PDF chosen is valid. 


#Ex2 (a)
# Define the number of trials for the binomial distribution
n <- 5

# Step 1: Generate a value for P from a Uniform distribution
set.seed(101112)
P <- runif(1, 0, 1)
# Step 2: Generate B from a Binom(n, P) distribution
B <- rbinom(1, size = n, prob = P)

# Step 3: Declaring X
X <- B + 1

# Initialize vectors to store the outcomes
Psample <- numeric(1000)
Xsample <- numeric(1000)

# Run the experiment 1000 times
set.seed(131415) 
for(i in 1:1000) {
  Psample[i] <- runif(1, 0, 1) # Generate P
  Xsample[i] <- rbinom(1, size = n, prob = Psample[i]) + 1 # Generate X
}

#Histogram 
hist(Psample, main="Histogram of Psample", xlab="Values of P")

#histogram with Xsample with breaks
hist(Xsample, breaks=c(0.5:6.5), main="Histogram of Xsample", xlab="Values of X")



#Ex2 (b)
# Calculate the proportions of each outcome in Xsample
opbservedfreq <- table(Xsample) 
proportions <- opbservedfreq / length(Xsample)
proportions



#Ex2 (c)
# Assuming we already have the proportions from part (b)
# Check if all proportions are approximately equal to 1/6
#is_uniform <- all(abs(proportions - 1/6) < 0.05) #Allow some tolerance
#is_uniform
#checking if it is by chisquared
expectedfreq <- rep(1000/6, 6)
chisquaredtesting <- chisq.test(opbservedfreq, p = rep(1/6, 6))
chisquaredtesting 
# Null hypothesis - Dr Watson's claims are true and it is indeed a uniform distribution, 
# Alternative hypothesis -Dr Watson's claims are false and it is not a uniform distribution.
#
#Since the p-value is greater than 0.05 we have insufficient evidence to reject the null 
#hypothesis therefore it can be concluded that Dr Watsons statement is true. 


#Ex2 (d)
#what about different values of n
#claimtest <- function(n, num_trials = 1000) {
#  Psample <- runif(num_trials, 0, 1)
#  Xsample <- rbinom(num_trials, size = n, prob = Psample) + 1
#  proportions <- table(Xsample) / length(Xsample)
#  hist(Xsample, breaks = seq(0.5, max(Xsample)+0.5, by = 1), main = paste("n =", n))
#  return(all(abs(proportions - 1/(n+1)) < 0.05))
#}



claimtestX2 <- function(n, num_trials = 1000) {
  Psample <- runif(num_trials, 0, 1)
  Xsample <- rbinom(num_trials, size = n, prob = Psample) + 1
  
  #observed frequencies
  observedFreq <- table(Xsample)
  #expected frequencies assuming uniform distribution
  expectedFreq <- rep(num_trials / (n+1), (n+1))
  expectedFreq <- expectedFreq[names(observedFreq)]
  
  #perform chi-squared test
  chisquaredResult <- chisq.test(observedFreq, p = rep(1/(n+1), length(observedFreq)))
  
  #histogram
  hist(Xsample, breaks = seq(0.5, max(Xsample)+0.5, by = 1), main = paste("n =", n))
  return(chisquaredResult[["p.value"]])
}


#n < 5
set.seed(161718)
claimtestX2(n = 3)
# Null hypothesis - Dr Watson's claims are true and it is indeed a uniform distribution, 
# Alternative hypothesis -Dr Watson's claims are false and it is not a uniform distribution.
#
#Since the p-value is greater than 0.05 at 0.7799717 we have insufficient evidence to reject the null 
#hypothesis therefore it can be concluded that Dr Watsons statement is true for n < 5.

#n > 5
set.seed(192021)
claimtestX2(n = 20)

# Null hypothesis - Dr Watson's claims still remain true and it is indeed a uniform distribution, 
# Alternative hypothesis -Dr Watson's claims are false and it is not a uniform distribution.
#
#Since the p-value is greater than 0.05 at 0.8528437 we have insufficient evidence to reject the null 
#hypothesis therefore it can be concluded that Dr Watsons statement is still true for n > 5. 


