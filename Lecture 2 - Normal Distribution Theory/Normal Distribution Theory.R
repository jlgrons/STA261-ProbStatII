# Linear combination of normally distributed RVs example.

?pnorm

pnorm(0.9, mean = 0, sd = 1, lower.tail = TRUE)

1 - pnorm(0.9, mean = 0, sd = 1)

 1 - 2* pnorm(0.9, mean = 0, sd = 1, lower.tail = FALSE)

 
 # Finding a quantile.
 qnorm(0.975)
 
 
# Visualizing the CLT.
 
set.seed(92047)

n_vals <- c(20, 50, 100, 250, 500, 1000)

p <- pi/4

num_sims <- 1500
 
z_n <- function(n_sims, n, p){
  
  num_succ <- rbinom(n_sims, n, p)

  xbar <- num_succ / n 
  
  std_xbar <- (xbar - p) / sqrt( p * (1-p) / n )
  
  return(std_xbar)
}

results <- sapply(1:length(n_vals), function(xx) z_n(num_sims,  n_vals[xx], p))       

colnames(results) <- c('n_20', 'n_50', 'n_100',
                       'n_250', 'n_500', 'n_1000')

hist(results[,1])

hist(results[, 6])


# CLT Example.

pnorm(1.2, mean = 1.5, sd = sqrt(1/100))


# Chi Squared Example.

qchisq(0.95, df = 6)


