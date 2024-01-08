# Code to visualize the (weak) law of large numbers from lecture.

# Set seed so the results can be reproduced.
set.seed(41987)

# Generate 500 independent coin tosses.
n_tosses <- 500
p <- 0.5
toss_results <- rbinom(n_tosses, 1, p)

# Calculate the rolling sum for each n.
xbar_for_each_n <- cumsum(toss_results) / (1:n_tosses)
# Plot results.
plot(1:n_tosses, xbar_for_each_n, 
     ylim = c(0, 1), type = "l")
abline(h = 0.5, col = "red")

# Repeat the experiment 100 times and plot each time.
for (i in 1:100){
  
  lines(1:n_tosses, 
        cumsum(rbinom(n_tosses, 1, p))/1:n_tosses, 
        col = rgb(0, 0, 0, alpha = 0.3))
}

abline(h = 0.5, col = "red")
abline(h = c(0.4, 0.6), col = "green")
