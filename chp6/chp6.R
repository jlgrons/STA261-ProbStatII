# define the objective function
ll.poisson <- function(lambda, x) {
  sum(x) * log(lambda) - length(x) * lambda
}
data <- rpois(1000, 5) # generate some data
# by default optim finds the min, but the negative min is the max
# therefore we need to use list(fnscale = -1)
opt <- optim(par = 2, fn = ll.poisson, method = "BFGS",
             control = list(fnscale = -1), x = data)

c(opt$par, mean(data))

curve(ll.poisson(x, data), 0,10, xlab = "lambda")
abline(h = opt$value, v = opt$par, lty = 2, lwd = 3, col = "red")


Ltheta <- function(theta) 1/theta^length(data)
data <- runif(100,0,3)
optimize(Ltheta,c(max(data),1000),maximum=T)
