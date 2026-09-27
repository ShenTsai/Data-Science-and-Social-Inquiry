log_abs_normal <- function (x){return(log(abs(x)) * dnorm(x))}

cal_riemann <- function(from, to, n_interval){

    total <- 0

    interval_length <- (to - from) / n_interval

    for(i in 1:n_interval){

      x_i <- from + (i - 0.5) * interval_length

      total <- log_abs_normal(x_i) * interval_length + total
      
    }

    print(total)
}

cal_riemann(from = -10, to = 10, n_interval = 1000)

set.seed(67)
B <- 1000
x <- rnorm(B)
sum(log(abs(x))) / B

