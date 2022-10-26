Lab 09
================
2022-10-26

## Problem 2.

Create a n x k matric of Poisson variables with mean lambda

``` r
set.seed(1235)
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  return(x)
}
f1 <- fun1(100, 4)
mean(f1)
```

    ## [1] 4.1575

``` r
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  # YOUR CODE HERE
  x <- matrix( rpois(n*k, lambda), ncol = 4)
  
  return(x)
}

f1 <- fun1alt(50000, 4)

# Benchmarking
microbenchmark::microbenchmark(
  fun1(),
  fun1alt()
)
```

    ## Unit: microseconds
    ##       expr   min    lq     mean  median     uq    max neval
    ##     fun1() 422.8 842.2 1088.451 1046.60 1222.8 2243.6   100
    ##  fun1alt()  24.5  31.1   91.662   37.05   50.2 3918.0   100

``` r
d <- matrix(1:16, ncol=4)
d
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    5    9   13
    ## [2,]    2    6   10   14
    ## [3,]    3    7   11   15
    ## [4,]    4    8   12   16
