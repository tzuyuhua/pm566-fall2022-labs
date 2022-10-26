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
    ##       expr   min     lq    mean median     uq    max neval
    ##     fun1() 585.0 720.45 937.427 824.85 962.00 2975.3   100
    ##  fun1alt()  23.8  26.50  90.736  32.50  41.95 5429.5   100

``` r
d <- matrix(1:16, ncol=4)
d
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    5    9   13
    ## [2,]    2    6   10   14
    ## [3,]    3    7   11   15
    ## [4,]    4    8   12   16

``` r
diag(d)
```

    ## [1]  1  6 11 16

``` r
d[2]
```

    ## [1] 2

``` r
d[2,1]
```

    ## [1] 2

``` r
d[c(1, 6, 11, 16)]
```

    ## [1]  1  6 11 16

``` r
cbind(1:4, 1:4)
```

    ##      [,1] [,2]
    ## [1,]    1    1
    ## [2,]    2    2
    ## [3,]    3    3
    ## [4,]    4    4

``` r
d[cbind(1:4, 1:4)]
```

    ## [1]  1  6 11 16

## Problem 3

Find the column max (hint: Checkout the function max.col()).

``` r
#Data generating process
set.seed(1234)
M <- matrix(runif(12), ncol=4)
M
```

    ##           [,1]      [,2]        [,3]      [,4]
    ## [1,] 0.1137034 0.6233794 0.009495756 0.5142511
    ## [2,] 0.6222994 0.8609154 0.232550506 0.6935913
    ## [3,] 0.6092747 0.6403106 0.666083758 0.5449748

``` r
# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}
fun2(M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
fun2alt <- function(x) {
  # YOUR CODE HERE
    
    idx <- max.col(t(x))
    x[cbind(idx, 1:4)]
  
}
fun2alt(M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
x <- matrix(rnorm(1e4), nrow=10)

# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x)
)
```

    ## Unit: microseconds
    ##        expr    min      lq     mean median      uq    max neval
    ##     fun2(x) 1400.7 1529.75 1991.324 1629.2 2314.15 7120.3   100
    ##  fun2alt(x)  147.8  181.70  293.525  216.9  261.25 5483.4   100
