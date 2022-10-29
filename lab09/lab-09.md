Lab 09
================
2022-10-28

## Problem 2

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
    ##       expr   min     lq     mean  median     uq    max neval
    ##     fun1() 439.3 753.45 1120.057 1113.85 1263.2 3181.2   100
    ##  fun1alt()  24.9  30.80   82.047   37.80   51.6 3172.5   100

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
    ##        expr    min      lq     mean  median     uq    max neval
    ##     fun2(x) 1419.5 1560.05 1971.400 1740.50 2047.0 5280.0   100
    ##  fun2alt(x)  147.7  193.50  278.632  217.35  267.6 3627.4   100

## Problem 4. Show PSOCK cluster example.

``` r
library(parallel)

my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  # STEP 1: GOES HERE
  cl <- makePSOCKcluster(4)   
  clusterSetRNGStream(cl, 123) # Equivalent to `set.seed(123)`
  
  # STEP 2: GOES HERE
  
  clusterExport(cl  , c("stat", "dat", "idx"), envir=environment())
  
    # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- parLapply(cl, seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: GOES HERE
  
  ans
  
}
```

``` r
# Bootstrap of an OLS
my_stat <- function(d) coef(lm(y ~ x, data=d))

# DATA SIM
set.seed(1)
n <- 500; R <- 1e4

x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)

# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)
#stopCluster(cl)

# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1386903 0.04856752
    ## x            4.8685162 5.04351239

``` r
ans0
```

    ##                  2.5 %     97.5 %
    ## (Intercept) -0.1379033 0.04797344
    ## x            4.8650100 5.04883353
