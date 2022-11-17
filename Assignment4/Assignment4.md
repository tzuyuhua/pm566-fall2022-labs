Lab 09
================
2022-11-17

# HPC

## Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster.

``` r
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  # YOUR CODE HERE
  rowSums(mat)
}

#install.packages('matrixStats')
library(matrixStats)
```

    ## Warning: 套件 'matrixStats' 是用 R 版本 4.2.2 來建造的

``` r
# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  # YOUR CODE HERE
  rowCumsums(mat)
}

# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), check = "equivalent"
)
```

    ## Unit: microseconds
    ##          expr   min     lq    mean median    uq    max neval
    ##     fun1(dat) 325.2 535.75 746.372 704.55 927.3 1310.8   100
    ##  fun1alt(dat)  58.9  71.05 122.049  84.50 105.5 3105.9   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), check = "equivalent"
)
```

    ## Unit: microseconds
    ##          expr    min      lq     mean  median      uq     max neval
    ##     fun2(dat) 2830.3 3652.50 6520.470 5708.85 9521.85 14733.1   100
    ##  fun2alt(dat)  120.4  140.15  206.132  163.55  218.10  2408.4   100

## Problem 2: Make things run faster with parallel computing

The following function allows simulating PI

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

In order to get accurate estimates, we can run this function multiple
times, with the following code:

``` r
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ## 使用者   系統   流逝 
    ##   5.28   1.38   7.32

Rewrite the previous code using parLapply() to make it run faster.

``` r
library(parallel)
system.time({
cl <- makePSOCKcluster(2)   
clusterSetRNGStream(cl, 1231) # Equivalent to `set.seed(123)`
  # YOUR CODE HERE
  ans <- unlist(parLapply(cl, 1:4000, sim_pi, n = 10000))
  print(mean(ans))
  # YOUR CODE HERE
})
```

    ## [1] 3.141577

    ## 使用者   系統   流逝 
    ##   0.00   0.04   8.24

``` r
stopCluster(cl)
```

\#SQL

Setup a temporary database by running the following chunk

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
```

    ## Warning: 套件 'RSQLite' 是用 R 版本 4.2.2 來建造的

``` r
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

\##Question 1

How many many movies is there avaliable in each rating catagory.

``` sql
  SELECT  rating,
    COUNT(*) AS count
  FROM film
  GROUP BY rating
```

| rating | count |
|:-------|------:|
| G      |   180 |
| NC-17  |   210 |
| PG     |   194 |
| PG-13  |   223 |
| R      |   195 |

5 records

## Question 2

What is the average replacement cost and rental rate for each rating
category.

``` sql
  SELECT  rating,
    AVG(replacement_cost) AS avg_replacement_cost, 
    AVG(rental_rate) AS avg_rental_rate
  FROM film
  GROUP BY rating
```

| rating | avg_replacement_cost | avg_rental_rate |
|:-------|---------------------:|----------------:|
| G      |             20.12333 |        2.912222 |
| NC-17  |             20.13762 |        2.970952 |
| PG     |             18.95907 |        3.051856 |
| PG-13  |             20.40256 |        3.034843 |
| R      |             20.23103 |        2.938718 |

5 records

## Question 3

Use table film_category together with film to find the how many films
there are witth each category ID

``` sql
  SELECT  category_id,
    COUNT(*) AS count
  FROM film AS a INNER JOIN film_category AS b
  ON a.film_id = b.film_id
  GROUP BY category_id
```

| category_id | count |
|:------------|------:|
| 1           |    64 |
| 2           |    66 |
| 3           |    60 |
| 4           |    57 |
| 5           |    58 |
| 6           |    68 |
| 7           |    62 |
| 8           |    69 |
| 9           |    73 |
| 10          |    61 |

Displaying records 1 - 10

## Question 4

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

First, we look at which category_id is the most popular.

``` sql
  SELECT  category_id,
    COUNT(category_id) AS count
  FROM film AS a INNER JOIN film_category AS b 
  ON a.film_id = b.film_id
  GROUP BY category_id
  ORDER BY count DESC
```

| category_id | count |
|------------:|------:|
|          15 |    74 |
|           9 |    73 |
|           8 |    69 |
|           6 |    68 |
|           2 |    66 |
|           1 |    64 |
|          13 |    63 |
|           7 |    62 |
|          14 |    61 |
|          10 |    61 |

Displaying records 1 - 10

Check the name of category_id 15.

``` sql
  SELECT  category_id, name
  FROM  category 
  WHERE category_id = 15
```

| category_id | name   |
|------------:|:-------|
|          15 | Sports |

1 records
