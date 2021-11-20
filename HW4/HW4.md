HW 4
================

### HPC:

#### Problem 1:

``` r
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

# Rewrite
fun1alt <- function(mat) {
  ans<-rowSums(mat)
  ans
}
```

``` r
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
   ans <- rowCumsums(mat)
   ans
}
```

``` r
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq     max neval
    ##     fun1(dat) 10.73175 11.57439 8.143305 11.63317 12.33941 1.63398   100
    ##  fun1alt(dat)  1.00000  1.00000 1.000000  1.00000  1.00000 1.00000   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr     min       lq     mean   median       uq       max neval
    ##     fun2(dat) 3.79325 2.569731 1.821321 2.335294 2.181657 0.2849135   100
    ##  fun2alt(dat) 1.00000 1.000000 1.000000 1.000000 1.000000 1.0000000   100

#### Problem 2: Make things run faster with parallel computing

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

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##   3.220   0.925   4.211

``` r
# Rewrite the previous code using parLapply() to make it run faster. 
cl <- makePSOCKcluster(4L) 
clusterSetRNGStream(cl, 123)
clusterExport(cl, c("sim_pi"), envir = environment())

system.time({
  ans <- unlist(parLapply(cl,1:4000, sim_pi, n = 10000)) 
  print(mean(ans))
})
```

    ## [1] 3.141482

    ##    user  system elapsed 
    ##   0.004   0.000   1.788

``` r
stopCluster(cl)
```

### SQL:

``` r
library(RSQLite)
library(DBI)
```

``` r
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

#### Question 1: How many movies is there avaliable in each rating catagory.

``` sql
SELECT rating,
  COUNT(*) AS count
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | count |
|:-------|------:|
| G      |   180 |
| NC-17  |   210 |
| PG     |   194 |
| PG-13  |   223 |
| R      |   195 |

5 records

</div>

There are 180 movies with rating “G”; 210 movies with rating “NC-17”;
194 movies with rating “PG”; 233 movies with rating “PG-13”; 195 movies
with rating “R”.

#### Question 2: What is the average replacement cost and rental rate for each rating category.

``` sql
SELECT rating,
  AVG(replacement_cost) AS avg_replacement_cost, 
  AVG(rental_rate) AS avg_rental_rate
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | avg\_replacement\_cost | avg\_rental\_rate |
|:-------|-----------------------:|------------------:|
| G      |               20.12333 |          2.912222 |
| NC-17  |               20.13762 |          2.970952 |
| PG     |               18.95907 |          3.051856 |
| PG-13  |               20.40256 |          3.034843 |
| R      |               20.23103 |          2.938718 |

5 records

</div>

For the category with rating “G”, the average replacement cost is 20.12
and average rental rate is 2.91;For the category with rating “NC-17”,
the average replacement cost is 20.13 and average rental rate is
2.97;For the category with rating “PG”, the average replacement cost is
18.96 and average rental rate is 3.05;For the category with rating
“PG-13”, the average replacement cost is 20.40 and average rental rate
is 3.03;For the category with rating “R”, the average replacement cost
is 20.23 and average rental rate is 2.94.

#### Question 3: Use table film\_category together with film to find the how many films there are witth each category ID.

``` sql
Select category_id,
  COUNT(*) AS count
FROM film AS a INNER JOIN film_category AS b 
  ON a.film_id = b.film_id
GROUP BY category_id
```

<div class="knitsql-table">

| category\_id | count |
|:-------------|------:|
| 1            |    64 |
| 2            |    66 |
| 3            |    60 |
| 4            |    57 |
| 5            |    58 |
| 6            |    68 |
| 7            |    62 |
| 8            |    69 |
| 9            |    73 |
| 10           |    61 |

Displaying records 1 - 10

</div>

#### Question 4: Incorporate table category into the answer to the previous question to find the name of the most popular category.

``` sql
SELECT film_category.category_id AS Category_id, category.name AS Name,
  COUNT(*) AS count
FROM film_category
  INNER JOIN film ON film_category.film_id = film.film_id
  INNER JOIN category ON category.category_id = film_category.category_id
GROUP BY film_category.category_id
ORDER BY COUNT (*) DESC 
LIMIT 3
```

<div class="knitsql-table">

| Category\_id | Name    | count |
|-------------:|:--------|------:|
|           15 | Sports  |    74 |
|            9 | Foreign |    73 |
|            8 | Family  |    69 |

3 records

</div>

The most popular category is sports which contains 74 movies.
