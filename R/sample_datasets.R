# This is a function that creates our datasets needed for testing select.R
# It creates 3 different datasets of increasing size an complexity.

# Helper data normalization function
z1_norm <- function(x) {(x-min(x)) / (max(x) - min(x))}

# This is a simple dataset with hard coded distributions for our 9 variables
# The predicted variable is a linear combination of 3 chosen variables + error
get_simple_dataset = function(){
  x1 = z1_norm(stats::rnorm(1000, 9, .5))
  x2 = z1_norm(stats::rnorm(1000, 200, 17))
  x3 = z1_norm(stats::rbinom(1000, 55, .12))
  x4 = z1_norm(stats::rpois(1000, 7))
  x5 = z1_norm(stats::rexp(1000, 8))
  x6 = z1_norm(stats::rpois(1000, 12))
  x7 = z1_norm(stats::rnorm(1000, 99, 14))
  x8 = z1_norm(stats::rnorm(1000, 34, 8))
  x9 = z1_norm(stats::rexp(1000, 6))
  error = z1_norm(stats::rnorm(1000, 0, 50))
  y = 12 + 0.4*x1 + 0.6*x2 + error

  df <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, y)
  return(df)
}

# This is a large dataset with hard coded distributions for our 13 variables
# The predicted variable is a linear combination of 3 chosen variables + error
get_large_dataset = function(){
  x1 = z1_norm(stats::rnorm(1000, 9, .5))
  x2 = z1_norm(stats::rnorm(1000, 200, 17))
  x3 = z1_norm(stats::rbinom(1000, 55, .12))
  x4 = z1_norm(stats::rpois(1000, 7))
  x5 = z1_norm(stats::rexp(1000, 8))
  x6 = z1_norm(stats::rpois(1000, 12))
  x7 = z1_norm(stats::rnorm(1000, 99, 14))
  x8 = z1_norm(stats::rnorm(1000, 34, 8))
  x9 = z1_norm(stats::rexp(1000, 6))
  x10 = z1_norm(stats::rpois(1000, 25))
  x11 = z1_norm(stats::rnorm(1000, 87, 88))
  x12 = z1_norm(stats::rnorm(1000, 3, 4))
  x13 = z1_norm(stats::rexp(1000, 19))
  error = z1_norm(stats::rnorm(1000, 0, 50))
  y = 12 + 3.2*x5 + 4.4*x8 + 2.3*x13 + error

  df <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9,
                   x10, x11, x12, x13, y)
  return(df)
}


# This is a huge dataset with random distributions for our 50 variables
# The predicted variable is a linear combination of 3 variables + error
# The distributions and their parameters for each variable are random
# The variables that represent our y are also selected randomly.
get_huge_dataset = function(){
  n = 1000
  df = data.frame(matrix(ncol = 51, nrow = n))
  for(i in 1:50){
    dist = floor(stats::runif(1, 0, 3))
    if(dist==0){
      x = z1_norm(stats::rnorm(n,
                               stats::runif(1, 0, 100),
                               stats::runif(1, 0, 100)))
    }
    else if(dist==1){
      x = z1_norm(stats::rexp(n,
                              stats::runif(1, 0, 100)))
    }
    else if(dist==2){
      x = z1_norm(stats::rpois(n,
                               stats::runif(1, 0, 100)))
    }
    df[i] = x
  }

  idx_1 = floor(stats::runif(1, 0, 21))
  idx_2 = floor(stats::runif(1, 21, 41))
  idx_3 = floor(stats::runif(1, 41, 51))
  error = z1_norm(stats::rnorm(n,
                               stats::runif(1, 0, 100),
                               stats::runif(1, 0, 100)))
  true_idx = c(idx_1, idx_2, idx_3)
  y = stats::runif(1, 0, 5) +3.2*df[idx_1] +4.4*df[idx_2] +2.3*df[idx_3] +error
  df[51] = y
  return(list('df'=df, 'true_idx'=true_idx))
}
