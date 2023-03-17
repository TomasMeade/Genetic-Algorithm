test_that("Does default crossover work?", {
  # create data frame of paired parents
  parent1s <- c("01010111", "10111011", "00001111")
  parent2s <- c("11001010", "11001000", "11000000")
  parents <- data.frame(parent1s, parent2s)

  kid_vec <- crossover(parents, c_ind = c(4, 7, 3))
  expect_equal(kid_vec,
               c("01011010", "10111010", "00000000",
                 "11000111", "11001001", "11001111"))
})

test_that("Simple user-provided genetic operator works", {
  # create genetic operator that simply returns kids with all 1s
  gen_op <- function(parent1, parent2) {
    n <- nchar(parent1)
    all_1s <- paste(rep(1, times = n), collapse = "")
    return(c(all_1s, all_1s))
  }

  # create data frame of paired parents
  parent1s <- c("01010111", "10111011", "00001111")
  parent2s <- c("11001010", "11001000", "11000000")
  parents <- data.frame(parent1s, parent2s)

  kid_vec <- crossover(parents, gen_op = gen_op)
  expect_equal(kid_vec,
               rep("11111111", times = 6))
})
