# apply crossover to all pairs of parents
# argument c_ind is used for testing but has no functionality otherwise
crossover <- function(parents, gen_op = NULL, c_ind = NULL){
  n <- nchar(parents[1, 1])

  # if alternate genetic operator is provided
  if(!is.null(gen_op)) {
    kid_mat <- sapply(1:nrow(parents),
                        function(i) { gen_op(parents$parent1s[i], parents$parent2s[i]) })
    kid_vec <- as.vector(kid_mat)
    assertthat::assert_that(is.character(kid_vec),
                            length(kid_vec) == 2 * nrow(parents),
                            all(nchar(kid_vec) == rep(n, times = 2 * nrow(parents))),
                            msg = paste("provided genetic operator gives unexpected output",
                                        "(should output character vector of length 2,",
                                        "where each string has the same length as the two inputs)"))
    return(kid_vec)
  }
  else {
    if(is.null(c_ind)){
      # Get random split points
      c_ind <- sample(1:(n-1), nrow(parents), replace = TRUE)
    }
    kid_vec = c(paste0(substr(parents$parent1s, 1, c_ind), substr(parents$parent2s, c_ind+1, n)),
                paste0(substr(parents$parent2s ,1 , c_ind), substr(parents$parent1s, c_ind+1, n)))
    return(kid_vec)
  }
}
