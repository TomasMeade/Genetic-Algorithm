mutate_all <- function(ind, prob){

  # Get number of genes in each individual
  var_num <- nchar(ind[1])

  # Select individuals to mutate at rate prob
  y <- sample(c(TRUE, FALSE), length(ind), replace = TRUE, prob = c(prob, 1-prob))

  mutated <- ind[y]

  # Mutate selected individuals at random if any have been selected to be mutated
  if (length(mutated) == 0){
    return(ind)
  }

  for (i in 1:length(mutated)) {

    index <- sample(1:var_num, 1)

    rand <- substr(mutated[i], index, index)

    if (rand == "0"){
      substr(mutated[i], index, index) <- "1"
    }
    else {
      substr(mutated[i], index, index) <- "0"
    }
  }

  ind[y] <- mutated

  return(ind)

}


