select_parents <- function(ind_vec, fit_vec){

  # Get size of generation
  n <- length(ind_vec)

  # Rank individuals
  ranked <- ind_vec[order(-fit_vec)]

  # Select first parent based on ranked probabilities
  parent1s <- sample(ranked, n/2, replace = TRUE, prob = 1:n/sum(n))

  # Select parent two at random
  parent2s <- rep("", length(parent1s))

  for (i in 1:length(parent1s)) {

    parent2s[i] <- sample(ranked[ranked != parent1s[i]], 1)

  }

  # Return data frame with pairs of parents
  return(data.frame(parent1s, parent2s))

}
