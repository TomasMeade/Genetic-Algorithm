init_first_gen <- function(n_ind, n_var) {
  # sample [n_var] random bits ('0' or '1') [n_ind] times
  first_gen <- sapply(1:n_ind,
                      function(i) {
                        paste0(sample(c('0', '1'), n_var, replace = TRUE), collapse = '')
                      })
  return(first_gen)
}
