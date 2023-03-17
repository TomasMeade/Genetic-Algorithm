#' Use a basic genetic algorithm to select GLM variables.
#'
#' @param data Data frame on which GLMs are fitted. One variable should be the response variable.
#' All other variables are considered potential predictor variables.
#'
#' @param y_ind Column index of response variable in \code{data}.
#' @param glm_family Type of GLM regression (see documentation for \code{family} argument of \code{glm}).
#'
#' @param objective_fun (optional) An optional objective function that should take in a binary string of
#' length \code{ncol(data)-1} and return a numeric value. Smaller values are considered better
#' (individuals with smaller objective function values are considered "more fit"). Each digit of the binary string of
#' length \code{ncol(data)-1} should be thought of as corresponding to the \code{ncol(data)-1} possible predictor
#' variables in \code{data}, e.g. a \code{'1'} as the first digit corresponds to selecting the first non-reponse
#' variable in \code{data} to be included in a GLM model, while a \code{'0'} in the same place is interpreted as not
#' selecting the corresponding variable. The default objective function returns the AIC of the GLM model that
#' corresponds to the binary string.
#'
#' @param gen_op (optional) An optional genetic operator function that should take in a vector of two binary strings
#' of length \code{ncol(data)-1} and return a vector of two binary strings of the same length.
#'
#' @param mut_prob Probability for each potential child chromosome to be mutated. If a child chromosome is
#' randomly selected to be mutated, a gene at random within the chromosome is selected to switch (if it is 0, it becomes 1, and vice versa).
#'
#' @param n_ind Number of individuals in each generation (must be a positive, even integer).
#' @param max_n_iter Maximum number of generations. The function returns if
#' \code{n_ind} generations of individuals have been generated.
#'
#' @param abs_conv_thresh Threshold determining when the function returns. If among the last
#' \code{abs_conv_n_iter} generations, the standard deviation of the objective function value of the
#' fittest individual of each generation is less than \code{abs_conv_thresh}, the function returns.
#'
#' @param abs_conv_n_iter See definition of \code{abs_conv_thresh}.
#'
#' @return A list of the fittest individual in the final generation, the objective function value of
#' the fittest individual, the regression formula corresponding to the fittest individual,
#' the final generation and corresponding objective function values, the number of iterations of the algorithm
#' (number of generations), and whether the algorithm converged.
#'
#' @export
#'
#' @examples
#' # large_testing_data contains 13 potential predictor variables,
#' # where the response variable is generated using x5, x8, and x13
#' # the response variable is the 14th column of the data frame
#' data(large_testing_data)
#' select(large_testing_data, 14, n_ind = 4*13)
#'
#' # huge_testing_data$df contains 50 potential predictor variables,
#' # where the response variable is generated using x15, x26, x45
#' # (as indicated by huge_testing_data$true_idx)
#' # the response variable is the 51st column of the data frame
#' data(huge_testing_data)
#' select(huge_testing_data$df, 51, abs_conv_thresh = 0.1)

select <- function(data, y_ind, glm_family = "gaussian",
                   objective_fun = NULL, gen_op = NULL, mut_prob = 0.05,
                   n_ind = 2*(ncol(data) - 1), max_n_iter = 1000, abs_conv_thresh = 0.01, abs_conv_n_iter = 10) {
  # input checking
  assertthat::assert_that(is.data.frame(data),
                          msg = "invalid data argument (must be data frame)")
  assertthat::assert_that(is.numeric(y_ind),
                          length(y_ind) == 1,
                          1 <= y_ind,
                          y_ind <= ncol(data),
                          msg = "invalid y_ind argument (must be the column index of the response variable)")
  assertthat::assert_that(is.numeric(mut_prob),
                          length(mut_prob) == 1,
                          0 <= mut_prob,
                          mut_prob <= 1,
                          msg = "invalid mut_prob argument (must be a number between 0 and 1)")
  assertthat::assert_that(is.numeric(n_ind),
                          length(n_ind) == 1,
                          n_ind > 0,
                          n_ind %% 2 == 0,
                          msg = "invalid n_ind argument (must be a positive, even integer)")
  assertthat::assert_that(is.numeric(max_n_iter),
                          is.numeric(abs_conv_n_iter),
                          length(max_n_iter) == 1,
                          length(abs_conv_n_iter) == 1,
                          max_n_iter > 1,
                          abs_conv_n_iter > 1,
                          max_n_iter %% 1 == 0,
                          abs_conv_n_iter %% 1 == 0,
                          msg = "invalid max_n_iter argument, or invalid abs_conv_n_iter argument (both should be integers > 1)")
  assertthat::assert_that(is.numeric(abs_conv_thresh),
                          length(abs_conv_thresh) == 1,
                          msg = "invalid abs_conv_thresh argument (must be a number)")

  # set number of possible predictor variables
  n_var <- ncol(data) - 1
  assertthat::assert_that(n_var > 1,
                          msg = "need at least two potential predictor variables in data")

  ind_vec <- init_first_gen(n_ind, n_var)
  fit_vec <- eval_fit(ind_vec, data, y_ind, glm_family, objective_fun)

  iter <- 1
  conv <- FALSE

  # create vector to record fitness of most fit individual
  # (aka smallest objective function value) for each generation
  gen_fit <- rep(0, times = max_n_iter)
  gen_fit[iter] <- min(fit_vec, na.rm = TRUE)

  while((iter < max_n_iter) & isFALSE(conv)) {
    # pair parents and compute chromosomes of children
    parents <- select_parents(ind_vec, fit_vec)
    kid_vec <- crossover(parents, gen_op)
    kid_vec <- mutate_all(kid_vec, mut_prob)

    # evaluate and record fitness of new generation
    ind_vec <- kid_vec
    fit_vec <- eval_fit(ind_vec, data, y_ind, glm_family, objective_fun)

    iter <- iter + 1
    gen_fit[iter] <- min(fit_vec, na.rm = TRUE)

    # evaluate whether there is convergence
    if(iter >= abs_conv_n_iter) {
      # if the standard deviation of the best fitness of the last [abs_conv_n_iter] generations
      # is less than [abs_conv_thresh], we consider the algorithm to have converged
      if(stats::sd(gen_fit[(iter - abs_conv_n_iter + 1):iter]) < abs_conv_thresh) {
        conv <- TRUE
      }
    }
  }

  # when the function reaches this point, either
  # the maximum number of iterations has been reached, or
  # it has reached convergence
  fin_gen_vec <- kid_vec
  # calculate fitness of individuals in final generation
  fin_gen_fit_vec <- eval_fit(fin_gen_vec, data, y_ind, glm_family, objective_fun)
  # find most fit individual
  best_ind <- fin_gen_vec[order(fin_gen_fit_vec)[1]]
  # find fitness of most fit individual
  best_ind_fit <- fin_gen_fit_vec[order(fin_gen_fit_vec)[1]]

  # find model formula corresponding to most fit individual
  # find response variable name
  resp_var <- names(data)[y_ind]
  # find predictor variable names
  pred_vars <- names(data)[-y_ind][as.logical(as.integer(strsplit(best_ind, split = '')[[1]]))]
  # construct regression formula
  if(length(pred_vars) == 0) {
    best_formula <- paste(resp_var, "~", "1")
  }
  else {
    best_formula <- paste(resp_var, "~", paste(pred_vars, collapse = " + "))
  }

  return(list(best_ind = best_ind,
              best_ind_fit = best_ind_fit,
              best_formula = best_formula,
              fin_gen = fin_gen_vec,
              fin_gen_fit = fin_gen_fit_vec,
              n_iter = iter,
              convergence = conv))
}
