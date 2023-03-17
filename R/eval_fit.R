# helper function to evaluate AIC fitness for one individual
eval_aic <- function(ind, data, y_ind, glm_family) {
  # find response variable name
  resp_var <- names(data)[y_ind]
  # find predictor variable names
  pred_vars <- names(data)[-y_ind][as.logical(as.integer(strsplit(ind, split = '')[[1]]))]

  # construct regression formula based on individual
  # treat individual with all 0's separately
  if(length(pred_vars) == 0) {
    formula <- paste(resp_var, "~", "1")
  }
  else {
    formula <- paste(resp_var, "~", paste(pred_vars, collapse = " + "))
  }

  # fit model and return AIC
  mod <- stats::glm(formula, data = data, family = glm_family)
  return(stats::AIC(mod))
}

eval_fit <- function(ind_vec, data, y_ind, glm_family = "gaussian", objective_fun = NULL) {
  # if user provides a fitness function,
  # return fitness function evaluated for all individuals
  if(!is.null(objective_fun)) {
    fit <- sapply(ind_vec, objective_fun, USE.NAMES = FALSE)
    assertthat::assert_that(is.numeric(fit),
                            length(fit) == length(ind_vec),
                            msg = paste("provided objective function gives unexpected output",
                                        "(should output single numeric value)"))
    return(fit)
  }
  # else, return AIC
  else {
    return(sapply(ind_vec, eval_aic, data = data, y_ind = y_ind, glm_family = glm_family, USE.NAMES = FALSE))
  }
}
