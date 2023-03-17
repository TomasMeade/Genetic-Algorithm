# This is the test set for our overall select function.
# It involves taking two random data sets, one with 13 variables and another with 50 variables
# And comparing it to some known truth
# Helper function to get all combinations of variables for modelling
get_var_idx_subsets = function(var_idx){
  l = list()
  for(i in 1:length(var_idx)){
    comb_vars = combn(var_idx, i, simplify = F)
    x = rep(0, length(var_idx))
    l = c(l ,combn(var_idx, i, simplify = F))
  }
  return(l)
}

# Helper function to create lm formula by column index of data
lm_idx_formula = function(df, obj_idx, var_idx_vec){
  x = as.formula(paste(colnames(df)[obj_idx], "~",
                       paste(colnames(df)[var_idx_vec], collapse = "+"),
                       sep = ""
  ))
  return(x)
}

# Helper function to calculate AIC scores for Naive best fit search
get_aic_scores = function(l, df, obj_idx){
  aic_scores = c()
  for(i in 1:length(l)){
    new_score = AIC(lm(lm_idx_formula(df, obj_idx, l[[i]]),data=df))
    aic_scores = append(aic_scores, new_score)
  }
  return(aic_scores)
}

# Converts a vector of indexes into a genetic binary string
idx_to_gen_string = function(l, n){
  y = rep(0, n);
  y[l] = 1;
  return(paste(y, collapse = ""))
}

# Converts a genetic binary string into a vector of indexes
gen_string_to_idx = function(l){
  c = c()
  for(i in 1:nchar(l)){
    if(substr(l, i, i)=='1'){
      c = append(c,i)
    }
  }
  return(c)
}

# This test takes a small dataset of 1000 random observations and 13 variables.
# It does both a naive grid search for the best variable combination as well as
# A Genetic Algorithm search. It then compares results to some known truths.
test_that("Is the Genetic Algorithm as Accurate and Faster Than Naive Variable Selection", {
  df = large_testing_data
  var_idx = 1:13
  obj_idx = 14

  full_time = system.time({ # Get Naive Results
    l = get_var_idx_subsets(var_idx)
    aic_scores = get_aic_scores(l ,df, obj_idx)
  })
  ga_time = system.time({ga_res=select(df, obj_idx, n_ind = 4*13)}) # Get GA results

  expect_lt(ga_time[3], full_time[3]) # GA time should be less than naive time

  true_idx = c(5,8,13)
  sol_idx = gen_string_to_idx(ga_res$best_ind)
  # The true variables that consist of our parameter should be present in our GA results
  expect_true(all(true_idx %in% sol_idx))

  min_idx = which.min(aic_scores)
  full_best_score = aic_scores[min_idx]
  # We expect on a small dataset like this that GA and a naive grid search
  # have extremely similar scores
  # print(full_best_score)
  # print(ga_res$best_ind_fit)
  expect_gte(full_best_score, ga_res$best_ind_fit- 2)
})

# This test takes a large dataset of 100 random observations and 50 variables.
# It does both a stepAIC search for the best variable combination as well as
# A Genetic Algorithm search. It then compares results to some known truths.
test_that("Can the Genetic Algorithm Find a Good Model Quickly On a Very Large Dataset", {
  data = huge_testing_data
  gen_res = select(data$df, 51, abs_conv_thresh = 0.1)

  step_result = MASS::stepAIC(lm(X51~., data=data$df), trace = F) # Get stepAIC results

  sol_idx = gen_string_to_idx(gen_res$best_ind)

  # The true variables that consist of our parameter should be present in our GA results
  expect_true(all(data$true_idx %in% sol_idx))

  # Get the best score from stepAIC
  stepAIC_idx = which(names(data$df) %in% names(step_result$coefficients))
  stepAIC_score = AIC(lm(lm_idx_formula(data$df, 51, stepAIC_idx),
                         data=data$df))

  # print(gen_res$best_ind_fit)
  # print(stepAIC_score)

  # We expect the GA best score to be equal to or at least close to the stepAIC best score
  expect_lte(gen_res$best_ind_fit-4,
             stepAIC_score)

})

