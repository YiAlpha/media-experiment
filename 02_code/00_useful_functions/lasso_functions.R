# Core functions for the lasso analysis -----------------------------------

# This function performs 10-fold cross-validation 10 times, selecting the 
# tuning parameter (lambda) with the lowest average (across folds)
# test error at each iteration. It returns the 10 values of lambda that 
# minimize cross-validation error. The average of these values is considered 
# the optimal lambda. 

get_optimal_lambda <-
	function(outcome_name,
					 covariates,
					 data,
					 N_folds = 4,
					 sims = 10) {
		lambdas <-
			replicate(n = sims, expr = {
				y <- as.matrix(data[, outcome_name])
				X <- as.matrix(data[, covariates])
				cv_out <- cv.glmnet(
					x = X,
					y = y,
					nfolds = N_folds,
					alpha = 1
				)
				cv_out$lambda.min
			})
		return(lambdas)
	}

# Using the best lambda - as determined by the cross-validation exercise - 
# perform penalized maximum likelihoof via lasso, and return the fitted model.

lasso_with_best_lambda <-
	function(outcome_name, covariates, Lambda, data) {
		y <- as.matrix(data[, outcome_name])
		X <- as.matrix(data[, covariates])
		glm_fit <- glmnet(
			x = X,
			y = y,
			family = "gaussian",
			alpha = 1,
			nlambda = 1,
			lambda = Lambda,
			standardize = T
		)
		return(glm_fit)
	}

# This function retrieves the non-zero coefficients from the model.

get_varnames <- function(lasso_fit) {
	varnames <- as.matrix(coef(lasso_fit))
	non_zero <- abs(varnames) > 0
	varnames[non_zero, ]
}

# This function runs all of the other functions in order to get the non-zero
# coefficients

run_lasso_get_variables <-
	function(outcome_name,
					 covariates,
					 data,
					 N_folds,
					 sims) {
		lambdas <- get_optimal_lambda(
			outcome_name = outcome_name,
			covariates = covariates,
			N_folds = N_folds,
			sims = sims,
			data = data
		)
		lasso_fit <- lasso_with_best_lambda(
			outcome_name = outcome_name,
			covariates = covariates,
			Lambda = mean(lambdas),
			data = data
		)
		return(get_varnames(lasso_fit))
	}


# Function for grabbing covariates from lasso output ----------------------

get_covariates <-
	function(var_name,
					 lasso_list,
					 names_only = TRUE,
					 no_festivalcept = TRUE) {
		covariates <- lasso_list[var_name][[1]]
		if (names_only) {
			covariates <- names(covariates)
		}
		if (no_festivalcept) {
			covariates <- covariates[-1]
		}
		return(covariates)
	}

prep_covariates <- function(var_name, lasso_list) {
	covariates <- names(lasso_list[var_name][[1]])
	
	if (covariates[1] == "(Intercept)") {
		covariates <- covariates[-1]
	}
	
	covariates <- covariates[!grepl("block_", covariates)]
	
	return(covariates)
}

