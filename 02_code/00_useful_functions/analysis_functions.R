# Function to get cluster means

create_cluster_means <- function(outcome, the_data) {
	aggregate(
		as.formula(paste0(outcome, " ~ tc_id")),
		data = the_data,
		FUN = mean,
		na.rm = T
	)
}

# Main regression analysis 

ols_main <- function(outcome,
										 treatment,
										 resample_FE = FALSE,
										 block_FE = TRUE,
										 audience_size = FALSE,
										 cluster_SE = TRUE,
										 covariates = NULL,
										 interact_covariates = FALSE,
										 the_data = el,
										 dosage = FALSE,
										 dosage_indicator = "dosage_corrected",
										 crossover_specification = NULL,
										 crossover_robust = FALSE,
										 ...) {
	if (crossover_robust) {
		the_data <- subset(the_data, treatment != "abortion_absenteeism")
	}
	
	if (dosage) {
		the_formula <-
			paste0(outcome,
						 " ~ dosage_",
						 treatment,
						 " + as.factor(",
						 dosage_indicator,
						 ")")
	} else {
		the_formula <- paste0(outcome, " ~ ", treatment)
	}
	
	if (!is.null(crossover_specification)) {
		if (crossover_specification == "abortion") {
			the_formula <- paste0(outcome, " ~ IPV * abortion")
		}
		if (crossover_specification == "saturated") {
			the_formula <- paste0(outcome, " ~ IPV * abortion * absenteeism")
		}
		
	}
	
	
	if (resample_FE) {
		the_formula <- paste0(the_formula, " + resample")
	}
	
	if (block_FE) {
		the_formula <- paste0(the_formula, " + as.factor(block_id)")
	}
	
	if (audience_size) {
		the_formula <- paste0(the_formula, " + n_end_mean")
	}
	
	if (!is.null(covariates)) {
		the_formula <- paste0(the_formula,
													" + ",
													paste(covariates,
																collapse = " + "))
		if (interact_covariates) {
			the_formula <-
				paste0(the_formula, " + ", paste(paste0(covariates, ":", treatment), collapse = " + "))
			
		}
		
	}
	
	the_formula <- as.formula(the_formula)
	
	the_fit <- lm(formula = the_formula,
								data = the_data,
								... = ...)
	
	if (cluster_SE) {
		vcov_mat <-
			cluster.vcov(model = the_fit, cluster = as.factor(the_data$tc_id))
	} else {
		vcov_mat <- vcov(the_fit)
	}
	
	cluster_means <-
		create_cluster_means(outcome = outcome, the_data = the_data)
	cluster_sd <- sd(cluster_means[, 2], na.rm = T)
	n_clusters <- length(unique(the_data$tc_id))
	
	desc_stats <- c(cluster_sd = cluster_sd, n_clusters = n_clusters)
	
	the_fit_sum <- coeftest(x = the_fit, vcov. = vcov_mat)
	
	return(list(
		fit = the_fit,
		fit_summary = the_fit_sum,
		desc_stats = desc_stats
	))
	
}

# Function to compute RI p-values
get_RI_pvals <- function(outcome,
												 treatment,
												 resample_FE = TRUE,
												 block_FE = TRUE,
												 audience_size = FALSE,
												 cluster_SE = FALSE,
												 covariates = NULL,
												 interact_covariates = FALSE,
												 the_data = el,
												 assignment_data = treatment_assignment,
												 extract_function = coef,
												 analysis_function = ols_main,
												 crossover_specification = NULL,
												 sims = 1000,
												 lwr_upr_two = "two",
												 crossover_robust = FALSE,
												 seed = 123456789,
												 ...) {
	set.seed(seed)
	
	observed <-
		extract_function(
			analysis_function(
				outcome = outcome,
				treatment = treatment,
				resample_FE = resample_FE,
				block_FE = block_FE,
				audience_size = audience_size,
				cluster_SE = cluster_SE,
				covariates = covariates,
				interact_covariates = interact_covariates,
				the_data = the_data,
				crossover_specification = crossover_specification,
				crossover_robust = crossover_robust
			)$fit
		)
	
	replications <- replicate(n = sims,
														expr = {
															new_data <-
																rerandomize(tc_level_data = assignment_data, analysis_data = the_data)
															extract_function(
																analysis_function(
																	outcome = outcome,
																	treatment = treatment,
																	resample_FE = resample_FE,
																	block_FE = block_FE,
																	audience_size = audience_size,
																	cluster_SE = cluster_SE,
																	covariates = covariates,
																	interact_covariates = interact_covariates,
																	the_data = new_data,
																	crossover_specification = crossover_specification,
																	crossover_robust = crossover_robust
																)$fit
															)
														})
	
	if (!lwr_upr_two %in% c("upr", "lwr", "two")) {
		stop(
			"Please use Lwr_Upr_two to specify whether the hypothesis is upper-tailed (Upr), lower-tailed (Lwr) or two-tailed (two)."
		)
	}
	
	
	
	if (lwr_upr_two == "two") {
		ri_pvals <- rowMeans(abs(observed) <= abs(replications))
	} else {
		if (lwr_upr_two == "upr") {
			ri_pvals <- rowMeans(observed <= replications)
		} else {
			ri_pvals <- rowMeans(observed >= replications)
		}
	}
	
	
	return(list(
		observed = observed,
		replications = replications,
		ri_pvals = ri_pvals
	))
	
}


# Function to run ordered logit model

ordered_logit_main <- function(outcome,
															 treatment,
															 resample_FE = FALSE,
															 block_FE = TRUE,
															 audience_size = FALSE,
															 cluster_SE = TRUE,
															 covariates = NULL,
															 interact_covariates = FALSE,
															 the_data = el,
															 crossover_specification = NULL,
															 crossover_robust = FALSE,
															 ...) {
	the_formula <- paste0(outcome, " ~ ", treatment)
	
	
	if (resample_FE) {
		the_formula <- paste0(the_formula, " + resample")
	}
	
	if(block_FE){
		
		blocks <- paste(sapply(2:16, function(x) paste0("block_",x)),collapse = " + ")
		
		the_formula <- paste0(the_formula, " +", blocks)
	}

	
	if (audience_size) {
		the_formula <- paste0(the_formula, " + n_end_mean")
	}
	
	if (!is.null(covariates)) {
		the_formula <- paste0(the_formula,
													" + ",
													paste(covariates,
																collapse = " + "))
		
		if (interact_covariates) {
			the_formula <-
				paste0(the_formula, " + ", paste(paste0(covariates, ":", treatment), collapse = " + "))
			
		}
		
	}
	
	the_formula <- as.formula(the_formula)
	
	the_fit <- lrm(
		the_formula,
		data = the_data,
		x = TRUE,
		y = TRUE,
		... = ...
	)
	
	if (cluster_SE) {
		the_fit <- robcov(the_fit, as.factor(the_data$tc_id))
		
	}
	
	cluster_means <-
		create_cluster_means(outcome = outcome, the_data = the_data)
	cluster_sd <- sd(cluster_means[, 2], na.rm = T)
	n_clusters <- length(unique(the_data$tc_id))
	
	desc_stats <- c(cluster_sd = cluster_sd, n_clusters = n_clusters)
	
	return(list(fit = the_fit, desc_stats = desc_stats))
	
}

ordered_probit_main <- function(
  outcome,
  treatment,
  resample_FE = FALSE,
  block_FE = TRUE,
  audience_size = FALSE,
  cluster_SE = TRUE,
  covariates = NULL,
  interact_covariates = FALSE,
  the_data = el,
  crossover_specification = NULL,
  crossover_robust = FALSE,
  ...
){
  
  the_formula <- paste0("as.factor(",outcome,")", " ~ ", treatment)  
  
  
  if(resample_FE){
    the_formula <- paste0(the_formula, " + resample")
  }
  
  if (block_FE) {
  	the_formula <- paste0(the_formula, " + as.factor(block_id)")
  }
  
  if(audience_size){
    the_formula <- paste0(the_formula, " + n_end_mean")
  }
  
  if(!is.null(covariates)){
    the_formula <- paste0(
      the_formula, 
      " + ",
      paste(covariates,
            collapse = " + ")
    )
    
    if(interact_covariates){
      the_formula <- paste0(the_formula, " + ", paste(paste0(covariates,":",treatment),collapse = " + "))
      
    }  
    
  }
  
  the_formula <- as.formula(the_formula)
  
  cluster_means <- create_cluster_means(outcome = outcome,the_data = the_data)
  cluster_sd <- sd(cluster_means[,2],na.rm = T)
  
  the_data[,outcome] <- as.factor(the_data[,outcome])
  
  the_fit <- polr(formula = the_formula, 
                  method = "probit",
                  data = the_data)
  
  
  
  if(cluster_SE){
    # THis has to be done in global environment, it seems
    print("Cluster SEs must be calculated in the global environment")
  } 
  
  n_clusters <- length(unique(the_data$tc_id))
  
  desc_stats <- c(cluster_sd = cluster_sd,n_clusters = n_clusters)
  
  return(list(fit = the_fit,desc_stats = desc_stats))
  
}


poisson_analysis <- function(
	outcome,
	treatment,
	resample_FE = FALSE,
	block_FE = TRUE,
	audience_size = FALSE,
	cluster_SE = TRUE,
	covariates = NULL,
	interact_covariates = FALSE,
	the_data = el,
	dosage = FALSE,
	dosage_indicator = "dosage_corrected",
	crossover_specification = NULL,
	crossover_robust = FALSE,
	...
){
	
	if(dosage) {
		the_formula <- paste0(outcome, " ~ dosage_", treatment, " + as.factor(", dosage_indicator, ")")  
	} else {
		the_formula <- paste0(outcome, " ~ ", treatment)  
	}
	
	if(!is.null(crossover_specification)){
		if(crossover_specification == "abortion"){
			the_formula <- paste0(outcome, " ~ IPV * abortion" )
		} 
		if(crossover_specification == "saturated"){
			the_formula <- paste0(outcome, " ~ IPV * abortion * absenteeism - IPV : abortion : absenteeism")
		}
		
	}
	
	
	if(resample_FE){
		the_formula <- paste0(the_formula, " + resample")
	}
	
	if(block_FE){
		the_formula <- paste0(the_formula, " + as.factor(block_id)")
	}
	
	if(audience_size){
		the_formula <- paste0(the_formula, " + n_end_mean")
	}
	
	if(!is.null(covariates)){
		the_formula <- paste0(
			the_formula, 
			" + ",
			paste(covariates,
						collapse = " + ")
		)
		if(interact_covariates){
			the_formula <- paste0(the_formula, " + ", paste(paste0(covariates,":",treatment),collapse = " + "))
			
		}  
		
	}
	
	the_formula <- as.formula(the_formula)
	
	the_fit <- glm(
		formula = the_formula,
		data = the_data,
		family = poisson(),
		... = ...
	)
	
	if(cluster_SE){ }
	
	cluster_means <- create_cluster_means(outcome = outcome,the_data = the_data)
	cluster_sd <- sd(cluster_means[,2],na.rm = T)
	n_clusters <- length(unique(the_data$tc_id))
	
	desc_stats <- c(cluster_sd = cluster_sd,n_clusters = n_clusters)
	
	the_fit_sum <- summary(the_fit)$coefficients
	
	return(list(fit = the_fit,fit_summary = the_fit_sum,desc_stats = desc_stats))
	
}



ols_heterogeneous_effects <- function(
	outcome,
	treatment,
	resample_FE = FALSE,
	block_FE = TRUE,
	audience_size = FALSE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = el,
	dosage = FALSE,
	dosage_indicator = "dosage_corrected",
	...
){
	
	
	the_formula <- paste0(outcome, " ~ ", treatment,"*",covariates)  
	
	
	if(resample_FE){
		the_formula <- paste0(the_formula, " + resample")
	}
	
	if(block_FE){
		the_formula <- paste0(the_formula, " + as.factor(block_id)")
	}
	
	if(audience_size){
		the_formula <- paste0(the_formula, " + n_end_mean")
	}
	
	the_formula <- as.formula(the_formula)
	
	the_fit <- lm(
		formula = the_formula,
		data = the_data,
		... = ...
	)
	
	if(cluster_SE){
		vcov_mat <- cluster.vcov(model = the_fit,cluster = as.factor(the_data$tc_id))
	} else {
		vcov_mat <- vcov(the_fit)
	}
	
	cluster_means <- create_cluster_means(outcome = outcome,the_data = the_data)
	cluster_sd <- sd(cluster_means[,2],na.rm = T)
	n_clusters <- length(unique(the_data$tc_id))
	
	desc_stats <- c(cluster_sd = cluster_sd,n_clusters = n_clusters)
	
	the_fit_sum <- coeftest(x = the_fit,vcov. = vcov_mat)
	
	return(list(fit = the_fit,fit_summary = the_fit_sum,desc_stats = desc_stats))
	
}


probit_analysis <- function(
	outcome,
	treatment,
	resample_FE = FALSE,
	block_FE = TRUE,
	audience_size = FALSE,
	cluster_SE = TRUE,
	covariates = NULL,
	interact_covariates = FALSE,
	the_data = el,
	dosage = FALSE,
	dosage_indicator = "dosage_corrected",
	crossover_specification = NULL,
	crossover_robust = FALSE,
	...
){
	
	if(dosage) {
		the_formula <- paste0(outcome, " ~ dosage_", treatment, " + as.factor(", dosage_indicator, ")")  
	} else {
		the_formula <- paste0(outcome, " ~ ", treatment)  
	}
	
	if(!is.null(crossover_specification)){
		if(crossover_specification == "abortion"){
			the_formula <- paste0(outcome, " ~ IPV * abortion" )
		} 
		if(crossover_specification == "saturated"){
			the_formula <- paste0(outcome, " ~ IPV * abortion * absenteeism - IPV : abortion : absenteeism")
		}
		
	}
	
	
	if(resample_FE){
		the_formula <- paste0(the_formula, " + resample")
	}
	
	if(block_FE){
		the_formula <- paste0(the_formula, " + as.factor(block_id)")
	}
	
	if(audience_size){
		the_formula <- paste0(the_formula, " + n_end_mean")
	}
	
	if(!is.null(covariates)){
		the_formula <- paste0(
			the_formula, 
			" + ",
			paste(covariates,
						collapse = " + ")
		)
		if(interact_covariates){
			the_formula <- paste0(the_formula, " + ", paste(paste0(covariates,":",treatment),collapse = " + "))
			
		}  
		
	}
	
	the_formula <- as.formula(the_formula)
	
	the_fit <- glm(
		formula = the_formula,
		data = the_data,
		family = binomial(link = "probit"),
		... = ...
	)
	
	if(cluster_SE){ 
		# This has to be done in global environment, it seems
		print("Cluster SEs must be calculated in the global environment")
	}
	
	cluster_means <- create_cluster_means(outcome = outcome,the_data = the_data)
	cluster_sd <- sd(cluster_means[,2],na.rm = T)
	n_clusters <- length(unique(the_data$tc))
	
	desc_stats <- c(cluster_sd = cluster_sd,n_clusters = n_clusters)
	
	the_fit_sum <- summary(the_fit)$coefficients
	
	return(list(fit = the_fit,fit_summary = the_fit_sum,desc_stats = desc_stats))
	
}


