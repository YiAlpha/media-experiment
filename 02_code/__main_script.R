rm(list = ls())

# Load useful functions ---------------------------------------------------

source("02_code/00_useful_functions/variable_building_functions.R")

# Make codebooks ----------------------------------------------------------

# Codebooks are R list objects that correspond to each question in the 
# survey. For example, mlcb$sex_q2_1 gives the labels corresponding to 
# variable sex_q2_1 in the adult household survey

source("02_code/01_codebook/make_codebook.R")

# Load data and packages --------------------------------------------------

# - packages will be installed locally via packrat
# - load the different datasets used in the analysis

source("02_code/02_load_and_clean_data/00_load_packages_and_data.R")

# Building data for analysis ----------------------------------------------

rerun_data_building <- TRUE

if(rerun_data_building){
	
	# Clean data --------------------------------------------------------------
	
	# These scripts implement the cleaning of data conducted in the field by 
	# the field manager.  
	
	# Random sampling data 
	source("02_code/02_load_and_clean_data/01_clean_random_sampling_data.R")
	
	# Sampling Radius Data
	source("02_code/02_load_and_clean_data/01_clean_sampling_radius_data.R")
	
	# Intervention data 
	source("02_code/02_load_and_clean_data/01_clean_festival_data.R")
	
	# Household data
	source("02_code/02_load_and_clean_data/01_clean_household_data_midline.R")
	source("02_code/02_load_and_clean_data/01_clean_household_data_endline.R")
	
	# VHT data
	source("02_code/02_load_and_clean_data/01_clean_vht_data_midline.R")
	source("02_code/02_load_and_clean_data/01_clean_vht_data_endline.R")
	
	# Code Variables Midline---------------------------------------------------
	
	# These scripts create the outcomes and covariates used in the analysis
	# of the midline data.
	
	# Household Survey 
	source("02_code/03_variable_coding/01_code_outcomes_hh_midline.R")
	source("02_code/03_variable_coding/01_code_covariates_hh_midline.R")
	
	# VHT Survey
	source("02_code/03_variable_coding/01_code_variables_vht_midline.R")
	
	# Build lists of variable names
	source("02_code/03_variable_coding/02_build_variable_lists_midline.R")
	
	# Save non-imputed midline data
	ml_no_imp <- ml
	
	# Impute Variables Midline ------------------------------------------------
	
	# Here we use multiple chained imputation as pre-registered to impute 
	# item-level missingness in the midline.
	
	# Household Survey
	ml <- arrange(ml, KEY)
	source("02_code/03_variable_coding/03_impute_variables_hh_midline.R")
	
	# VHT Survey
	vht_ml <- arrange(vht_ml,KEY)
	source("02_code/03_variable_coding/03_impute_variables_vht_midline.R")
	
	# Merging -----------------------------------------------------------------
	
	# These scripts merge midline and endline surveys, and also bring in 
	# cluster-level covariates into the household data.
	
	# Merge midline and endline household surveys
	source("02_code/04_merging/01_merge_midline_endline.R")
	
	# Merge trading center level variables into other datasets
	source("02_code/04_merging/02_merge_in_trading_center_level_variables.R")
	
	# Code Variables Endline --------------------------------------------------
	
	# These scripts build variables used in the analysis of he endline data.
	
	# Household Survey
	source("02_code/03_variable_coding/01_code_outcomes_hh_endline.R")
	
	# These questions were randomized and so need to be re-merged
	source("02_code/03_variable_coding/01_code_outcomes_hh_endline_randomized.R")
	source("02_code/03_variable_coding/01_code_compliance_related_hh_endline.R")
	source("02_code/03_variable_coding/01_code_covariates_hh_endline.R")
	
	# VHT Survey
	source("02_code/03_variable_coding/01_code_covariates_vht_endline.R")
	source("02_code/03_variable_coding/01_code_outcomes_vht_endline.R")
	
	# Build lists of variable names for use in imputation and in analysis
	source("02_code/03_variable_coding/02_build_variable_lists_endline.R")
	
	# Save non-imputed endline data
	el_no_imp <- el
	
	# Impute Variables Endline ------------------------------------------------
	
	# Here we use multiple chained imputation as pre-registered to impute 
	# item-level missingness in the endline.
	
	# Household Survey
	el <- arrange(el,KEY)
	source('02_code/03_variable_coding/02_impute_variables_hh_endline.R')
	
	# VHT Survey
	vht_el <- arrange(vht_el,KEY)
	source("02_code/03_variable_coding/03_impute_variables_vht_endline.R")
	
	# Save data image ---------------------------------------------------------
	
	save.image(file = "01_data/UG_VAW_data.Rdata")
	
} else {
	
	load(file = "01_data/UG_VAW_data.Rdata")
	
}

# Build Indices -----------------------------------------------------------

# Building index variables from imputed components

source("02_code/03_variable_coding/04_build_indices.R")

# Select covariates -------------------------------------------------------

# Load custom-written lasso selection functions
source("02_code/00_useful_functions/lasso_functions.R")

# Set to TRUE to rerun lasso selection
rerun_lasso <- TRUE
source("02_code/05_covariate_selection/01_select_covariates.R")

# Analyses ----------------------------------------------------------------

# These are the estimator and RI functions used in all analyses
source('02_code/00_useful_functions/analysis_functions.R')
source("02_code/00_useful_functions/randomization_functions.R")

# Set simulations for analyses
sims <- 3000

rerun_paper_analyses <- TRUE
if(rerun_paper_analyses){
	source("02_code/06_analyses_paper/attendance_plot.R")
	source("02_code/06_analyses_paper/reporting_attitudes_women.R")
	source("02_code/06_analyses_paper/reporting_attitudes_men.R")
	source('02_code/06_analyses_paper/costs.R')
	source('02_code/06_analyses_paper/victimization.R')
	source('02_code/06_analyses_paper/alternative_exp_coef_plot.R')
}

rerun_appendix_analyses <- TRUE
if(rerun_appendix_analyses){
	source("02_code/07_analyses_appendix/descriptive_stats_compliers.R")
	source("02_code/07_analyses_appendix/extreme_value_bounds.R",print.eval = T)
	source('02_code/07_analyses_appendix/effects_on_compliance.R',print.eval = T)
	source('02_code/07_analyses_appendix/attendance_balance.R')
	source('02_code/07_analyses_appendix/ml_balance_all.R')
	source('02_code/07_analyses_appendix/ml_balance_compliers.R')
	source('02_code/07_analyses_appendix/ml_balance_women_compliers.R')
	source('02_code/07_analyses_appendix/ml_balance_men_compliers.R')
	source('02_code/07_analyses_appendix/el_balance_all.R')
	source('02_code/07_analyses_appendix/el_balance_women.R')
	source('02_code/07_analyses_appendix/el_balance_compliers.R')
	source('02_code/07_analyses_appendix/el_balance_women_compliers.R')
	source('02_code/07_analyses_appendix/balance_women_panel_compliers_el.R')
	source('02_code/07_analyses_appendix/balance_men_all_compliers_el.R')	
	source('02_code/07_analyses_appendix/balance_men_panel_compliers_el.R')
	source('02_code/07_analyses_appendix/f_tests_for_crossovers.R')
	source('02_code/07_analyses_appendix/crossover_robust_estimation.R')
	source('02_code/07_analyses_appendix/nonlinear_models.R')
	source('02_code/07_analyses_appendix/main_result_robustness_table.R')
	source('02_code/07_analyses_appendix/free_riding.R')
	source('02_code/07_analyses_appendix/table_coef_plot_men.R')
	source('02_code/07_analyses_appendix/table_coef_plot_women.R')
	source('02_code/07_analyses_appendix/violence_prevalence.R')
	source('02_code/07_analyses_appendix/violence_prevalence_correlations.R',print.eval = T)
	rerun_victimization_extrapolations <- TRUE
	if(rerun_victimization_extrapolations){
		bootstrap_sims <- 3000
		ml <- arrange(ml, KEY)
		source("02_code/07_analyses_appendix/victimization_bootstrap_effects.R")
	}
	source('02_code/07_analyses_appendix/heterogeneous_effects_socio_economic_status.R',print.eval = T)
	source('02_code/07_analyses_appendix/panel_complier_effect.R')
	source('02_code/07_analyses_appendix/correlations_attitudes_violence.R')
}












