reference_categories <- c(
  "not_married",
  "illiterate",
  "luganda_lang",
  "muganda_tribe",
  "no_work",
  "christian_only",
  "all_boys",
  "all_girls",
  "more_boys_than_girls",
  "more_girls_than_boys",
  "roughly_equal",
  "idntcraslngasonisaby",
  "idntcraslngasonisagr",
  "it_doesnt_matter",
  "not_married_ml",
  "illiterate_ml",
  "luganda_lang_ml",
  "muganda_tribe_ml",
  "no_work_ml",
  "christian_only_ml",
  "block_1"
)

IPV_outcomes_el_men <- IPV_outcomes_el[-which(IPV_outcomes_el %in% c("household_violence","household_violence_frequency"))]

if(rerun_lasso){
  
  # Set seed ----------------------------------------------------------------
  
  set.seed(123456789)
  
  # Do for compliers using complier data 
  
  lasso_covariates <- notin(c(covariates_el,covariates_ml,block_vars),reference_categories)
  
  IPV_lasso_covariates_panel_M <- lapply(
    FUN = run_lasso_get_variables,
    X = c(IPV_outcomes_el_men,IPV_outcomes_ml),
    covariates = lasso_covariates,
    data = el[el$respondent_category == "Complier" & el$female == 0,],
    N_folds = 10,
    sims = 10
  )
  
  IPV_lasso_covariates_panel_W <- lapply(
    FUN = run_lasso_get_variables,
    X = c(IPV_outcomes_el,IPV_outcomes_ml),
    covariates = lasso_covariates,
    data = el[el$respondent_category == "Complier" & el$female == 1,],
    N_folds = 10,
    sims = 10
  )
  
  # Do for all respondents using all data
  
  lasso_covariates <- notin(c(covariates_el,block_vars),reference_categories)
  
  IPV_lasso_covariates_M <- lapply(
    FUN = run_lasso_get_variables,
    X = IPV_outcomes_el_men,
    covariates = lasso_covariates,
    data = subset(el, female == 0),
    N_folds = 10,
    sims = 10
  )
  
  IPV_lasso_covariates_W <- lapply(
    FUN = run_lasso_get_variables,
    X = IPV_outcomes_el,
    covariates = lasso_covariates,
    data = subset(el, female == 1),
    N_folds = 10,
    sims = 10
  )
  
  names(IPV_lasso_covariates_M) <- IPV_outcomes_el_men
  names(IPV_lasso_covariates_panel_M) <- c(IPV_outcomes_el_men,IPV_outcomes_ml)
  names(IPV_lasso_covariates_W) <- IPV_outcomes_el
  names(IPV_lasso_covariates_panel_W) <- c(IPV_outcomes_el,IPV_outcomes_ml)
  
  save(
    IPV_lasso_covariates_M,IPV_lasso_covariates_panel_M,
    IPV_lasso_covariates_W,IPV_lasso_covariates_panel_W,
       file = "01_data/lasso_selected_covariates.Rdata")

} else{
  
  load("01_data/lasso_selected_covariates.Rdata")
  
}

