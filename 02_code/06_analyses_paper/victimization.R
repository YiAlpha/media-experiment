# Collapse to village level -----------------------------------------------
village_level_all <- 
  el %>% 
  filter(female == 1) %>% 
  group_by(tc_id,block_id,IPV) %>% 
  summarize(
    household_violence = mean(household_violence,na.rm = T),
    any_violence = mean(any_violence,na.rm = T),
    household_violence_frequency = mean(household_violence_frequency,na.rm = T),
    n_end_mean = unique(n_end_mean)
  )

village_level_all$tc <- village_level_all$tc_id

# Estimate effects --------------------------------------------------------

household_violence_FE <- lm(
  household_violence ~ IPV + as.factor(block_id) + n_end_mean, data = village_level_all
)
household_violence_FE_pvals <- get_RI_pvals(
  outcome = "household_violence",
  treatment = "IPV",
  resample_FE = F,
  block_FE = T,
  audience_size = TRUE,
  cluster_SE = F,
  covariates = NULL,
  the_data = village_level_all,
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")

household_violence_ind_FE <- ols_main(
  outcome = "household_violence",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE)

household_violence_ind_FE_pvals <- get_RI_pvals(
  outcome = "household_violence",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el, female == 1),
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")

household_violence_ind_FE_comp <- ols_main(
  outcome = "household_violence",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  the_data = subset(el, female == 1 & complier),
  dosage = FALSE,
  dosage_indicator = FALSE)
household_violence_ind_FE_comp_pvals <- get_RI_pvals(
  outcome = "household_violence",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el, female == 1 & complier),
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")

any_violence_FE <- lm(
  any_violence ~ IPV + as.factor(block_id) + n_end_mean, data = village_level_all
)
any_violence_FE_pvals <- get_RI_pvals(
  outcome = "any_violence",
  treatment = "IPV",
  resample_FE = F,
  block_FE = T,
  audience_size = TRUE,
  cluster_SE = F,
  covariates = NULL,
  the_data = village_level_all,
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")

any_violence_ind_FE <- ols_main(
  outcome = "any_violence",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE)
any_violence_ind_FE_pvals <- get_RI_pvals(
  outcome = "any_violence",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el, female == 1),
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")

any_violence_ind_FE_comp <- ols_main(
  outcome = "any_violence",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  the_data = subset(el, female == 1 & complier),
  dosage = FALSE,
  dosage_indicator = FALSE)
any_violence_ind_FE_comp_pvals <- get_RI_pvals(
  outcome = "any_violence",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el, female == 1 & complier),
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")


# household frequency individual ordered probit
household_frequency_ind_oprobit <-
  ordered_probit_main(
    outcome = "household_violence_frequency",
    treatment = "IPV",
    resample_FE = TRUE,
    block_FE = TRUE,
    audience_size = TRUE,
    cluster_SE = TRUE,
    covariates = NULL,
    the_data = subset(el, female == 1)
  )
# All of this is necessary to get SEs 
# Can't be done inside functional 
# environment due to the way fit objects work
household_frequency_ind_oprobit_fit <- polr(formula = as.factor(household_violence_frequency) ~ IPV + resample + 
                                              as.factor(block_id) + n_end_mean, 
                                            method = "probit",
                                            data = subset(el, female == 1))
vcov_mat <- vcovCL(x = household_frequency_ind_oprobit_fit, 
                   cluster = subset(el, female == 1)$tc_id) 
household_frequency_ind_oprobit_ses <- coeftest(x = household_frequency_ind_oprobit_fit, 
                                                vcov. = vcov_mat)[,"Std. Error"]

household_frequency_ind_oprobit_pvals <- get_RI_pvals(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el, female == 1),
  analysis_function = ordered_probit_main,
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")


household_frequency_ind_oprobit_comp <-
  ordered_probit_main(
    outcome = "household_violence_frequency",
    treatment = "IPV",
    resample_FE = TRUE,
    block_FE = TRUE,
    audience_size = TRUE,
    cluster_SE = TRUE,
    the_data = subset(el, female == 1 & complier)
  )
household_frequency_ind_oprobit_comp_fit <- 
  polr(formula = as.factor(household_violence_frequency) ~ IPV + resample + 
         as.factor(block_id) + n_end_mean, 
       method = "probit",
       data = subset(el, female == 1 & complier))
vcov_mat <- vcovCL(x = household_frequency_ind_oprobit_comp_fit, 
                   cluster = subset(el, female == 1 & complier)$tc_id) 
household_frequency_ind_oprobit_comp_ses <- 
  coeftest(x = household_frequency_ind_oprobit_comp_fit, 
           vcov. = vcov_mat)[,"Std. Error"]

household_frequency_ind_oprobit_comp_pvals <- get_RI_pvals(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el, female == 1 & complier),
  analysis_function = ordered_probit_main,
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")


control_means <- with(
  village_level_all, 
  c(
    "Control Mean",
    round(mean(el$household_violence[el$IPV == 0 & el$female == 1],na.rm = TRUE), 2),
    round(mean(household_violence[IPV == 0],na.rm = TRUE), 2),
    round(mean(household_violence[IPV == 0],na.rm = TRUE), 2),
    round(mean(el$any_violence[el$IPV == 0 & el$female == 1],na.rm = TRUE), 2),
    round(mean(any_violence[IPV == 0],na.rm = TRUE), 2),
    round(mean(any_violence[IPV == 0],na.rm = TRUE), 2),
    round(mean(el$household_violence_frequency[el$IPV == 0 & el$female == 1], na.rm = TRUE), 2),
    round(mean(el$household_violence_frequency[el$IPV == 0 & el$female == 1], na.rm = TRUE), 2)
  )
)

pval_lines <- c(
  "RI $p$-values: IPV",
  round(household_violence_FE_pvals$ri_pvals["IPV"],3),
  round(household_violence_ind_FE_pvals$ri_pvals["IPV"],3),
  round(household_violence_ind_FE_comp_pvals$ri_pvals["IPV"],3),
  round(any_violence_FE_pvals$ri_pvals["IPV"],3),
  round(any_violence_ind_FE_pvals$ri_pvals["IPV"],3),
  round(any_violence_ind_FE_comp_pvals$ri_pvals["IPV"],3),
  round(household_frequency_ind_oprobit_pvals$ri_pvals["IPV"],3),
  round(household_frequency_ind_oprobit_comp_pvals$ri_pvals["IPV"],3)
)


hypothesis_lines <- c(
  "Hypothesis",
  "Two",
  "Two",
  "Two",
  "Two",
  "Two",
  "Two",
  "Two",
  "Two"
)

sink("03_tables/victimization.tex")
stargazer(
  ... = list(
    household_violence_FE,
    household_violence_ind_FE$fit,
    household_violence_ind_FE_comp$fit,
    any_violence_FE,
    any_violence_ind_FE$fit,
    any_violence_ind_FE_comp$fit,
    household_frequency_ind_oprobit_fit, 
    household_frequency_ind_oprobit_comp_fit
  ),
  type = "latex",
  p = list(
    household_violence_FE_pvals$ri_pvals,
    household_violence_ind_FE_pvals$ri_pvals,
    household_violence_ind_FE_comp_pvals$ri_pvals,
    any_violence_FE_pvals$ri_pvals,
    any_violence_ind_FE_pvals$ri_pvals,
    any_violence_ind_FE_comp_pvals$ri_pvals,
    household_frequency_ind_oprobit_pvals$ri_pvals,
    household_frequency_ind_oprobit_comp_pvals$ri_pvals
  ),
  se = list(
    summary(household_violence_FE)$coefficients[, "Std. Error"],
    household_violence_ind_FE$fit_summary[, "Std. Error"],
    household_violence_ind_FE_comp$fit_summary[, "Std. Error"],
    summary(any_violence_FE)$coefficients[, "Std. Error"],
    any_violence_ind_FE$fit_summary[, "Std. Error"],
    any_violence_ind_FE_comp$fit_summary[, "Std. Error"],
    household_frequency_ind_oprobit_ses,
    household_frequency_ind_oprobit_comp_ses
  ),
  covariate.labels = "Anti-VAW Media",
  keep = c("IPV"),
  omit.stat = c("rsq","f","ser", "chi2"),
  column.separate = c(3,3, 2),
  column.labels = c("Number of Incidents","Any Incidents", "Violence Frequency"),
  table.layout = "=c#-t-as=n",
  dep.var.labels.include = FALSE,
  no.space = T,
  omit = "block_id",
  add.lines = list(
    control_means,
    pval_lines,
    hypothesis_lines,
    c("Sample",
      "All W","All W","W compl.",
      "All W","All W","W compl.",
      "All W","W compl."),
    c("Analysis Level",
      "Clus.","Indiv.","Indiv.",
      "Clus.","Indiv.","Indiv.",
      "Indiv.", "Indiv."),    
    c("Block FE",
      "Yes","Yes","Yes",
      "Yes","Yes","Yes",
      "Yes", "Yes"),
    c("Estimator", 
      "OLS", "OLS", "OLS",
      "OLS", "OLS", "OLS",
      "Ordered Probit", "Ordered Probit")),
  notes.label = "",
  float = FALSE
  # style = "qje"
)
sink()







