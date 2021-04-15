# Household violence - all 

# Get fit
household_violence_ind_FE <- glm(household_violence ~ IPV + as.factor(block_id) + resample + n_end_mean, 
                                 data = subset(el,female == 1), 
                                 family = poisson)

# Calculate clustered standard errors
vcov_mat <-  vcovCL(household_violence_ind_FE, cluster = subset(el, female == 1)$tc_id)
household_violence_ind_FE_ses <- coeftest(household_violence_ind_FE, vcov. = vcov_mat)[,"Std. Error"]

# Caluclate RI p-values
household_violence_ind_FE_pvals <- get_RI_pvals(
  outcome = "household_violence",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el,female == 1),
  analysis_function = poisson_analysis,
  sims = sims
)

# Household violence - compliers

# Get fit
household_violence_ind_FE_comp <- glm(household_violence ~ IPV + as.factor(block_id) + resample + n_end_mean, 
                                 data = subset(el,female == 1 & complier), 
                                 family = poisson)

# Calculate clustered standard errors
vcov_mat <-  vcovCL(household_violence_ind_FE_comp, cluster = subset(el, female == 1 & complier)$tc_id)
household_violence_ind_FE_comp_ses <- coeftest(household_violence_ind_FE_comp, vcov. = vcov_mat)[,"Std. Error"]


# Calculate RI p-values
household_violence_ind_FE_comp_pvals <- get_RI_pvals(
  outcome = "household_violence",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el,female == 1 & complier),
  analysis_function = poisson_analysis,
  sims = sims
)

# Any violence - all

# Get fit
any_violence_ind_FE <- glm(any_violence ~ IPV + as.factor(block_id) + resample + n_end_mean, 
                                 data = subset(el,female == 1), 
                                 family = binomial(link = "probit"))

# Calculate clustered standard errors
vcov_mat <-  vcovCL(any_violence_ind_FE, cluster = subset(el, female == 1)$tc_id)
any_violence_ind_FE_ses <- coeftest(any_violence_ind_FE, vcov. = vcov_mat)[,"Std. Error"]


any_violence_ind_FE_pvals <- get_RI_pvals(
  outcome = "any_violence",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el,female == 1),
  analysis_function = probit_analysis,
  sims = sims
)

# Any violence - compliers

# Get fit
any_violence_ind_FE_comp <- glm(any_violence ~ IPV + as.factor(block_id) + resample + n_end_mean, 
                           data = subset(el,female == 1 & complier), 
                           family = binomial(link = "probit"))

# Calculate clustered standard errors
vcov_mat <-  vcovCL(any_violence_ind_FE_comp, cluster = subset(el, female == 1 & complier)$tc_id)
any_violence_ind_FE_comp_ses <- coeftest(any_violence_ind_FE_comp, vcov. = vcov_mat)[,"Std. Error"]


any_violence_ind_FE_comp_pvals <- get_RI_pvals(
  outcome = "any_violence",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el,female == 1 & complier),
  analysis_function = probit_analysis,
  sims = sims
)


# Any violence
household_violence_frequency_ind_FE <- ols_main(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el,female == 1)
)
household_violence_frequency_ind_FE_pvals <- get_RI_pvals(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el,female == 1),
  analysis_function = ols_main,
  sims = sims
)
household_violence_frequency_ind_FE_comp <- ols_main(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el,female == 1 & complier)
)
household_violence_frequency_ind_FE_comp_pvals <- get_RI_pvals(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el,female == 1 & complier),
  analysis_function = ols_main,
  sims = sims
)








control_means <- 
  c(
    "Control Mean",
    round(mean(el$household_violence[el$IPV == 0 & el$female == 1],na.rm = TRUE), 2),
    round(mean(el$household_violence[el$IPV == 0 & el$female == 1 & el$complier],na.rm = TRUE), 2),
    round(mean(el$any_violence[el$IPV == 0 & el$female == 1],na.rm = TRUE), 2),
    round(mean(el$any_violence[el$IPV == 0 & el$female == 1 & el$complier],na.rm = TRUE), 2),
    round(mean(el$household_violence_frequency[el$IPV == 0 & el$female == 1], na.rm = TRUE), 2),
    round(mean(el$household_violence_frequency[el$IPV == 0 & el$female == 1& el$complier], na.rm = TRUE), 2)
  )

pval_lines <- c(
  "RI $p$-values: IPV",
  round(household_violence_ind_FE_pvals$ri_pvals["IPV"],3),
  round(household_violence_ind_FE_comp_pvals$ri_pvals["IPV"],3),
  round(any_violence_ind_FE_pvals$ri_pvals["IPV"],3),
  round(any_violence_ind_FE_comp_pvals$ri_pvals["IPV"],3),
  round(household_violence_frequency_ind_FE_pvals$ri_pvals["IPV"],3),
  round(household_violence_frequency_ind_FE_comp_pvals$ri_pvals["IPV"],3)
)


hypothesis_lines <- c(
  "Hypothesis",
  "Two",
  "Two",
  "Two",
  "Two",
  "Two",
  "Two"
)

sink("03_tables/non_linear_models.tex")
stargazer(
  ... = list(
    household_violence_ind_FE,
    household_violence_ind_FE_comp,
    any_violence_ind_FE,
    any_violence_ind_FE_comp,
    household_violence_frequency_ind_FE$fit, 
    household_violence_frequency_ind_FE_comp$fit
  ),
  type = "latex",
  p = list(
    household_violence_ind_FE_pvals$ri_pvals,
    household_violence_ind_FE_comp_pvals$ri_pvals,
    any_violence_ind_FE_pvals$ri_pvals,
    any_violence_ind_FE_comp_pvals$ri_pvals,
    household_violence_frequency_ind_FE_pvals$ri_pvals,
    household_violence_frequency_ind_FE_comp_pvals$ri_pvals
  ),
  se = list(household_violence_ind_FE_ses,
            household_violence_ind_FE_comp_ses,
            any_violence_ind_FE_ses,
            any_violence_ind_FE_comp_ses,
            household_violence_frequency_ind_FE$fit_summary[,"Std. Error"],
            household_violence_frequency_ind_FE_comp$fit_summary[,"Std. Error"]),
  covariate.labels = "Anti-VAW Media",
  keep = c("IPV"),
  omit.stat = c("rsq","f","ser", "chi2"),
  column.separate = c(2,2, 2),
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
      "All W","W compl.",
      "All W","W compl.",
      "All W","W compl."),
    c("Analysis Level",
      "Indiv.","Indiv.",
      "Indiv.","Indiv.",
      "Indiv.", "Indiv."),    
    c("Block FE",
      "Yes","Yes",
      "Yes","Yes",
      "Yes", "Yes"),
    c("Estimator", 
      "Poisson", "Poisson", 
      "Probit","Probit", 
      "OLS", "OLS")),
  notes.label = "",
  float = FALSE
  # style = "qje"
)
sink()


