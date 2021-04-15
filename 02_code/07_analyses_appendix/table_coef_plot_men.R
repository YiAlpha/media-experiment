
spiral_risk <- ols_main(
  outcome = "spiral_risk",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE)

spiral_risk_pvals <- get_RI_pvals(
  outcome = "spiral_risk",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")




violence_disapproval_ml <- ols_main(
  outcome = "violence_disapproval_ml",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE)

violence_disapproval_ml_pvals <- get_RI_pvals(
  outcome = "violence_disapproval_ml",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")

violence_disapproval_el <- ols_main(
  outcome = "violence_disapproval",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE)

violence_disapproval_el_pvals <- get_RI_pvals(
  outcome = "violence_disapproval",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")


# empathy
empathy_pair <- ols_main(
  outcome = "empathy_pair",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE)

empathy_pair_pvals <- get_RI_pvals(
  outcome = "empathy_pair",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")



# Support Gender Equality Men -------------------------------------------------


support_gender_equality_ml <- ols_main(
  outcome = "support_gender_equality_ml",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE)

support_gender_equality_ml_pvals <- get_RI_pvals(
  outcome = "support_gender_equality_ml",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")

support_gender_equality_el <- ols_main(
  outcome = "support_gender_equality",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE)

support_gender_equality_el_pvals <- get_RI_pvals(
  outcome = "support_gender_equality",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")


control_means <- with(
  el, 
  c(
    "Control Mean",
    round(mean(violence_disapproval_ml[female == 0 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(violence_disapproval[female == 0 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(empathy_pair[female == 0 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(spiral_risk[female == 0 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(support_gender_equality_ml[female == 0 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(support_gender_equality[female == 0 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2)
    
  )
)

pval_lines <- c(
  "RI $p$-values",
  round(violence_disapproval_ml_pvals$ri_pvals["IPV"],3),
  round(violence_disapproval_el_pvals$ri_pvals["IPV"],3),
  round(empathy_pair_pvals$ri_pvals["IPV"],3),
  round(spiral_risk_pvals$ri_pvals["IPV"],3),
  round(support_gender_equality_ml_pvals$ri_pvals["IPV"],3),
  round(support_gender_equality_el_pvals$ri_pvals["IPV"],3)
  
)

hypothesis_lines <- c(
  "Hypothesis",
  "Upr",
  "Upr",
  "Lwr",
  "Upr",
  "Upr",
  "Upr"
)



sink("03_tables/table_coef_plot_men.tex")
stargazer(
  ... = list(
    violence_disapproval_ml$fit,
    violence_disapproval_el$fit,
    empathy_pair$fit,
    spiral_risk$fit,
    support_gender_equality_ml$fit,
    support_gender_equality_el$fit
    ),
  type = "latex",
  p = list(
    violence_disapproval_ml_pvals$ri_pvals,
    violence_disapproval_el_pvals$ri_pvals,
    empathy_pair_pvals$ri_pvals,
    spiral_risk_pvals$ri_pvals,
    support_gender_equality_ml_pvals$ri_pvals,
    support_gender_equality_el_pvals$ri_pvals
  ),
  se = list(
    violence_disapproval_ml$fit_summary[,"Std. Error"],
    violence_disapproval_el$fit_summary[,"Std. Error"],
    empathy_pair$fit_summary[,"Std. Error"],
    spiral_risk$fit_summary[,"Std. Error"],
    support_gender_equality_ml$fit_summary[,"Std. Error"],
    support_gender_equality_el$fit_summary[,"Std. Error"]
  ),
  covariate.labels = "Anti-VAW Media",
  keep = "IPV",
  omit.stat = c("rsq","f","ser"),
  column.separate = c(2,1,1,2),
  column.labels = c(
    "VAW Not Acceptable",
    "Women Suffer Greatly",
    "Violence Spirals Out of Control",
    "Support Gender Equality"
    ),
  table.layout = "=cd#-t-as=n",
  dep.var.labels = c(
    "Midline","Endline",
    "Endline",
    "Endline",
    "Midline","Endline"  ),
  dep.var.labels.include = TRUE,
  no.space = T,
  omit = "block_id",
  add.lines = list(
    control_means,
    pval_lines,
    hypothesis_lines,
    c("Block FE",
      "Yes","Yes","Yes",
      "Yes",
      "Yes","Yes"
      )),
  notes.label = "",
  float = FALSE
  # style = "qje"
)
sink()
