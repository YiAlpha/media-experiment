
# react_comm potential perpetrators

react_comm_el_M <- ols_main(
  outcome = "react_comm",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier"  & female == 0 ),
  dosage = FALSE,
  dosage_indicator = FALSE)

react_comm_el_pvals_M <- get_RI_pvals(
  outcome = "react_comm",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier"  & female == 0 ),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")


react_comm_ml_M <- ols_main(
  outcome = "react_comm_ml",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier"  & female == 0 ),
  dosage = FALSE,
  dosage_indicator = FALSE)

react_comm_ml_pvals_M <- get_RI_pvals(
  outcome = "react_comm_ml",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier"  & female == 0 ),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")

# react_comm potential perpetrators

react_comm_el_W <- ols_main(
  outcome = "react_comm",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier"  & female == 1 ),
  dosage = FALSE,
  dosage_indicator = FALSE)

react_comm_el_pvals_W <- get_RI_pvals(
  outcome = "react_comm",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier"  & female == 1 ),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")


react_comm_ml_W <- ols_main(
  outcome = "react_comm_ml",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier"  & female == 1 ),
  dosage = FALSE,
  dosage_indicator = FALSE)

react_comm_ml_pvals_W <- get_RI_pvals(
  outcome = "react_comm_ml",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier"  & female == 1 ),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")

# would_take_revenge

would_take_revenge_el_M <- ols_main(
  outcome = "would_take_revenge",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE)

would_take_revenge_el_pvals_M <- get_RI_pvals(
  outcome = "would_take_revenge",
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
  lwr_upr_two = "lwr")

would_take_revenge_el_W <- ols_main(
  outcome = "would_take_revenge",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE)

would_take_revenge_el_pvals_W <- get_RI_pvals(
  outcome = "would_take_revenge",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "lwr")



# anti_intervention_consequence

anti_intervention_consequence_el_M <- ols_main(
  outcome = "anti_intervention_consequence",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE)

anti_intervention_consequence_el_pvals_M <- get_RI_pvals(
  outcome = "anti_intervention_consequence",
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
  lwr_upr_two = "lwr")


anti_intervention_consequence_el_W <- ols_main(
  outcome = "anti_intervention_consequence",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE)

anti_intervention_consequence_el_pvals_W <- get_RI_pvals(
  outcome = "anti_intervention_consequence",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE,
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "lwr")






control_means <- with(
  el, 
  c(
    "Control Mean",
    round(mean(anti_intervention_consequence[female == 0 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(anti_intervention_consequence[female == 1 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(would_take_revenge[female == 0 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(would_take_revenge[female == 1 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(react_comm_ml[female == 0 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(react_comm[female == 0 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(react_comm_ml[female == 1 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2),
    round(mean(react_comm[female == 1 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2)
    
  )
)

pval_lines <- c(
  "RI $p$-values",
  round(anti_intervention_consequence_el_pvals_M$ri_pvals["IPV"],3),
  round(anti_intervention_consequence_el_pvals_W$ri_pvals["IPV"],3),
  round(would_take_revenge_el_pvals_M$ri_pvals["IPV"],3),
  round(would_take_revenge_el_pvals_W$ri_pvals["IPV"],3),
  round(react_comm_ml_pvals_M$ri_pvals["IPV"],3),
  round(react_comm_el_pvals_M$ri_pvals["IPV"],3),
  round(react_comm_ml_pvals_W$ri_pvals["IPV"],3),
  round(react_comm_el_pvals_W$ri_pvals["IPV"],3)
)

hypothesis_lines <- c(
  "Hypothesis",
  "Lwr",
  "Lwr",
  "Lwr",
  "Lwr",
  "Upr",
  "Upr",
  "Upr",
  "Upr"
)



sink("03_tables/costs.tex")
stargazer(
  ... = list(
    anti_intervention_consequence_el_M$fit,
    anti_intervention_consequence_el_W$fit,
    would_take_revenge_el_M$fit,
    would_take_revenge_el_W$fit,
    react_comm_ml_M$fit,
    react_comm_el_M$fit,
    react_comm_ml_W$fit,
    react_comm_el_W$fit),
  type = "latex",
  p = list(
    anti_intervention_consequence_el_pvals_M$ri_pvals,
    anti_intervention_consequence_el_pvals_W$ri_pvals,
    would_take_revenge_el_pvals_M$ri_pvals,
    would_take_revenge_el_pvals_W$ri_pvals,
    react_comm_ml_pvals_M$ri_pvals,
    react_comm_el_pvals_M$ri_pvals,
    react_comm_ml_pvals_W$ri_pvals,
    react_comm_el_pvals_W$ri_pvals
  ),
  se = list(
    anti_intervention_consequence_el_M$fit_summary[,"Std. Error"],
    anti_intervention_consequence_el_W$fit_summary[,"Std. Error"],
    would_take_revenge_el_M$fit_summary[,"Std. Error"],
    would_take_revenge_el_W$fit_summary[,"Std. Error"],
    react_comm_ml_M$fit_summary[,"Std. Error"],
    react_comm_el_M$fit_summary[,"Std. Error"],
    react_comm_ml_W$fit_summary[,"Std. Error"],
    react_comm_el_W$fit_summary[,"Std. Error"]
  ),
  covariate.labels = "Anti-VAW Media",
  keep = "IPV",
  omit.stat = c("rsq","f","ser"),
  column.separate = c(2,2,4),
  column.labels = c("Social Repercussions","Personal Retribution","Community Would Intervene" ),
  table.layout = "=cd#-t-as=n",
  dep.var.labels = c(
    "Endline","Endline",
    "Midline","Endline",
    "Midline","Endline"
  ),
  dep.var.labels.include = TRUE,
  no.space = T,
  omit = "block_id",
  add.lines = list(
    control_means,
    pval_lines,
    hypothesis_lines,
    c("Sample",
      "Men","Women",
      "Men","Women",
      "Men","Men",
      "Women","Women"
    ),
    c("Block FE",
      "Yes","Yes",
      "Yes","Yes",
      "Yes","Yes",
      "Yes","Yes"
      )
    ),
  notes.label = "",
  float = FALSE
  # style = "qje"
)
sink()



