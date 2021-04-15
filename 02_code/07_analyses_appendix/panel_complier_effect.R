
el$panel_complier <- as.numeric(el$respondent_category == "Complier")

panel_complier_effect <- ols_main(
  outcome = "any_violence",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = "panel_complier",
  interact_covariates = TRUE,
  the_data = subset(el,complier & female == 1),
  dosage = F,
  dosage_indicator = F,
  crossover_specification = NULL)
panel_complier_effect_pvals <- get_RI_pvals(
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = 2000,
  lwr_upr_two = "two",
  outcome = "any_violence",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = "panel_complier",
  interact_covariates = TRUE,
  the_data = subset(el, complier & female == 1),
  dosage = F,
  dosage_indicator = F,
  crossover_specification = NULL
)



control_means <- with(
  el, 
  c(
    "Control Mean",
    round(mean(el$any_violence[female == 1 & IPV == 0 & complier],na.rm = TRUE), 2)
  )
)

pval_lines <- c(
  "RI $p$-values",
  round(panel_complier_effect_pvals$ri_pvals["IPV"],3)
)

hypothesis_lines <- c(
  "Hypothesis",
  "two"
)



sink("03_tables/panel_complier_effect.tex")
stargazer(
  ... = list(
    panel_complier_effect$fit
  ),
  type = "latex",
  p = list(
    panel_complier_effect_pvals$ri_pvals
  ),
  se = list(
    panel_complier_effect$fit_summary[,"Std. Error"]
  ),
  covariate.labels = c("Anti-VAW Media","Interviewed in Midline","Anti-VAW Media x Interviewed Midline"),
  keep = c("IPV","panel_complier"),
  omit.stat = c("rsq","f","ser"),
  table.layout = "=cd#-t-as=n",
  dep.var.labels = c(
    "Any Incidents"
  ),
  dep.var.labels.include = TRUE,
  no.space = T,
  omit = "block_id",
  add.lines = list(
    control_means,
    pval_lines,
    hypothesis_lines,
    c("Block FE",
      "Yes"
    )),
  notes.label = "",
  float = FALSE
  # style = "qje"
)
sink()
