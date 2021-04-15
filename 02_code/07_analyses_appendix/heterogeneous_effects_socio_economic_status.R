el$tc <- el$tc_id

# Factor analysis to get latend variable ----------------------------------
set.seed(123456789)
female_compliers <- subset(el, female ==1 & respondent_category == "Complier")
factor_analysis_data <- dplyr::select(female_compliers, 
                                      illiterate, 
                                      household_children_ml, 
                                      highest_grade,
                                      living_conditions_compared,
                                      asset_index)


# Run factor analysis
fit <- factanal(factor_analysis_data, factors = 1, scores = "regression")

#  latent factor
female_compliers$soecio_economic_status_latent <- fit$scores

# Latent factor (only compliers)

latent_factor <- ols_heterogeneous_effects(
  outcome = "any_violence",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = "soecio_economic_status_latent",
  the_data = female_compliers,
  dosage = FALSE,
  dosage_indicator = FALSE
)

latent_factor_pvals <- get_RI_pvals(
  outcome = "any_violence",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = female_compliers,
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two",
  covariates = "soecio_economic_status_latent",
  analysis_function = ols_heterogeneous_effects
)


control_means <- with(
  el,
  c(
    "Control Mean",
    round(mean(any_violence[female == 1 & IPV == 0 & respondent_category == "Complier"],na.rm = TRUE), 2)
    )
)



pval_lines_treat <- c(
  "RI $p$-values IPV",
  round(latent_factor_pvals$ri_pvals["IPV"],3)
)

pval_lines_inter <- c(
  "RI $p$-values Interaction",
  round(latent_factor_pvals$ri_pvals["IPV:soecio_economic_status_latent"],3)
)



hypothesis_lines <- c(
  "Hypothesis",
  "Two"
  )

sample_lines <- c(
  "Sample",
  "Compliers"
)



sink("03_tables/effect_by_socioeconomic_status.tex")
stargazer(
  ... = list(
    latent_factor$fit),
  type = "latex",
  p = list(
    latent_factor_pvals$ri_pvals),
  se = list(
    latent_factor$fit_summary[,"Std. Error"]
  ),
  covariate.labels = c("Anti-VAW Media",
                       "Socioeconomic Status",
                       "Socioeconomic Status x Anti-VAW Media"),
  keep = c("IPV", "soecio_economic_status_latent"),
  omit.stat = c("rsq","f","ser"),
  table.layout = "=cd#-t-as=n",
  dep.var.labels = c("Any Incidents"),
  dep.var.labels.include = TRUE,
  no.space = T,
  omit = "block_id",
  add.lines = list(
    control_means,
    pval_lines_treat,
    pval_lines_inter,
    hypothesis_lines,
    sample_lines,
    c("Block FE",
      "Yes")),
  notes.label = "",
  float = FALSE
)
sink()


