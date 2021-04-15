

# Empathy spiral men ------------------------------------------------------


violence_disapproval_el_M <- ols_main(
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

violence_disapproval_ml_M <- ols_main(
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

empathy_pair_M <- ols_main(
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

spiral_risk_M <- ols_main(
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



# Spiral empathy women ----------------------------------------------------

violence_disapproval_el_W <- ols_main(
  outcome = "violence_disapproval",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE)


violence_disapproval_ml_W <- ols_main(
  outcome = "violence_disapproval_ml",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE)



empathy_pair_W <- ols_main(
  outcome = "empathy_pair",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE)


spiral_risk_W <- ols_main(
  outcome = "spiral_risk",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE)



# Gender Equality Men -----------------------------------------------------

support_gender_equality_ml_M <- ols_main(
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

support_gender_equality_el_M <- ols_main(
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


# Gender Equality Women ---------------------------------------------------

support_gender_equality_ml_W <- ols_main(
  outcome = "support_gender_equality_ml",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE)

support_gender_equality_el_W <- ols_main(
  outcome = "support_gender_equality",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE)


# Make plot ---------------------------------------------------------------

outcome_list <- c(
  "VAW Not Acceptable (EL)",
  "VAW Not Acceptable (ML)",
  "Women Suffer Greatly (EL)",
  "Violence Spirals Out of Control (EL)",
  "Support Gender Equality (ML)",
  "Support Gender Equality (EL)",
  "VAW Not Acceptable (EL)",
  "VAW Not Acceptable (ML)",
  "Women Suffer Greatly (EL)",
  "Violence Spirals Out of Control (EL)",
  "Support Gender Equality (ML)",
  "Support Gender Equality (EL)"
)
facet_list <- c(
  "Men Compliers (N = 720)", 
  "Men Compliers (N = 720)",
  "Men Compliers (N = 720)", 
  "Men Compliers (N = 720)",
  "Men Compliers (N = 720)", 
  "Men Compliers (N = 720)",
  "Women Compliers (N = 321)", 
  "Women Compliers (N = 321)",
  "Women Compliers (N = 321)", 
  "Women Compliers (N = 321)",
  "Women Compliers (N = 321)", 
  "Women Compliers (N = 321)"
)


est_list <- list(
  violence_disapproval_el_M,
  violence_disapproval_ml_M,
  empathy_pair_M,
  spiral_risk_M,
  support_gender_equality_ml_M,
  support_gender_equality_el_M,
  violence_disapproval_el_W,
  violence_disapproval_ml_W,
  empathy_pair_W,
  spiral_risk_W,
  support_gender_equality_ml_W,
  support_gender_equality_el_W
)



effects <- sapply(est_list, function(x) {
  obs <- x$fit_summary["IPV", "Estimate"]
  sde <-  x$fit_summary["IPV", "Std. Error"]
  lo <-  obs - 1.645 * sde
  up <- obs + 1.645 * sde
  
  c(observed = obs,
    bound_lower = lo,
    bound_upper = up)
})

effects <- as.data.frame(t(effects))

effects$outcome <- factor(
  x = outcome_list,
  levels = c(
    "Support Gender Equality (EL)",
    "Support Gender Equality (ML)",
    "Violence Spirals Out of Control (EL)",
    "Women Suffer Greatly (EL)",
    "VAW Not Acceptable (EL)",
    "VAW Not Acceptable (ML)"),ordered = TRUE)
effects$facet <- factor(
  x = facet_list,
  levels = c(
             "Men Compliers (N = 720)",
             "Women Compliers (N = 321)"))

pdf("04_figures/coef_plot_param.pdf", width = 5, height = 7)
print(ggplot(effects,aes(x = outcome)) +
  geom_hline(yintercept = 0,size = .1,linetype = 2) + 
  geom_linerange(aes(ymin = bound_lower, ymax = bound_upper),size = .5,alpha = .75) +
  geom_point(aes(y = observed), shape =4, color = "black",size = 2) +
  scale_y_continuous(name = "Estimated effect") +
  coord_cartesian(ylim = c(-.2,.2)) + 
  scale_x_discrete(name = "") +
  facet_wrap(~ facet,ncol = 1,scales = "free",shrink = TRUE) +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.border = element_blank()) +
  coord_flip() )
dev.off()
