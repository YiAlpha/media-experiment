


free_ride_FE_formulae <- list(
	as.formula(
		would_report ~ as.factor(block_id) + resample + n_end_mean + others_observe
	),
	as.formula(
		would_report ~ as.factor(block_id) + resample + n_end_mean + others_observe + IPV
	),
	as.formula(
		would_report ~ as.factor(block_id) + resample + n_end_mean + others_observe * IPV
	)
)


fits <- lapply(
	X = free_ride_FE_formulae,
	FUN = function(formula) {
		lm(formula = formula,
			 data = el,
			 subset = respondent_category == "Complier")
	}
)

coef_tests <- lapply(
	X = fits,
	FUN = function(fit) {
		vcov_mat <- cluster.vcov(model = fit,
														 cluster = as.factor(subset(el, respondent_category == "Complier")$tc_id))
		fit_summary <- coeftest(x = fit, vcov. = vcov_mat)
		return(fit_summary)
	}
)

SEs <- lapply(
	X = coef_tests,
	FUN = function(x)
		x[, "Std. Error"]
)

# Customized function for doing RI with both the tretment assignment on cluster level and survey treatment assignment in individual level
rerandomize_with_survey_exp <- function(sims, formula) {
	replications <- replicate(n = sims,
														expr = {
															new_data <- rerandomize(
																tc_level_data = treatment_assignment,
																analysis_data = subset(el, respondent_category == "Complier")
															)
															new_data$others_observe <-
																rbinom(n = nrow(new_data),
																			 size = 1,
																			 prob = .5)
															
															fit <- lm(formula = formula,
																				data = new_data,
																				subset = respondent_category == "Complier")
															
															coef(fit)
															
														})
	return(replications)
}

# Calculate p-values using two-sided tests for all coefs except IPV
set.seed(123456789)
# others_observe
free_ride_sims_1 <-
	rerandomize_with_survey_exp(sims = sims, formula = free_ride_FE_formulae[[1]])
free_ride_pvals_1 <-
	rowMeans(abs(coef(fits[[1]])) <= abs(free_ride_sims_1))
# others_observe + IPV
free_ride_sims_2 <-
	rerandomize_with_survey_exp(sims = sims, formula = free_ride_FE_formulae[[2]])
free_ride_pvals_2 <-
	rowMeans(abs(coef(fits[[2]])) <= abs(free_ride_sims_2))
free_ride_pvals_2_IPV <-
	rowMeans(coef(fits[[2]])  <= free_ride_sims_2)
free_ride_pvals_2["IPV"] <- free_ride_pvals_2_IPV["IPV"]
# others_observe * IPV
free_ride_sims_3 <-
	rerandomize_with_survey_exp(sims = sims, formula = free_ride_FE_formulae[[3]])
free_ride_pvals_3 <-
	rowMeans(abs(coef(fits[[3]])) <= abs(free_ride_sims_3))
free_ride_pvals_3_IPV <-
	rowMeans(coef(fits[[3]])  <= free_ride_sims_3)
free_ride_pvals_3["IPV"] <- free_ride_pvals_3_IPV["IPV"]


add_lines <- list(
	c(
		"Control Mean (Women)",
		round(mean(el$would_report[el$others_observe == 0 &
															 	el$female == 1], na.rm = TRUE), 2),
		round(mean(el$would_report[el$IPV == 0 &
															 	el$others_observe == 0 & el$female == 1], na.rm = TRUE), 2),
		round(mean(el$would_report[el$IPV == 0 &
															 	el$others_observe == 0 & el$female == 1], na.rm = TRUE), 2)
	),
	c(
		"Control Mean (Men)",
		round(mean(el$would_report[el$others_observe == 0 &
															 	el$female == 0], na.rm = TRUE), 2),
		round(mean(el$would_report[el$IPV == 0 &
															 	el$others_observe == 0 & el$female == 0], na.rm = TRUE), 2),
		round(mean(el$would_report[el$IPV == 0 &
															 	el$others_observe == 0 & el$female == 0], na.rm = TRUE), 2)
	),
	c("Block FE", "Yes", "Yes", "Yes", "Yes")
)




sink("03_tables/free_riding.tex")
stargazer(
	... = fits,
	type = "latex",
	p = list(free_ride_pvals_1,
					 free_ride_pvals_2,
					 free_ride_pvals_3),
	se = SEs,
	covariate.labels = c(
		"Others Observe",
		"Anti-VAW Media",
		"Others Observe x Anti-VAW Media"
	),
	keep = c("IPV", "others_observe"),
	column.separate = 3,
	table.layout = "=c#-t-as=n",
	omit.stat = c("rsq", "f", "ser"),
	column.labels = "Would Report IPV Incident",
	dep.var.labels.include = FALSE,
	no.space = T,
	omit = "block_id",
	add.lines = add_lines,
	notes.label = "",
	float = FALSE
	# style = "qje"
)
sink()
