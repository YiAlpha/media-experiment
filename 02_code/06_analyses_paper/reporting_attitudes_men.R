

intervene_index_el <- ols_main(
	outcome = "intervene_index",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE
)

intervene_index_el_pvals <- get_RI_pvals(
	outcome = "intervene_index",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two = "upr"
)



intervene_index_ml <- ols_main(
	outcome = "intervene_index_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE
)

intervene_index_ml_pvals <- get_RI_pvals(
	outcome = "intervene_index_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two = "upr"
)



# involve_parents

involve_parents_el <- ols_main(
	outcome = "involve_parents",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE
)

involve_parents_el_pvals <- get_RI_pvals(
	outcome = "involve_parents",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two = "upr"
)


involve_parents_ml <- ols_main(
	outcome = "involve_parents_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE
)

involve_parents_ml_pvals <- get_RI_pvals(
	outcome = "involve_parents_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two = "upr"
)



# involve_nabakyala

involve_nabakyala_el <- ols_main(
	outcome = "involve_nabakyala",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE
)

involve_nabakyala_el_pvals <- get_RI_pvals(
	outcome = "involve_nabakyala",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two = "upr"
)


involve_nabakyala_ml <- ols_main(
	outcome = "involve_nabakyala_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE
)

involve_nabakyala_ml_pvals <- get_RI_pvals(
	outcome = "involve_nabakyala_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two = "upr"
)



# involve_lc1


involve_lc1_el <- ols_main(
	outcome = "involve_lc1",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE
)

involve_lc1_el_pvals <- get_RI_pvals(
	outcome = "involve_lc1",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two = "upr"
)


involve_lc1_ml <- ols_main(
	outcome = "involve_lc1_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE
)

involve_lc1_ml_pvals <- get_RI_pvals(
	outcome = "involve_lc1_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two = "upr"
)

# report_police

report_police_el <- ols_main(
	outcome = "report_police",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE
)

report_police_el_pvals <- get_RI_pvals(
	outcome = "report_police",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two = "upr"
)


report_police_ml <- ols_main(
	outcome = "report_police_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE
)

report_police_ml_pvals <- get_RI_pvals(
	outcome = "report_police_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two = "upr"
)



control_means <- with(
	el,
	c(
		"Control Mean",
		round(mean(involve_parents[female == 0 &
															 	IPV == 0 & respondent_category == "Complier"], na.rm = TRUE), 2),
		round(mean(involve_parents_ml[female == 0 &
																		IPV == 0 & respondent_category == "Complier"], na.rm = TRUE), 2),
		round(mean(involve_nabakyala[female == 0 &
																 	IPV == 0 & respondent_category == "Complier"], na.rm = TRUE), 2),
		round(mean(involve_nabakyala_ml[female == 0 &
																			IPV == 0 & respondent_category == "Complier"], na.rm = TRUE), 2),
		round(mean(involve_lc1[female == 0 &
													 	IPV == 0 & respondent_category == "Complier"], na.rm = TRUE), 2),
		round(mean(involve_lc1_ml[female == 0 &
																IPV == 0 & respondent_category == "Complier"], na.rm = TRUE), 2),
		round(mean(report_police[female == 0 &
														 	IPV == 0 & respondent_category == "Complier"], na.rm = TRUE), 2),
		round(mean(report_police_ml[female == 0 &
																	IPV == 0 & respondent_category == "Complier"], na.rm = TRUE), 2),
		round(mean(intervene_index[female == 0 &
															 	IPV == 0 & respondent_category == "Complier"], na.rm = TRUE), 2),
		round(mean(intervene_index_ml[female == 0 &
																		IPV == 0 & respondent_category == "Complier"], na.rm = TRUE), 2)
	)
)


pval_lines <- c(
	"RI $p$-values",
	round(involve_parents_ml_pvals$ri_pvals["IPV"], 3),
	round(involve_parents_el_pvals$ri_pvals["IPV"], 3),
	round(involve_nabakyala_ml_pvals$ri_pvals["IPV"], 3),
	round(involve_nabakyala_el_pvals$ri_pvals["IPV"], 3),
	round(involve_lc1_ml_pvals$ri_pvals["IPV"], 3),
	round(involve_lc1_el_pvals$ri_pvals["IPV"], 3),
	round(report_police_ml_pvals$ri_pvals["IPV"], 3),
	round(report_police_el_pvals$ri_pvals["IPV"], 3),
	round(intervene_index_ml_pvals$ri_pvals["IPV"], 3),
	round(intervene_index_el_pvals$ri_pvals["IPV"], 3)
)


hypothesis_lines <- c("Hypothesis",
											"upr",
											"upr",
											"upr",
											"upr",
											"upr",
											"upr",
											"upr",
											"upr",
											"upr",
											"upr")


sink("03_tables/reporting_attitudes_men.tex")
stargazer(
	... = list(
		involve_parents_ml$fit,
		involve_parents_el$fit,
		involve_nabakyala_ml$fit,
		involve_nabakyala_el$fit,
		involve_lc1_ml$fit,
		involve_lc1_el$fit,
		report_police_ml$fit,
		report_police_el$fit,
		intervene_index_ml$fit,
		intervene_index_el$fit
	),
	type = "latex",
	p = list(
		involve_parents_ml_pvals$ri_pvals,
		involve_parents_el_pvals$ri_pvals,
		involve_nabakyala_ml_pvals$ri_pvals,
		involve_nabakyala_el_pvals$ri_pvals,
		involve_lc1_ml_pvals$ri_pvals,
		involve_lc1_el_pvals$ri_pvals,
		report_police_ml_pvals$ri_pvals,
		report_police_el_pvals$ri_pvals,
		intervene_index_ml_pvals$ri_pvals,
		intervene_index_el_pvals$ri_pvals
	),
	se = list(
		involve_parents_ml$fit_summary[, "Std. Error"],
		involve_parents_el$fit_summary[, "Std. Error"],
		involve_nabakyala_ml$fit_summary[, "Std. Error"],
		involve_nabakyala_el$fit_summary[, "Std. Error"],
		involve_lc1_ml$fit_summary[, "Std. Error"],
		involve_lc1_el$fit_summary[, "Std. Error"],
		report_police_ml$fit_summary[, "Std. Error"],
		report_police_el$fit_summary[, "Std. Error"],
		intervene_index_ml$fit_summary[, "Std. Error"],
		intervene_index_el$fit_summary[, "Std. Error"]
	),
	covariate.labels = "Anti-VAW Media",
	keep = "IPV",
	omit.stat = c("rsq", "f", "ser"),
	column.separate = c(2, 2, 2, 2, 2),
	column.labels = c(
		"Involve Parents",
		"Involve Counselor",
		"Involve Village Leader",
		"Report Police",
		"Reporting Index"
	),
	table.layout = "=cd#-t-as=n",
	dep.var.labels = c(
		"Midline",
		"Endline",
		"Midline",
		"Endline",
		"Midline",
		"Endline",
		"Midline",
		"Endline",
		"Midline",
		"Endline",
		"Midline",
		"Endline"
	),
	dep.var.labels.include = TRUE,
	no.space = T,
	omit = "block_id",
	add.lines = list(
		control_means,
		pval_lines,
		hypothesis_lines,
		c(
			"Block FE",
			"Yes",
			"Yes",
			"Yes",
			"Yes",
			"Yes",
			"Yes",
			"Yes",
			"Yes",
			"Yes",
			"Yes"
		)
	),
	notes.label = "",
	float = FALSE
	# style = "qje"
)
sink()
