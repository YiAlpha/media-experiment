# intervene_index - M - sat
intervene_index_el_m <- ols_main(
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
	crossover_specification = "saturated"
)

intervene_index_el_pvals_m <- get_RI_pvals(
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
	crossover_specification = "saturated",
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two ="upr"
)

# intervene_index - W - abo
intervene_index_el_w <- ols_main(
	outcome = "intervene_index",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 1),
	dosage = FALSE,
	dosage_indicator = FALSE,
	crossover_specification = "abortion"
)

intervene_index_el_pvals_m <- get_RI_pvals(
	outcome = "intervene_index",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 1),
	dosage = FALSE,
	dosage_indicator = FALSE,
	crossover_specification = "abortion",
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two ="upr"
)


# report_police_ml - W - sat
report_police_ml_w <- ols_main(
	outcome = "report_police_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 1),
	dosage = FALSE,
	dosage_indicator = FALSE,
	crossover_specification = "saturated"
)
report_police_ml_pvals_w <- get_RI_pvals(
	outcome = "report_police_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 1),
	dosage = FALSE,
	dosage_indicator = FALSE,
	crossover_specification = "saturated",
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two ="upr"
)

# react_comm - PP - sat
react_comm_el_perp <- ols_main(
	outcome = "react_comm",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(
		el,
		respondent_category == "Complier"  &
			female == 0 & status_q2_5 %in% c(1:3)
	),
	crossover_specification = "saturated",
	dosage = FALSE,
	dosage_indicator = FALSE
)

react_comm_el_perp_pvals <- get_RI_pvals(
	outcome = "react_comm",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(
		el,
		respondent_category == "Complier"  &
			female == 0 & status_q2_5 %in% c(1:3)
	),
	crossover_specification = "saturated",
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two ="upr"
)

# react_comm_ml - all_comp - abo
react_comm_el <- ols_main(
	outcome = "react_comm",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier"),
	crossover_specification = "abortion",
	dosage = FALSE,
	dosage_indicator = FALSE
)

react_comm_el_pvals <- get_RI_pvals(
	outcome = "react_comm",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier"),
	crossover_specification = "abortion",
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two ="upr"
)

# ipv_efficacy - M - sat
ipv_efficacy_el <- ols_main(
	outcome = "ipv_efficacy",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	crossover_specification = "saturated",
	dosage = FALSE,
	dosage_indicator = FALSE
)

ipv_efficacy_el_pvals <- get_RI_pvals(
	outcome = "ipv_efficacy",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	crossover_specification = "saturated",
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two ="upr"
)

# violence_dissaproval - M - sat
violence_disapproval_el <- ols_main(
	outcome = "violence_disapproval",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	crossover_specification = "saturated",
	dosage = FALSE,
	dosage_indicator = FALSE
)

violence_disapproval_el_pvals <- get_RI_pvals(
	outcome = "violence_disapproval",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = NULL,
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	crossover_specification = "saturated",
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	lwr_upr_two ="upr"
)

pval_lines_IPV <- c(
	"RI $p$-values",
	round(intervene_index_el_pvals_m$ri_pvals["IPV"], 3),
	round(intervene_index_el_pvals_m$ri_pvals["IPV"], 3),
	round(report_police_ml_pvals_w$ri_pvals["IPV"], 3),
	round(react_comm_el_perp_pvals$ri_pvals["IPV"], 3),
	round(react_comm_el_pvals$ri_pvals["IPV"], 3),
	round(ipv_efficacy_el_pvals$ri_pvals["IPV"], 3),
	round(violence_disapproval_el_pvals$ri_pvals["IPV"], 3)
)


sink("03_tables/no_crossover_null_rejected.tex")
stargazer(
	... = list(
		intervene_index_el_m$fit,
		intervene_index_el_w$fit,
		report_police_ml_w$fit,
		react_comm_el_perp$fit,
		react_comm_el$fit,
		ipv_efficacy_el$fit,
		violence_disapproval_el$fit
	),
	type = "latex",
	p = list(
		intervene_index_el_pvals_m$ri_pvals,
		intervene_index_el_pvals_m$ri_pvals,
		report_police_ml_pvals_w$ri_pvals,
		react_comm_el_perp_pvals$ri_pvals,
		react_comm_el_pvals$ri_pvals,
		ipv_efficacy_el_pvals$ri_pvals,
		violence_disapproval_el_pvals$ri_pvals
	),
	se = list(
		intervene_index_el_m$fit_summary[, "Std. Error"],
		intervene_index_el_w$fit_summary[, "Std. Error"],
		report_police_ml_w$fit_summary[, "Std. Error"],
		react_comm_el_perp$fit_summary[, "Std. Error"],
		react_comm_el$fit_summary[, "Std. Error"],
		ipv_efficacy_el$fit_summary[, "Std. Error"],
		violence_disapproval_el$fit_summary[, "Std. Error"]
	),
	covariate.labels = c(
		"Anti-IPV Media",
		"Anti-Abortion Stigma Media",
		"Anti-Absenteeism Media",
		"IPV x Abortion",
		"IPV x Absenteeism",
		"Abortion x Absenteeism"
	),
	keep = c("IPV", "abortion", "absenteeism"),
	omit.stat = c("rsq", "f", "ser"),
	column.labels = c(
		"Additive Index",
		"Report Police",
		"Community Would Intervene",
		"Intervention Effective",
		"IPV not Acceptable",
		" "
	),
	
	column.separate = c(2, 1, 2, 1, 1),
	
	table.layout = "=cd#-t-as=n",
	dep.var.labels = c(
		"Endline",
		"Endline",
		"Midline",
		"Endline",
		"Endline",
		"Endline",
		"Endline"
	),
	dep.var.labels.include = TRUE,
	no.space = T,
	omit = "block_id",
	add.lines = list(
		pval_lines_IPV,
		c("Block FE",
			"Yes", "Yes", "Yes",
			"Yes", "Yes", "Yes",
			"Yes"),
		c("Sample", "HH (M)", "HH (W)", "HH (W)", "HH (PP)", "HH (M)", "HH (M)")
	),
	notes.label = "",
	float = FALSE
	# style = "qje"
)
sink()
