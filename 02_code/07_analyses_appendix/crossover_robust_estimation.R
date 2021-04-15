



village_level_all <-
	el %>%
	filter(female == 1) %>%
	group_by(tc_id, block_id, IPV) %>%
	dplyr::summarize(
		household_violence = mean(household_violence, na.rm = T),
		any_violence_clus = mean(any_violence, na.rm = T),
		household_violence_frequency = mean(household_violence_frequency, na.rm = T),
		n_end_mean = unique(n_end_mean),
		treatment = unique(treatment),
		absenteeism = unique(absenteeism),
		abortion = unique (abortion)
	)

any_violence_FE <- lm(
	any_violence_clus ~ IPV + as.factor(block_id) + absenteeism + abortion + n_end_mean,
	data = subset(village_level_all, treatment != "abortion_absenteeism")
)

any_violence_FE_pvals <- get_RI_pvals(
	outcome = "any_violence_clus",
	treatment = "IPV",
	resample_FE = F,
	block_FE = T,
	audience_size = TRUE,
	cluster_SE = F,
	covariates = c("abortion", "absenteeism"),
	the_data = village_level_all,
	assignment_data = treatment_assignment,
	analysis_function = ols_main,
	sims = sims,
	two_Upr_two = "Two",
	crossover_robust = TRUE
)

any_violence_ind_FE <- ols_main(
	outcome = "any_violence",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(el, female == 1 &
											treatment != "abortion_absenteeism"),
	dosage = FALSE,
	dosage_indicator = FALSE
)

any_violence_ind_FE_pvals <- get_RI_pvals(
	outcome = "any_violence",
	treatment = "IPV",
	resample_FE = T,
	block_FE = T,
	audience_size = T,
	cluster_SE = T,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(el, female == 1),
	assignment_data = treatment_assignment,
	analysis_function = ols_main,
	sims = sims,
	crossover_robust = TRUE,
	two_Upr_two = "Two"
)

intervene_index_ml <- ols_main(
	outcome = "intervene_index_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(
		el,
		respondent_category == "Complier" &
			female == 1 & treatment != "abortion_absenteeism"
	),
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
	covariates = c("abortion", "absenteeism"),
	the_data = subset(el, respondent_category == "Complier" &
											female == 1),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	crossover_robust = TRUE,
	lwr_upr_two= "upr"
)

intervene_index_el <- ols_main(
	outcome = "intervene_index",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(
		el,
		respondent_category == "Complier" &
			female == 1 & treatment != "abortion_absenteeism"
	),
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
	covariates = c("abortion", "absenteeism"),
	the_data = subset(el, respondent_category == "Complier" &
											female == 1),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	crossover_robust = TRUE,
	lwr_upr_two= "upr"
)


anti_intervention_consequence_el <- ols_main(
	outcome = "anti_intervention_consequence",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(
		el,
		respondent_category == "Complier" &
			female == 1 & treatment != "abortion_absenteeism"
	),
	dosage = FALSE,
	dosage_indicator = FALSE
)

anti_intervention_consequence_el_pvals <- get_RI_pvals(
	outcome = "anti_intervention_consequence",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(el, respondent_category == "Complier" &
											female == 1),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	crossover_robust = TRUE,
	lwr_upr_two= "lwr"
)


intervene_index_ml_men <- ols_main(
	outcome = "intervene_index_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(
		el,
		respondent_category == "Complier" &
			female == 0 & treatment != "abortion_absenteeism"
	),
	dosage = FALSE,
	dosage_indicator = FALSE
)

intervene_index_ml_pvals_men <- get_RI_pvals(
	outcome = "intervene_index_ml",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	crossover_robust = TRUE,
	lwr_upr_two= "upr"
)

intervene_index_el_men <- ols_main(
	outcome = "intervene_index",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(
		el,
		respondent_category == "Complier" &
			female == 0 & treatment != "abortion_absenteeism"
	),
	dosage = FALSE,
	dosage_indicator = FALSE
)

intervene_index_el_pvals_men <- get_RI_pvals(
	outcome = "intervene_index",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	crossover_robust = TRUE,
	lwr_upr_two= "upr"
)

anti_intervention_consequence_el_men <- ols_main(
	outcome = "anti_intervention_consequence",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(
		el,
		respondent_category == "Complier" &
			female == 0 & treatment != "abortion_absenteeism"
	),
	dosage = FALSE,
	dosage_indicator = FALSE
)

anti_intervention_consequence_el_pvals_men <- get_RI_pvals(
	outcome = "anti_intervention_consequence",
	treatment = "IPV",
	resample_FE = TRUE,
	block_FE = TRUE,
	audience_size = TRUE,
	cluster_SE = TRUE,
	covariates = c("abortion", "absenteeism"),
	the_data = subset(el, respondent_category == "Complier" &
											female == 0),
	dosage = FALSE,
	dosage_indicator = FALSE,
	assignment_data = treatment_assignment,
	extract_function = coef,
	analysis_function = ols_main,
	sims = sims,
	crossover_robust = TRUE,
	lwr_upr_two= "lwr"
)


control_means <- with(village_level_all,
											c(
												"Control Mean",
												round(mean(el$any_violence[el$IPV == 0 &
																									 	el$female == 1 &
																									 	el$treatment != "abortion_absenteeism"], na.rm = TRUE), 2),
												round(mean(any_violence_clus[IPV == 0 &
																										 	treatment != "abortion_absenteeism"], na.rm = TRUE), 2),
												round(mean(el$intervene_index_ml[el$IPV == 0 &
																												 	el$female == 1 &
																												 	el$treatment != "abortion_absenteeism"], na.rm = TRUE), 2),
												round(mean(el$intervene_index[el$IPV == 0 &
																												el$female == 1 &
																												el$treatment != "abortion_absenteeism"], na.rm = TRUE), 2),
												round(mean(
													el$anti_intervention_consequence[el$IPV == 0 &
																													 	el$female == 1 &
																													 	el$treatment != "abortion_absenteeism"], na.rm = TRUE
												), 2)
											))

pval_lines <- c(
	"RI $p$-values VAW",
	round(any_violence_ind_FE_pvals$ri_pvals["IPV"], 3),
	round(any_violence_FE_pvals$ri_pvals["IPV"], 3),
	round(intervene_index_ml_pvals$ri_pvals["IPV"], 3),
	round(intervene_index_el_pvals$ri_pvals["IPV"], 3),
	round(anti_intervention_consequence_el_pvals$ri_pvals["IPV"], 3)
)

pval_lines_abo <- c(
	"RI $p$-values Abortion",
	round(any_violence_ind_FE_pvals$ri_pvals["abortion"], 3),
	round(any_violence_FE_pvals$ri_pvals["abortion"], 3),
	round(intervene_index_ml_pvals$ri_pvals["abortion"], 3),
	round(intervene_index_el_pvals$ri_pvals["abortion"], 3),
	round(anti_intervention_consequence_el_pvals$ri_pvals["abortion"], 3)
)

pval_lines_abs <- c(
	"RI $p$-values Absenteeism",
	round(any_violence_ind_FE_pvals$ri_pvals["absenteeism"], 3),
	round(any_violence_FE_pvals$ri_pvals["absenteeism"], 3),
	round(intervene_index_ml_pvals$ri_pvals["absenteeism"], 3),
	round(intervene_index_el_pvals$ri_pvals["absenteeism"], 3),
	round(anti_intervention_consequence_el_pvals$ri_pvals["absenteeism"], 3)
)

hypothesis_lines <- c("Hypothesis",
											"Two",
											"Two",
											"Upr",
											"Upr",
											"Lwr")


sink("03_tables/crossover_robust_women.tex")
stargazer(
	... = list(
		any_violence_ind_FE$fit,
		any_violence_FE,
		intervene_index_ml$fit,
		intervene_index_el$fit,
		anti_intervention_consequence_el$fit
	),
	type = "latex",
	p = list(
		any_violence_ind_FE_pvals$ri_pvals,
		any_violence_FE_pvals$ri_pvals,
		intervene_index_ml_pvals$ri_pvals,
		intervene_index_el_pvals$ri_pvals,
		anti_intervention_consequence_el_pvals$ri_pvals
	),
	se = list(
		any_violence_ind_FE$fit_summary[, "Std. Error"],
		summary(any_violence_FE)$coefficients[, "Std. Error"],
		intervene_index_ml$fit_summary[, "Std. Error"],
		intervene_index_el$fit_summary[, "Std. Error"],
		anti_intervention_consequence_el$fit_summary[, "Std. Error"]
	),
	covariate.labels = c("Anti-VAW Media", "Abortion", "Absenteeism"),
	keep = c("IPV", "abortion", "absenteeism"),
	omit.stat = c("rsq", "f", "ser"),
	column.separate = c(2, 2, 1),
	column.labels = c("Any Incidents", "Reporting Index", "Social Repercussion"),
	table.layout = "=cd#-t-as=n",
	dep.var.labels.include = T,
	dep.var.labels = c("Endline", "Endline", "Midline", "Endline", "Endline"),
	no.space = T,
	omit = "block_id",
	add.lines = list(
		control_means,
		pval_lines,
		pval_lines_abo,
		pval_lines_abs,
		hypothesis_lines,
		c("Block FE",
			"Yes", "Yes", "Yes",
			"Yes", "Yes"),
		c(
			"Analysis Level",
			"Indiv.",
			"Clus.",
			"Indiv.",
			"Indiv.",
			"Indiv."
		)
	),
	notes.label = "",
	float = FALSE
	# style = "qje"
)
sink()



control_means <-
	c("Control Mean",
		round(mean(el$intervene_index_ml[el$IPV == 0 &
																		 	el$female == 0 &
																		 	el$treatment != "abortion_absenteeism"], na.rm = TRUE), 2),
		round(mean(el$intervene_index[el$IPV == 0 &
																		el$female == 0 &
																		el$treatment != "abortion_absenteeism"], na.rm = TRUE), 2),
		round(mean(
			el$anti_intervention_consequence[el$IPV == 0 &
																			 	el$female == 0 &
																			 	el$treatment != "abortion_absenteeism"], na.rm = TRUE
		), 2))

pval_lines <- c(
	"RI $p$-values VAW",
	round(intervene_index_ml_pvals_men$ri_pvals["IPV"], 3),
	round(intervene_index_el_pvals_men$ri_pvals["IPV"], 3),
	round(anti_intervention_consequence_el_pvals_men$ri_pvals["IPV"], 3)
)

pval_lines_abo <- c(
	"RI $p$-values Abortion",
	round(intervene_index_ml_pvals_men$ri_pvals["abortion"], 3),
	round(intervene_index_el_pvals_men$ri_pvals["abortion"], 3),
	round(anti_intervention_consequence_el_pvals_men$ri_pvals["abortion"], 3)
)

pval_lines_abs <- c(
	"RI $p$-values Absenteeism",
	round(intervene_index_ml_pvals_men$ri_pvals["absenteeism"], 3),
	round(intervene_index_el_pvals_men$ri_pvals["absenteeism"], 3),
	round(anti_intervention_consequence_el_pvals_men$ri_pvals["absenteeism"], 3)
)

hypothesis_lines <- c("Hypothesis",
											"Upr",
											"Upr",
											"Lwr")


sink("03_tables/crossover_robust_men.tex")
stargazer(
	... = list(
		intervene_index_ml_men$fit,
		intervene_index_el_men$fit,
		anti_intervention_consequence_el_men$fit
	),
	type = "latex",
	p = list(
		intervene_index_ml_pvals_men$ri_pvals,
		intervene_index_el_pvals_men$ri_pvals,
		anti_intervention_consequence_el_pvals_men$ri_pvals
	),
	se = list(
		intervene_index_ml_men$fit_summary[, "Std. Error"],
		intervene_index_el_men$fit_summary[, "Std. Error"],
		anti_intervention_consequence_el_men$fit_summary[, "Std. Error"]
	),
	covariate.labels = c("Anti-VAW Media", "Abortion", "Absenteeism"),
	keep = c("IPV", "abortion", "absenteeism"),
	omit.stat = c("rsq", "f", "ser"),
	column.separate = c(2, 1),
	column.labels = c("Reporting Index", "Social Repercussion"),
	table.layout = "=cd#-t-as=n",
	dep.var.labels.include = T,
	dep.var.labels = c("Midline", "Endline", "Endline"),
	no.space = T,
	omit = "block_id",
	add.lines = list(
		control_means,
		pval_lines,
		pval_lines_abo,
		pval_lines_abs,
		hypothesis_lines,
		c("Block FE",
			"Yes",
			"Yes", "Yes"),
		c("Analysis Level",
			"Indiv.",
			"Indiv.", "Indiv.")
	),
	notes.label = "",
	float = FALSE
	# style = "qje"
)
sink()
