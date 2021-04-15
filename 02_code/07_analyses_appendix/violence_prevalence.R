village_level_ml <-
	ml %>%
	group_by(tc_id, block_id, IPV) %>%
	summarize(
		beating_frequency_ml = mean(beating_frequency, na.rm = T),
		woman_beaten_ml = mean(woman_beaten, na.rm = T),
		any_violence_village_ml = mean(any_violence_village, na.rm = T),
		n_end_mean = unique(n_end_mean)
	)

village_level_ml$tc <- village_level_ml$tc_id
village_level_el <-
	el %>%
	group_by(tc_id, block_id, IPV) %>%
	summarize(
		beating_frequency_el = mean(beating_frequency[female == 0], na.rm = T),
		woman_beaten_el = mean(woman_beaten, na.rm = T),
		any_violence_village_el = mean(any_violence_village, na.rm = T),
		n_end_mean = unique(n_end_mean)
	)


village_level_el$tc <- village_level_el$tc_id

vht_village_level_ml <-
	vht_ml %>%
	group_by(tc_id, block_id, IPV) %>%
	summarize(
		ipv_incidents_ml = mean(reported_ipv_incidents, na.rm = T),
		radius = unique(radius),
		n_end_mean = unique(n_end_mean)
	)
vht_village_level_ml$tc <- vht_village_level_ml$tc_id
vht_village_level_el <-
	vht_el %>%
	group_by(tc_id, block_id, IPV) %>%
	summarize(
		ipv_incidents_el = mean(reported_ipv_incidents, na.rm = T),
		radius = unique(radius),
		n_end_mean = unique(n_end_mean)
	)
vht_village_level_el$tc <- vht_village_level_el$tc_id


# woman_beaten midline
woman_beaten_ml_FE <- lm(woman_beaten_ml ~ IPV + as.factor(block_id) + n_end_mean,
												 data = village_level_ml)
woman_beaten_ml_FE_pvals <- get_RI_pvals(
	outcome = "woman_beaten_ml",
	treatment = "IPV",
	resample_FE = F,
	block_FE = TRUE,
	audience_size = T,
	cluster_SE = F,
	covariates = NULL,
	the_data = village_level_ml,
	assignment_data = treatment_assignment,
	sims = sims,
	lwr_upr_two = "two"
)

# woman_beaten endline
woman_beaten_el_FE <- lm(woman_beaten_el ~ IPV + as.factor(block_id) + n_end_mean,
												 data = village_level_el)
woman_beaten_el_FE_pvals <- get_RI_pvals(
	outcome = "woman_beaten_el",
	treatment = "IPV",
	resample_FE = F,
	block_FE = TRUE,
	audience_size = T,
	cluster_SE = F,
	covariates = NULL,
	the_data = village_level_el,
	assignment_data = treatment_assignment,
	sims = sims,
	lwr_upr_two = "two"
)


# any women beaten midline

any_violence_village_ml_FE <- lm(any_violence_village_ml ~ IPV + as.factor(block_id) + n_end_mean,
																 data = village_level_ml)
any_violence_village_ml_FE_pvals <- get_RI_pvals(
	outcome = "any_violence_village_ml",
	treatment = "IPV",
	resample_FE = F,
	block_FE = TRUE,
	audience_size = T,
	cluster_SE = F,
	covariates = NULL,
	the_data = village_level_ml,
	assignment_data = treatment_assignment,
	sims = sims,
	lwr_upr_two = "two"
)




# any women beaten endline

any_violence_village_el_FE <- lm(any_violence_village_el ~ IPV + as.factor(block_id) + n_end_mean,
																 data = village_level_el)
any_violence_village_el_FE_pvals <- get_RI_pvals(
	outcome = "any_violence_village_el",
	treatment = "IPV",
	resample_FE = F,
	block_FE = TRUE,
	audience_size = T,
	cluster_SE = F,
	covariates = NULL,
	the_data = village_level_el,
	assignment_data = treatment_assignment,
	sims = sims,
	lwr_upr_two = "two"
)



# beating_frequency midline
beating_frequency_ml_FE <- lm(beating_frequency_ml ~ IPV + as.factor(block_id) + n_end_mean,
															data = village_level_ml)
beating_frequency_ml_FE_pvals <- get_RI_pvals(
	outcome = "beating_frequency_ml",
	treatment = "IPV",
	resample_FE = F,
	block_FE = TRUE,
	audience_size = T,
	cluster_SE = F,
	covariates = NULL,
	the_data = village_level_ml,
	assignment_data = treatment_assignment,
	sims = sims,
	lwr_upr_two = "two"
)


# beating_frequency endline
beating_frequency_el_FE <- lm(beating_frequency_el ~ IPV + as.factor(block_id) + n_end_mean,
															data = village_level_el)
beating_frequency_el_FE_pvals <- get_RI_pvals(
	outcome = "beating_frequency_el",
	treatment = "IPV",
	resample_FE = F,
	block_FE = TRUE,
	audience_size = T,
	cluster_SE = F,
	covariates = NULL,
	the_data = village_level_el,
	assignment_data = treatment_assignment,
	sims = sims,
	lwr_upr_two = "two"
)



# ipv_incidents
ipv_incidents_ml_FE <- lm(ipv_incidents_ml ~ IPV + as.factor(block_id) + n_end_mean,
													data = vht_village_level_ml)
ipv_incidents_ml_FE_pvals <- get_RI_pvals(
	outcome = "ipv_incidents_ml",
	treatment = "IPV",
	resample_FE = F,
	block_FE = TRUE,
	audience_size = T,
	cluster_SE = F,
	covariates = NULL,
	the_data = vht_village_level_ml,
	assignment_data = treatment_assignment,
	sims = sims,
	lwr_upr_two = "two"
)

# ipv_incidents
ipv_incidents_el_FE <- lm(ipv_incidents_el ~ IPV + as.factor(block_id) + n_end_mean,
													data = vht_village_level_el)
ipv_incidents_el_FE_pvals <- get_RI_pvals(
	outcome = "ipv_incidents_el",
	treatment = "IPV",
	resample_FE = F,
	block_FE = TRUE,
	audience_size = T,
	cluster_SE = F,
	covariates = NULL,
	the_data = vht_village_level_el,
	assignment_data = treatment_assignment,
	sims = sims,
	lwr_upr_two = "two"
)

control_means <- c("Control Mean",
	round(mean(
		village_level_ml$woman_beaten_ml[village_level_ml$IPV == 0], na.rm = TRUE
	), 2),
	round(mean(
		village_level_el$woman_beaten_el[village_level_el$IPV == 0], na.rm = TRUE
	), 2),
	round(
		mean(village_level_ml$any_violence_village_ml[village_level_ml$IPV == 0], na.rm = TRUE),
		2
	),
	round(
		mean(village_level_el$any_violence_village_el[village_level_el$IPV == 0], na.rm = TRUE),
		2
	),
	round(mean(
		village_level_ml$beating_frequency_ml[village_level_ml$IPV == 0], na.rm = TRUE
	), 2),
	round(mean(
		village_level_el$beating_frequency_el[village_level_el$IPV == 0], na.rm = TRUE
	), 2),
	round(mean(
		vht_village_level_ml$ipv_incidents_ml[vht_village_level_ml$IPV == 0], na.rm = TRUE
	), 2),
	round(mean(
		vht_village_level_el$ipv_incidents_el[vht_village_level_el$IPV == 0], na.rm = TRUE
	), 2)
)

pval_lines <-
	c("RI $p$-values",
		round(
			c(
				woman_beaten_ml_FE_pvals$ri_pvals["IPV"],
				woman_beaten_el_FE_pvals$ri_pvals["IPV"],
				any_violence_village_ml_FE_pvals$ri_pvals["IPV"],
				any_violence_village_el_FE_pvals$ri_pvals["IPV"],
				beating_frequency_ml_FE_pvals$ri_pvals["IPV"],
				beating_frequency_el_FE_pvals$ri_pvals["IPV"],
				ipv_incidents_ml_FE_pvals$ri_pvals["IPV"],
				ipv_incidents_el_FE_pvals$ri_pvals["IPV"]
			),
			3
		))

hypothesis_lines <- c("Hypothesis",
											"two",
											"two",
											"two",
											"two",
											"two",
											"two",
											"two",
											"two")


sink("03_tables/prevalence.tex")
stargazer(
	... = list(
		woman_beaten_ml_FE,
		woman_beaten_el_FE,
		any_violence_village_ml_FE,
		any_violence_village_el_FE,
		beating_frequency_ml_FE,
		beating_frequency_el_FE,
		ipv_incidents_ml_FE,
		ipv_incidents_el_FE
	),
	type = "latex",
	p = list(
		woman_beaten_ml_FE_pvals$ri_pvals,
		woman_beaten_el_FE_pvals$ri_pvals,
		any_violence_village_ml_FE_pvals$ri_pvals,
		any_violence_village_el_FE_pvals$ri_pvals,
		beating_frequency_ml_FE_pvals$ri_pvals,
		beating_frequency_el_FE_pvals$ri_pvals,
		ipv_incidents_ml_FE_pvals$ri_pvals,
		ipv_incidents_el_FE_pvals$ri_pvals
	),
	se = list(
		summary(woman_beaten_ml_FE)$coefficients[, "Std. Error"],
		summary(woman_beaten_el_FE)$coefficients[, "Std. Error"],
		summary(any_violence_village_ml_FE)$coefficients[, "Std. Error"],
		summary(any_violence_village_el_FE)$coefficients[, "Std. Error"],
		summary(beating_frequency_ml_FE)$coefficients[, "Std. Error"],
		summary(beating_frequency_el_FE)$coefficients[, "Std. Error"],
		summary(ipv_incidents_ml_FE)$coefficients[, "Std. Error"],
		summary(ipv_incidents_el_FE)$coefficients[, "Std. Error"]
	),
	covariate.labels = "Anti-VAW Media",
	keep = "IPV",
	omit.stat = c("rsq", "f", "ser"),
	column.separate = c(2, 2, 2, 2),
	column.labels = c(
		"Number of Incidents (Comm.)",
		"Any Incidents (Comm.)",
		"Viol. Freq. (Comm.)",
		"Number of Incidents (VHT)"
	),
	dep.var.labels = c(
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
	table.layout = "=cd#-t-as=n",
	add.lines = list(
		control_means,
		pval_lines,
		hypothesis_lines,
		c("Block FE",
			"Yes", "Yes",
			"Yes", "Yes",
			"Yes", "Yes",
			"Yes", "Yes"),
		c("Sample", "HH", "HH", "HH", "HH", "HH", "HH (M)", "VHT", "VHT")
	),
	notes.label = "",
	float = FALSE
	# style = "qje"
)
sink()
