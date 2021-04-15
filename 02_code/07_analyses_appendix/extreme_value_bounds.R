# Functions ---------------------------------------------------------------

imputeValues <- function(outcome, data, maximum, minimum) {
	data[, paste0(outcome, "_max")] <-
		data[, paste0(outcome, "_min")] <- data[, outcome]
	data[is.na(data[, outcome]), paste0(outcome, "_max")] <- maximum
	data[is.na(data[, outcome]), paste0(outcome, "_min")] <- minimum
	return(data)
}

getEstimates <-
	function(outcome,
					 data,
					 hypothesis = "upr",
					 sims = 1000,
					 effect = "positive",
					 resample_FE = TRUE) {
		observed_effect <- ols_main(
			outcome = outcome,
			treatment = "IPV",
			resample_FE = resample_FE,
			block_FE = TRUE,
			audience_size = TRUE,
			cluster_SE = TRUE,
			covariates = NULL,
			the_data = data,
			dosage = FALSE,
			dosage_indicator = FALSE
		)
		
		observed_effect_pvals <- get_RI_pvals(
			outcome = outcome,
			treatment = "IPV",
			resample_FE = resample_FE,
			block_FE = TRUE,
			audience_size = TRUE,
			cluster_SE = TRUE,
			covariates = NULL,
			the_data = data,
			dosage = FALSE,
			dosage_indicator = FALSE,
			assignment_data = treatment_assignment,
			extract_function = coef,
			analysis_function = ols_main,
			sims = sims,
			lwr_upr_two = hypothesis
		)
		
		## Lower
		effect_min <- ols_main(
			outcome = paste0(outcome, "_min"),
			treatment = "IPV",
			resample_FE = resample_FE,
			block_FE = TRUE,
			audience_size = TRUE,
			cluster_SE = TRUE,
			covariates = NULL,
			the_data = data,
			dosage = FALSE,
			dosage_indicator = FALSE
		)
		
		effect_min_pvals <- get_RI_pvals(
			outcome = paste0(outcome, "_min"),
			treatment = "IPV",
			resample_FE = resample_FE,
			block_FE = TRUE,
			audience_size = TRUE,
			cluster_SE = TRUE,
			covariates = NULL,
			the_data = data,
			dosage = FALSE,
			dosage_indicator = FALSE,
			assignment_data = treatment_assignment,
			extract_function = coef,
			analysis_function = ols_main,
			sims = sims,
			lwr_upr_two = hypothesis
		)
		
		## Upper
		effect_max <- ols_main(
			outcome = paste0(outcome, "_max"),
			treatment = "IPV",
			resample_FE = resample_FE,
			block_FE = TRUE,
			audience_size = TRUE,
			cluster_SE = TRUE,
			covariates = NULL,
			the_data = data,
			dosage = FALSE,
			dosage_indicator = FALSE
		)
		
		effect_max_pvals <- get_RI_pvals(
			outcome = paste0(outcome, "_max"),
			treatment = "IPV",
			resample_FE = resample_FE,
			block_FE = TRUE,
			audience_size = TRUE,
			cluster_SE = TRUE,
			covariates = NULL,
			the_data = data,
			dosage = FALSE,
			dosage_indicator = FALSE,
			assignment_data = treatment_assignment,
			extract_function = coef,
			analysis_function = ols_main,
			sims = sims,
			lwr_upr_two = hypothesis
		)
		
		if (effect == "negative") {
			store_effect_min  <- effect_min
			store_effect_min_pvals <- effect_min_pvals
			effect_min <- effect_max
			effect_min_pvals <- effect_max_pvals
			effect_max <- store_effect_min
			effect_max_pvals <- store_effect_min_pvals
		}
		
		results <- c(
			observed_effect$fit$coefficients["IPV"],
			observed_effect_pvals$ri_pvals["IPV"],
			effect_max$fit$coefficients["IPV"],
			effect_max_pvals$ri_pvals["IPV"],
			effect_min$fit$coefficients["IPV"],
			effect_min_pvals$ri_pvals["IPV"]
		)
		
		return(results)
		
	}


# Predicting the number of compliers --------------------------------------

# Predictors

attendance_women <- festival[, c(
	"n_women_end1",
	"n_women_end2",
	"n_women_end3",
	"n_women_end4",
	"n_women_end5",
	"n_women_end6"
)]


attendance_men <- festival[, c("n_men_end1",
															 "n_men_end2",
															 "n_men_end3",
															 "n_men_end4",
															 "n_men_end5",
															 "n_men_end6")]

attendance_men <- as.data.frame(apply(attendance_men, 2, as.integer))
attendance_women <-
	as.data.frame(apply(attendance_women, 2, as.integer))


festival$n_end_mean_men <- rowMeans(attendance_men[, c("n_men_end1",
																											 "n_men_end2",
																											 "n_men_end3",
																											 "n_men_end4",
																											 "n_men_end5",
																											 "n_men_end6")], na.rm = TRUE)

festival$n_end_mean_women <- rowMeans(attendance_women[, c(
	"n_women_end1",
	"n_women_end2",
	"n_women_end3",
	"n_women_end4",
	"n_women_end5",
	"n_women_end6"
)], na.rm = TRUE)

rm(attendance_women, attendance_men)

data_tc_level <-
	merge(
		x = tc_data,
		y = dplyr::select(festival, tc_id, n_end_mean_women, n_end_mean_men),
		by = "tc_id",
		all = TRUE
	)

# Number of compliers
number_compliers_ml <-
	ml %>%
	group_by(tc_id) %>%
	dplyr::summarize(
		n_compliers_ml = sum(compliance == 1, na.rm = T),
		n_compliers_women_ml = sum(compliance == 1 &
															 	female == 1, na.rm = T),
		n_compliers_men_ml = sum(compliance == 1 &
														 	female == 0, na.rm = T)
	)

number_compliers_el <-
	el %>%
	group_by(tc_id) %>%
	dplyr::summarize(
		n_compliers = sum(respondent_category == "Complier", na.rm = T),
		n_compliers_women = sum(respondent_category == "Complier" &
															female == 1, na.rm = T),
		n_compliers_men = sum(respondent_category == "Complier" &
														female == 0, na.rm = T)
	)


plot_data <-
	data.frame(
		n_compliers = c(
			number_compliers_el$n_compliers_women,
			number_compliers_el$n_compliers_men
		),
		gender = c(rep(
			"Women", length(number_compliers_el$n_compliers_women)
		),
		rep(
			"Men", length(number_compliers_el$n_compliers_men)
		))
	)


pdf("04_figures/complier_distribution.pdf", height = 4)
ggplot(data = plot_data, aes(n_compliers)) +
	geom_histogram(binwidth = 1,
								 fill = "grey",
								 col = "black") +
	facet_wrap( ~ gender) +
	theme_bw() +
	xlab(label = "Number of Compliers") +
	ylab(label = "Count (Villages)")
dev.off()


response_rate <-
	mean(number_compliers_el$n_compliers / number_compliers_ml$n_compliers_ml)
response_rate_women <-
	mean(
		number_compliers_el$n_compliers_women / number_compliers_ml$n_compliers_women_ml,
		na.rm = T
	)
response_rate_men <-
	mean(
		number_compliers_el$n_compliers_men / number_compliers_ml$n_compliers_men_ml,
		na.rm = T
	)


# Merge Number of Compliers and Predictors
data_tc_level <-
	merge(number_compliers_el,
				data_tc_level,
				by = "tc_id",
				all = T)
data_tc_level <-
	merge(number_compliers_ml,
				data_tc_level,
				by = "tc_id",
				all = T)
data_tc_level <-
	merge(data_tc_level,
				treatment_assignment,
				by = "tc_id",
				all = T)


# Predicted Number of Compliers

# Predict number of compliers in ml
m1 <-
	glm.nb(
		n_compliers_ml ~ n_end_mean + radius + as.factor(block_id) + tc_latitude + tc_longitude,
		data = data_tc_level
	)

n_compliers_ml_hat <-
	predict(m1, newdata = data_tc_level, type = "response")

# Multiply by response rate to get predicted number of compliers in el
n_compliers_el_hat  <- n_compliers_ml_hat * response_rate
round(n_compliers_el_hat[is.na(data_tc_level$n_compliers)], 0)

m4 <-
	glm.nb(
		n_compliers ~  n_end_mean_men + radius + as.factor(block_id) + tc_latitude + tc_longitude,
		data = data_tc_level
	)
n_compliers_hat <-
	predict(m4, newdata = data_tc_level, type = "response")
round(n_compliers_hat[is.na(data_tc_level$n_compliers)], 0)


# Predicted Number of Female Compliers

# Predict number of female compliers in ml

m2 <-
	glm.nb(
		n_compliers_women_ml ~ n_end_mean_women + radius + as.factor(block_id) + tc_latitude + tc_longitude,
		data = data_tc_level
	)

n_compliers_women_ml_hat <-
	predict(m2, newdata = data_tc_level, type = "response")

# Multiply by response rate to get predicted number of female compliers in el
n_compliers_women_el_hat  <-
	n_compliers_women_ml_hat * response_rate_women

prediction_women <-
	data.frame(
		tc_id = data_tc_level$tc_id[is.na(data_tc_level$n_compliers_women)],
		n_women_compliers = round(n_compliers_women_el_hat[is.na(data_tc_level$n_compliers_women)], 0)
	)


m5 <-
	glm.nb(
		n_compliers_women ~ n_end_mean_women + radius + as.factor(block_id) + tc_latitude + tc_longitude,
		data = data_tc_level
	)

n_compliers_women_hat <-
	predict(m5, newdata = data_tc_level, type = "response")

round(n_compliers_women_hat[is.na(data_tc_level$n_compliers)], 0)

# Predicted Number of Male Compliers

# Predict number of male compliers in ml (negatiev binomial and poisson since there does not seem to be overdispersion, see warning message and huge theta)

m3 <-
	glm.nb(
		n_compliers_men_ml ~ n_end_mean_men + radius + as.factor(block_id) + tc_latitude + tc_longitude,
		data = data_tc_level
	)

m3_pois <-
	glm(
		n_compliers_men_ml ~ n_end_mean_men + radius + as.factor(block_id) + tc_latitude + tc_longitude,
		family = "poisson",
		data = data_tc_level
	)

n_compliers_men_ml_hat <-
	predict(m3, newdata = data_tc_level, type = "response")
n_compliers_men_ml_hat_pois <-
	predict(m3_pois, newdata = data_tc_level, type = "response")

# Multiply by response rate to get predicted number of male compliers in el

n_compliers_men_el_hat  <- n_compliers_men_ml_hat * response_rate_men
round(n_compliers_men_el_hat[is.na(data_tc_level$n_compliers)], 0)
n_compliers_men_el_hat_pois  <-
	n_compliers_men_ml_hat_pois * response_rate_men
round(n_compliers_men_el_hat_pois[is.na(data_tc_level$n_compliers)], 0)

prediction_men <-
	data.frame(tc_id = data_tc_level$tc_id[is.na(data_tc_level$n_compliers)],
						 n_men_compliers = round(n_compliers_men_el_hat[is.na(data_tc_level$n_compliers)], 0))


m6 <-
	glm.nb(
		n_compliers_men ~ n_end_mean_men + radius + as.factor(block_id) + tc_latitude + tc_longitude,
		data = data_tc_level
	)

m6_pois <-
	glm(
		n_compliers_men ~ n_end_mean_men + radius + as.factor(block_id) + tc_latitude + tc_longitude,
		family = "poisson",
		data = data_tc_level
	)


n_compliers_men_hat <-
	predict(m6, newdata = data_tc_level, type = "response")
n_compliers_men_hat_pois <-
	predict(m6_pois, newdata = data_tc_level, type = "response")
round(n_compliers_men_hat[is.na(data_tc_level$n_compliers)], 0)
round(n_compliers_men_hat_pois[is.na(data_tc_level$n_compliers)], 0)

# Prepare data for individual level analysis -----------------------------------------------

outcomes <- c(
	"intervene_index",
	"intervene_index_ml",
	"any_violence",
	"anti_intervention_consequence"
)

subset <- dplyr::filter(el,
												respondent_category == "Complier")



subset <- dplyr::select(
	subset,
	tc_id,
	female,
	block_id,
	IPV,
	resample,
	n_end_mean,
	intervene_index,
	intervene_index_ml,
	any_violence,
	anti_intervention_consequence
)


predictions <- merge(prediction_men,
										 prediction_women,
										 by = "tc_id")

predictions$total_compliers <-
	predictions$n_men_compliers + predictions$n_women_compliers


additional_rows <- data.frame(
	tc_id = unlist(apply(predictions, 1,
											 function(x)
											 	rep(x["tc_id"], x["total_compliers"]))),
	female = unlist(apply(predictions, 1,
												function(x)
													c(
														rep(1, x["n_women_compliers"]),
														rep(0, x["n_men_compliers"])
													))),
	block_id = unlist(apply(predictions, 1,
													function(x)
														rep(data_tc_level$block_id[data_tc_level$tc_id == x["tc_id"]],
																x["total_compliers"]))),
	IPV =  unlist(apply(predictions, 1,
											function(x)
												rep(festival$IPV[festival$tc_id == x["tc_id"]],
														x["total_compliers"]))),
	resample = 0,
	n_end_mean = unlist(apply(predictions, 1,
														function(x)
															rep(festival$n_end_mean[festival$tc_id == x["tc_id"]],
																	x["total_compliers"])))
	
)

additional_rows[, outcomes] <- NA

subset <- rbind(subset, additional_rows)

# Individual Level Analysis with theoretical min/max -----------------------------------------------

for (i in 1:length(outcomes)) {
	subset <- imputeValues(outcomes[i], subset, 1, 0)
}

subset$tc <- subset$tc_id

set.seed(1:6)
intervene_index_women_el <-
	getEstimates(outcome = "intervene_index",
							 data = subset(subset, female == 1),
							 sims = sims)

intervene_index_men_el <- getEstimates(outcome = "intervene_index",
																			 data = subset(subset, female == 0),
																			 sims = sims)

intervene_index_women_ml <-
	getEstimates(outcome = "intervene_index_ml",
							 data = subset(subset, female == 1),
							 sims = sims)

intervene_index_men_ml <-
	getEstimates(outcome = "intervene_index_ml",
							 data = subset(subset, female == 0),
							 sims = sims)

any_violence <- getEstimates(
	outcome = "any_violence",
	data = subset(subset, female == 1),
	hypothesis = "two",
	sims = sims,
	effect = "negative"
)

anti_intervention_consequence_women <-
	getEstimates(
		outcome = "anti_intervention_consequence",
		data = subset(subset, female == 1),
		hypothesis = "lwr",
		sims = sims,
		effect = "negative"
	)

anti_intervention_consequence_men <-
	getEstimates(
		outcome = "anti_intervention_consequence",
		data = subset(subset, female == 0),
		hypothesis = "lwr",
		sims = sims,
		effect = "negative"
	)

# Make table

table_data_women <- data.frame(
	any_violence = any_violence,
	intervene_index_women_ml = intervene_index_women_ml,
	intervene_index_women_el = intervene_index_women_el,
	anti_intervention_consequence_women = anti_intervention_consequence_women
)

table_data_men <- data.frame(
	intervene_index_men_ml = intervene_index_men_ml,
	intervene_index_men_el = intervene_index_men_el,
	anti_intervention_consequence_men = anti_intervention_consequence_men
)

rownames(table_data_men) <-
	rownames(table_data_women) <- c(
		"Observed effect",
		"RI p-value observed",
		"Upper Bound",
		"RI p-value upper bound",
		"Lower Bound",
		"RI p-value lower bound"
	)

sink("03_tables/bounds_individual_level_women.tex")
kable(
	table_data_women,
	col.names = c(
		"Any Incidents (EL)",
		"Reporting Index (ML)",
		"Reporting Index (EL)",
		"Social Repercussions (EL)"
	),
	digits = 3,
	format = "latex"
)
sink()

sink("03_tables/bounds_individual_level_men.tex")
kable(
	table_data_men,
	col.names = c(
		"Reporting Index (ML)",
		"Reporting Index (EL)",
		"Social Repercussions (EL)"
	),
	digits = 3,
	format = "latex"
)
sink()

# Collapse to Village Level -----------------------------------------------

village_level_all <-
	el %>%
	filter(female == 1) %>%
	group_by(tc_id, block_id, IPV) %>%
	dplyr::summarize(
		n_end_mean = unique(n_end_mean),
		any_violence = mean(any_violence, na.rm = T),
		household_violence = mean(household_violence, na.rm = T),
		household_violence_frequency = mean(household_violence_frequency, na.rm = T)
	)

# Adding the two attrited clusters to df ----------------------------------------

ids_attrited <-
	festival$tc_id[!festival$tc_id %in% unique(el$tc_id)]

# Checking that trading center IDs match up
festival$tc[!festival$tc %in% unique(el$tc)]
festival$tc[festival$tc_id == 85]
festival$tc[festival$tc_id == 106]

attrited_tcs <-
	festival[festival$tc_id %in% ids_attrited, c("tc_id", "block_id", "IPV", "n_end_mean")]

# both attrited clusters are in the treatment group

outcomes <- c("any_violence",
							"household_violence",
							"household_violence_frequency")

attrited_tcs[, outcomes] <- NA

village_level_all <- bind_rows(village_level_all, attrited_tcs)

village_level_all$tc <- village_level_all$tc_id

any_violence_empirical_max <-
	max(village_level_all$any_violence, na.rm = T)
any_violence_empirical_min <-
	min(village_level_all$any_violence, na.rm = T)

village_level_all <- imputeValues(
	"any_violence",
	village_level_all,
	any_violence_empirical_max,
	any_violence_empirical_min
)

household_violence_max <-
	max(village_level_all$household_violence, na.rm = T)
household_violence_min <-
	min(village_level_all$household_violence, na.rm = T)

village_level_all <- imputeValues(
	"household_violence",
	village_level_all,
	household_violence_max,
	household_violence_min
)


household_violence_frequency_max <-
	max(village_level_all$household_violence_frequency, na.rm = T)
household_violence_frequency_min <-
	min(village_level_all$household_violence_frequency, na.rm = T)

village_level_all <- imputeValues(
	"household_violence_frequency",
	village_level_all,
	household_violence_frequency_max,
	household_violence_frequency_min
)

set.seed(1:6)
any_violence <-
	getEstimates(
		outcome = "any_violence",
		data = village_level_all,
		hypothesis = "two",
		sims = sims,
		effect = "negative",
		resample_FE = FALSE
	)

household_violence <-
	getEstimates(
		outcome = "household_violence",
		data = village_level_all,
		hypothesis = "two",
		sims = sims,
		effect = "negative",
		resample_FE = FALSE
	)

household_violence_frequency <-
	getEstimates(
		outcome = "household_violence_frequency",
		data = village_level_all,
		hypothesis = "two",
		sims = sims,
		effect = "negative",
		resample_FE = FALSE
	)

# Make table

table_data_cluster <- data.frame(
	any_violence = any_violence,
	household_violence = household_violence,
	household_violence_frequency = household_violence_frequency
)


table_data_cluster <- rbind(
	table_data_cluster,
	c(
		any_violence_empirical_max,
		household_violence_max,
		household_violence_frequency_max
	),
	c(
		any_violence_empirical_min,
		household_violence_min,
		household_violence_frequency_min
	)
)

rownames(table_data_cluster) <-
	c(
		"Observed effect",
		"RI p-value observed",
		"Upper Bound",
		"RI p-value upper bound",
		"Lower Bound",
		"RI p-value lower bound",
		"Maximum",
		"Minimum"
	)

sink("03_tables/bounds_cluster_level.tex")
kable(
	table_data_cluster,
	col.names = c("Any Incidents",
								"Number Of Incidents",
								"Violence Frequency"),
	digits = 3,
	format = "latex"
)
sink()
