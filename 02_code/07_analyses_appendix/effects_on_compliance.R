
ml$complier <- (ml$compliance == 1) * 1

full_compliance_conditional <-
	glm(
		complier ~ treatment  + as.factor(block_id) + resample,
		family = binomial(link = 'logit'),
		data = subset(ml)
	)

full_compliance_marginal <-
	glm(
		complier ~ IPV  + as.factor(block_id) + resample,
		family = binomial(link = 'logit'),
		data = ml
	)

nested_compliance <-  glm(
	complier ~  as.factor(block_id) + resample,
	family = binomial(link = 'logit'),
	data = subset(ml)
)

marginal_test <-
	lmtest::lrtest(full_compliance_marginal, nested_compliance)
conditional_test <-
	lmtest::lrtest(full_compliance_conditional, nested_compliance)
obs_ratio_compliance_marginal <-
	2 * (marginal_test$LogLik[1] - marginal_test$LogLik[2])
obs_ratio_compliance_conditional <-
	2 * (conditional_test$LogLik[1] - conditional_test$LogLik[2])

full_compliance_conditional_w <-
	glm(
		complier ~ treatment  + as.factor(block_id) + resample,
		family = binomial(link = 'logit'),
		data = subset(ml, female == 1)
	)

full_compliance_marginal_w <-
	glm(
		complier ~ IPV  + as.factor(block_id) + resample,
		family = binomial(link = 'logit'),
		data = subset(ml, female == 1)
	)

nested_compliance_w <-  glm(
	complier ~  as.factor(block_id) + resample,
	family = binomial(link = 'logit'),
	data = subset(ml, female == 1)
)

marginal_test_w <-
	lmtest::lrtest(full_compliance_marginal_w, nested_compliance_w)
conditional_test_w <-
	lmtest::lrtest(full_compliance_conditional_w, nested_compliance_w)
obs_ratio_compliance_marginal_w <-
	2 * (marginal_test_w$LogLik[1] - marginal_test_w$LogLik[2])
obs_ratio_compliance_conditional_w <-
	2 * (conditional_test_w$LogLik[1] - conditional_test_w$LogLik[2])


full_compliance_conditional_m <-
	glm(
		complier ~ treatment  + as.factor(block_id) + resample,
		family = binomial(link = 'logit'),
		data = subset(ml, female == 0)
	)

full_compliance_marginal_m <-
	glm(
		complier ~ IPV  + as.factor(block_id) + resample,
		family = binomial(link = 'logit'),
		data = subset(ml, female == 0)
	)

nested_compliance_m <-  glm(
	complier ~  as.factor(block_id) + resample,
	family = binomial(link = 'logit'),
	data = subset(ml, female == 0)
)

marginal_test_m <-
	lmtest::lrtest(full_compliance_marginal_m, nested_compliance_m)
conditional_test_m <-
	lmtest::lrtest(full_compliance_conditional_m, nested_compliance_m)
obs_ratio_compliance_marginal_m <-
	2 * (marginal_test_m$LogLik[1] - marginal_test_m$LogLik[2])
obs_ratio_compliance_conditional_m <-
	2 * (conditional_test_m$LogLik[1] - conditional_test_m$LogLik[2])


# Permute likelihood ratios under null ------------------------------------



lik_ratio_compliance_null_marginal <-
	lik_ratio_compliance_null_conditional <- c()
lik_ratio_compliance_null_marginal_w <-
	lik_ratio_compliance_null_conditional_w <- c()
lik_ratio_compliance_null_marginal_m <-
	lik_ratio_compliance_null_conditional_m <- c()

set.seed(123456789)

for (i in 1:sims) {
	print(i)
	
	new_data <- rerandomize(analysis_data = ml)
	
	# All
	
	full_compliance_conditional <-
		glm(
			complier ~ treatment  + as.factor(block_id) + resample,
			family = binomial(link = 'logit'),
			data = new_data
		)
	
	full_compliance_marginal <-
		glm(
			complier ~ IPV  + as.factor(block_id) + resample,
			family = binomial(link = 'logit'),
			data = new_data
		)
	
	nested_compliance <-  glm(
		complier ~  as.factor(block_id) + resample,
		family = binomial(link = 'logit'),
		data = new_data
	)
	
	marginal_test <-
		lmtest::lrtest(full_compliance_marginal, nested_compliance)
	conditional_test <-
		lmtest::lrtest(full_compliance_conditional, nested_compliance)
	lik_ratio_compliance_null_marginal[i] <-
		2 * (marginal_test$LogLik[1] - marginal_test$LogLik[2])
	lik_ratio_compliance_null_conditional[i] <-
		2 * (conditional_test$LogLik[1] - conditional_test$LogLik[2])
	
	
	# Women
	
	full_compliance_conditional_w <-
		glm(
			complier ~ treatment  + as.factor(block_id) + resample,
			family = binomial(link = 'logit'),
			data = subset(new_data, female == 1)
		)
	
	full_compliance_marginal_w <-
		glm(
			complier ~ IPV  + as.factor(block_id) + resample,
			family = binomial(link = 'logit'),
			data = subset(new_data, female == 1)
		)
	
	nested_compliance_w <-  glm(
		complier ~  as.factor(block_id) + resample,
		family = binomial(link = 'logit'),
		data = subset(new_data, female == 1)
	)
	
	marginal_test_w <-
		lmtest::lrtest(full_compliance_marginal_w, nested_compliance_w)
	conditional_test_w <-
		lmtest::lrtest(full_compliance_conditional_w, nested_compliance_w)
	lik_ratio_compliance_null_marginal_w[i] <-
		2 * (marginal_test_w$LogLik[1] - marginal_test_w$LogLik[2])
	lik_ratio_compliance_null_conditional_w[i] <-
		2 * (conditional_test_w$LogLik[1] - conditional_test_w$LogLik[2])
	
	# Men
	
	full_compliance_conditional_m <-
		glm(
			complier ~ treatment  + as.factor(block_id) + resample,
			family = binomial(link = 'logit'),
			data = subset(new_data, female == 0)
		)
	
	full_compliance_marginal_m <-
		glm(
			complier ~ IPV  + as.factor(block_id) + resample,
			family = binomial(link = 'logit'),
			data = subset(new_data, female == 0)
		)
	
	nested_compliance_m <-  glm(
		complier ~  as.factor(block_id) + resample,
		family = binomial(link = 'logit'),
		data = subset(new_data, female == 0)
	)
	
	marginal_test_m <-
		lmtest::lrtest(full_compliance_marginal_m, nested_compliance_m)
	conditional_test_m <-
		lmtest::lrtest(full_compliance_conditional_m, nested_compliance_m)
	lik_ratio_compliance_null_marginal_m[i] <-
		2 * (marginal_test_m$LogLik[1] - marginal_test_m$LogLik[2])
	lik_ratio_compliance_null_conditional_m[i] <-
		2 * (conditional_test_m$LogLik[1] - conditional_test_m$LogLik[2])
	
}

# Compute p-values --------------------------------------------------------

marginal_compliance_pval <- mean(lik_ratio_compliance_null_marginal >= obs_ratio_compliance_marginal)

conditional_compliance_pval <- mean(lik_ratio_compliance_null_conditional >= obs_ratio_compliance_conditional)


marginal_compliance_pval_w <- mean(lik_ratio_compliance_null_marginal_w >= obs_ratio_compliance_marginal_w)

conditional_compliance_pval_w <- mean(lik_ratio_compliance_null_conditional_w >= obs_ratio_compliance_conditional_w)

marginal_compliance_pval_m <- mean(lik_ratio_compliance_null_marginal_m >= obs_ratio_compliance_marginal_m)

conditional_compliance_pval_m <- mean(lik_ratio_compliance_null_conditional_m >= obs_ratio_compliance_conditional_m)


full_n_compliers_conditional <-
	lm(n_compliers ~ treatment  + as.factor(block_id),
		 data = data_tc_level)

full_n_compliers_marginal <-
	lm(n_compliers ~ IPV  + as.factor(block_id),
		 data = data_tc_level)

nested_n_compliers <-  lm(n_compliers ~  as.factor(block_id),
													data = data_tc_level)

obs_fstat_n_compliers_conditional  <-
	anova(full_n_compliers_conditional, nested_n_compliers)[5][[1]][2]
obs_fstat_n_compliers_marginal  <-
	anova(full_n_compliers_marginal, nested_n_compliers)[5][[1]][2]

# Women

full_n_compliers_conditional_w <-
	lm(n_compliers_women ~ treatment  + as.factor(block_id),
		 data = data_tc_level)

full_n_compliers_marginal_w <-
	lm(n_compliers_women ~ IPV  + as.factor(block_id),
		 data = data_tc_level)

nested_n_compliers_w <-  lm(n_compliers_women ~  as.factor(block_id),
														data = data_tc_level)

obs_fstat_n_compliers_conditional_w  <-
	anova(full_n_compliers_conditional_w, nested_n_compliers_w)[5][[1]][2]
obs_fstat_n_compliers_marginal_w  <-
	anova(full_n_compliers_marginal_w, nested_n_compliers_w)[5][[1]][2]

# Men

full_n_compliers_conditional_m <-
	lm(n_compliers_men ~ treatment  + as.factor(block_id),
		 data = data_tc_level)

full_n_compliers_marginal_m <-
	lm(n_compliers_men ~ IPV  + as.factor(block_id),
		 data = data_tc_level)

nested_n_compliers_m <-  lm(n_compliers_men ~  as.factor(block_id),
														data = data_tc_level)

obs_fstat_n_compliers_conditional_m  <-
	anova(full_n_compliers_conditional_m, nested_n_compliers_m)[5][[1]][2]
obs_fstat_n_compliers_marginal_m  <-
	anova(full_n_compliers_marginal_m, nested_n_compliers_m)[5][[1]][2]


# Permute likelihood ratios under null ------------------------------------

fstat_n_compliers_conditional <- fstat_n_compliers_marginal <- c()
fstat_n_compliers_conditional_w <-
	fstat_n_compliers_marginal_w <- c()
fstat_n_compliers_conditional_m <-
	fstat_n_compliers_marginal_m <- c()

set.seed(123456789)

for (i in 1:sims) {
	print(i)
	
	new_data <- rerandomize(analysis_data = data_tc_level)
	
	# All
	
	full_n_compliers_conditional <-
		lm(n_compliers ~ treatment  + as.factor(block_id),
			 data = new_data)
	
	full_n_compliers_marginal <-
		lm(n_compliers ~ IPV  + as.factor(block_id),
			 data = new_data)
	
	nested_n_compliers <-  lm(n_compliers ~  as.factor(block_id),
														data = new_data)
	
	fstat_n_compliers_conditional[i]  <-
		anova(full_n_compliers_conditional, nested_n_compliers)[5][[1]][2]
	fstat_n_compliers_marginal[i]  <-
		anova(full_n_compliers_marginal, nested_n_compliers)[5][[1]][2]
	
	# Women
	
	full_n_compliers_conditional_w <-
		lm(n_compliers_women ~ treatment  + as.factor(block_id),
			 data = new_data)
	
	full_n_compliers_marginal_w <-
		lm(n_compliers_women ~ IPV  + as.factor(block_id),
			 data = new_data)
	
	nested_n_compliers_w <-  lm(n_compliers_women ~  as.factor(block_id),
															data = new_data)
	
	fstat_n_compliers_conditional_w[i]  <-
		anova(full_n_compliers_conditional_w, nested_n_compliers_w)[5][[1]][2]
	fstat_n_compliers_marginal_w[i]  <-
		anova(full_n_compliers_marginal_w, nested_n_compliers_w)[5][[1]][2]
	
	# Men
	
	full_n_compliers_conditional_m <-
		lm(n_compliers_men ~ treatment  + as.factor(block_id),
			 data = new_data)
	
	full_n_compliers_marginal_m <-
		lm(n_compliers_men ~ IPV  + as.factor(block_id),
			 data = new_data)
	
	nested_n_compliers_m <-
		lm(n_compliers_men ~  as.factor(block_id),
			 data = new_data)
	
	fstat_n_compliers_conditional_m[i]  <-
		anova(full_n_compliers_conditional_m, nested_n_compliers_m)[5][[1]][2]
	fstat_n_compliers_marginal_m[i]  <-
		anova(full_n_compliers_marginal_m, nested_n_compliers_m)[5][[1]][2]
	
}

# Compute p-values --------------------------------------------------------

marginal_n_complier_pval <- mean(fstat_n_compliers_marginal >= obs_fstat_n_compliers_marginal)

conditional_n_compliers_pval <- mean(fstat_n_compliers_conditional >= obs_fstat_n_compliers_conditional)


marginal_n_complier_pval_w <- mean(fstat_n_compliers_marginal_w >= obs_fstat_n_compliers_marginal_w)

conditional_n_compliers_pval_w <- mean(fstat_n_compliers_conditional_w >= obs_fstat_n_compliers_conditional_w)

marginal_n_complier_pval_m <- mean(fstat_n_compliers_marginal_m >= obs_fstat_n_compliers_marginal_m)

conditional_n_compliers_pval_m <- mean(fstat_n_compliers_conditional_m >= obs_fstat_n_compliers_conditional_m)



table_data <- data.frame(
	restrictedmodel = c(
		"Pr(Complier| Block + Resample Indicators)",
		"Pr(Complier| Block + Resample Indicators)",
		"Pr(Complier| Block + Resample Indicators)",
		"Pr(Complier| Block + Resample Indicators)",
		"Pr(Complier| Block + Resample Indicators)",
		"Pr(Complier| Block + Resample Indicators)",
		"E(N Compliers| Block + Resample Indicators)",
		"E(N Compliers| Block + Resample Indicators)",
		"E(N Women Compliers| Block + Resample Indicators)",
		"E(N Women Compliers| Block + Resample Indicators)",
		"E(N Men Compliers| Block + Resample Indicators)",
		"E(N Men Compliers| Block + Resample Indicators)"
	),
	unrestrictedmodel = c(
		"Pr(Complier| Block + Resample +  VAW Treatment Indicators)",
		"Pr(Complier| Block + Resample + 7 Treatment Condition Indicators)",
		"Pr(Complier| Block + Resample +  VAW Treatment Indicators)",
		"Pr(Complier| Block + Resample + 7 Treatment Condition Indicators)",
		"Pr(Complier| Block + Resample +  VAW Treatment Indicators)",
		"Pr(Complier| Block + Resample + 7 Treatment Condition Indicators)",
		"E(N Compliers| Block + Resample +  VAW Treatment Indicators)",
		"E(N Compliers| Block + Resample + 7 Treatment Condition Indicators)",
		"E(N Compliers| Block + Resample +  VAW Treatment Indicators)",
		"E(N Compliers| Block + Resample + 7 Treatment Condition Indicators)",
		"E(N Compliers| Block + Resample +  VAW Treatment Indicators)",
		"E(N Compliers| Block + Resample + 7 Treatment Condition Indicators)"
	),
	sample = c(
		rep("All Respondents (ML, N = 5528)", 2),
		rep("Women (ML, N = 2743)", 2),
		rep("Men (ML, N = 2785)", 2),
		rep("110 Clusters", 6)
	),
	pvalue = c(
		round(marginal_compliance_pval, 3),
		round(conditional_compliance_pval, 3),
		round(marginal_compliance_pval_w, 3),
		round(conditional_compliance_pval_w, 3),
		round(marginal_compliance_pval_m, 3),
		round(conditional_compliance_pval_m, 3),
		round(marginal_n_complier_pval, 3),
		round(conditional_n_compliers_pval, 3),
		round(marginal_n_complier_pval_w, 3),
		round(conditional_n_compliers_pval_w, 3),
		round(marginal_n_complier_pval_m, 3),
		round(conditional_n_compliers_pval_m, 3)
	)
)


sink("03_tables/compliance_on_treatment.tex")
kable(
	table_data,
	row.names = F,
	col.names = c("Restricted Model",
								"Unrestricted Model",
								"Sample",
								"P-Value"),
	format = "latex"
)
sink()
