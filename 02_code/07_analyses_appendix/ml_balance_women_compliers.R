


ml_comp_w <- subset(ml, compliance == 1 & female == 1)

# Balance function --------------------------------------------------------

# Function should first take mean of all of the three conditions, then the 7
# Then computes a likelihood ratio test

observed_lik_dif <- sapply(
	X = 1:length(covariates),
	FUN = function(i) {
		full_model_marginal <- lm(formula = as.formula(
			paste0(
				covariates[i],
				" ~ IPV + abortion + absenteeism + resample + as.factor(block_id)"
			)
		),
		data = ml_comp_w)
		
		full_model_conditional <- lm(formula = as.formula(
			paste0(
				covariates[i],
				" ~ treatment + resample + as.factor(block_id)"
			)
		),
		data = ml_comp_w)
		
		nested_model <- lm(formula = as.formula(paste0(
			covariates[i], " ~ resample + as.factor(block_id)"
		)),
		data = ml_comp_w)
		
		marginal_test <-
			lmtest::lrtest(full_model_marginal, nested_model)
		conditional_test <-
			lmtest::lrtest(full_model_conditional, nested_model)
		
		obs_ratio_marginal <-
			2 * (marginal_test$LogLik[1] - marginal_test$LogLik[2])
		obs_ratio_conditonal <-
			2 * (conditional_test$LogLik[1] - conditional_test$LogLik[2])
		
		return(c(obs_ratio_marginal, obs_ratio_conditonal))
	}
)

rownames(observed_lik_dif) <- c("Marginal", "Conditional")
colnames(observed_lik_dif) <- covariates

marginal_mat <- conditional_mat <- list()

set.seed(123456789)
for (j in (length(marginal_mat) + 1):sims) {
	new_data <- rerandomize(tc_level_data = treatment_assignment,
													analysis_data = ml_comp_w)
	null_lik_dif <- sapply(
		X = 1:length(covariates),
		FUN = function(i) {
			full_model_marginal <- lm(formula = as.formula(
				paste0(
					covariates[i],
					" ~ IPV + abortion + absenteeism + resample + as.factor(block_id)"
				)
			),
			data = new_data)
			
			full_model_conditional <- lm(formula = as.formula(
				paste0(
					covariates[i],
					" ~ treatment + resample + as.factor(block_id)"
				)
			),
			data = new_data)
			
			nested_model <- lm(formula = as.formula(
				paste0(covariates[i], " ~ resample + as.factor(block_id)")
			),
			data = new_data)
			
			marginal_test <-
				lmtest::lrtest(full_model_marginal, nested_model)
			conditional_test <-
				lmtest::lrtest(full_model_conditional, nested_model)
			
			null_ratio_marginal <-
				2 * (marginal_test$LogLik[1] - marginal_test$LogLik[2])
			null_ratio_conditonal <-
				2 * (conditional_test$LogLik[1] - conditional_test$LogLik[2])
			
			return(c(null_ratio_marginal, null_ratio_conditonal))
		}
	)
	
	marginal_mat[[j]] <- null_lik_dif[1, ]
	conditional_mat[[j]] <- null_lik_dif[2, ]
	
	print(j)
	
}

marginal_sims <- as.data.frame(do.call("rbind", marginal_mat))
conditional_sims <- as.data.frame(do.call("rbind", conditional_mat))

colnames(marginal_sims) <-
	colnames(conditional_sims) <- covariates

marginal_pvalues <-
	colMeans(t(apply(
		X = marginal_sims,
		MARGIN = 1,
		FUN = function(sim) {
			sim >= observed_lik_dif[1, ]
		}
	)))
conditional_pvalues <-
	colMeans(t(apply(
		X = conditional_sims,
		MARGIN = 1,
		FUN = function(sim) {
			sim >= observed_lik_dif[2, ]
		}
	)))

# Marginalizing over treatment conditions ---------------------------------


baltab_IPV <-
	ml_comp_w %>%
	subset(select = c(covariates, "IPV")) %>%
	group_by(IPV) %>%
	summarize_each(funs = "mean") %>%
	t()
baltab_ABO <-
	ml_comp_w %>%
	subset(select = c(covariates, "abortion")) %>%
	group_by(abortion) %>%
	summarize_each(funs = "mean") %>%
	t()
baltab_ABS <-
	ml_comp_w %>%
	subset(select = c(covariates, "absenteeism")) %>%
	group_by(absenteeism) %>%
	summarize_each(funs = "mean") %>%
	t()
baltab_placebo <-
	ml_comp_w %>%
	subset(select = c(covariates, "placebo")) %>%
	group_by(placebo) %>%
	summarize_each(funs = "mean") %>%
	t()

marginal_baltab_comp_w <-
	cbind.data.frame(covariate = covariates,
									 round(
									 	data.frame(
									 		placebo = baltab_placebo[-1, 2],
									 		IPV = baltab_IPV[-1, 2],
									 		abortion = baltab_ABO[-1, 2],
									 		absenteeism = baltab_ABS[-1, 2],
									 		pval = marginal_pvalues
									 	),
									 	2
									 )) %>%
	arrange(pval)

rm(baltab_placebo,
	 baltab_IPV,
	 baltab_ABO,
	 baltab_ABS)

# Preserving conditional distribution -------------------------------------

baltab_conditional_comp_w <-
	ml_comp_w %>%
	subset(select = c(covariates, "treatment")) %>%
	group_by(treatment) %>%
	summarize_each(funs = "mean") %>%
	ungroup() %>%
	dplyr::select(-treatment) %>%
	t() %>%
	data.frame()

names(baltab_conditional_comp_w) <-
	sort(unique(ml_comp_w$treatment))

conditional_baltab_comp_w <- cbind.data.frame(covariate = covariates,
																							round(
																								data.frame(baltab_conditional_comp_w[, c(
																									"placebo",
																									"IPV",
																									"abortion",
																									"absenteeism",
																									"abortion_absenteeism",
																									"IPV_abortion",
																									"IPV_absenteeism"
																								)],
																								pval = conditional_pvalues),
																								2
																							)) %>%
	arrange(pval)



names(conditional_baltab_comp_w) <- c("covariate",
																			"PLA",
																			"IPV",
																			"ABO",
																			"ABS",
																			"ABO_ABS",
																			"IPV_ABS",
																			"IPV_ABO",
																			"p-value")
rownames(conditional_baltab_comp_w) <-
	conditional_baltab_comp_w$covariate
conditional_baltab_comp_w$covariate <- NULL

conditional_baltab_comp_w <-
	conditional_baltab_comp_w[-which(rownames(conditional_baltab_comp_w) ==
																	 	"female"), ]


sink("03_tables/balance_conditional_comp_w.tex")
print(
	xtable(conditional_baltab_comp_w),
				include.rownames = TRUE,
	floating = FALSE,
	sanitize.rownames.function = texttt,
	tabular.environment = "longtable"
)
sink()
