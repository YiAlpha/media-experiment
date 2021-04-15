# Generate absolute number of men and women

attendance <- festival[, c(
	"n_men_end1",
	"n_men_end2",
	"n_men_end3",
	"n_men_end4",
	"n_men_end5",
	"n_men_end6",
	"n_women_end1",
	"n_women_end2",
	"n_women_end3",
	"n_women_end4",
	"n_women_end5",
	"n_women_end6"
)]

attendance <-
	as.data.frame(apply(attendance, 2, function(x)
		as.integer(as.character(x))))

# attendance <- complete(mice(attendance))

attendance <- within(attendance, {
	n_end1 = n_men_end1 + n_women_end1
	n_end2 = n_men_end2 + n_women_end2
	n_end3 = n_men_end3 + n_women_end3
	n_end4 = n_men_end4 + n_women_end4
	n_end5 = n_men_end5 + n_women_end5
	n_end6 = n_men_end6 + n_women_end6
})

festival$n_end_mean <- rowMeans(attendance[, c("n_end1",
																							 "n_end2",
																							 "n_end3",
																							 "n_end4",
																							 "n_end5",
																							 "n_end6")], na.rm = TRUE)

festival$n_end_total <- rowSums(attendance[, c("n_end1",
																							 "n_end2",
																							 "n_end3",
																							 "n_end4",
																							 "n_end5",
																							 "n_end6")], na.rm = TRUE)


festival$n_men_end_mean <- rowMeans(attendance[, c("n_men_end1",
																									 "n_men_end2",
																									 "n_men_end3",
																									 "n_men_end4",
																									 "n_men_end5",
																									 "n_men_end6")], na.rm = TRUE)

festival$n_men_end_total <- rowSums(attendance[, c("n_men_end1",
																									 "n_men_end2",
																									 "n_men_end3",
																									 "n_men_end4",
																									 "n_men_end5",
																									 "n_men_end6")], na.rm = TRUE)

festival$n_women_end_mean <- rowMeans(attendance[, c(
	"n_women_end1",
	"n_women_end2",
	"n_women_end3",
	"n_women_end4",
	"n_women_end5",
	"n_women_end6"
)], na.rm = TRUE)

festival$n_women_end_total <- rowSums(attendance[, c(
	"n_women_end1",
	"n_women_end2",
	"n_women_end3",
	"n_women_end4",
	"n_women_end5",
	"n_women_end6"
)], na.rm = TRUE)


attendance_baltabs <-
	festival %>%
	dplyr::select(
		tc_id,
		n_end_mean,
		n_end_total,
		n_men_end_mean,
		n_men_end_total,
		n_women_end_mean,
		n_women_end_total
	) %>%
	left_join(y = treatment_assignment, by = "tc_id")

rm(attendance)

# Testing balance of attendance across outcomes ---------------------------

attend_vars <- c(
	"n_end_mean",
	"n_end_total",
	"n_men_end_mean",
	"n_men_end_total",
	"n_women_end_mean",
	"n_women_end_total"
)

# Balance function --------------------------------------------------------

# Function should first take mean of all of the three conditions, then the 7
# Then computes a likelihood ratio test

observed_lik_dif <- sapply(
	X = 1:length(attend_vars),
	FUN = function(i) {
		full_model_marginal <- lm(formula = as.formula(
			paste0(
				attend_vars[i],
				" ~ IPV + abortion + absenteeism  + as.factor(block_id)"
			)
		),
		data = attendance_baltabs)
		
		full_model_conditional <- lm(formula = as.formula(paste0(
			attend_vars[i], " ~ treatment  + as.factor(block_id)"
		)),
		data = attendance_baltabs)
		
		nested_model <- lm(formula = as.formula(paste0(attend_vars[i], " ~  as.factor(block_id)")),
											 data = attendance_baltabs)
		
		marginal_test <- lmtest::lrtest(full_model_marginal, nested_model)
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
colnames(observed_lik_dif) <- attend_vars

marginal_mat <- conditional_mat <- list()

set.seed(123456789)

for (j in (length(marginal_mat) + 1):sims) {
	new_data <-
		rerandomize(tc_level_data = treatment_assignment, analysis_data = attendance_baltabs)
	null_lik_dif <- sapply(
		X = 1:length(attend_vars),
		FUN = function(i) {
			full_model_marginal <- lm(formula = as.formula(
				paste0(
					attend_vars[i],
					" ~ IPV + abortion + absenteeism + as.factor(block_id)"
				)
			),
			data = new_data)
			
			full_model_conditional <- lm(formula = as.formula(paste0(
				attend_vars[i], " ~ treatment + as.factor(block_id)"
			)),
			data = new_data)
			
			nested_model <- lm(formula = as.formula(paste0(
				attend_vars[i], " ~  as.factor(block_id)"
			)),
			data = new_data)
			
			marginal_test <- lmtest::lrtest(full_model_marginal, nested_model)
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

colnames(marginal_sims) <- colnames(conditional_sims) <- attend_vars

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
	attendance_baltabs %>%
	subset(select = c(attend_vars, "IPV")) %>%
	group_by(IPV) %>%
	summarize_each(funs = "mean") %>%
	t()
baltab_ABO <-
	attendance_baltabs %>%
	subset(select = c(attend_vars, "abortion")) %>%
	group_by(abortion) %>%
	summarize_each(funs = "mean") %>%
	t()
baltab_ABS <-
	attendance_baltabs %>%
	subset(select = c(attend_vars, "absenteeism")) %>%
	group_by(absenteeism) %>%
	summarize_each(funs = "mean") %>%
	t()
baltab_placebo <-
	attendance_baltabs %>%
	subset(select = c(attend_vars, "placebo")) %>%
	group_by(placebo) %>%
	summarize_each(funs = "mean") %>%
	t()

marginal_baltab <-
	cbind.data.frame(covariate = attend_vars,
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

baltab_conditional <-
	attendance_baltabs %>%
	subset(select = c(attend_vars, "treatment")) %>%
	group_by(treatment) %>%
	summarize_each(funs = "mean") %>%
	ungroup() %>%
	dplyr::select(-treatment) %>%
	t() %>%
	data.frame()

names(baltab_conditional) <- sort(unique(ml$treatment))

conditional_baltab <- cbind.data.frame(covariate = attend_vars,
																			 round(data.frame(baltab_conditional[, c(
																			 	"placebo",
																			 	"IPV",
																			 	"abortion",
																			 	"absenteeism",
																			 	"abortion_absenteeism",
																			 	"IPV_abortion",
																			 	"IPV_absenteeism"
																			 )],
																			 pval = conditional_pvalues),
																			 2))

rm(baltab_conditional)

baltab_out <- conditional_baltab[c(1,2,5,6,3,4),-which(colnames(conditional_baltab) == "covariate")]

rownames(baltab_out) <- c(
	"Average attendees per screening",
	"Total attendees per trading center",
	"Average women attendees per screening", 
	"Total women attendees per trading center",
	"Average men attendees per screening", 
	"Total men attendees per trading center"
)

colnames(baltab_out) <- c("Placebo","VAW","ABO","ABS","ABO+ABS","VAW+ABO","VAW+ABS","p-value")

sink("03_tables/attendance_balance.tex")
print.xtable(xtable(baltab_out,
										align = c("p{4cm}","p{1cm}","p{1cm}","p{1cm}","p{1cm}","p{2cm}","p{2cm}","p{2cm}","p{1.5cm}")),floating = FALSE)
sink()












