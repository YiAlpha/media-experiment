# Step 1:
# - Bootstrap midline data and get weights
# - Hold clusters constant and bootstrap within them with replacement
# Step 2:
# - Bootstrap endline data
# - Get proportions
# - Multiply number of households by proportion of compliers in each
# - Multiply number of households by proportion of non-compliers in each
# - Multiply those numbers by effect sizes and store


el$any_violence <- el$household_violence > 0
el$any_violence[is.na(el$household_violence)] <- NA

bootstrap_effects <- function() {
	prop_comp_bs <- group_by(ml, tc_id) %>%
		mutate(KEY = sample(
			x = KEY,
			size = n(),
			replace = TRUE
		)) %>%
		dplyr::select(tc_id, KEY, IPV) %>%
		ungroup() %>%
		left_join(ml, by = c("tc_id", "KEY", "IPV")) %>%
		group_by(tc_id, IPV) %>%
		summarize(prop_comp = mean(compliance == 1, na.rm = TRUE)) %>%
		ungroup() %>%
		left_join(with(festival, data.frame(
			tc_id = tc_id, N_hh_listed = N_hh_listed
		)), by = "tc_id") %>%
		group_by(IPV) %>%
		summarize(
			N_comp = sum(prop_comp * N_hh_listed, na.rm = TRUE),
			N_non_comp = sum((1 - prop_comp) * N_hh_listed, na.rm = TRUE)
		)
	
	N_comp_IPV <- prop_comp_bs$N_comp[prop_comp_bs$IPV == 1]
	N_non_comp_IPV <- prop_comp_bs$N_non_comp[prop_comp_bs$IPV == 1]
	
	village_bs <- el %>%
		filter(female == 1) %>%
		group_by(tc_id) %>%
		mutate(KEY = sample(
			x = KEY,
			size = n(),
			replace = TRUE
		)) %>%
		dplyr::select(tc_id, KEY) %>%
		ungroup() %>%
		left_join(el, by = c("tc_id", "KEY"))
	
	village_non_comp <-
		filter(village_bs, !complier) %>%
		group_by(tc_id, block_id, IPV) %>%
		summarize(
			any_violence = mean(any_violence, na.rm = T),
			n_end_mean = unique(n_end_mean)
		)
	
	village_comp <-
		filter(village_bs, complier) %>%
		group_by(tc_id, block_id, IPV) %>%
		summarize(
			any_violence = mean(any_violence, na.rm = T),
			n_end_mean = unique(n_end_mean)
		)
	
	any_violence_comp <- lm(any_violence ~ IPV + as.factor(block_id) + n_end_mean, data = village_comp)
	
	any_violence_non_comp <- lm(any_violence ~ IPV + as.factor(block_id) + n_end_mean, data = village_non_comp)
	
	effect_comp <- coef(any_violence_comp)["IPV"]
	effect_non_comp <- coef(any_violence_non_comp)["IPV"]
	
	
	return(
		c(
			extrapolation_comp = N_comp_IPV * effect_comp,
			extrapolation_non_comp = N_non_comp_IPV * effect_non_comp
		)
	)
}

set.seed(123456789)
bstrap_effs <-
	replicate(n = bootstrap_sims, expr = bootstrap_effects())

bstraps <- as.data.frame(t(bstrap_effs))

names(bstraps) <- c("complier_effect", "non_complier_effect")
bstraps$combined_effect <-
	bstraps$complier_effect + bstraps$non_complier_effect

bootstrap_table <- sapply(bstraps,quantile,probs = c(.025,.975))

sink("03_tables/extrapolation_bootstraps.tex")
print(kable(bootstrap_table,digits = 0,format = "latex",col.names = c("Compliers","Non-Compliers","Weighted Combination")))
sink()



mean(bstraps$combined_effect)
hist(bstraps$combined_effect)

# Point estimates ---------------------------------------------------------

prop_comp <- group_by(ml, tc_id) %>%
	group_by(tc_id, IPV) %>%
	summarize(prop_comp = mean(compliance == 1, na.rm = TRUE)) %>%
	ungroup() %>%
	left_join(with(festival, data.frame(
		tc_id = tc_id, N_hh_listed = N_hh_listed
	)), by = "tc_id") %>%
	group_by(IPV) %>%
	summarize(
		N_comp = sum(prop_comp * N_hh_listed, na.rm = TRUE),
		N_non_comp = sum((1 - prop_comp) * N_hh_listed, na.rm = TRUE)
	)

N_comp_IPV <- prop_comp$N_comp[prop_comp$IPV == 1]
N_non_comp_IPV <- prop_comp$N_non_comp[prop_comp$IPV == 1]


village_non_comp <-
	filter(el, !complier & female == 1) %>%
	group_by(tc_id, block_id, IPV) %>%
	summarize(
		any_violence = mean(any_violence, na.rm = T),
		n_end_mean = unique(n_end_mean)
	)

village_comp <-
	filter(el, complier & female == 1) %>%
	group_by(tc_id, block_id, IPV) %>%
	summarize(
		any_violence = mean(any_violence, na.rm = T),
		n_end_mean = unique(n_end_mean)
	)

any_violence_comp <- lm(any_violence ~ IPV + as.factor(block_id) + n_end_mean, data = village_comp)

any_violence_non_comp <- lm(any_violence ~ IPV + as.factor(block_id) + n_end_mean, data = village_non_comp)

effect_comp <- coef(any_violence_comp)["IPV"]
effect_non_comp <- coef(any_violence_non_comp)["IPV"]

extrapolation_comp <- N_comp_IPV * effect_comp
extrapolation_non_comp <- N_non_comp_IPV * effect_non_comp

extrapolation_non_comp + extrapolation_comp

save(bstrap_effs, bstraps, file = "01_data//bootstrap_extrapolation.Rdata")
