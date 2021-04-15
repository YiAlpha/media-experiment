village_level <-
	el %>%
	group_by(tc_id) %>%
	dplyr::summarize(
		household_violence_el = mean(household_violence[female == 1], na.rm = T),
		beating_frequency_el_women = mean(beating_frequency[female == 1], na.rm = T),
		beating_frequency_el_men = mean(beating_frequency[female == 0], na.rm = T),
		woman_beaten_el_men = mean(woman_beaten[female == 0], na.rm = T)
	)



village_level_ml <-
	ml %>%
	group_by(tc_id) %>%
	dplyr::summarize(
		beating_frequency_ml_men = mean(beating_frequency[female == 0], na.rm = T),
		beating_frequency_ml_women = mean(beating_frequency[female == 1], na.rm = T),
		woman_beaten_ml_men = mean(woman_beaten[female == 0], na.rm = T),
		woman_beaten_ml_women = mean(woman_beaten[female == 1], na.rm = T)
	)


vht_village_level_ml <-
	vht_ml %>%
	group_by(tc_id) %>%
	dplyr::summarize(vht_ipv_incidents_ml = mean(reported_ipv_incidents, na.rm = T))
vht_village_level_el <-
	vht_el %>%
	group_by(tc_id) %>%
	dplyr::summarize(vht_ipv_incidents_el = mean(reported_ipv_incidents, na.rm = T))

village_level <-
	left_join(village_level, village_level_ml, by = "tc_id")
village_level <-
	left_join(village_level, vht_village_level_ml, by = "tc_id")
village_level <-
	left_join(village_level, vht_village_level_el, by = "tc_id")


village_level <- as.matrix(village_level[, -1])
cormat <- cor(village_level)
rownames(cormat) <-
	colnames(cormat) <-
	c(
		"N EL W",
		"Frq EL W",
		"Frq EL M",
		"Cnt EL M",
		"Frq ML M",
		"Frq ML W",
		"Cnt ML M",
		"Cnt ML W",
		"Cnt ML VHT",
		"Cnt EL VHT"
	)


sink("03_tables/violence_prevalence_corrmat.tex")
kable(cormat,
			digits = 2,
			format = "latex",
			align = "c")
sink()
