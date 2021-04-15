# Incidents unreported
vht_village_level_ml <-
	vht_ml %>%
	group_by(tc_id, block_id, IPV, abortion, absenteeism) %>%
	dplyr::summarize(
		unreported_ipv_ml = mean(unreported_ipv, na.rm = T),
		radius = unique(radius),
		n_end_mean = unique(n_end_mean)
	)
vht_village_level_ml$tc <- vht_village_level_ml$tc_id
vht_village_level_el <-
	vht_el %>%
	group_by(tc_id, block_id, IPV, abortion, absenteeism) %>%
	dplyr::summarize(
		unreported_ipv_el = mean(unreported_ipv, na.rm = T),
		radius = unique(radius),
		n_end_mean = unique(n_end_mean)
	)
vht_village_level_el$tc <- vht_village_level_el$tc_id
vht_village_level_el$resample <- 1
vht_village_level_ml$resample <- 1

do_f_tests <-
	function(outcome_name,
					 data,
					 wave_label,
					 subset_label) {
		restricted <- reformulate(
			termlabels = c("IPV", "as.factor(block_id)", "resample"),
			response = outcome_name
		)
		
		abortion_interaction <-
			update.formula(restricted, . ~ . + IPV * abortion)
		
		fully_saturated <-
			update.formula(restricted, . ~  . + IPV * abortion * absenteeism)
		
		
		r_fit <- lm(formula = restricted, data = data)
		a_fit <- lm(formula = abortion_interaction, data = data)
		f_fit <- lm(formula = fully_saturated, data = data)
		
		test_1 <- anova(r_fit, a_fit)
		test_2 <- anova(r_fit, f_fit)
		
		return(
			data.frame(
				outcome = outcome_name,
				wave = wave_label,
				subset = subset_label,
				abortion_p = test_1[2, "Pr(>F)"],
				saturated_p = test_2[2, "Pr(>F)"]
			)
		)
		
		
	}


f_test_list <- list(
	list(
		"household_violence",
		"endline",
		"all women",
		subset(el, female == 1)
	),
	list("any_violence", "endline", "all women", subset(el, female == 1)),
	
	list(
		"violence_disapproval",
		"endline",
		"complier potential perpetrators",
		subset(
			el,
			respondent_category == "Complier" &
				female == 0 & status_q2_5 %in% c(1:3)
		)
	),
	list(
		"violence_disapproval_ml",
		"midline",
		"complier potential perpetrators",
		subset(
			el,
			respondent_category == "Complier" &
				female == 0 & status_q2_5 %in% c(1:3)
		)
	),
	list(
		"empathy_pair",
		"endline",
		"complier potential perpetrators",
		subset(
			el,
			respondent_category == "Complier" &
				female == 0 & status_q2_5 %in% c(1:3)
		)
	),
	list(
		"empathy_husband",
		"endline",
		"all compliers",
		subset(el, respondent_category == "Complier")
	),
	
	list(
		"intervene_index",
		"endline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"involve_parents",
		"endline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"involve_nabakyala",
		"endline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"involve_lc1",
		"endline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"report_police",
		"endline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"intervene_index",
		"endline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	list(
		"involve_parents",
		"endline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	list(
		"involve_nabakyala",
		"endline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	list(
		"involve_lc1",
		"endline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	list(
		"report_police",
		"endline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	
	list(
		"intervene_index_ml",
		"midline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"involve_parents_ml",
		"midline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"involve_nabakyala_ml",
		"midline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"involve_lc1_ml",
		"midline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"report_police_ml",
		"midline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"intervene_index_ml",
		"midline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	list(
		"involve_parents_ml",
		"midline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	list(
		"involve_nabakyala_ml",
		"midline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	list(
		"involve_lc1_ml",
		"midline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	list(
		"report_police_ml",
		"midline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	
	list(
		"react_comm",
		"endline",
		"complier potential perpetrators",
		subset(
			el,
			respondent_category == "Complier" &
				female == 0 & status_q2_5 %in% c(1:3)
		)
	),
	list(
		"react_comm_ml",
		"midline",
		"complier potential perpetrators",
		subset(
			el,
			respondent_category == "Complier" &
				female == 0 & status_q2_5 %in% c(1:3)
		)
	),
	list(
		"react_comm",
		"endline",
		"all compliers",
		subset(el, respondent_category == "Complier")
	),
	list(
		"react_comm_ml",
		"midline",
		"all compliers",
		subset(el, respondent_category == "Complier")
	),
	list("unreported_ipv_el", "endline", "VHTs", vht_village_level_el),
	list("unreported_ipv_ml", "midline", "VHTs", vht_village_level_ml),
	
	list(
		"ipv_efficacy",
		"endline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"ipv_efficacy",
		"endline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	list(
		"ipv_efficacy_ml",
		"midline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"ipv_efficacy_ml",
		"midline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	
	list(
		"violence_disapproval",
		"endline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"violence_disapproval",
		"endline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	list(
		"violence_disapproval_ml",
		"midline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"violence_disapproval_ml",
		"midline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	
	list(
		"empathy_pair",
		"endline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"empathy_pair",
		"endline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	),
	list(
		"spiral_risk",
		"endline",
		"complier men",
		subset(el, respondent_category == "Complier" & female == 0)
	),
	list(
		"spiral_risk",
		"endline",
		"complier women",
		subset(el, respondent_category == "Complier" & female == 1)
	)
)

f_test_frame <- data.frame()

for (i in 1:length(f_test_list)) {
	f_test_frame <- rbind(
		f_test_frame,
		do_f_tests(
			outcome_name = f_test_list[[i]][[1]],
			wave_label = f_test_list[[i]][[2]],
			subset_label = f_test_list[[i]][[3]],
			data = f_test_list[[i]][[4]]
		)
	)
}

f_test_frame$outcome <-
	gsub(pattern = "_",
			 replacement = "\\\\_",
			 x = f_test_frame$outcome)
f_test_frame$outcome <- paste0("\\texttt{", f_test_frame$outcome, "}")

sink(file = "03_tables/crossover_f_tests.tex")
knitr::kable(
	f_test_frame,
	col.names = c(
		"Outcome",
		"Wave",
		"Subset",
		"Abortion interaction $p$-value",
		"Fully saturated $p$-value"
	),
	format = "latex",
	digits = 3,
	align = c("l", "l", "l", "c", "c"),
	escape = F
)
sink()
