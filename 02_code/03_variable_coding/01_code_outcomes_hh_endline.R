# Violence Against Women --------------------------------------------------


# Incidents

# Community Level Number of Incidents
elcb$beaten_comm_q16_1
elcb$woman_beaten_q16_1b
el$woman_beaten <- el$woman_beaten_q16_1b
el$woman_beaten[el$beaten_comm_q16_1 == 0] <- 0


# Community Level Reporting

elcb$report_q16_1c
elcb$report_q16_1c_oth

reporting_na <-
	el$beaten_comm_q16_1 %in% c(0, 999, 888, NA) |
	el$woman_beaten_q16_1b %in% c(NA, 0, 999, 888) |
	(el$report_q16_1c == 999)

el$reporting_any <-
	as.numeric(grepl("1|2|3", el$report_q16_1c, ignore.case = T))
el$reporting_any[reporting_na] <- NA

el$reporting_police <- as.numeric(grepl("3", el$report_q16_1c, T))
el$reporting_police[reporting_na] <- NA

el$reporting_nab <- as.numeric(grepl("1", el$report_q16_1c, T))
el$reporting_nab[reporting_na] <- NA

el$reporting_LC1 <- as.numeric(grepl("2", el$report_q16_1c, T))
el$reporting_LC1[reporting_na] <- NA

rm(reporting_na)


# Community Level Beating frequency
# coded so that less than once a week = 0, once a week = 1, almost every day = 2
table(el$beating_freq_q17_2, useNA = "always")
el$beating_freq_q17_2[el$beaten_comm_q16_1 == 0]
elcb$beating_freq_q17_2
elcb$beating_yes_q17_2b
elcb$beating_no_q17_2b

el$beating_frequency <- NA
el$beating_frequency[el$beating_no_q17_2b == 3] <- 0
el$beating_frequency[el$beating_no_q17_2b == 2] <- 1
el$beating_frequency[el$beating_no_q17_2b == 1] <- 2
el$beating_frequency[el$beating_yes_q17_2b == 1] <- 3
el$beating_frequency[el$beating_yes_q17_2b == 2] <- 4

# Household Level Incidents

elcb$hh_violence_yesno_q17_2
elcb$incidents_q17_2b

table(el$incidents_q17_2b, useNA = "always")

el$household_violence <- NA
el$household_violence <- el$incidents_q17_2b
el$household_violence[el$hh_violence_yesno_q17_2 == 2] <- 0


el$household_violence_2 <- 0
el$household_violence_2[el$incidents_q17_2b > 0 &
													!is.na(el$incidents_q17_2b)] <-
	el$incidents_q17_2b[el$incidents_q17_2b > 0 &
												!is.na(el$incidents_q17_2b)]
el$household_violence_2[is.na(el$incidents_q17_2b)] <- 0
el$household_violence_2[el$female == 0] <- NA


elcb$incidents_q17_2b
el$incidents_q17_2b[el$female == 1 &
											el$hh_violence_yesno_q17_2 == 1]
el$incidents_q17_2b[el$female == 1 &
											el$hh_violence_yesno_q17_2 == 2]
elcb$hh_violence_yesno_q17_2


# Household Level Incident Frequency

elcb$hh_violence_freq_q17_3
elcb$hh_violence_yes_q17_3b
elcb$hh_violence_no_q17_3b


el$household_violence_frequency <- NA
el$household_violence_frequency[el$hh_violence_no_q17_3b == 3] <-
	0
el$household_violence_frequency[el$hh_violence_no_q17_3b == 2] <- 1
el$household_violence_frequency[el$hh_violence_no_q17_3b == 1] <- 2
el$household_violence_frequency[el$hh_violence_yes_q17_3b == 1] <- 3
el$household_violence_frequency[el$hh_violence_yes_q17_3b == 2] <- 4
el$household_violence_frequency[el$hh_violence_yesno_q17_2 == 2] <-
	0

# Efficacy

elcb$ipv_efficacy_q17_4
el$ipv_efficacy <- NA
el$ipv_efficacy[el$ipv_efficacy_q17_4 == 2] <- 0
el$ipv_efficacy[el$ipv_efficacy_q17_4 == 1] <- 1

# IPV Cognitive Attitudes -------------------------------------------------

# Disobey
elcb$disobeys_q8_1a
elcb$disobeys_no
elcb$disobeys_yes
el$disobeys_yes
# Coded so that more force = 0, slapped = 1, yes = 2, no = 3
# then normalized to 1
el$disobey <-
	ifelse(is.na(el$disobeys_no),
				 2 - el$disobeys_yes,
				 el$disobeys_no + 1) / 3

table(el$disobey, el$disobeys_q8_1a)

el$disobey_label[el$disobey == 0] <- "More force than that"
el$disobey_label[round(el$disobey, 2) == .33] <- "Slapped"
el$disobey_label[round(el$disobey, 2) == .67] <- "Yes"
el$disobey_label[el$disobey == 1] <- "No"

el$disobey_label <- factor(
	x = el$disobey_label,
	labels = c("More force than that", "Slapped", "Yes", "No")
)

elcb$chat_q8_1b
el$chatting <- el$chat_q8_1b - 1

# financial_pressure
# coded so that yes = 0, no = 1
elcb$fin_pressure_q8_1c
el$financial_pressure <- el$fin_pressure_q8_1c - 1

# housework
# coded so that yes = 0, no = 1
elcb$no_housework_q8_1e
el$housework <- el$no_housework_q8_1e - 1

# IPV cognitive attitude
el$ipv_attitude <- el$chatting
el$ipv_attitude[!is.na(el$financial_pressure)] <-
	el$financial_pressure[!is.na(el$financial_pressure)]
el$ipv_attitude[!is.na(el$housework)] <-
	el$housework[!is.na(el$housework)]

# IPV Index
el$violence_disapproval <-  (el$ipv_attitude + el$disobey) / 2

# Cognitive norms ---------------------------------------------------------

elcb$disobeys_community_q8_2
el$disobey_comm <- NA
el$disobey_comm[el$disobeys_community_q8_2 == 1] <- 0
el$disobey_comm[el$disobeys_community_q8_2 == 2] <- 1


elcb$housework_comm_q8_4
el$housework_comm <- NA
el$housework_comm[el$housework_comm_q8_4 == 1] <- 0
el$housework_comm[el$housework_comm_q8_4 == 2] <- 1

elcb$chat_community_q8_5
el$chat_comm <- NA
el$chat_comm[el$chat_community_q8_5 == 1] <- 0
el$chat_comm[el$chat_community_q8_5 == 2] <- 1


elcb$fin_press_comm_q8_6
el$fin_press_comm <- NA
el$fin_press_comm[el$fin_press_comm_q8_6 == 1] <- 0
el$fin_press_comm[el$fin_press_comm_q8_6 == 2] <- 1

# IPV cognitive attitude
el$ipv_norm <- el$chat_comm
el$ipv_norm[!is.na(el$housework_comm)] <-
	el$housework_comm[!is.na(el$housework_comm)]
el$ipv_norm[!is.na(el$fin_press_comm)] <-
	el$fin_press_comm[!is.na(el$fin_press_comm)]


el$violence_disapproval_comm <- (el$disobey_comm + el$ipv_norm) / 2

# Conative Descriptive Norms ---------------------------------------------------------

# IPV
elcb$react_q8_7
el$react_comm <- NA
el$react_comm[el$react_q8_7 == 1] <- 1
el$react_comm[el$react_q8_7 == 2] <- 0

# Prescriptive norms ------------------------------------------------------

# IPV

elcb$intervene_q8_9
el$should_intervene <- NA
el$should_intervene[el$intervene_q8_9 == 2] <- 0
el$should_intervene[el$intervene_q8_9 == 1] <- 1

# Goals -------------------------------------------------------------------

elcb$goals_q6_2

el$IPV_goal <- as.numeric(grepl("2", el$goals_q6_2, T))
el$IPV_goal[is.na(el$goals_q6_2)] <- NA
el$abo_goal <- as.numeric(grepl("6", el$goals_q6_2, T))
el$abo_goal[is.na(el$goals_q6_2)] <- NA
el$abs_goal <- as.numeric(grepl("4", el$goals_q6_2, T))
el$abs_goal[is.na(el$goals_q6_2)] <- NA

# Discussion Outcomes -----------------------------------------------------

elcb$convers_q3_3

el$IPV_discussion <- as.numeric(grepl(
	pattern = "1 |1$",
	x = el$convers_q3_3,
	ignore.case = TRUE
))

el$IPV_discussion[el$convers_q3_3 == 1] <- 1
el$IPV_discussion[is.na(el$convers_q3_3)] <- NA

el$abo_discussion <- as.numeric(grepl(
	pattern = "3",
	x = el$convers_q3_3,
	ignore.case = TRUE
))
el$abo_discussion[is.na(el$convers_q3_3)] <- NA

el$abs_discussion <- as.numeric(grepl(
	pattern = "7",
	x = el$convers_q3_3,
	ignore.case = TRUE
))
el$abs_discussion[is.na(el$convers_q3_3)] <- NA

# IPV Mediators and Moderators -----------------------------------------------------------

# Spiral Risk

elcb$spiral_risk_q8_7
el$spiral_risk <- NA
el$spiral_risk[el$spiral_risk_q8_7 == 1] <- 0
el$spiral_risk[el$spiral_risk_q8_7 == 2] <- 1
el$spiral_risk[is.na(el$spiral_risk_q8_7)] <- NA

# empathy_pair_q8_8

elcb$empathy_pair_q8_8
el$empathy_pair <- NA
el$empathy_pair[el$empathy_pair_q8_8 == 1] <- 1
el$empathy_pair[el$empathy_pair_q8_8 == 2] <- 0


# empathy_husband_q8_9
elcb$empathy_husband_q8_9
el$empathy_husband <- NA
el$empathy_husband[el$empathy_husband_q8_9 == 1] <- 1
el$empathy_husband[el$empathy_husband_q8_9 == 2] <- 0


# revenge_q8_11
elcb$revenge_q8_11
el$would_take_revenge <- 2 - el$revenge_q8_11


# reporting_exp_q8_12

elcb$reporting_exp_q8_12
table(el$forum)
el$reporting_exp_q8_12
el$has_reported_any <- 2 - el$reporting_exp_q8_12

el$has_reported_nabakyala <- el$has_reported_any
el$has_reported_nabakyala[grepl(pattern = "LC1|Police",
																x = el$forum,
																ignore.case = FALSE)] <- NA

el$has_reported_police <- el$has_reported_any
el$has_reported_police[grepl(pattern = "LC1|nabakyala",
														 x = el$forum,
														 ignore.case = FALSE)] <- NA

el$has_reported_lc1 <- el$has_reported_any
el$has_reported_lc1[grepl(pattern = "nabakyala|Police",
													x = el$forum,
													ignore.case = FALSE)] <- NA

elcb$reporting_cost3_q8_13c

el$treat_polite <- 2 - el$reporting_cost1_q8_13a
el$take_seriously <- 2 - el$reporting_cost2_q8_13b
el$treat_same <- 2 - el$reporting_cost3_q8_13c

# intervention_q9_1
# intervention_consequence_q9_2

elcb$intervention_q9_1
el$anti_intervention_norm <- el$intervention_q9_1 - 1

elcb$interv_consequence_q9_2
el$anti_intervention_consequence <- el$interv_consequence_q9_2 - 1

# IPV Free-riding Survey Experiment ---------------------------------------

# Survey experiment variable
el$others_observe <- NA
el$others_observe <- as.numeric(el$free_riding_ran > .5)
el$free_riding[el$others_observe == 1]

elcb$free.riding_q8_10
elcb$free.riding_q8_10_2
el$would_report <- 2 - el$free.riding_q8_10
el$would_report[!is.na(el$free.riding_q8_10_2)] <-
	2 - el$free.riding_q8_10_2[!is.na(el$free.riding_q8_10_2)]

# boys_school_q7_1

elcb$boys_school_q7_1
el$boys_school_not_more_imp <- NA
el$boys_school_not_more_imp[el$boys_school_q7_1 ==1] <- 0
el$boys_school_not_more_imp[el$boys_school_q7_1 ==2] <- 1

# able_marry_q7_2

elcb$able_marry_q7_2
el$able_marry <- NA
el$able_marry[el$able_marry_q7_2 ==1] <- 1
el$able_marry[el$able_marry_q7_2 ==2] <- 0

# final_say_q7_3
elcb$final_say_q7_3
el$father_not_final_say <- NA
el$father_not_final_say[el$final_say_q7_3 ==1] <- 0
el$father_not_final_say[el$final_say_q7_3 ==2] <- 1


# child_rearing_q7_4
elcb$child_rearing_q7_4
el$men_should_participate <- NA
el$men_should_participate[el$child_rearing_q7_4 ==1] <- 1
el$men_should_participate[el$child_rearing_q7_4 ==2] <- 0
