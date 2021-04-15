# IPV Cognitive Attitudes -------------------------------------------------

# Disobey
mlcb$disobeys_q8_1a
mlcb$disobeys_no
mlcb$disobeys_yes
ml$disobeys_yes
# Coded so that more force = 0, slapped = 1, yes = 2, no = 3
# then normalized to 1
ml$disobey <- 
  ifelse(
    is.na(ml$disobeys_no),
    2 - ml$disobeys_yes,
    ml$disobeys_no + 1
  ) / 3

ml$disobey_label[ml$disobey == 0] <- "More force than that"
ml$disobey_label[round(ml$disobey,2) == .33] <- "Slapped"
ml$disobey_label[round(ml$disobey,2) == .67] <- "Yes"
ml$disobey_label[ml$disobey == 1] <- "No"

ml$disobey_label <- factor(
  x = ml$disobey_label,
  labels = c("More force than that","Slapped","Yes","No"))

# chatting 
# coded so that yes = 0, no = 1
mlcb$chat_q8_1b
ml$chatting <- ml$chat_q8_1b - 1 

# financial_pressure
# coded so that yes = 0, no = 1
mlcb$fin_pressure_q8_1c
ml$financial_pressure <- ml$fin_pressure_q8_1c - 1

# housework
# coded so that yes = 0, no = 1
mlcb$no_housework_q8_1e
ml$housework <- ml$no_housework_q8_1e - 1 

# Moving to after imputation
# IPV cognitive attitude
ml$ipv_attitude <- ml$chatting
ml$ipv_attitude[!is.na(ml$financial_pressure)] <- ml$financial_pressure[!is.na(ml$financial_pressure)]
ml$ipv_attitude[!is.na(ml$housework)] <- ml$housework[!is.na(ml$housework)]

# IPV Incident Reporting  -------------------------------------------------

# To whom was VAW incident reported, conditional on VAW:
mlcb$report_q16_1c

reporting_na <- ml$beaten_comm_q16_1 %in% c(0,999,888,NA) | ml$woman_beaten_q16_1b %in% c(NA,0,999,888) | is.na(ml$report_q16_1c)

ml$reporting_any <- as.numeric(grepl("1|2|3",ml$report_q16_1c,ignore.case = T))
ml$reporting_any[reporting_na] <- NA

ml$reporting_police <- as.numeric(grepl("3",ml$report_q16_1c,T))
ml$reporting_police[reporting_na] <- NA

ml$reporting_nab <- as.numeric(grepl("1",ml$report_q16_1c,T))
ml$reporting_nab[reporting_na] <- NA

ml$reporting_LC1 <- as.numeric(grepl("2",ml$report_q16_1c,T))
ml$reporting_LC1[reporting_na] <- NA

rm(reporting_na)

# Beating frequency
# coded so that less than once a week = 0, once a week = 1, almost every day = 2
mlcb$once_week_q16_2
mlcb$once_week_q16_2b
mlcb$once_month_q16_2b
ml$once_week_q16_2
ml$beating_more_than_once <- 2 - ml$once_week_q16_2

ml$beating_frequency <- ml$once_week_q16_2b + 2
ml$once_month <- 3 - ml$once_month_q16_2b
ml$beating_frequency[!is.na(ml$once_month)] <- ml$once_month[!is.na(ml$once_month)]  

# Woman beaten
ml$woman_beaten <- ml$woman_beaten_q16_1b
ml$woman_beaten[ml$woman_beaten %in% c(888888)] <- NA
ml$woman_beaten[ml$beaten_comm_q16_1 == 0] <- 0


# Goals -------------------------------------------------------------------

mlcb$goals_q6_2

ml$IPV_goal <- as.numeric(grepl("2",ml$goals_q6_2,T))
ml$IPV_goal[is.na(ml$goals_q6_2)] <- NA
ml$abo_goal <- as.numeric(grepl("6",ml$goals_q6_2,T))
ml$abo_goal[is.na(ml$goals_q6_2)] <- NA
ml$abs_goal <- as.numeric(grepl("4",ml$goals_q6_2,T))
ml$abs_goal[is.na(ml$goals_q6_2)] <- NA

# Conative attitudes (non-randomized) -------------------------------------

# Randomized outcomes -----------------------------------------------------

# Conative attitudes
source("02_code/03_variable_coding/01_code_outcomes_hh_midline_randomized.R")

# Prescriptive norms ------------------------------------------------------

# IPV 

mlcb$intervene_q8_9
ml$should_intervene <- NA
ml$should_intervene[ml$intervene_q8_9 == 2] <- 0
ml$should_intervene[ml$intervene_q8_9 == 1] <- 1

# Efficacy ----------------------------------------------------------------

# IPV

mlcb$friends_fam_q16_3
ml$ipv_efficacy <- NA
ml$ipv_efficacy[ml$friends_fam_q16_3 == 2] <- 0
ml$ipv_efficacy[ml$friends_fam_q16_3 == 1] <- 1

# Discussion --------------------------------------------------------------

ml$convers_q3_3
mlcb$convers_q3_3

ml$IPV_discussion <- as.numeric(
  grepl(
    pattern = "1",
    x = ml$convers_q3_3,
    ignore.case = TRUE
  ))
ml$IPV_discussion[is.na(ml$convers_q3_3)] <- NA

ml$abo_discussion <- as.numeric(
  grepl(
    pattern = "3",
    x = ml$convers_q3_3,
    ignore.case = TRUE
  ))
ml$abo_discussion[is.na(ml$convers_q3_3)] <- NA

ml$abs_discussion <- as.numeric(
  grepl(
    pattern = "7",
    x = ml$convers_q3_3,
    ignore.case = TRUE
  ))
ml$abs_discussion[is.na(ml$convers_q3_3)] <- NA  



# Cognitive Norms ---------------------------------------------------------

# IPV

mlcb$disobeys_community_q8_2
ml$disobey_comm <- NA
ml$disobey_comm[ml$disobeys_community_q8_2 == 1] <- 0
ml$disobey_comm[ml$disobeys_community_q8_2 == 2] <- 1


mlcb$housework_comm_q8_4
ml$housework_comm <- NA
ml$housework_comm[ml$housework_comm_q8_4 == 1] <- 0
ml$housework_comm[ml$housework_comm_q8_4 == 2] <- 1

mlcb$chat_community_q8_5
ml$chat_comm <- NA
ml$chat_comm[ml$chat_community_q8_5 == 1] <- 0
ml$chat_comm[ml$chat_community_q8_5 == 2] <- 1


mlcb$fin_press_comm_q8_6
ml$fin_press_comm <- NA
ml$fin_press_comm[ml$fin_press_comm_q8_6 == 1] <- 0
ml$fin_press_comm[ml$fin_press_comm_q8_6 == 2] <- 1


# IPV cognitive attitude 
ml$ipv_norm <- ml$chat_comm 
ml$ipv_norm[!is.na(ml$housework_comm)] <- ml$housework_comm[!is.na(ml$housework_comm)]
ml$ipv_norm[!is.na(ml$fin_press_comm)] <- ml$fin_press_comm[!is.na(ml$fin_press_comm)]

# Conative Descriptive Norms ---------------------------------------------------------

# IPV
mlcb$react_q8_7
ml$react_comm <- NA
ml$react_comm[ml$react_q8_7 == 1] <- 1
ml$react_comm[ml$react_q8_7 == 2] <- 0

mlcb$turn_backs_q12_6
ml$turn_back <- NA
ml$turn_back[ml$turn_backs_q12_6 == 1] <- 3
ml$turn_back[ml$turn_backs_q12_6 == 2] <- 2
ml$turn_back[ml$turn_backs_q12_6 == 3] <- 1
ml$turn_back[ml$turn_backs_q12_6 == 4] <- 0

mlcb$back_school_q12_8
ml$welcome_school <- NA
ml$welcome_school[ml$back_school_q12_8 == 1] <- 1
ml$welcome_school[ml$back_school_q12_8 == 2] <- 0

# Additional non-prespecified outcomes: views on gender -------------------

# achiever_q7_1
mlcb$achiever_q7_1

ml$not_better_man_achiever <- NA
ml$not_better_man_achiever[ml$achiever_q7_1 == 1] <-0 
ml$not_better_man_achiever[ml$achiever_q7_1 == 2] <-1 

  
# kneel_q7_3
mlcb$kneel_q7_3
ml$women_should_not_kneel <- NA
ml$women_should_not_kneel[ml$kneel_q7_3 == 1] <-1
ml$women_should_not_kneel[ml$kneel_q7_3 == 2] <-0 

# woman_earns_q7_3
mlcb$woman_earns_q7_3

ml$woman_earns_no_problem <- NA
ml$woman_earns_no_problem[ml$woman_earns_q7_3 == 1] <-0
ml$woman_earns_no_problem[ml$woman_earns_q7_3 == 2] <-1 

# women_leaders_q7_5
mlcb$women_leaders_q7_5

ml$women_better_leaders <- NA
ml$women_better_leaders[ml$women_leaders_q7_5 == 1] <-1
ml$women_better_leaders[ml$women_leaders_q7_5 == 2] <-0 

