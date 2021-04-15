

# Individual level covariates ---------------------------------------------

# age
vht_elcb$age_q2_2
vht_el$age_vht <- NA
vht_el$age_vht <- vht_el$age_q2_2

# experience (in years)
vht_elcb$unit_q2_3vht
vht_elcb$number_q2_3vht
vht_elcb$unit_q2_3naba
vht_elcb$number_q2_3naba

vht_el$experience_vht_naba <- NA
vht_el$experience_vht_naba[vht_el$unit_q2_3vht == 1 &
													 	!is.na(vht_el$number_q2_3vht)] <-
	vht_el$number_q2_3vht[vht_el$unit_q2_3vht == 1 &
													!is.na(vht_el$number_q2_3vht)] / 12
vht_el$experience_vht_naba[vht_el$unit_q2_3vht == 2 &
													 	!is.na(vht_el$number_q2_3vht)] <-
	vht_el$number_q2_3vht[vht_el$unit_q2_3vht == 2 &
													!is.na(vht_el$number_q2_3vht)]
vht_el$experience_vht_naba[vht_el$unit_q2_3naba == 1 &
													 	!is.na(vht_el$number_q2_3naba)] <-
	vht_el$number_q2_3naba[vht_el$unit_q2_3naba == 1 &
												 	!is.na(vht_el$number_q2_3naba)] / 12
vht_el$experience_vht_naba[vht_el$unit_q2_3naba == 2 &
													 	!is.na(vht_el$number_q2_3naba)] <-
	vht_el$number_q2_3naba[vht_el$unit_q2_3naba == 2 &
												 	!is.na(vht_el$number_q2_3naba)]

# salary

vht_elcb$paid_q2_5vht
vht_elcb$paid_q2_5naba

vht_el$salary_vht_naba <- NA
vht_el$salary_vht_naba[vht_el$paid_q2_5vht == 1] <- 1
vht_el$salary_vht_naba[vht_el$paid_q2_5vht == 2] <- 0
vht_el$salary_vht_naba[vht_el$paid_q2_5naba == 1] <- 1
vht_el$salary_vht_naba[vht_el$paid_q2_5naba == 2] <- 0

# witchcraft

vht_elcb$killed_witch_q14_1
vht_elcb$possible_kill_q14_1b

vht_el$killed_witchcraft <- NA
vht_el$possible_kill_witchcraft <- NA

vht_el$killed_witchcraft[vht_el$killed_witch_q14_1 == 1] <- 1
vht_el$killed_witchcraft[vht_el$killed_witch_q14_1 == 2] <- 0

vht_el$possible_kill_witchcraft[vht_el$possible_kill_q14_1b == 1] <-
	1
vht_el$possible_kill_witchcraft[vht_el$possible_kill_q14_1b == 2] <-
	0


# Covariates that should be aggregated to cluster level -------------------

# distance to health center 2 in minutes
vht_elcb$unit_q2_7
vht_elcb$number_q2_7

vht_el$health_center_2 <- NA
vht_el$health_center_2[vht_el$unit_q2_7 == 1] <-
	vht_el$number_q2_7[vht_el$unit_q2_7 == 1]
vht_el$health_center_2[vht_el$unit_q2_7 == 2] <-
	vht_el$number_q2_7[vht_el$unit_q2_7 == 2] * 60

# distance to health center 3 in minutes

vht_elcb$unit_q2_7b
vht_elcb$number_q2_7b


vht_el$health_center_3 <- NA
vht_el$health_center_3[vht_el$unit_q2_7b == 1] <-
	vht_el$number_q2_7b[vht_el$unit_q2_7b == 1]
vht_el$health_center_3[vht_el$unit_q2_7b == 2] <-
	vht_el$number_q2_7b[vht_el$unit_q2_7b == 2] * 60


# number of households responsible for

vht_elcb$households_q2_8

vht_el$housheholds_vht <- vht_el$households_q2_8

# rounds per month

vht_elcb$visits_q2_9units
vht_elcb$visits_q2_9number

vht_el$rounds_vht <- NA
vht_el$rounds_vht[vht_el$visits_q2_9units == 1 &
										!is.na(vht_el$visits_q2_9number)] <-
	vht_el$visits_q2_9number[vht_el$visits_q2_9units == 1 &
													 	!is.na(vht_el$visits_q2_9number)] * 30
vht_el$rounds_vht[vht_el$visits_q2_9units == 2 &
										!is.na(vht_el$visits_q2_9number)] <-
	vht_el$visits_q2_9number[vht_el$visits_q2_9units == 2 &
													 	!is.na(vht_el$visits_q2_9number)] * 4
vht_el$rounds_vht[vht_el$visits_q2_9units == 4 &
										!is.na(vht_el$visits_q2_9number)] <-
	vht_el$visits_q2_9number[vht_el$visits_q2_9units == 4 &
													 	!is.na(vht_el$visits_q2_9number)] / 12


# doctor rounds

vht_elcb$rounds_q6_1

vht_el$doctor_rounds <- NA
vht_el$doctor_round[vht_el$rounds_q6_1 == 1] <- 1
vht_el$doctor_round[vht_el$rounds_q6_1 == 2] <- 0

# medical care

vht_elcb$machete_q6_2
table(vht_el$machete_q6_2, useNA = "always")

vht_el$medical_care_clinic <- (vht_el$machete_q6_2 == 1) * 1
vht_el$medical_care_hospital <- (vht_el$machete_q6_2 == 2) * 1
vht_el$medical_care_others <- (vht_el$machete_q6_2 == 3) * 1
vht_el$medical_care_vht <- (vht_el$machete_q6_2 == 4) * 1
vht_el$medical_care_nurse <- (vht_el$machete_q6_2 == 5) * 1


# proportion witch doctor

vht_elcb$witchdoctor_q6_4
vht_el$proportion_witch_doctor <- NA
vht_el$proportion_witch_doctor <- 4 - vht_el$witchdoctor_q6_4
vht_el$proportion_witch_doctor[vht_el$witchdoctor_q6_4 == 999] <-
	NA
