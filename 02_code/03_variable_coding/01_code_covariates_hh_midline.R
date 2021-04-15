# Resample indicator -------------------------------------------------------

ml$resample <-
	as.numeric(as.Date(ml$date_time) >= as.Date(x = "2016-12-13"))

ml$resample[ml$id == 30933351] <- 0

# Compliance variable -----------------------------------------------------

#             Answer to 19.1)                       Answer to 19.2)
# complrs     1,2,3,4,5, or 6                       anything
# ind compl   0 but knew about screenings           Yes, friends and family
#             0 did not know about the screenings   Yes, friends
#             Don’t know                            Yes, family
#             Refuse to answer
# appr. nt    0 but knew about screenings           No
#                                                   Don't know
#                                                   refuse
# never-takr  0 did not know about screenings       No
#             Don't know                            Don't know
#             Refuse to answer                      Refuse

mlcb$films_q17_1
mlcb$other_attend_q17_2

ml$compliance[ml$films_q17_1 %in% 1:6] <- 1
ml$compliance[!(ml$films_q17_1 %in% 1:6) &
								ml$other_attend_q17_2 %in% 1:3] <- 2
ml$compliance[ml$films_q17_1 == 7 &
								(ml$other_attend_q17_2 == 4 |
								 	is.na(ml$other_attend_q17_2))] <- 3
ml$compliance[ml$films_q17_1 == 8 &
								(ml$other_attend_q17_2 == 4 |
								 	is.na(ml$other_attend_q17_2))] <- 4

ml$compliance_label[ml$compliance == 1] <- "Complier"
ml$compliance_label[ml$compliance == 2] <- "Indirect Complier"
ml$compliance_label[ml$compliance == 3] <- "Apprised Never-Taker"
ml$compliance_label[ml$compliance == 4] <- "Never-Taker"

# Non compliant trading center indicator:
ml$nctc <- ml$tc_id %in% c(79, 96)

# Covariates --------------------------------------------------------------

# Living standard parents
mlcb$cond_parents_q5_3
ml$living_standard <- NA
ml$living_standard[ml$cond_parents_q5_3 == 1] <- 0
ml$living_standard[ml$cond_parents_q5_3 == 3] <- 1
ml$living_standard[ml$cond_parents_q5_3 == 2] <- 2

# Living standard children
mlcb$cond_children_q5_4
ml$living_standard_children <- NA
ml$living_standard_children[ml$cond_children_q5_4 == 1] <- 0
ml$living_standard_children[ml$cond_children_q5_4 == 3] <- 1
ml$living_standard_children[ml$cond_children_q5_4 == 2] <- 2

# Religion
mlcb$religion_q14_1
ml$christian_only <- dummy_q("religion_q14_1", 2)
ml$catholic <- dummy_q("religion_q14_1", 3)
ml$protestant <- dummy_q("religion_q14_1", 4)
ml$english_christian <- dummy_q("religion_q14_1", c(5, 6))
ml$holy_spirit <- dummy_q("religion_q14_1", 7:10)
ml$muslim <- dummy_q("religion_q14_1", 11)
ml$atheist <- dummy_q("religion_q14_1", c(1, 15, 16))
ml$atheist[ml$q14_1_oth == "Has no religion but believes in God"] <-
	1
ml$minority_religion <-
	as.numeric(
		grepl(pattern = "unity|orthodox|bishaka|wobu|bisaka|owobo|obozi|abundance|apost|rusaka", x = ml$q14_1_oth, T) |
			ml$religion_q14_1 %in% c(13, 14)
	)
ml$minority_religion[(is.na(ml$religion_q14_1) &
												is.na(ml$q14_1_oth))] <- NA

# Religious or not
ml$religious <- NA
ml$religious[(!ml$religion_q14_1 %in% c(1, 15, 16)) &
						 	!is.na(ml$religion_q14_1)] <- 1
ml$religious[(ml$religion_q14_1 %in% c(1, 15, 16))] <- 0
ml$religious[ml$q14_1_oth == "No religion"] <- 0


# Religious Service
mlcb$religion_times_q14_2
ml$religious_service <- ml$religion_times_q14_2

# Pray in private
mlcb$pray_private_q14_4
ml$pray_at_least_once_day <-
	(ml$pray_private_q14_4 %in% c(1, 2)) * 1
ml$pray_at_least_once_day[is.na(ml$pray_private_q14_4)] <- NA

ml$pray_private <- NA
ml$pray_private[ml$pray_private_q14_4 == 9] <- 0
ml$pray_private[ml$pray_private_q14_4 == 8] <- 1
ml$pray_private[ml$pray_private_q14_4 == 7] <- 2
ml$pray_private[ml$pray_private_q14_4 == 6] <- 3
ml$pray_private[ml$pray_private_q14_4 == 5] <- 4
ml$pray_private[ml$pray_private_q14_4 == 10] <- 5
ml$pray_private[ml$pray_private_q14_4 == 4] <- 6
ml$pray_private[ml$pray_private_q14_4 == 3] <- 7
ml$pray_private[ml$pray_private_q14_4 == 2] <- 8
ml$pray_private[ml$pray_private_q14_4 == 1] <- 9

# Witchcraft
mlcb$killed_witch_q14_1
mlcb$possible_kill_q14_1b

ml$witchcraft <- NA
ml$witchcraft[ml$killed_witch_q14_1 == 2 &
								ml$possible_kill_q14_1b == 2] <- 0
ml$witchcraft[ml$killed_witch_q14_1 == 2 &
								ml$possible_kill_q14_1b == 1] <- 1
ml$witchcraft[ml$killed_witch_q14_1 == 1] <- 2

# Job Kampala
mlcb$move_son_q15_3
mlcb$move_daughter_q15_3

ml$job_kampala <- NA
ml$job_kampala[ml$move_son_q15_3 == 1 |
							 	ml$move_daughter_q15_3 == 1] <- 1
ml$job_kampala[ml$move_son_q15_3 == 2 |
							 	ml$move_daughter_q15_3 == 2] <- 0


# Dwelling - not clear how to code this!

mlcb$dwelling_q16_1
no.na(ml$q16_1_oth)
ml$several_huts <- dummy_q("dwelling_q16_1", 1)
ml$single_hut <- dummy_q("dwelling_q16_1", 2)
ml$single_hut[grepl("muzigo|rental|mizigo", ml$q16_1_oth, T)] <- 1
ml$share_house <- dummy_q("dwelling_q16_1", 3)
ml$share_house[grepl("guest", ml$q16_1_oth, T)] <- 1

# Rooms
mlcb$rooms_q16_2
ml$rooms <- ml$rooms_q16_2
ml$rooms[ml$rooms_q16_2 == 99999] <- NA

# Walls
mlcb$walls_q16_5
no.na(ml$q16_5_oth)
ml$mud_wall <- dummy_q("walls_q16_5", c(1, 2, 3))
ml$stone_wall <- dummy_q("walls_q16_5", c(6, 4))
ml$brick_wall <- dummy_q("walls_q16_5", c(5))
ml$cement_wall <- dummy_q("walls_q16_5", c(7))
ml$misc_wall <- dummy_q("walls_q16_5", c(11, 10, 9, 8))

# Floor
mlcb$floor_q16_6
no.na(ml$q16_6_oth)
ml$misc_floor <- dummy_q("floor_q16_6", c(2, 3, 4, 5, 6, 8, 9))
ml$cement_floor <- dummy_q("floor_q16_6", 7)
ml$earth_floor <- dummy_q("floor_q16_6", 1)

# Lighting
no.na(ml$q16_7_oth)
mlcb$light_q16_7
table(factor_q("light_q16_7", data = ml))
ml$kerosene_light <- dummy_q("light_q16_7", 1)
ml$misc_light <- dummy_q("light_q16_7", c(2, 3, 4, 8, 9, 19, 11))
ml$solar_light <- dummy_q("light_q16_7", c(5))
ml$electric_light <- dummy_q("light_q16_7", c(7))

# Cooking
mlcb$fuel_q16_8
no.na(ml$q16_8_oth)
ml$misc_fuel <- dummy_q("fuel_q16_8", c(1, 4, 5, 6, 7, 8, 9))
ml$charcoal_fuel <- dummy_q("fuel_q16_8", c(2))
ml$firewood_fuel <- dummy_q("fuel_q16_8", c(3))


# Radio
mlcb$radio_q17_1
ml$radio <- (ml$radio_q17_1 == 1) * 1
ml$radio[is.na(ml$radio_q17_1)] <- NA

# TV_Set

mlcb$tv_q17_2
ml$tv <- (ml$tv_q17_2 == 1) * 1
ml$tv[is.na(ml$tv_q17_2)] <- NA


# cell_phone

mlcb$cellphone_q17_5
ml$cellphone <- (ml$cellphone_q17_5 == 1) * 1
ml$cellphone[is.na(ml$cellphone_q17_5)] <- NA

# chair

mlcb$chair_q17_7
ml$chair <- (ml$chair_q17_7 == 1) * 1
ml$chair[is.na(ml$chair_q17_7)] <- NA

# sofa

mlcb$sofa_q17_8
ml$sofa <- (ml$sofa_q17_8 == 1) * 1
ml$sofa[is.na(ml$sofa_q17_8)] <- NA


# motor cycle

mlcb$motor_cycle_q17_15
ml$motor_cycle <- (ml$motor_cycle_q17_15 == 1) * 1
ml$motor_cycle[is.na(ml$motor_cycle_q17_15)] <- NA


# other_person
mlcb$person_q18_1
ml$other_person <- (ml$person_q18_1 == 1) * 1
ml$other_person[is.na(ml$person_q18_1)] <- NA



# living_conditions_compared

mlcb$observe_conditions_q18_4
ml$living_conditions_compared <- ml$observe_conditions_q18_4 - 1
ml$living_conditions_compared[ml$observe_conditions_q18_4 == 6] <-
	NA


# men_beaten
mlcb$beaten_comm_q16_1
mlcb$woman_beaten_q16_1b

ml$community_beaten <- ml$beaten_comm_q16_1
ml$community_beaten[ml$beaten_comm_q16_1 %in% c(-999, 888888)] <-
	NA
ml$men_beaten <- ml$community_beaten - ml$woman_beaten

# female
mlcb$sex_q2_1
ml$female <- NA
ml$female[ml$sex_q2_1 == 2] <- 1
ml$female[ml$sex_q2_1 == 1] <- 0

# day
mlcb$day_q2_2
ml$day <- ml$day_q2_2
ml$day[ml$day_q2_2 == 3] <- 0

# age
ml$age <- ml$age_q2_3

# relationship_household_head
mlcb$rel_q2_4
ml$household_head <- dummy_q("rel_q2_4", 1)
ml$household_spouse <- dummy_q("rel_q2_4", 2)
ml$household_other <- dummy_q("rel_q2_4", 3:12)

# marital_status
find_q("status")
mlcb$status_q2_5
no.na(ml$q2_5_oth)
ml$married <- dummy_q("status_q2_5", 1)
ml$living_as_married <- dummy_q("status_q2_5", 2)
ml$not_married <- dummy_q("status_q2_5", c(3, 7))
ml$not_married[ml$q2_5_oth == "Not married "] <- 1
ml$separated <- dummy_q("status_q2_5", c(4, 5, 6))
ml$separated[ml$q2_5_oth == "Widower"] <- 1
ml$married_or_living_as_married <-
	(ml$married == 1 | ml$living_as_married == 1) * 1
ml$married_or_living_as_married[is.na(ml$status_q2_5)] <- NA

# household_members
find_q("household")
mlcb$members_q2_6
ml$members <- ml$members_q2_6

# household_children
find_q("how many of them are children")
ml$household_children <- ml$child_q2_7
ml$household_children[ml$members == 1] <- 0

# household_older
find_q("older than you")
mlcb$older_q2_8
ml$household_older <- ml$older_q2_8
ml$household_older[ml$members == 1] <- 0

# household_younger
ml$household_younger <- ml$younger_q2_9
ml$household_younger[ml$members == 1] <- 0

# number_children
find_q("how many children")
ml$number_children <- ml$born_q2_10

# village_official
find_q("an LC1 chairperson")
mlcb$lc1_naba_vht_q2_11
ml$village_official <-
	dummy_q("lc1_naba_vht_q2_11", c(1, 3, 5, 7, 9))

# frequency_discussion
find_q("how often do you")
mlcb$discuss_q2_13
ml$frequency_discussion <-  4 - ml$discuss_q2_13

# travel_big_city
find_q("have you ever")
mlcb$big_city_q2_14
ml$travel_big_city <- 2 - ml$big_city_q2_14

# close_relatives
find_q("do you have any")
mlcb$kla_q2_14
ml$close_relatives <- 2 - ml$kla_q2_14

# same_village
find_q("16 years old")
mlcb$vill16_q2_15
ml$same_village <- 2 - ml$vill16_q2_15

# highest_grade
find_q("grade")
mlcb$educ_q2_21
ml$highest_grade <- ml$educ_q2_21
# Polytechnic -> 16
ml$highest_grade[ml$educ_q2_21 == 14] <- 16
# Informal training -> 14
ml$highest_grade[ml$educ_q2_21 == 15] <- 14
# Polytechnic -> 15
ml$highest_grade[ml$educ_q2_21 == 16] <- 15
# University -> 16
ml$highest_grade[ml$educ_q2_21 == 19] <- 16
# None -> 0
ml$highest_grade[ml$educ_q2_21 == 18] <- 0
ml$educ <- ml$highest_grade

# university
ml$university <- as.numeric(ml$highest_grade == 16)

# read_write
find_q("read and write")

mlcb$read_q2_22
mlcb$read_q2_22b

ml$read_write_int <- ml$read_q2_22
ml$read_write_int[!is.na(ml$read_q2_22b)] <-
	ml$read_q2_22b[!is.na(ml$read_q2_22b)]

ml$read_only <- dummy_q("read_write_int", 1)
ml$write_and_read <- dummy_q("read_write_int", 2)
ml$write_only <- dummy_q("read_write_int", 3)
ml$illiterate <- dummy_q("read_write_int", 4)

# main_language
mlcb$lang_q2_23

ml$fumbira_lang <- dummy_q("lang_q2_23", c(26, 30))
ml$luganda_lang <- dummy_q("lang_q2_23", c(2))
ml$runyannkole_lang <- dummy_q("lang_q2_23", c(29))
ml$minority_lang <- dummy_q("lang_q2_23", (1:36)[-c(26, 30, 2, 29)])

# tribe
mlcb$tribe_q2_25
ml$muganda_tribe <- dummy_q("tribe_q2_25", 1)
ml$mufumbira_tribe <- dummy_q("tribe_q2_25", 14)
ml$mukiga <- dummy_q("tribe_q2_25", 18)
ml$munyankole <- dummy_q("tribe_q2_25", 21)
ml$munyarwanda <- dummy_q("tribe_q2_25", 22)
ml$munyoro <- dummy_q("tribe_q2_25", 24)
ml$mutooro <- dummy_q("tribe_q2_25", 29)
ml$minority_tribe <-
	dummy_q("tribe_q2_25", (1:35)[-c(1, 14, 18, 21, 22, 24, 29)])

# main_activity
find_q("main activity")
mlcb$activity_q3_1
table(factor_q("activity_q3_1", data = ml))
no.na(ml$q3_1_oth)
ml$agriculture_work <- dummy_q("activity_q3_1", c(1, 19, 3))
ml$education_work <- dummy_q("activity_q3_1", c(16, 17))
ml$domestic_work <- dummy_q("activity_q3_1", c(12))
ml$retail_work <- dummy_q("activity_q3_1", c(7))
ml$transport_work <- dummy_q("activity_q3_1", c(13))
ml$manual_work <- dummy_q("activity_q3_1", c(4, 5, 6, 10, 11))
ml$hospitality_work <- dummy_q("activity_q3_1", c(8, 9))
ml$no_work <- dummy_q("activity_q3_1", c(14))
# This should probably be recoded:
ml$other_work <- dummy_q("activity_q3_1", c(15))


# mobile_phone
find_q("mobile")
mlcb$mobile_q4_2
ml$mobile_phone_use <- ml$mobile_q4_2 - 1
ml$phone_every_day <- (ml$mobile_q4_2 == 5) * 1
ml$phone_every_day[is.na(ml$mobile_q4_2)] <- NA

# living_conditions
find_q("living conditions")
mlcb$living_cond_q5_1
ml$living_conditions <- ml$living_cond_q5_1
ml$living_conditions[ml$living_conditions == 1] <- -1
ml$living_conditions[ml$living_conditions == 3] <- 0
ml$living_conditions[ml$living_conditions == 2] <- 1

# living_conditions_tribe
find_q("are their economic")
mlcb$living_tribe_q5_2
mlcb$living_tribe_q5_2b
ml$living_conditions_tribe <- ml$living_tribe_q5_2
ml$living_conditions_tribe[!is.na(ml$living_tribe_q5_2b)] <-
	ml$living_tribe_q5_2b[!is.na(ml$living_tribe_q5_2b)]
ml$living_conditions_tribe[ml$living_conditions_tribe == 1] <- -1
ml$living_conditions_tribe[ml$living_conditions_tribe == 3] <- 0
ml$living_conditions_tribe[ml$living_conditions_tribe == 2] <- 1


# News
mlcb$news_q4_1
ml$news_everyday <- (ml$news_q4_1 == 5) * 1
ml$news_everyday[is.na(ml$news_q4_1)] <- NA

# Education
mlcb$educ_q2_21
ml$seven_years_edu_or_less <-
	(ml$educ_q2_21 %in% c(1:7, 15, 18)) * 1
ml$seven_years_edu_or_less[is.na(ml$educ_q2_21)] <- NA

# ml survey_luganda variable
ml$survey_luganda <- dummy_q("survey_language", 1, data = ml)
