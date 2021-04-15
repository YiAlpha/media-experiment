

# sex_q2_1

# female
elcb$sex_q2_1
el$female <- NA
el$female[el$sex_q2_1 == 2] <- 1
el$female[el$sex_q2_1 == 1] <- 0


# day_q2_2

sumna(el$day_q2_2)
elcb$day_q2_2
el$day <- el$day_q2_2
el$day[el$day_q2_2 == 3] <- 0

# age_q2_3_c
el$age <- el$age_q2_3
el$age[!is.na(el$age_q2_3_c)] <-
	el$age_q2_3_c[!is.na(el$age_q2_3_c)]

# rel_q2_4
elcb$rel_q2_4
el$household_head <- dummy_q("rel_q2_4", 1, data = el)
el$household_spouse <- dummy_q("rel_q2_4", 2, data = el)
el$household_other <- dummy_q("rel_q2_4", 3:12, data = el)

sumna(el$rel_q2_4)
table(is.na(el$rel_q2_4), el$resp_status)
# Merging in ml data
el$household_head[!is.na(el$household_head_ml)] <-
	el$household_head_ml[!is.na(el$household_head_ml)]

varname <- "household_head"

# status_q2_5

sumna(el$status_q2_5)

el$married <- dummy_q("status_q2_5", 1, data = el)
el$living_as_married <- dummy_q("status_q2_5", 2, data = el)
el$not_married <- dummy_q("status_q2_5", c(3, 7), data = el)
el$not_married[el$q2_5_oth == "Not married "] <- 1
el$separated <- dummy_q("status_q2_5", c(4, 5, 6), data = el)
el$separated[el$q2_5_oth == "Widower"] <- 1
el$married_or_living_as_married <-
	(el$married == 1 | el$living_as_married == 1) * 1
el$married_or_living_as_married[is.na(el$status_q2_5)] <- NA

# lc1_naba_vht_q2_11
sumna(el$lc1_naba_vht_q2_11)
elcb$lc1_naba_vht_q2_11
el$village_official <-
	dummy_q("lc1_naba_vht_q2_11", c(1, 3, 5, 7, 9), data = el)

# educ_q2_21
sumna(el$educ_q2_21)

elcb$educ_q2_21
el$highest_grade <- el$educ_q2_21
# Polytechnic -> 16
el$highest_grade[el$educ_q2_21 == 14] <- 16
# Informal training -> 14
el$highest_grade[el$educ_q2_21 == 15] <- 14
# Polytechnic -> 15
el$highest_grade[el$educ_q2_21 == 16] <- 15
# University -> 16
el$highest_grade[el$educ_q2_21 == 19] <- 16
# None -> 0
el$highest_grade[el$educ_q2_21 == 18] <- 0

# university
el$university <- as.numeric(el$highest_grade == 16)

# read_q2_22
sumna(el$read_q2_22)


# read_q2_22b
elcb$read_q2_22
elcb$read_q2_22b

el$read_write_int <- el$read_q2_22
el$read_write_int[!is.na(el$read_q2_22b)] <-
	el$read_q2_22b[!is.na(el$read_q2_22b)]

el$read_only <- dummy_q("read_write_int", 1, data = el)
el$write_and_read <- dummy_q("read_write_int", 2, data = el)
el$write_only <- dummy_q("read_write_int", 3, data = el)
el$illiterate <- dummy_q("read_write_int", 4, data = el)

# lang_q2_23

elcb$lang_q2_23

el$fumbira_lang <- dummy_q("lang_q2_23", c(26, 30), data = el)
el$luganda_lang <- dummy_q("lang_q2_23", c(2), data = el)
el$runyannkole_lang <- dummy_q("lang_q2_23", c(29), data = el)
el$minority_lang <-
	dummy_q("lang_q2_23", (1:36)[-c(26, 30, 2, 29)], data = el)

# Tribe
elcb$tribe_q2_25$answer

# recoding "other" responses:
el$tribe_q2_25[el$q2_25_oth == "Mululu"] <- 20

el$muganda_tribe <- dummy_q("tribe_q2_25", 1, data = el)
el$mufumbira_tribe <- dummy_q("tribe_q2_25", 14, data = el)
el$mukiga <- dummy_q("tribe_q2_25", 18, data = el)
el$munyankole <- dummy_q("tribe_q2_25", 21, data = el)
el$munyarwanda <- dummy_q("tribe_q2_25", 22, data = el)
el$munyoro <- dummy_q("tribe_q2_25", 24, data = el)
el$mutooro <- dummy_q("tribe_q2_25", 29, data = el)
el$minority_tribe <-
	dummy_q("tribe_q2_25", (1:35)[-c(1, 14, 18, 21, 22, 24, 29)], data = el)

# activity_q3_1
find_q("main activity")
elcb$activity_q3_1
table(factor_q("activity_q3_1"))
no.na(el$q3_1_oth)
el$agriculture_work <- dummy_q("activity_q3_1", c(1, 19, 3), data = el)
el$education_work <- dummy_q("activity_q3_1", c(16, 17), data = el)
el$domestic_work <- dummy_q("activity_q3_1", c(12), data = el)
el$retail_work <- dummy_q("activity_q3_1", c(7), data = el)
el$transport_work <- dummy_q("activity_q3_1", c(13), data = el)
el$manual_work <- dummy_q("activity_q3_1", c(4, 5, 6, 10, 11), data = el)
el$hospitality_work <- dummy_q("activity_q3_1", c(8, 9), data = el)
el$no_work <- dummy_q("activity_q3_1", c(8, 9), data = el)
# This should probably be recoded:
el$other_work <- dummy_q("activity_q3_1", c(15), data = el)

# frequency_discussion
find_q("how often do you")
elcb$frequency_discussion_q3_1
el$frequency_discussion <-  4 - el$frequency_discussion_q3_1


# granchildren_q3_4f

elcb$granchildren_q3_4

el$grandchildren <- el$granchildren_q3_4

# granchildren_boys_q3_5

el$grandchildren_boys <- el$granchildren_boys_q3_5
varname <- "granchildren_boys_q3_5"

dummy_out_q <- function(varname) {
	factored <- factor_q(var_name = varname)
	new_vars <- gsub(pattern = "[[:punct:]]", "", levels(factored))
	factored <- gsub(pattern = "[[:punct:]]", "", factored)
	new_vars <- abbreviate(new_vars, minlength = 20)
	factored <- abbreviate(factored, minlength = 20)
	new_vars <- tolower(gsub(" ", "_", new_vars))
	factored <- tolower(gsub(" ", "_", factored))
	for (var in new_vars) {
		el[, var] <<- as.numeric(factored == var)
	}
}

dummy_out_q("granchildren_boys_q3_5")
# New variables created:
# "grandchildren"
# "grandchildren_boys"
# "grandchildren_girls"
# "all_boys"
# "all_girls"
# "more_boys_than_girls"
# "more_girls_than_boys"
# "roughly_equal"
# "idntcraslngasonisaby"
# "idntcraslngasonisagr"
# "it_doesnt_matter"


# mobile_q4_2

elcb$mobile_q4_2
el$mobile_phone_use <- el$mobile_q4_2 - 1
el$phone_every_day <- (el$mobile_q4_2 == 5) * 1
el$phone_every_day[is.na(el$mobile_q4_2)] <- NA



# Political family connections

elcb$doctor_q5_1a

actors <- c("doctor",
						"teacher",
						"principal",
						"official",
						"judge",
						"police",
						"clergy")

versions <- c("a", "b", "c", "d", "e", "f")


for (i in 1:length(actors)) {
	el[, actors[i]] <- NA
	
	for (j in 1:length(versions)) {
		variablename <- paste0(actors[i], "_q5_1", versions[j])
		el[, actors[i]][el[, variablename] == 1] <- 1
		el[, actors[i]][el[, variablename] == 2] <- 0
	}
}

# Religion

elcb$religion_q14_1

el$christian_only <- dummy_q("religion_q14_1", 2, data = el)
el$catholic <- dummy_q("religion_q14_1", 3, data = el)
el$english_christian <- dummy_q("religion_q14_1", c(5, 6), data = el)
el$holy_spirit <- dummy_q("religion_q14_1", 7:10, data = el)
el$muslim <- dummy_q("religion_q14_1", 11, data = el)
el$atheist <- dummy_q("religion_q14_1", c(1, 15, 16), data = el)

el$english_christian[el$q14_1_oth %in% c("Aglican", "Anglican")] <-
	1

el$minority_religion <-
	as.numeric(
		grepl(pattern = "unity|bisaka|Mungu", x = el$q14_1_oth, T) |
			el$religion_q14_1 %in% c(13, 14)
	)

el$minority_religion[(is.na(el$religion_q14_1) &
												is.na(el$q14_1_oth))] <- NA

# Religious or not
ml$religious <- NA
ml$religious[(!ml$religion_q14_1 %in% c(1, 15, 16)) &
						 	!is.na(ml$religion_q14_1)] <- 1
ml$religious[(ml$religion_q14_1 %in% c(1, 15, 16))] <- 0


# Religious Service
elcb$religion_times_q14_2
el$religious_service <- el$religion_times_q14_2
el$religious_service[el$religion_times_q14_2 == -1] <-
	1 # double check with Cristina whether this is NA or 1


# Cooperative
elcb$cooperative_q20_1
el$cooperative <- 4 - el$cooperative_q20_1

# Other_person
elcb$person_q18_1
el$other_person <- (el$person_q18_1 == 1) * 1
el$other_person[is.na(el$person_q18_1)] <- NA


# living_conditions_compared

elcb$observe_conditions_q18_4
el$living_conditions_compared <- el$observe_conditions_q18_4 - 1
el$living_conditions_compared[el$observe_conditions_q18_4 == 6] <-
	NA


# Survey language
elcb$survey_language

el$survey_luganda <- dummy_q("survey_language", 1, data = el)
el$survey_runyannkole <- dummy_q("survey_language", 2, data = el)
el$survey_runyarwanda <- dummy_q("survey_language", 3, data = el)
el$survey_english <- dummy_q("survey_language", 4, data = el)



# Merging the data from ml when we didn’t ask in el ---------------------


# Find covariates from ml that are in el at all
covariates_in_el <- covariates[which(covariates %in% names(el))]
# Find the covariates that have more than 1000 missing values, i.e. that
# weren't asked to compliers
asked_of_compliers <- sapply(
	X = covariates_in_el,
	FUN = function(x) {
		sumna(el[, x])
	}
) > 1000

covariates[!covariates %in% names(ml)]

mergeable <- covariates_in_el[asked_of_compliers]

paste0(mergeable, "_ml")[!paste0(mergeable, "_ml") %in% names(el)]

# Merge in
sapply(mergeable, merge_ml)

rm(covariates_in_el, merge_ml, asked_of_compliers, mergeable)




# Dummy out blocks --------------------------------------------------------


factored <- as.factor(el$block_id)
new_vars <-
	paste0("block_", gsub(pattern = "[[:punct:]]", "", levels(factored)))
levels(factored) <- paste0("block_", levels(factored))
for (var in new_vars) {
	el[, var] <- as.numeric(factored == var)
}

block_vars <- new_vars

rm(new_vars, factored)
