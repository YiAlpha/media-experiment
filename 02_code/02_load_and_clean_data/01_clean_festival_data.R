# Get names of all counts of visits
attendance_count_vars <-
	names(festival)[grepl(pattern = "n_",
												x = names(festival),
												ignore.case = F) &
										grepl(pattern = "_end",
													x = names(festival),
													ignore.case = F)]

# Get rid of rogue comment that makes this integer a character:
festival$n_men_end4[grepl(pattern = "councillor",
													x = festival$n_men_end4,
													ignore.case = TRUE)] <- NA
festival$n_men_end4 <- as.integer(as.character(festival$n_men_end4))
# Remove variables that include children and teenagers
attendance_count_vars <-
	attendance_count_vars[!grepl("children|teenager", attendance_count_vars)]

# Get total vists by men and women at all six films for every cluster
attendance <- festival[, attendance_count_vars]

# Coerce answers to integer for summation, some will be NAs
attendance <- as.data.frame(apply(attendance, 2, as.integer))

# Sum values for men and women, converting to NA when either men or women
attendance <- within(attendance, {
	n_end1 = n_men_end1 + n_women_end1
	n_end2 = n_men_end2 + n_women_end2
	n_end3 = n_men_end3 + n_women_end3
	n_end4 = n_men_end4 + n_women_end4
	n_end5 = n_men_end5 + n_women_end5
	n_end6 = n_men_end6 + n_women_end6
})

# Take mean of all festivals, ignoring ones where men or women counts
# were missing
festival$n_end_mean <- rowMeans(attendance[, c("n_end1",
																							 "n_end2",
																							 "n_end3",
																							 "n_end4",
																							 "n_end5",
																							 "n_end6")], na.rm = TRUE)


# Left join festival data and random sampling data

festival <- left_join(x = festival, y = with(random_sampling_ml ,data.frame(tc_id = tc_id, N_hh_listed = num_draws)),by = "tc_id")


# Recode some numbers that we are missing based on info from field

# Numbers that got lost from mobilization data: 
# Busumbi: 80
# Buyambi: 76
# Mwera: 79
# Kyengerere: 76

festival$N_hh_listed[festival$tc_id == 5] <- 80
festival$N_hh_listed[festival$tc_id == 97] <- 76
festival$N_hh_listed[festival$tc_id== 35] <- 79
festival$N_hh_listed[festival$tc_id==24] <- 76

# Merge in block ID from random assigment
festival <- merge(x = festival, y = treatment_assignment[,c("tc_id", "block_id")], by = "tc_id")

# Merge with radius data
festival <- left_join(festival,with(sampling_radius,data.frame(tc_id = tc_id, radius = radius)),by = c("tc_id"))

# Create total visits
festival$total_visits <- rowSums(festival[,attendance_count_vars], na.rm = T)

# Use total visits to impute missing values in N_hh_listeds

predictor <- lm(N_hh_listed ~ total_visits + as.factor(block_id),festival)
festival$predictions <- predict(predictor,newdata = festival)
festival$N_hh_listed[is.na(festival$N_hh_listed)] <- festival$predictions[is.na(festival$N_hh_listed)]

# Remove objects
rm(attendance, attendance_count_vars, predictor)


