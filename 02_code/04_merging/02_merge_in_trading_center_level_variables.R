# Merge radius data and intervention data to gather tc level variables relevant for analysis
tc_data  <-
	merge(x = sampling_radius, y = festival[, c("n_end_mean", "tc_id")], by = "tc_id")
names(tc_data)[which(names(tc_data) %in% c("longitude", "latitude"))] <-
	c("tc_longitude", "tc_latitude")

# Merge trading center level variables into endline and vht data
el <- merge(el, tc_data, by = "tc_id")
ml <- merge(ml, tc_data, by = "tc_id")
ml_no_imp <- merge(ml_no_imp, tc_data, by = "tc_id")

vht_ml <- merge(vht_ml, tc_data, by = "tc_id")
vht_el <- merge(vht_el, tc_data, by = "tc_id")

# Merge treatment assignment into other data sets
festival <- merge(festival, 
									treatment_assignment[,c("tc_id", "treatment",
																					"IPV",
																					"abortion",
																					"absenteeism",
																					"placebo"  )], 
									by = "tc_id")
el <- merge(el, treatment_assignment, by = "tc_id")
ml <- merge(ml, treatment_assignment, by = "tc_id")
ml_no_imp <- merge(ml_no_imp, treatment_assignment, by = "tc_id")

vht_ml <- merge(vht_ml, treatment_assignment, by = "tc_id")
vht_el <- merge(vht_el, treatment_assignment, by = "tc_id")

# # Numbers that got lost from random_sampling data:
festival$N_hh_listed[festival$tc_id == 5] <- 80
festival$N_hh_listed[festival$tc_id == 97] <- 76
festival$N_hh_listed[festival$tc_id == 35] <- 79
festival$N_hh_listed[festival$tc_id == 24] <- 76


