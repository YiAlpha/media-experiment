# Recode date variables
random_sampling_1[, "starttime"] <-
	as.POSIXct(random_sampling_1[, "starttime"],
						 format = "%b %d, %Y %I:%M:%S %p",
						 tz = "Etc/GMT+3")
random_sampling_2[, "starttime"] <-
	as.POSIXct(random_sampling_2[, "starttime"],
						 format = "%b %d, %Y %I:%M:%S %p",
						 tz = "Etc/GMT+3")

# The dataset used for randomly sampling households contains observations
# from the pilot, midline and endline. We only want those from the midline.
random_sampling_ml <-
	rbind(random_sampling_1[random_sampling_1$starttime > "2016-10-01",],
				random_sampling_2)

random_sampling_ml$tc_id <- random_sampling_ml$tc
random_sampling_ml$tc <- NULL


# Averaging for the two trading centers that show up twice
random_sampling_ml$num_draws[random_sampling_ml$tc_id == 42] <-
	mean(random_sampling_ml$num_draws[random_sampling_ml$tc_id == 42])

# Filtering out duplicate
random_sampling_ml <-
	subset(random_sampling_ml,!instanceID %in% random_sampling_ml$instanceID[random_sampling_ml$tc_id ==
																																					 	42][1])