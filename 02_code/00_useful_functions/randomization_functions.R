# Same function used to assign treatment
assign_treatment <- function(block_vector, condition_names, unit_ID) {
	# Find unique blocks
	unique_blocks <- unique(block_vector)
	
	# Check condition_names length == number within blocks
	block_lengths <- table(block_vector)
	if (any(block_lengths != length(condition_names)))
		stop("There must be as many condition names as units within each block.")
	
	# Create assignment vector
	assignment_vector <- rep(NA, length(block_vector))
	
	# Permute treatment within blocks
	for (block_j in unique_blocks) {
		assignment_vector[block_vector == block_j] <-
			sample(condition_names)
	}
	
	# Create data.frame without backup units
	unit_data <- data.frame(tc_id = unit_ID,
													block_vector = block_vector,
													treatment = assignment_vector)
	
	return(unit_data)
	
}

# Takes default treatment variable and creates dummies
dummy_treatment <- function(treatment) {
	data.frame(
		IPV = as.numeric(grepl(
			pattern = "IPV",
			x = treatment,
			ignore.case = TRUE
		)),
		abortion = as.numeric(grepl(
			pattern = "abortion",
			x = treatment,
			ignore.case = TRUE
		)),
		absenteeism = as.numeric(
			grepl(
				pattern = "absenteeism",
				x = treatment,
				ignore.case = TRUE
			)
		),
		placebo = as.numeric(grepl(
			pattern = "placebo",
			x = treatment,
			ignore.case = TRUE
		))
	)
}


# This function re-assigns treatment at the cluster-level (using tc_level_data),
# and merges it with the analysis data (ml by default)
rerandomize <- function(tc_level_data = treatment_assignment,
												analysis_data = el) {
	treatment_vars <-
		c("IPV", "abortion", "absenteeism", "placebo", "treatment")
	
	analysis_data[, treatment_vars] <- NULL
	
	assignment <- assign_treatment(
		block_vector = tc_level_data$block_id,
		condition_names = c(
			"placebo",
			"IPV",
			"abortion",
			"absenteeism",
			"IPV_abortion",
			"IPV_absenteeism",
			"abortion_absenteeism"
		),
		unit_ID = tc_level_data$tc_id
	)
	
	assignment <-
		cbind(assignment,
					dummy_treatment(treatment = assignment$treatment))
	
	
	
	new_data <- merge(x = analysis_data,
										y = assignment[, c("tc_id", treatment_vars)],
										by = "tc_id")
	
	return(new_data)
	
}
