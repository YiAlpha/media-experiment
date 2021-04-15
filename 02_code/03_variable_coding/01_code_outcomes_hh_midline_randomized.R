# Function for Aggregating Behavioral Proclivities

get_dummy_ml <- function(new_var, old_vars, pos_level, neg_level) {
	for (i in 1:length(old_vars)) {
		# uses <<- to perform assignment in global environment:
		ml[, new_var][ml[, old_vars[i]] == pos_level] <<- 1
		ml[, new_var][ml[, old_vars[i]] == neg_level] <<- 0
	}
	return(data)
}


# Conative Attitudes ------------------------------------------------------

# IPV ---------------------------------------------------------------------

# The action in line with reporting is 1 for the first q,
# 2 for the second, 1 for the third, and 2 for the fourth
mlcb$action_q8_1a_1$answer[1, ]
mlcb$action_q8_1a_2$answer[1, ]
mlcb$action_q8_1a_3$answer[1, ]
mlcb$action_q8_1a_4$answer[1, ]
mlcb$action_q8_1b_1$answer[2, ]
mlcb$action_q8_1b_2$answer[2, ]
mlcb$action_q8_1b_3$answer[2, ]
mlcb$action_q8_1b_4$answer[2, ]
mlcb$action_q8_1c_1$answer[1, ]
mlcb$action_q8_1c_2$answer[1, ]
mlcb$action_q8_1c_3$answer[1, ]
mlcb$action_q8_1c_4$answer[1, ]
mlcb$action_q8_1d_1$answer[2, ]
mlcb$action_q8_1d_2$answer[2, ]
mlcb$action_q8_1d_3$answer[2, ]
mlcb$action_q8_1d_4$answer[2, ]

# Create blank variables
ipv_vars <- c("report_police",
							"involve_lc1",
							"involve_parents",
							"involve_nabakyala")
ml[, ipv_vars] <- NA

# mapply function through parameters
mapply(
	FUN = get_dummy_ml,
	new_var = ipv_vars,
	old_vars = list(
		c(
			"action_q8_1a_1",
			"action_q8_1a_2",
			"action_q8_1a_3",
			"action_q8_1a_4"
		),
		c(
			"action_q8_1b_1",
			"action_q8_1b_2",
			"action_q8_1b_3",
			"action_q8_1b_4"
		),
		c(
			"action_q8_1c_1",
			"action_q8_1c_2",
			"action_q8_1c_3",
			"action_q8_1c_4"
		),
		c(
			"action_q8_1d_1",
			"action_q8_1d_2",
			"action_q8_1d_3",
			"action_q8_1d_4"
		)
	),
	pos_level = c(1, 2, 1, 2),
	neg_level = c(2, 1, 2, 1)
)

rm(ipv_vars)

# Additional variables for repsondents who are lc1s
lc1s <- c("action_q8_1b_2_1",
					"action_q8_1b_2_2",
					"action_q8_1b_2_3",
					"action_q8_1b_2_4")

# Answer 2 is in line with reporting for LC1s
mlcb$action_q8_1b_2_2$answer[2, ]
mlcb$action_q8_1b_2_2$answer[2, ]
mlcb$action_q8_1b_2_3$answer[2, ]
mlcb$action_q8_1b_2_4$answer[2, ]

for (variable in lc1s) {
	ml$involve_lc1[ml[, variable] == 2] <- 1
	ml$involve_lc1[ml[, variable] == 1] <- 0
}

rm(lc1s)

# Additional variables for repsondents who are nabakyalas
nabas <- c("action_q8_1d_2_1",
					 "action_q8_1d_2_2",
					 "action_q8_1d_2_3",
					 "action_q8_1d_2_4")

# Answer 2 is in line with reporting for Nabakyalas
mlcb$action_q8_1d_2_1$answer[2, ]
mlcb$action_q8_1d_2_2$answer[2, ]
mlcb$action_q8_1d_2_3$answer[2, ]
mlcb$action_q8_1d_2_4$answer[2, ]

for (variable in nabas) {
	ml$involve_nabakyala[ml[, variable] == 2] <- 1
	ml$involve_nabakyala[ml[, variable] == 1] <- 0
}

rm(nabas)


rm(variable, get_dummy_ml)
