# Load packages -----------------------------------------------------------

packages <- c(
	"magrittr",
	"ggplot2",
	"mice",
	"foreign",
	"knitr",
	"multiwayvcov",
	"stargazer",
	"lmtest",
	"glmnet",
	"xtable",
	"ggrepel",
	"MASS",
	"rms",
	"dplyr",
	"data.table",
	'sandwich'
)

 
package_installed <-
	sapply(packages, function(pack)
		pack %in% rownames(installed.packages()))

if (any(!package_installed)) {
	sapply(packages[!package_installed], install.packages)
}

sapply(packages, require, character.only = TRUE)

rm(packages, package_installed)

# Read in TC level data ---------------------------------------------------

# Film Festival Data
festival <-
	read.csv("01_data/cluster_level_data/film_festival.csv",
					 stringsAsFactors = F)

# Random Assignment Data
treatment_assignment <-
	read.csv("01_data/cluster_level_data/treatment_assignment.csv",
					 stringsAsFactors = F)

# Random Sampling Radius Areas
sampling_radius <-
	read.csv("01_data/cluster_level_data/sampling_radius.csv",
					 stringsAsFactors = F)

# Data on Locations of Clusters
location_data <-
	read.csv("01_data/cluster_level_data/location_data.csv",
					 stringsAsFactors = F)

# Data on Random Sampling of Households
random_sampling_1 <-
	read.csv("01_data/household/random_sampling_1.csv",
					 stringsAsFactors = F)
random_sampling_2 <-
	read.csv("01_data/household/random_sampling_2.csv",
					 stringsAsFactors = F)

# Data on distance of respondents to video halls
distance_data <- read.csv('01_data/household/distance_data.csv',
													stringsAsFactors = FALSE)

# Read in Household Data --------------------------------------------------

# Midline
ml <- lapply(
	X = c(
		"midline_1.csv",
		"midline_2.csv",
		"midline_3.csv",
		"midline_3_1.csv",
		"midline_3_2.csv",
		"midline_4.csv",
		"midline_5.csv"
	),
	FUN = function(file_name) {
		read.csv(
			file = paste0("01_data/household/",
										file_name),
			stringsAsFactors = FALSE,
			na.strings = c("999", "888", "", " ", "NA")
		)
	}
	
)

# These are variables that are not present in the datasets
missing_vars <- lapply(ml,
											 function(x) {
											 	unique(unlist(lapply(ml, function(y)
											 		names(y)[!names(y) %in% names(x)])))
											 })

# Put any missing variables in
for (i in 1:length(ml)) {
	ml[[i]][, missing_vars[[i]]] <- NA
}

#  Order columns according to the order of the first file
var_names <- names(ml[[1]])
for (i in 1:length(ml)) {
	ml[[i]] <- ml[[i]][, var_names]
}

# Sanity check:
order_check <- all(unlist(lapply(ml,
																 function(x) {
																 	all(unlist(lapply(ml, function(y)
																 		names(y) == names(x))))
																 })))
# If this breaks, something went wrong with the above
stopifnot(order_check)

# rbind the data into a single data.frame
ml <- do.call(what = rbind.data.frame, args = ml)

# The variable types look as follows:
table(sapply(ml, class))

rm(var_names, order_check, missing_vars)

# Endline

el <- lapply(
	X = c(
		"endline_1.csv",
		"endline_2.csv",
		"endline_3.csv",
		"endline_4.csv"
	),
	FUN = function(file_name) {
		read.csv(
			file = paste0("01_data/household/",
										file_name),
			stringsAsFactors = FALSE,
			na.strings = c("999", "888", "", " ", "NA")
		)
	}
)


# Fixing mistakes in survey that were changed during data collection

el_1_original <- el[[1]]

el[[1]] <- within(data = el[[1]],
									expr = {
										official_q5_1c <- el_1_original$teacher_q5_1c
										judge_q5_1c <- el_1_original$police_q5_1c
										doctor_q5_1c <- el_1_original$clergy_q5_1c
										teacher_q5_1c <- el_1_original$principal_q5_1c
										police_q5_1c <- el_1_original$official_q5_1c
										clergy_q5_1c <- el_1_original$judge_q5_1c
										principal_q5_1c <- el_1_original$doctor_q5_1c
										
										official_q5_1d <- el_1_original$clergy_q5_1d
										judge_q5_1d <- el_1_original$principal_q5_1d
										police_q5_1d <- el_1_original$official_q5_1d
										clergy_q5_1d <- el_1_original$judge_q5_1d
										principal_q5_1d <- el_1_original$police_q5_1d
										
										official_q5_1e <- el_1_original$judge_q5_1e
										judge_q5_1e <- el_1_original$doctor_q5_1e
										doctor_q5_1e <- el_1_original$teacher_q5_1e
										teacher_q5_1e <- el_1_original$police_q5_1e
										police_q5_1e <- el_1_original$clergy_q5_1e
										clergy_q5_1e <- el_1_original$principal_q5_1e
										principal_q5_1e <- el_1_original$official_q5_1e
										
										official_q5_1f <- el_1_original$judge_q5_1f
										judge_q5_1f <- el_1_original$doctor_q5_1f
										doctor_q5_1f <- el_1_original$clergy_q5_1f
										teacher_q5_1f <- el_1_original$principal_q5_1f
										police_q5_1f <- el_1_original$teacher_q5_1f
										clergy_q5_1f <- el_1_original$police_q5_1f
										principal_q5_1f <- el_1_original$official_q5_1f
									})

el_2_original <- el[[2]]

el[[2]] <- within(data = el[[2]],
									expr = {
										official_q5_1c <- el_2_original$teacher_q5_1c
										judge_q5_1c <- el_2_original$police_q5_1c
										doctor_q5_1c <- el_2_original$clergy_q5_1c
										teacher_q5_1c <- el_2_original$principal_q5_1c
										police_q5_1c <- el_2_original$official_q5_1c
										clergy_q5_1c <- el_2_original$judge_q5_1c
										principal_q5_1c <- el_2_original$doctor_q5_1c
										
										official_q5_1d <- el_2_original$clergy_q5_1d
										judge_q5_1d <- el_2_original$principal_q5_1d
										police_q5_1d <- el_2_original$official_q5_1d
										clergy_q5_1d <- el_2_original$judge_q5_1d
										principal_q5_1d <- el_2_original$police_q5_1d
										
										official_q5_1e <- el_2_original$judge_q5_1e
										judge_q5_1e <- el_2_original$doctor_q5_1e
										doctor_q5_1e <- el_2_original$teacher_q5_1e
										teacher_q5_1e <- el_2_original$police_q5_1e
										police_q5_1e <- el_2_original$clergy_q5_1e
										clergy_q5_1e <- el_2_original$principal_q5_1e
										principal_q5_1e <- el_2_original$official_q5_1e
										
										official_q5_1f <- el_2_original$judge_q5_1f
										judge_q5_1f <- el_2_original$doctor_q5_1f
										doctor_q5_1f <- el_2_original$clergy_q5_1f
										teacher_q5_1f <- el_2_original$principal_q5_1f
										police_q5_1f <- el_2_original$teacher_q5_1f
										clergy_q5_1f <- el_2_original$police_q5_1f
										principal_q5_1f <- el_2_original$official_q5_1f
									})

# These are variables that are not present in the datasets
missing_vars <- lapply(el,
											 function(x) {
											 	unique(unlist(lapply(el, function(y)
											 		names(y)[!names(y) %in% names(x)])))
											 })

# Put any missing variables in
for (i in 1:length(el)) {
	el[[i]][, missing_vars[[i]]] <- NA
}

#  Order columns according to the order of the first file
var_names <- names(el[[1]])
for (i in 1:length(el)) {
	el[[i]] <- el[[i]][, var_names]
}

# Sanity check:
order_check <- all(unlist(lapply(el,
																 function(x) {
																 	all(unlist(lapply(el, function(y)
																 		names(y) == names(x))))
																 })))
# If this breaks, something went wrong with the above
stopifnot(order_check)

# rbind the data into a single data.frame
el <- do.call(what = rbind.data.frame, args = el)

# The variable types look as follows:
table(sapply(el, class))


rm(el_1_original,
	 el_2_original,
	 var_names,
	 order_check,
	 missing_vars)


# Read in VHT Data --------------------------------------------------------

# Midline
vht_ml <- lapply(
	X = c(
		"vht_ml_1.csv",
		"vht_ml_2.csv",
		"vht_ml_3.csv",
		"vht_ml_4.csv"
	),
	FUN = function(file_name) {
		read.csv(
			file = paste0("01_data/vht/",
										file_name),
			stringsAsFactors = FALSE,
			na.strings = c("999", "888", "", " ", "NA")
		)
	}
)

keeps <- c(
	"uuid:a052b4ae-397e-4e2a-9a85-58a9c07e6c14",
	"uuid:886c8bbf-e245-4576-a6c5-c1179e1d01c1",
	"uuid:43607c8d-2c76-4d61-a287-51c36fcba856",
	"uuid:f219c1f9-852f-4b94-966d-caa2943ef1da",
	"uuid:b5cb13cf-d93c-4d6f-b94c-a8014d3b810f"
)

vht_ml[[2]] <- dplyr::filter(vht_ml[[2]], KEY %in% keeps)

# Date variables have different formats for different files
# so are recoded before appending

bookends <- c("starttime", "endtime", "date_time")

for (variable in bookends) {
	for (i in 1:3) {
		vht_ml[[i]][, variable] <-
			as.POSIXct(vht_ml[[i]][, variable],
								 format = "%b %d, %Y %I:%M:%S %p",
								 tz = "Etc/GMT+3")
	}
}

for (variable in bookends) {
	vht_ml[[4]][, variable] <-
		as.POSIXct(vht_ml[[4]][, variable],
							 format = "%m/%d/%Y %H:%M",
							 tz = "Etc/GMT+3")
}

# These are questions that were taken out of the survey part way through
missing_vars <- lapply(vht_ml,
											 function(x) {
											 	unique(unlist(lapply(vht_ml, function(y)
											 		names(y)[!names(y) %in% names(x)])))
											 })

# Put any missing variables in
for (i in 1:length(vht_ml)) {
	vht_ml[[i]][, missing_vars[[i]]] <- NA
}

# Order columns according to the order of the first file
var_names <- names(vht_ml[[1]])
for (i in 1:length(vht_ml)) {
	vht_ml[[i]] <- vht_ml[[i]][, var_names]
}

# Sanity check:
order_check <- all(unlist(lapply(vht_ml,
																 function(x) {
																 	all(unlist(lapply(vht_ml, function(y)
																 		names(y) == names(x))))
																 })))
# If this breaks, something went wrong with the above
stopifnot(order_check)

# rbind the data into a single data.frame
vht_ml <- do.call(what = rbind.data.frame, args = vht_ml)

# Endline VHT
vht_el <- read.csv("01_data/vht/vht_el_1.csv")

rm(var_names,
	 order_check,
	 missing_vars,
	 keeps,
	 bookends,
	 i,
	 variable)
