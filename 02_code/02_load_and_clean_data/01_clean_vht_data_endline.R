time_stamps  <-
	names(vht_el)[grepl(pattern = "\\btime_", x = names(vht_el))]
bookends <- c("starttime", "endtime", "date_time")

for (variable in time_stamps) {
	vht_el[variable] <-
		as.POSIXct(vht_el[, variable], format = "%Y-%b-%d %I:%M:%S", tz = "Etc/GMT+3")
	vht_el[variable] <-
		as.POSIXct(vht_el[, variable], format = "%Y-%b-%d %I:%M:%S", tz = "Etc/GMT+3")
}
for (variable in bookends) {
	vht_el[, variable] <-
		as.POSIXct(vht_el[, variable], format = "%b %d, %Y %I:%M:%S %p", tz = "Etc/GMT+3")
	vht_el[, variable] <-
		as.POSIXct(vht_el[, variable], format = "%b %d, %Y %I:%M:%S %p", tz = "Etc/GMT+3")
}

rm(bookends, time_stamps, variable)


# Fixing Mistakes ---------------------------------------------------------


vht_el$number_q2_7[vht_el$unit_q2_7 == 2 &
									 	vht_el$number_q2_7 == 30] <- NA
vht_el$number_q2_7[vht_el$unit_q2_7 == 2 &
									 	vht_el$number_q2_7 == 60] <- NA
vht_el$number_q2_7b[vht_el$unit_q2_7b == 2 &
											vht_el$number_q2_7b == 30] <- NA
vht_el$number_q2_7b[vht_el$unit_q2_7b == 2 &
											vht_el$number_q2_7b == 60] <- NA
