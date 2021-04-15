# Function for quickly finding questions in codebook ----------------------

find_q <- function(wording, codebook = mlcb) {
	truefalse <- sapply(codebook, function(x)
		grepl(wording, x$question, T))
	names(codebook)[truefalse]
}

# Function for making factors from codebook -------------------------------


factor_q <- function(var_name,
										 data = el,
										 codebook = elcb) {
	if (!is.data.frame(codebook[var_name][[1]]$answer)) {
		return(data[, var_name])
	} else {
		return(
			factor(
				data[, var_name],
				levels = codebook[var_name][[1]]$answer$integer,
				labels = codebook[var_name][[1]]$answer$label
			)
		)
	}
}

# Function for dummying out variables -------------------------------------

dummy_q <- function(var_name, integers, data = ml) {
	variable <- data[, var_name]
	NAs <- is.na(variable)
	new_var <- as.numeric(variable %in% integers)
	new_var[NAs] <- NA
	return(new_var)
}


# Function for looking at object without NAs ------------------------------

no.na <- function(x)
	x[!is.na(x)]

sumna <- function(x)
	sum(is.na(x))

# Function to merge in ml covariates etc. --------------------------------

merge_ml <- function(varname) {
	# Get the ml varname
	ml_varname <- paste0(varname, "_ml")
	# Figure out what obs can be merged in
	to_merge <- !is.na(el[, ml_varname])
	# Check if there is any risk of overwriting new data
	will_overwrite <- any(!is.na(el[to_merge, varname]))
	if (will_overwrite) {
		warning(
			paste0(
				"You are overwriting ",
				sum(!is.na(el[to_merge, varname])),
				" observations from the el data with observations from the ml data."
			)
		)
	}
	# In the global environment (<<-) put the ml data into the el variable
	el[to_merge, varname] <<- el[to_merge, ml_varname]
}


# Quick function for finding complement -----------------------------------

notin <- function(x, y) {
	x[!x %in% y]
}


# Function for printing vector with no extra details ----------------------

minprint <- function(x)
	cat(paste(x, collapse = "\n"))


# Handy function for variable names ---------------------------------------

texttt <- function(x) {
	gsub(
		pattern = "_",
		replacement = "\\\\_",
		x = paste0('{\\texttt{', x, '}}')
	)
}
