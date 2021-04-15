# Adult IPV outcomes ------------------------------------------------------

# Remove any variables from imputation where too many values are missing:
cannot_be_imputed <- c(
  "has_reported_nabakyala",
  "has_reported_police",
  "has_reported_lc1",
  "chatting",
  "financial_pressure",
  "housework",
  "housework_comm",
  "fin_press_comm",
  "chat_comm", 
  "household_violence",
  "household_violence_frequency"
)

IPV_outcomes_el <- c(
  IPV_outcomes_old[!IPV_outcomes_old %in% cannot_be_imputed],
  IPV_outcomes_mediators_moderators_new[!IPV_outcomes_mediators_moderators_new %in% cannot_be_imputed]
)

# We can only impute these where we have both ml and el
IPV_outcomes_ml <- c(
  paste0(
    IPV_outcomes_old[!IPV_outcomes_old %in% cannot_be_imputed],
    "_ml"
    )
)

# First, impute variables for compliers all together

temporary_data <- mice(data = el[el$respondent_category == "Complier",c(IPV_outcomes_el,IPV_outcomes_ml)],m = 1,seed = 1234567)


temporary_data <- mice::complete(temporary_data)

el[el$respondent_category == "Complier",c(IPV_outcomes_el,IPV_outcomes_ml)] <- temporary_data[,c(IPV_outcomes_el,IPV_outcomes_ml)]
stopifnot(all(apply(el[el$respondent_category == "Complier",c(IPV_outcomes_el,IPV_outcomes_ml)],2,function(x)sum(is.na(x))) == 0))

# Now, impute just the el variables for all other respondents

temporary_data <- mice(data = el[,IPV_outcomes_el],m = 1,seed = 1234567)
temporary_data <- mice::complete(temporary_data)

el[,IPV_outcomes_el] <- temporary_data[,IPV_outcomes_el]
stopifnot(all(apply(el[,IPV_outcomes_el],2,function(x)sum(is.na(x))) == 0))

# imputing violence outcomes among women only

temporary_data <-
  mice(data = el[el$female == 1, c("household_violence",
                                   "household_violence_frequency",IPV_outcomes_el, IPV_outcomes_ml)], m = 1, seed = 1234567)
temporary_data <- mice::complete(temporary_data)

el[el$female == 1, c("household_violence",
                     "household_violence_frequency")] <-
  temporary_data[, c("household_violence",
                     "household_violence_frequency")]

IPV_outcomes_el <- c(IPV_outcomes_el,
                      "household_violence",
                      "household_violence_frequency")

rm(temporary_data,cannot_be_imputed)

# Adult covariates --------------------------------------------------------

cannot_be_imputed <- c(
  "minority_religion"
)

covariates_el <- c(
  notin(covariates_old,cannot_be_imputed),notin(covariates_new,cannot_be_imputed)
)

covariates_ml <- c(
  paste0(notin(covariates_old,cannot_be_imputed),"_ml")
)


temporary_data <- mice(data = el[el$respondent_category == "Complier",c(covariates_el,covariates_ml)],m = 1,seed = 1234567)
temporary_data <- mice::complete(temporary_data)

el[el$respondent_category == "Complier",c(covariates_el,covariates_ml)] <- temporary_data[,c(covariates_el,covariates_ml)]
# stopifnot(all(apply(el[el$respondent_category == "Complier",c(covariates_el,covariates_ml)],2,function(x)sum(is.na(x))) == 0))



temporary_data <- mice(data = el[,covariates_el],m = 1,seed = 1234567)
temporary_data <- mice::complete(temporary_data)

el[,covariates_el] <- temporary_data[,covariates_el]

stopifnot(all(
  apply(el[,covariates_el],2,function(x)sum(is.na(x))) == 0)
  )


# Additional Outcomes -----------------------------------------------------

additional_outcomes_ml <- paste0(additional_outcomes_ml, "_ml")

temporary_data <- mice(data = el[el$respondent_category == "Complier",c(additional_outcomes_el,additional_outcomes_ml)],m = 1,seed = 1234567)
temporary_data <- mice::complete(temporary_data)

el[el$respondent_category == "Complier",c(additional_outcomes_el,additional_outcomes_ml)] <- temporary_data[,c(additional_outcomes_el,additional_outcomes_ml)]
stopifnot(all(apply(el[el$respondent_category == "Complier",c(additional_outcomes_el,additional_outcomes_ml)],2,function(x)sum(is.na(x))) == 0))

temporary_data <- mice(data = el[,additional_outcomes_el],m = 1,seed = 1234567)
temporary_data <- mice::complete(temporary_data)

el[,additional_outcomes_el] <- temporary_data[,additional_outcomes_el]

stopifnot(all(
  apply(el[,additional_outcomes_el],2,function(x)sum(is.na(x))) == 0)
)


