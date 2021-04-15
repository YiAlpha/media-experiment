# Impute covariates -------------------------------------------------------

covariate_frame <- ml[, covariates]

covariate_imp <- mice(data = covariate_frame, m = 1, seed = 1234567)
covariate_imp <- mice::complete(covariate_imp)

ml[, covariates] <- covariate_imp[, covariates]

# Do it again

covariate_frame <- ml[, covariates]

covariate_imp <- mice(data = covariate_frame, m = 1, seed = 1234567)
covariate_imp <- mice::complete(covariate_imp)

ml[, covariates] <- covariate_imp[, covariates]

rm(covariate_frame, covariate_imp)

# Impute outcomes ---------------------------------------------------------

# IPV

IPV_frame <- ml[, IPV_outcomes]

IPV_imp <- mice(data = IPV_frame, m = 1, seed = 1234567)
IPV_imp <- mice::complete(IPV_imp, action = 1)

ml[, IPV_outcomes] <- IPV_imp[, IPV_outcomes]

rm(IPV_frame, IPV_imp)

# additional outcomes

additional_outcomes_frame <- ml[,additional_outcomes_ml]

additional_outcomes_imp <- mice(data = additional_outcomes_frame,m = 1,seed = 1234567)
additional_outcomes_imp <- mice::complete(additional_outcomes_imp)

ml[,additional_outcomes_ml] <- additional_outcomes_imp[,additional_outcomes_ml]

rm(additional_outcomes_frame,additional_outcomes_imp)
