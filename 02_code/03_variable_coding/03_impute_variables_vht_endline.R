# Impute Outcomes ---------------------------------------------------------

# IPV

IPV_frame <- vht_el[, IPV_outcomes_vht]

IPV_imp <- mice(data = IPV_frame, m = 1, seed = 1234567)
IPV_imp <- mice::complete(IPV_imp, action = 1)

vht_el[, IPV_outcomes_vht] <- IPV_imp[, IPV_outcomes_vht]

rm(IPV_frame, IPV_imp)
