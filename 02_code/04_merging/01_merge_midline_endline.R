IPV_outcomes <- c(
	"disobey",
	"ipv_attitude",
	"beating_frequency",
	"beating_more_than_once",
	"IPV_goal",
	"should_intervene",
	"ipv_efficacy",
	"IPV_discussion",
	"disobey_comm",
	"react_comm",
	"report_police",
	"involve_lc1",
	"involve_parents",
	"involve_nabakyala",
	"woman_beaten",
	"ipv_norm",
	"not_better_man_achiever",
	"women_should_not_kneel",
	"woman_earns_no_problem",
	"women_better_leaders"
)

# Add any other variables to grab here:
other_merge_variables <- c("id", "resample")

# Merge in compliers from midline
ml_tomerge <-
	ml[, c(other_merge_variables, covariates, IPV_outcomes)]

names(ml_tomerge) <- paste0(names(ml_tomerge), "_ml")
ml_tomerge$merge_id <- ml_tomerge$id_ml
ml_tomerge$resample <- ml_tomerge$resample_ml

el$merge_id <- NA
el$merge_id[el$respondent_category == "Complier"] <-
	el$id[el$respondent_category == "Complier"]

el <- merge(x = el,
						ml_tomerge,
						by = "merge_id",
						all.x = T)

el$resample[is.na(el$resample)] <- 0

rm(ml_tomerge)
