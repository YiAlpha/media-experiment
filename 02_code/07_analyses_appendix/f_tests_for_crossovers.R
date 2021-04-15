
# Incidents unreported 
vht_village_level_ml <- 
  vht_ml%>% 
  group_by(tc_id,block_id,IPV, abortion, absenteeism) %>% 
  summarize(
    unreported_ipv_ml = mean(unreported_ipv,na.rm = T),
    radius = unique(radius),
    n_end_mean = unique(n_end_mean)
  )
vht_village_level_ml$tc <- vht_village_level_ml$tc_id
vht_village_level_el <- 
  vht_el %>% 
  group_by(tc_id,block_id,IPV, abortion, absenteeism) %>% 
  summarize(
    unreported_ipv_el = mean(unreported_ipv,na.rm = T),
    radius = unique(radius),
    n_end_mean = unique(n_end_mean)
  )
vht_village_level_el$tc <- vht_village_level_el$tc_id
vht_village_level_el$resample <- 1
vht_village_level_ml$resample <- 1

do_f_tests <- function(outcome_name, data, wave_label, subset_label){
  
  restricted <- reformulate(
    termlabels = c("IPV", "as.factor(block_id)", "resample"),
    response = outcome_name
  )
  
  abortion_interaction <- update.formula(restricted, . ~ . + IPV * abortion)
  
  fully_saturated <- update.formula(restricted, . ~  . + IPV * abortion * absenteeism)
  
  
  r_fit <- lm(formula = restricted,data = data)
  a_fit <- lm(formula = abortion_interaction,data = data)
  f_fit <- lm(formula = fully_saturated,data = data)
  
  test_1 <- anova(r_fit,a_fit)
  test_2 <- anova(r_fit,f_fit)
  
  return(
  data.frame(
    outcome = outcome_name,
    wave = wave_label,
    subset = subset_label,
    abortion_p = test_1[2,"Pr(>F)"],
    saturated_p = test_2[2,"Pr(>F)"]
  )
  )
  
  
}


f_test_list <- list(
  list("household_violence", "endline", "all women", subset(el, female == 1)),
  list("any_violence", "endline", "all women", subset(el, female == 1)),
  
  list("intervene_index", "endline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("involve_parents", "endline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("involve_nabakyala", "endline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("involve_lc1", "endline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("report_police", "endline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("intervene_index", "endline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  list("involve_parents", "endline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  list("involve_nabakyala", "endline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  list("involve_lc1", "endline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  list("report_police", "endline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  
  list("intervene_index_ml", "midline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("involve_parents_ml", "midline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("involve_nabakyala_ml", "midline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("involve_lc1_ml", "midline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("report_police_ml", "midline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("intervene_index_ml", "midline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  list("involve_parents_ml", "midline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  list("involve_nabakyala_ml", "midline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  list("involve_lc1_ml", "midline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  list("report_police_ml", "midline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  
  
  list("would_take_revenge", "endline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("would_take_revenge", "endline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  list("anti_intervention_consequence", "endline", "complier men", subset(el, respondent_category == "Complier" & female == 0)),
  list("anti_intervention_consequence", "endline", "complier women", subset(el, respondent_category == "Complier" & female == 1)),
  list("react_comm_ml", "midline", "men compliers", subset(el, respondent_category == "Complier" & female == 0)),
  list("react_comm", "endline", "men compliers", subset(el, respondent_category == "Complier" & female == 0)),
  list("react_comm_ml", "midline", "women compliers", subset(el, respondent_category == "Complier" & female == 1)),
  list("react_comm", "endline", "women compliers", subset(el, respondent_category == "Complier" & female == 1))
)

f_test_frame <- data.frame()

for(i in 1:length(f_test_list)){
  f_test_frame <- rbind(f_test_frame,
                        do_f_tests(outcome_name = f_test_list[[i]][[1]],
                                   wave_label = f_test_list[[i]][[2]],
                                   subset_label = f_test_list[[i]][[3]],
                                   data = f_test_list[[i]][[4]]
                        ))
}


f_test_frame$outcome <- gsub(pattern = "_",replacement = "\\\\_",x = f_test_frame$outcome)
f_test_frame$outcome <- paste0("\\texttt{",f_test_frame$outcome,"}")

outcome_labels <- c("Number of Incidents",
                    "Any Incidents",
                    "Reporting Index",
                    "Involve Parents",
                    "Involve Counselor",
                    "Involve Village Leader",
                    "Report to Police",
                    "Reporting Index",
                    "Involve Parents",
                    "Involve Counselor",
                    "Involve Village Leader",
                    "Report to Police",
                    "Reporting Index",
                    "Involve Parents",
                    "Involve Counselor",
                    "Involve Village Leader",
                    "Report to Police",
                    "Reporting Index",
                    "Involve Parents",
                    "Involve Counselor",
                    "Involve Village Leader",
                    "Report to Police",
                    "Personal Retribution",
                    "Personal Retribution",
                    "Social Repercussions",
                    "Social Repercussions",
                    "Community Would Intervene",
                    "Community Would Intervene",
                    "Community Would Intervene",
                    "Community Would Intervene")

f_test_frame$outcome <- outcome_labels

sink(file = "03_tables/crossover_f_tests.tex")
print(knitr::kable(
  f_test_frame,
  col.names = c("Outcome",
                "Wave",
                "Subset",
                "Abortion interaction $p$-value",
                "Fully saturated $p$-value"),
  format = "latex",
  digits = 3,
  align = c("l","l","l","c","c"),
  escape = F
    ))
sink()












# Outcomes with hypothesis rejections -------------------------------------

would_take_revenge_el_m <- ols_main(
  outcome = "would_take_revenge",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE, 
  crossover_specification = "saturated"
)

would_take_revenge_el_pvals_m <- get_RI_pvals(
  outcome = "would_take_revenge",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE,
  crossover_specification = "saturated",
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")

intervene_index_el_w <- ols_main(
  outcome = "intervene_index",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE, 
  crossover_specification = "abortion"
)

intervene_index_el_pvals_w <- get_RI_pvals(
  outcome = "intervene_index",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE,
  crossover_specification = "abortion",
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")

intervene_index_el_m <- ols_main(
  outcome = "intervene_index",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE, 
  crossover_specification = "abortion"
)

intervene_index_el_pvals_m <- get_RI_pvals(
  outcome = "intervene_index",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 0),
  dosage = FALSE,
  dosage_indicator = FALSE,
  crossover_specification = "abortion",
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")

involve_parents_el_w <- ols_main(
  outcome = "involve_parents",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE, 
  crossover_specification = "abortion"
)

involve_parents_el_pvals_w <- get_RI_pvals(
  outcome = "involve_parents",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE,
  crossover_specification = "abortion",
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")

involve_nabakyala_el_w <- ols_main(
  outcome = "involve_nabakyala",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE, 
  crossover_specification = "abortion"
)

involve_nabakyala_el_pvals_w <- get_RI_pvals(
  outcome = "involve_nabakyala",
  treatment = "IPV",
  resample_FE = TRUE,
  block_FE = TRUE,
  audience_size = TRUE,
  cluster_SE = TRUE,
  covariates = NULL,
  the_data = subset(el, respondent_category == "Complier" & female == 1),
  dosage = FALSE,
  dosage_indicator = FALSE,
  crossover_specification = "abortion",
  assignment_data = treatment_assignment,
  extract_function = coef,
  analysis_function = ols_main,
  sims = sims,
  lwr_upr_two = "upr")



pval_lines_IPV <- c(
  "RI $p$-values",
  round(intervene_index_el_pvals_w$ri_pvals["IPV"],3),
  round(intervene_index_el_pvals_m$ri_pvals["IPV"],3),
  round(involve_parents_el_pvals_w$ri_pvals["IPV"],3),
  round(involve_nabakyala_el_pvals_w$ri_pvals["IPV"],3),
  round(would_take_revenge_el_pvals_m$ri_pvals["IPV"],3)
)


sink("03_tables/crossover_robustness.tex")
stargazer(
  ... = list(
    intervene_index_el_w$fit, 
    intervene_index_el_m$fit, 
    involve_parents_el_w$fit, 
    involve_nabakyala_el_w$fit, 
    would_take_revenge_el_m$fit
  ),
  type = "latex",
  p = list(
    intervene_index_el_pvals_w$ri_pvals,
    intervene_index_el_pvals_m$ri_pvals,
    involve_parents_el_pvals_w$ri_pvals,
    involve_nabakyala_el_pvals_w$ri_pvals,
    would_take_revenge_el_pvals_m$ri_pvals
  ),
  se = list(
    intervene_index_el_w$fit_summary[,"Std. Error"],
    intervene_index_el_m$fit_summary[,"Std. Error"],
    involve_parents_el_w$fit_summary[,"Std. Error"],
    involve_nabakyala_el_w$fit_summary[,"Std. Error"],
    would_take_revenge_el_m$fit_summary[,"Std. Error"]
  ),
  covariate.labels = c("Anti-VAW Media","Anti-Abortion Stigma Media", "Anti-Absenteeism Media","VAW x Abortion", "VAW x Absenteeism", "Abortion x Absenteeism"),
  keep = c("IPV","abortion","absenteeism"),
  omit.stat = c("rsq","f","ser"),
  column.labels = c( "Reporting Index",  "Involve Parents", "Involve Counselor", "Personal Retribution" ),
  
  column.separate = c(2,1,1,1),
  
  table.layout = "=cd#-t-as=n",
  dep.var.labels = c(
                     "Endline",
                     "Endline",
                     "Endline",
                     "Endline",
                     "Midline"
  ),
  dep.var.labels.include = TRUE,
  no.space = T,
  omit = "block_id",
  add.lines = list(
    pval_lines_IPV,
    c("Sample", "Women","Men","Women","Women","Men"),
    c("Block FE",
      "Yes","Yes","Yes",
      "Yes")
  ),
  notes.label = "",
  float = FALSE
  # style = "qje"
)
sink()

















