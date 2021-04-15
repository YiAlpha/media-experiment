
panel_compliers <- el$id[el$respondent_category == "Complier"]
el_no_imp$any_violence <- el_no_imp$household_violence > 0 

ml_no_imp <- arrange(ml_no_imp,KEY)

outcomes <- c( 
  "household_violence",
  "household_violence",
  "any_violence",
  "any_violence",
  "household_violence_frequency",
  "household_violence_frequency",
  
  "involve_parents",
  "involve_nabakyala",
  "involve_lc1",
  "report_police",
  
  "involve_parents",
  "involve_nabakyala",
  "involve_lc1",
  "report_police",
  "react_comm",
  "react_comm",
  "would_take_revenge",
  "would_take_revenge",
  "anti_intervention_consequence",
  "anti_intervention_consequence"
  )

what_table <- c( 
  "Table 1",
  "Table 1",
  "Table 1",
  "Table 1",
  "Table 1",
  "Table 1",
  
  "Table 3",
  "Table 3",
  "Table 3",
  "Table 3",
  
  "Table 2",
  "Table 2",
  "Table 2",
  "Table 2",
  "Table 4",
  "Table 4",
  "Table 4",
  "Table 4",
  "Table 4",
  "Table 4"
)
alternatives <- list( 
  c("no_imputations","covariates"),
  c("no_imputations","covariates"),
  c("no_imputations","covariates"),
  c("no_imputations","covariates"),
  c("no_imputations","covariates"),
  c("no_imputations","covariates"),
  c("no_imputations","covariates","panel full sample"),
  c("no_imputations","covariates","panel full sample"),
  c("no_imputations","covariates","panel full sample"),
  c("no_imputations","covariates","panel full sample"),
  c("no_imputations","covariates","panel full sample"),
  c("no_imputations","covariates","panel full sample"),
  c("no_imputations","covariates","panel full sample"),
  c("no_imputations","covariates","panel full sample"),
  c("no_imputations","covariates","panel full sample"),
  c("no_imputations","covariates","panel full sample"),
  c("no_imputations","covariates","full sample"),
  c("no_imputations","covariates","full sample"),
  c("no_imputations","covariates","full sample"),
  c("no_imputations","covariates","full sample")
)

data_list <- list(
  'female == 1',
  'female == 1 & complier',
  'female == 1',
  'female == 1 & complier',
  'female == 1',
  'female == 1 & complier',
  'id %in% panel_compliers & female == 0',
  'id %in% panel_compliers & female == 0',
  'id %in% panel_compliers & female == 0',
  'id %in% panel_compliers & female == 0',
  'id %in% panel_compliers & female == 1',
  'id %in% panel_compliers & female == 1',
  'id %in% panel_compliers & female == 1',
  'id %in% panel_compliers & female == 1',
  'id %in% panel_compliers & female == 0',
  'id %in% panel_compliers & female == 1',
  'id %in% panel_compliers & female == 0',
  'id %in% panel_compliers & female == 1',
  'id %in% panel_compliers & female == 0',
  'id %in% panel_compliers & female == 1')

sample_list <- list(
  "All women",
  "Women compliers (all)",
  "All women",
  "Women compliers (all)",
  "All women",
  "Women compliers (all)",
  "Men compliers (panel only)",
  "Men compliers (panel only)",
  "Men compliers (panel only)",
  "Men compliers (panel only)",
  "Women compliers (panel only)",
  "Women compliers (panel only)",
  "Women compliers (panel only)",
  "Women compliers (panel only)",
  "Men compliers (panel only)",
  "Women compliers (panel only)",
  "Men compliers (panel only)",
  "Women compliers (panel only)",
  "Men compliers (panel only)",
  "Women compliers (panel only)")


hypothesis_list <- c(
  "two",
  "two",
  "two",
  "two",
  "two",
  "two",
  "upr",
  "upr",
  "upr",
  "upr",
  "upr",
  "upr",
  "upr",
  "upr",
  "upr",
  "upr",
  "lwr",
  "lwr",
  "lwr",
  "lwr")

panel_list <- c( 
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  FALSE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  FALSE,
  FALSE,
  FALSE,
  FALSE
)

get_ests <- function(ri_object) {
  paste(c(
    sprintf("%.2f",round(ri_object$observed["IPV"], 2)),
    paste0("(p = ", sprintf(
      "%.3f", round(ri_object$ri_pvals["IPV"], 3)
    ), ")")
  ), collapse = " ")
}


make_row_of_table <- function(
  outcome, 
  subset_expr, 
  hypothesis, 
  panel, 
  sample_label,
  alternative,
  what_table,
  sims
){
  
  set.seed(123456789)
  
  robust_table <- list()
  
  
  if(panel){
    
    outcome_ml <- paste0(outcome,"_ml")
    
    original_analysis_ml <- get_RI_pvals(
      outcome = outcome_ml,
      treatment = "IPV",
      resample_FE = TRUE,
      block_FE = TRUE,
      audience_size = TRUE,
      cluster_SE = TRUE,
      covariates = NULL,
      the_data = subset(el,subset = eval(parse(text = subset_expr))),
      assignment_data = treatment_assignment,
      sims = sims,
      lwr_upr_two = hypothesis)
    
    original_analysis_el <- get_RI_pvals(
      outcome = outcome,
      treatment = "IPV",
      resample_FE = TRUE,
      block_FE = TRUE,
      audience_size = TRUE,
      cluster_SE = TRUE,
      covariates = NULL,
      the_data = subset(el,subset = eval(parse(text = subset_expr))),
      assignment_data = treatment_assignment,
      sims = sims,
      lwr_upr_two = hypothesis)
    
    original_ml <- get_ests(original_analysis_ml)
    original_el <- get_ests(original_analysis_el)
    
    if("no_imputations" %in% alternative){
      no_imputations_analysis_ml <- get_RI_pvals(
        outcome = outcome,
        treatment = "IPV",
        resample_FE = TRUE,
        block_FE = TRUE,
        audience_size = TRUE,
        cluster_SE = TRUE,
        covariates = NULL,
        the_data = subset(ml_no_imp,subset = eval(parse(text = subset_expr))),
        assignment_data = treatment_assignment,
        sims = sims,
        lwr_upr_two = hypothesis)
      no_imputations_analysis_el <- get_RI_pvals(
        outcome = outcome,
        treatment = "IPV",
        resample_FE = TRUE,
        block_FE = TRUE,
        audience_size = TRUE,
        cluster_SE = TRUE,
        covariates = NULL,
        the_data = subset(el_no_imp,subset = eval(parse(text = subset_expr))),
        assignment_data = treatment_assignment,
        sims = sims,
        lwr_upr_two = hypothesis)
      
      no_imputations_ml <- get_ests(no_imputations_analysis_ml)
      no_imputations_el <- get_ests(no_imputations_analysis_el)
      
      robust_table[[(length(robust_table)+1)]] <- 
        data.frame(
          outcome = c(outcome_ml,outcome), 
          original_sample = sample_label,
          alternative = "No imputations",
          est_alternative = c(no_imputations_ml,no_imputations_el),
          est_original = c(original_ml,original_el)
        )
    }
    
    if("covariates" %in% alternative){
      if( grepl("female == 1",subset_expr)){
        cov_list <- IPV_lasso_covariates_panel_W
      } else {
        cov_list <- IPV_lasso_covariates_panel_M
      }
      
      lasso_covariates_analysis_ml <- get_RI_pvals(
        outcome = outcome_ml,
        treatment = "IPV",
        resample_FE = TRUE,
        block_FE = TRUE,
        audience_size = TRUE,
        cluster_SE = TRUE,
        covariates = get_covariates(
          var_name = outcome,
          lasso_list = cov_list),
        the_data = subset(el,subset = eval(parse(text = subset_expr))),
        assignment_data = treatment_assignment,
        sims = sims,
        lwr_upr_two = hypothesis)
      lasso_covariates_analysis_el <- get_RI_pvals(
        outcome = outcome,
        treatment = "IPV",
        resample_FE = TRUE,
        block_FE = TRUE,
        audience_size = TRUE,
        cluster_SE = TRUE,
        covariates = get_covariates(
          var_name = outcome,
          lasso_list = cov_list),
        the_data = subset(el,subset = eval(parse(text = subset_expr))),
        assignment_data = treatment_assignment,
        sims = sims,
        lwr_upr_two = hypothesis)
      
      lasso_covariates_ml <- get_ests(lasso_covariates_analysis_ml)
      lasso_covariates_el <- get_ests(lasso_covariates_analysis_el)
      
      robust_table[[(length(robust_table)+1)]] <- 
        data.frame(
          outcome = c(outcome_ml,outcome), 
          original_sample = sample_label,
          alternative = "Lasso covariates",
          est_alternative = c(lasso_covariates_ml,lasso_covariates_el),
          est_original = c(original_ml,original_el)
        )
      
    }
    
    if("panel full sample" %in% alternative){
      if( grepl("female == 1",subset_expr)){
        subset_expr_new_el <- "complier & female == 1"
        subset_expr_new_ml <- "compliance_label == 'Complier' & female == 1"
        full_sample_alternative <- "Women compliers (all)"
      } else {
        subset_expr_new_el <- "complier & female == 0"
        subset_expr_new_ml <- "compliance_label == 'Complier' & female == 0"
        full_sample_alternative <- "Men compliers (all)"
      }
      
      full_sample_analysis_ml <- get_RI_pvals(
        outcome = outcome,
        treatment = "IPV",
        resample_FE = TRUE,
        block_FE = TRUE,
        audience_size = TRUE,
        cluster_SE = TRUE,
        covariates = NULL,
        the_data = subset(ml,subset = eval(parse(text = subset_expr_new_ml))),
        assignment_data = treatment_assignment,
        sims = sims,
        lwr_upr_two = hypothesis)
      full_sample_analysis_el <- get_RI_pvals(
        outcome = outcome,
        treatment = "IPV",
        resample_FE = TRUE,
        block_FE = TRUE,
        audience_size = TRUE,
        cluster_SE = TRUE,
        covariates = NULL,
        the_data = subset(el,subset = eval(parse(text = subset_expr_new_el))),
        assignment_data = treatment_assignment,
        sims = sims,
        lwr_upr_two = hypothesis)
      
      full_sample_ml <- get_ests(full_sample_analysis_ml)
      full_sample_el <- get_ests(full_sample_analysis_el)
      
      robust_table[[(length(robust_table)+1)]] <- 
        data.frame(
          outcome = c(outcome_ml,outcome), 
          original_sample = sample_label,
          alternative = full_sample_alternative,
          est_alternative = c(full_sample_ml,full_sample_el),
          est_original = c(original_ml,original_el)
        )
      
    }
    
  } else {
    
    original_analysis <- get_RI_pvals(
      outcome = outcome,
      treatment = "IPV",
      resample_FE = TRUE,
      block_FE = TRUE,
      audience_size = TRUE,
      cluster_SE = TRUE,
      covariates = NULL,
      the_data = subset(el,subset = eval(parse(text = subset_expr))),
      assignment_data = treatment_assignment,
      sims = sims,
      lwr_upr_two = hypothesis)
    
    original <- get_ests(original_analysis)
    
    if("no_imputations" %in% alternative){
      no_imputations_analysis <- get_RI_pvals(
        outcome = outcome,
        treatment = "IPV",
        resample_FE = TRUE,
        block_FE = TRUE,
        audience_size = TRUE,
        cluster_SE = TRUE,
        covariates = NULL,
        the_data = subset(el_no_imp,subset = eval(parse(text = subset_expr))),
        assignment_data = treatment_assignment,
        sims = sims,
        lwr_upr_two = hypothesis)
      
      no_imputations <- get_ests(no_imputations_analysis)
      
      robust_table[[(length(robust_table)+1)]] <- 
        data.frame(
          outcome = outcome, 
          original_sample = sample_label,
          alternative = "No imputations",
          est_alternative = no_imputations,
          est_original = original
        )
    }
    
    if("covariates" %in% alternative){
      if( grepl("female == 1",subset_expr)){
        cov_list <- IPV_lasso_covariates_W
      } else {
        cov_list <- IPV_lasso_covariates_M
      }
      
      lasso_covariates_analysis <- get_RI_pvals(
        outcome = outcome,
        treatment = "IPV",
        resample_FE = TRUE,
        block_FE = TRUE,
        audience_size = TRUE,
        cluster_SE = TRUE,
        covariates = get_covariates(
          var_name = outcome,
          lasso_list = cov_list),
        the_data = subset(el,subset = eval(parse(text = subset_expr))),
        assignment_data = treatment_assignment,
        sims = sims,
        lwr_upr_two = hypothesis)
      
      lasso_covariates <- get_ests(lasso_covariates_analysis)
      
      robust_table[[(length(robust_table)+1)]] <- 
        data.frame(
          outcome = outcome, 
          original_sample = sample_label,
          alternative = "Lasso covariates",
          est_alternative = lasso_covariates,
          est_original = original
        )
      
    }
    
    if("full sample" %in% alternative){
      if( grepl("female == 1",subset_expr)){
        subset_expr_new <- "complier & female == 1"
        full_sample_alternative <- "Women compliers (all)"
      } else {
        subset_expr_new <- "complier & female == 0"
        full_sample_alternative <- "Men compliers (all)"
      }
      
      full_sample_analysis <- get_RI_pvals(
        outcome = outcome,
        treatment = "IPV",
        resample_FE = TRUE,
        block_FE = TRUE,
        audience_size = TRUE,
        cluster_SE = TRUE,
        covariates = NULL,
        the_data = subset(el,subset = eval(parse(text = subset_expr_new))),
        assignment_data = treatment_assignment,
        sims = sims,
        lwr_upr_two = hypothesis)
      
      full_sample <- get_ests(full_sample_analysis)
      
      robust_table[[(length(robust_table)+1)]] <- 
        data.frame(
          outcome = outcome, 
          original_sample = sample_label,
          alternative = full_sample_alternative,
          est_alternative = full_sample,
          est_original = original
        )
      
    }
  }
  return_table <- do.call(rbind.data.frame,robust_table)
  return_table$what_table <- what_table
  return(return_table)
}




set.seed(123456789)
robustness_table <- make_row_of_table(
  outcome = outcomes[1],
  subset_expr = data_list[[1]],
  hypothesis = hypothesis_list[1],
  sample_label = sample_list[[1]],
  sims = sims,
  alternative = alternatives[[1]],
  panel = panel_list[1],
  what_table = what_table[1])

for(i in 2:length(outcomes)){
  
  robustness_table <- rbind(
    robustness_table,
    make_row_of_table(
      outcome = outcomes[i],
      subset_expr = data_list[[i]],
      hypothesis = hypothesis_list[i],
      sample_label = sample_list[[i]],
      sims = sims,
      alternative = alternatives[[i]],
      panel = panel_list[i],
      what_table = what_table[i])
  )
}



# Frequency results separately --------------------------------------------

# No imputations
household_frequency_ind_ologit_pvals <- get_RI_pvals(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el, female == 1),
  analysis_function = ordered_logit_main,
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")
household_frequency_ind_ologit_comp_pvals <- get_RI_pvals(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el, female == 1 & complier),
  analysis_function = ordered_logit_main,
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")
household_frequency_ind_ologit_pvals_no_imp <- get_RI_pvals(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el_no_imp, female == 1),
  analysis_function = ordered_logit_main,
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")
household_frequency_ind_ologit_comp_pvals_no_imp <- get_RI_pvals(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  the_data = subset(el_no_imp, female == 1 & complier),
  analysis_function = ordered_logit_main,
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")

# Lasso covariates 

household_frequency_ind_ologit_pvals_lasso <- get_RI_pvals(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  covariates = get_covariates(
    var_name = "household_violence_frequency",
    lasso_list = IPV_lasso_covariates_W),
  cluster_SE = T,
  the_data = subset(el, female == 1),
  analysis_function = ordered_logit_main,
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")
household_frequency_ind_ologit_comp_pvals_lasso <- get_RI_pvals(
  outcome = "household_violence_frequency",
  treatment = "IPV",
  resample_FE = T,
  block_FE = T,
  audience_size = T,
  cluster_SE = T,
  covariates = get_covariates(
    var_name = "household_violence_frequency",
    lasso_list = IPV_lasso_covariates_W),
  the_data = subset(el, female == 1 & complier),
  analysis_function = ordered_logit_main,
  assignment_data = treatment_assignment,
  sims = sims,
  lwr_upr_two = "two")




outcome_labels <- c(
  "household_violence",
  "any_violence",
  "household_violence_frequency",
  "involve_parents_ml",
  "involve_parents",
  "involve_nabakyala_ml",
  "involve_nabakyala",
  "involve_lc1_ml",
  "involve_lc1",
  "report_police_ml",
  "report_police",
  "react_comm_ml",
  "react_comm",
  "would_take_revenge",
  "anti_intervention_consequence")
column_labels <- c(
  "Number of Incidents (EL)",
  "Any Incidents (EL)",
  "Violence Frequency (EL)",
  " Parents (ML)",
  " Parents (EL)",
  " Counselor (ML)",
  " Counselor (EL)",
  " Village Leader (ML)",
  " Village Leader (EL)",
  " Police (ML)",
  " Police (EL)",
  "Community Would Intervene (ML)",
  "Community Would Intervene (EL)",
  "Personal Retribution (EL)",
  "Social Repercussions (EL)")

robustness_table$column_label <- NA
for( i in 1:length(outcome_labels)){
  robustness_table$column_label[robustness_table$outcome == outcome_labels[i]] <- column_labels[i]
}

table_1 <- 
  robustness_table[robustness_table$what_table == "Table 1", c("column_label","original_sample","alternative","est_alternative","est_original")]
table_2 <- 
  robustness_table[robustness_table$what_table == "Table 2", c("column_label","original_sample","alternative","est_alternative","est_original")]
table_3 <- 
  robustness_table[robustness_table$what_table == "Table 3", c("column_label","original_sample","alternative","est_alternative","est_original")]
table_4 <- 
  robustness_table[robustness_table$what_table == "Table 4", c("column_label","original_sample","alternative","est_alternative","est_original")]


colnames(table_1) <- c("Outcome","Original sample","Alternative specification","Alt. Est.","Orig. Est.")
colnames(table_2) <- c("Outcome","Original sample","Alternative specification","Alt. Est.","Orig. Est.")
colnames(table_3) <- c("Outcome","Original sample","Alternative specification","Alt. Est.","Orig. Est.")
colnames(table_4) <- c("Outcome","Original sample","Alternative specification","Alt. Est.","Orig. Est.")


# Insert frequency results into table 1 -----------------------------------


table_1[,"Orig. Est."] <- as.character(table_1[,"Orig. Est."])
table_1[,"Alt. Est."] <- as.character(table_1[,"Alt. Est."])

table_1[table_1[,"Outcome"] == "Violence Frequency (EL)","Orig. Est."] <- 
  c(get_ests(household_frequency_ind_ologit_pvals),
    get_ests(household_frequency_ind_ologit_pvals),
    get_ests(household_frequency_ind_ologit_comp_pvals),
    get_ests(household_frequency_ind_ologit_comp_pvals)
  )
table_1[table_1[,"Outcome"] == "Violence Frequency (EL)","Alt. Est."] <- 
  c(get_ests(household_frequency_ind_ologit_pvals_no_imp),
    get_ests(household_frequency_ind_ologit_pvals_lasso),
    get_ests(household_frequency_ind_ologit_comp_pvals_no_imp),
    get_ests(household_frequency_ind_ologit_comp_pvals_lasso)
  )

# Remove should intervene -------------------------------------------------




sink("03_tables/robustness_table_1.tex")
print.xtable(xtable(table_1,align = c("l","l","l","l","r","r")),include.rownames = FALSE,floating = FALSE)
sink()
sink("03_tables/robustness_table_2.tex")
print.xtable(xtable(table_2,align = c("l","l","l","l","r","r")),include.rownames = FALSE,floating = FALSE)
sink()
sink("03_tables/robustness_table_3.tex")
print.xtable(xtable(table_3,align = c("l","l","l","l","r","r")),include.rownames = FALSE,floating = FALSE)
sink()
sink("03_tables/robustness_table_4.tex")
print.xtable(xtable(table_4,align = c("l","l","l","l","r","r")),include.rownames = FALSE,floating = FALSE)
sink()






