
  
  el_comp <- subset(el,respondent_category == "Complier" & female == 1)
  
  # Balance function --------------------------------------------------------
  
  # Function should first take mean of all of the three conditions, then the 7 
  # Then computes a likelihood ratio test 
  
  observed_lik_dif <- sapply(
    X = 1:length(covariates_el),
    FUN = function(i){
      
      full_model_marginal <- lm(
        formula = as.formula(paste0(covariates_el[i]," ~ IPV + abortion + absenteeism + resample + as.factor(block_id)")),
        data = el_comp
      )
      
      full_model_conditional <- lm(
        formula = as.formula(paste0(covariates_el[i]," ~ treatment + resample + as.factor(block_id)")),
        data = el_comp
      )
      
      nested_model <- lm(
        formula = as.formula(paste0(covariates_el[i]," ~ resample + as.factor(block_id)")),
        data = el_comp
      )
      
      marginal_test <- lmtest::lrtest(full_model_marginal,nested_model)
      conditional_test <- lmtest::lrtest(full_model_conditional,nested_model)
      
      obs_ratio_marginal <- 2*(marginal_test$LogLik[1] - marginal_test$LogLik[2])
      obs_ratio_conditonal <- 2*(conditional_test$LogLik[1] - conditional_test$LogLik[2])
      
      return(c(obs_ratio_marginal,obs_ratio_conditonal))
    }
  )
  
  rownames(observed_lik_dif) <- c("Marginal","Conditional")
  colnames(observed_lik_dif) <- covariates_el
  
  marginal_mat <- conditional_mat <- list()
  
  set.seed(123456789)
  
  for(j in (length(marginal_mat)+1):sims){
    new_data <- rerandomize(tc_level_data = treatment_assignment,analysis_data = el_comp)
    null_lik_dif <- sapply(
      X = 1:length(covariates_el),
      FUN = function(i){
        
        full_model_marginal <- lm(
          formula = as.formula(paste0(covariates_el[i]," ~ IPV + abortion + absenteeism + resample + as.factor(block_id)")),
          data = new_data
        )
        
        full_model_conditional <- lm(
          formula = as.formula(paste0(covariates_el[i]," ~ treatment + resample + as.factor(block_id)")),
          data = new_data
        )
        
        nested_model <- lm(
          formula = as.formula(paste0(covariates_el[i]," ~ resample + as.factor(block_id)")),
          data = new_data
        )
        
        marginal_test <- lmtest::lrtest(full_model_marginal,nested_model)
        conditional_test <- lmtest::lrtest(full_model_conditional,nested_model)
        
        null_ratio_marginal <- 2*(marginal_test$LogLik[1] - marginal_test$LogLik[2])
        null_ratio_conditonal <- 2*(conditional_test$LogLik[1] - conditional_test$LogLik[2])
        
        return(c(null_ratio_marginal,null_ratio_conditonal))
      }
    )
    
    marginal_mat[[j]] <- null_lik_dif[1,]
    conditional_mat[[j]] <- null_lik_dif[2,]
    
    print(j)
    
  }
  
  marginal_sims <- as.data.frame(do.call("rbind",marginal_mat))
  conditional_sims <- as.data.frame(do.call("rbind",conditional_mat))
  
  colnames(marginal_sims) <- colnames(conditional_sims) <- covariates_el
  
  marginal_pvalues <- colMeans(t(apply(X = marginal_sims,MARGIN = 1,FUN = function(sim){sim >= observed_lik_dif[1,]})))
  conditional_pvalues <- colMeans(t(apply(X = conditional_sims,MARGIN = 1,FUN = function(sim){sim >= observed_lik_dif[2,]})))
  
  

# Marginalizing over treatment conditions ---------------------------------


baltab_IPV <- 
  el_comp %>% 
  subset(select = c(covariates_el,"IPV")) %>% 
  group_by(IPV) %>% 
  summarize_each(funs = "mean") %>% 
  t()
baltab_ABO <- 
  el_comp %>% 
  subset(select = c(covariates_el,"abortion")) %>% 
  group_by(abortion) %>% 
  summarize_each(funs = "mean") %>% 
  t()
baltab_ABS <- 
  el_comp %>% 
  subset(select = c(covariates_el,"absenteeism")) %>% 
  group_by(absenteeism) %>% 
  summarize_each(funs = "mean") %>% 
  t()
baltab_placebo <- 
  el_comp %>% 
  subset(select = c(covariates_el,"placebo")) %>% 
  group_by(placebo) %>% 
  summarize_each(funs = "mean") %>% 
  t()

marginal_baltab_comp <- 
  cbind.data.frame(
    covariate = covariates_el,
    round(
      data.frame(
        placebo = baltab_placebo[-1,2],
        IPV = baltab_IPV[-1,2],
        abortion = baltab_ABO[-1,2],
        absenteeism = baltab_ABS[-1,2],
        pval = marginal_pvalues
      ),2)) %>% 
  arrange(pval)

rm(baltab_placebo,
   baltab_IPV,
   baltab_ABO,
   baltab_ABS)

# Preserving conditional distribution -------------------------------------

baltab_conditional_comp <- 
  el_comp %>% 
  subset(select = c(covariates_el,"treatment")) %>% 
  group_by(treatment) %>% 
  summarize_each(funs = "mean") %>% 
  ungroup() %>% 
  dplyr::select(-treatment) %>%
  t() %>% 
  data.frame()

names(baltab_conditional_comp) <- sort(unique(el_comp$treatment))

conditional_baltab_comp <- cbind.data.frame(
  covariate = covariates_el,
  round(
    data.frame(
      baltab_conditional_comp[,c("placebo","IPV","abortion","absenteeism","abortion_absenteeism","IPV_abortion","IPV_absenteeism")],
      pval = conditional_pvalues),
    2)) %>% 
  arrange(pval)





# Conditional -------------------------------------------------------------


names(conditional_baltab_comp) <- c(
  "covariate",
  "PLA","VAW","ABO","ABS","ABO_ABS","VAW_ABS","VAW_ABO",
  "p-value")
rownames(conditional_baltab_comp) <- conditional_baltab_comp$covariate
conditional_baltab_comp$covariate <- NULL

sink("03_tables/balance_women_panel_compliers_el.tex")
print(
  xtable(
    conditional_baltab_comp),
  include.rownames = TRUE,
  floating = FALSE,
  sanitize.rownames.function = texttt,
  tabular.environment = "longtable")
sink()








