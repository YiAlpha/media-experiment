# IPV Index
el$violence_disapproval <-  (el$ipv_attitude + el$disobey)/2
el$violence_disapproval_ml <-  (el$ipv_attitude_ml + el$disobey_ml)/2

# Conative Attitudes Index

el$intervene_index <- (el$involve_lc1 + el$involve_nabakyala + el$involve_parents + el$report_police)/4

el$intervene_index_ml <- (el$involve_lc1_ml + 
                              el$involve_nabakyala_ml + 
                              el$involve_parents_ml + 
                              el$report_police_ml)/4 



# Adding Midline and Endline 
el$intervene_index_ml_3 <- (el$intervene_index + el$intervene_index_ml)/2


# Any Violence

el$any_violence <- el$household_violence > 0
el$any_violence[is.na(el$household_violence)] <- NA

el$any_violence_village <- (el$woman_beaten > 0)*1
ml$any_violence_village <- (ml$woman_beaten > 0)*1

el$violence_disapproval_comm <- (el$disobey_comm + el$ipv_norm)/2

# Asses Index -------------------------------------------------------------

el$asset_index <- with(el,tv_ml + radio_ml + chair_ml + sofa_ml +  (1-mud_wall_ml) + motor_cycle_ml)/6 

# Gender Equality ---------------------------------------------------------


# Midline
el$support_gender_equality_ml <- (el$not_better_man_achiever_ml + 
                                      el$women_should_not_kneel_ml + 
                                      el$woman_earns_no_problem_ml + 
                                      el$women_better_leaders_ml)/4


# Endline
el$support_gender_equality <- (el$boys_school_not_more_imp +
                                  el$able_marry + 
                                  el$father_not_final_say + 
                                  el$men_should_participate)/4
