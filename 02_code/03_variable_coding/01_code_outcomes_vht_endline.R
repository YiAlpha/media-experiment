
# VHT outcomes ------------------------------------------------------------


vht_elcb$immunization_q3_1a
vht_elcb$poisoning_q3_1b # general health
vht_elcb$malaria_q3_1c # child rearing/child health
vht_elcb$abortion_q3_1d
vht_elcb$birth_control_q3_1e
vht_elcb$violence_q3_1f
vht_elcb$other_q3_1g
vht_elcb$other_q3_1g_number

# Who asked about abortion

vht_elcb$woman_q3_2a
vht_elcb$husband_q3_2b
vht_elcb$family_q3_2c
vht_elcb$friends_q3_2d
vht_elcb$neighbors_q3_2e
vht_elcb$other_q3_2f
vht_elcb$other_q3_2f_number

# Recoding "other responses"


# Who reported violence

vht_elcb$victim_q3_3a
vht_elcb$perpetrator_q3_3b
vht_elcb$victim_fam_q3_3c
vht_elcb$perpetrator_fam_q3_3d
vht_elcb$victim_friends_q3_3e
vht_elcb$perpetrator_friends_q3_3f
vht_elcb$neighbors_q3_3g
vht_elcb$other_q3_3h
vht_elcb$other_q3_3h_number

# share reported victim
vht_el$prop_ipv_rep_victim <-
	vht_el$victim_q3_3a / vht_el$violence_q3_1f

# share reported perpetrator
vht_el$prop_ipv_rep_perp <-
	vht_el$perpetrator_q3_3b / vht_el$violence_q3_1f

# share reported by victim's family
vht_el$prop_ipv_rep_victfam <-
	vht_el$victim_fam_q3_3c / vht_el$violence_q3_1f

# share reported by perpetrators family
vht_el$prop_ipv_rep_perpfam <-
	vht_el$perpetrator_fam_q3_3d / vht_el$violence_q3_1f

# share reported by victim friends
vht_el$prop_ipv_rep_victfr <-
	vht_el$victim_friends_q3_3e / vht_el$violence_q3_1f

# share reported by perpetrators friends
vht_el$prop_ipv_rep_perpfr <-
	vht_el$perpetrator_friends_q3_3f / vht_el$violence_q3_1f

# share reported by neighbor

vht_el$prop_ipv_rep_neigh <-
	vht_el$neighbors_q3_3g / vht_el$violence_q3_1f



# Total violence incidents

vht_el$ipv_incidents <- NA
vht_el$ipv_incidents[vht_el$violence_q3_1f > 0] <-
	vht_el$violence_q3_1f[vht_el$violence_q3_1f > 0]
vht_el$ipv_incidents[vht_el$violence_q3_1f == 0 &
										 	vht_el$viol_unreported_q3_2 == 2] <- 0
vht_el$ipv_incidents[vht_el$violence_q3_1f == 0 &
										 	vht_el$viol_unreported_q3_2 == 1  &
										 	!is.na(vht_el$viol_unreported_q3_3b)] <-
	vht_el$viol_unreported_q3_3b[vht_el$violence_q3_1f == 0 &
															 	vht_el$viol_unreported_q3_2 == 1 &
															 	!is.na(vht_el$viol_unreported_q3_3b)]

# VHT reported violence incidents

vht_elcb$violence_q3_1f
vht_el$reporting_vht_ipv <- vht_el$violence_q3_1f
vht_el$reported_ipv_incidents <- vht_el$violence_q3_1f



# Categorical variable violence incidents (without reporting category)

# 0 = 0 reported incidents, 0 unreported incidents
# 1 = 0 reported incidents, unnreported incidents > 0
# 2 = reported incidents > 0

vht_el$incidents_categorical <- NA
vht_el$incidents_categorical[vht_el$violence_q3_1f == 0 &
														 	vht_el$viol_unreported_q3_2 == 2] <- 0
vht_el$incidents_categorical[vht_el$violence_q3_1f == 0 &
														 	vht_el$viol_unreported_q3_2 == 1] <- 1
vht_el$incidents_categorical[vht_el$violence_q3_1f > 0] <- 2


# Categorical variable violence incidents (incl. reporting category as in PAP)

# 0 = 0 reported incidents, 0 unreported incidents
# 1 = 0 reported incidents, unnreported incidents > 0
# 2 = reported incidents > 0 & all cases reported by victim
# 3 = reported incidents > 0 & at least one case not reported by victim
vht_el$incidents_reporting_categorical <- NA
vht_el$incidents_reporting_categorical[vht_el$violence_q3_1f == 0 &
																			 	vht_el$viol_unreported_q3_2 == 2] <- 0
vht_el$incidents_reporting_categorical[vht_el$violence_q3_1f == 0 &
																			 	vht_el$viol_unreported_q3_2 == 1] <- 1
vht_el$incidents_reporting_categorical[vht_el$violence_q3_1f > 0 &
																			 	(vht_el$victim_q3_3a == vht_el$violence_q3_1f)] <- 2
vht_el$incidents_reporting_categorical[vht_el$violence_q3_1f > 0 &
																			 	(vht_el$victim_q3_3a < vht_el$violence_q3_1f)] <- 3

# Unreported Violence

vht_elcb$viol_nomedic_q3_5
vht_el$unreported_ipv <- NA
vht_el$unreported_ipv[vht_el$viol_nomedic_q3_5 == 2] <- 0
vht_el$unreported_ipv[vht_el$viol_nomedic_q3_5 == 1] <- 1

# Norms and attitude outcomes

# IPV

vht_elcb$intervene_q8_9
vht_el$should_intervene <- NA
vht_el$should_intervene[vht_el$intervene_q8_9 == 2] <- 0
vht_el$should_intervene[vht_el$intervene_q8_9 == 1] <- 1

vht_elcb$friends_fam_q16_3
vht_el$ipv_efficacy <- NA
vht_el$ipv_efficacy[vht_el$friends_fam_q16_3 == 2] <- 0
vht_el$ipv_efficacy[vht_el$friends_fam_q16_3 == 1] <- 1

# VHT compliance ----------------------------------------------------------


#             Answer to 19.1)                       Answer to 19.2)
# complrs     1,2,3,4,5, or 6                       anything
# ind compl   0 but knew about screenings           Yes, friends and family
#             0 did not know about the screenings   Yes, friends
#             Don’t know                            Yes, family
#             Refuse to answer
# appr. nt    0 but knew about screenings           No
#                                                   Don't know
#                                                   refuse
# never-takr  0 did not know about screenings       No
#             Don't know                            Don't know
#             Refuse to answer                      Refuse

vht_elcb$films_q17_1
vht_elcb$other_attend_q17_2

vht_el$compliance[vht_el$films_q17_1 %in% 1:6] <- 1
vht_el$compliance[!(vht_el$films_q17_1 %in% 1:6) &
										vht_el$other_attend_q17_2 %in% 1:3] <- 2
vht_el$compliance[vht_el$films_q17_1 == 7 &
										(
											vht_el$other_attend_q17_2 == 4 |
												is.na(vht_el$other_attend_q17_2) |
												vht_el$other_attend_q17_2 %in% c(999, 888)
										)] <- 3
vht_el$compliance[vht_el$films_q17_1 == 8 &
										(
											vht_el$other_attend_q17_2 == 4 |
												is.na(vht_el$other_attend_q17_2) |
												vht_el$other_attend_q17_2 %in% c(999, 888)
										)] <- 4

# 1 NA

vht_el$compliance_label[vht_el$compliance == 1] <- "Complier"
vht_el$compliance_label[vht_el$compliance == 2] <-
	"Indirect Complier"
vht_el$compliance_label[vht_el$compliance == 3] <-
	"Apprised Never-Taker"
vht_el$compliance_label[vht_el$compliance == 4] <- "Never-Taker"
