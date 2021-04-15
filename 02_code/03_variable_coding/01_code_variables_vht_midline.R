# vht_ml outcomes ------------------------------------------------------------

# Recoding some "other" responses for the reporting outcome

vht_mlcb$immunization_q3_1a
vht_mlcb$poisoning_q3_1b # general health
vht_mlcb$malaria_q3_1c # child rearing/child health
vht_mlcb$abortion_q3_1d
vht_mlcb$birth_control_q3_1e
vht_mlcb$violence_q3_1f
vht_mlcb$other_q3_1g
vht_mlcb$other_q3_1g_number

# Who asked about abortion

vht_mlcb$woman_q3_2a
vht_mlcb$husband_q3_2b
vht_mlcb$family_q3_2c
vht_mlcb$friends_q3_2d
vht_mlcb$neighbors_q3_2e
vht_mlcb$other_q3_2f
vht_mlcb$other_q3_2f_number

# Who reported violence

vht_mlcb$victim_q3_3a
vht_mlcb$perpetrator_q3_3b
vht_mlcb$victim_fam_q3_3c
vht_mlcb$perpetrator_fam_q3_3d
vht_mlcb$victim_friends_q3_3e
vht_mlcb$perpetrator_friends_q3_3f
vht_mlcb$neighbors_q3_3g
vht_mlcb$other_q3_3h
vht_mlcb$other_q3_3h_number

# share reported victim
vht_ml$prop_ipv_rep_victim <-
	vht_ml$victim_q3_3a / vht_ml$violence_q3_1f

# share reported perpetrator
vht_ml$prop_ipv_rep_perp <-
	vht_ml$perpetrator_q3_3b / vht_ml$violence_q3_1f

# share reported by victim's family
vht_ml$prop_ipv_rep_victfam <-
	vht_ml$victim_fam_q3_3c / vht_ml$violence_q3_1f

# share reported by perpetrators family
vht_ml$prop_ipv_rep_perpfam <-
	vht_ml$perpetrator_fam_q3_3d / vht_ml$violence_q3_1f

# share reported by victim friends
vht_ml$prop_ipv_rep_victfr <-
	vht_ml$victim_friends_q3_3e / vht_ml$violence_q3_1f

# share reported by perpetrators friends
vht_ml$prop_ipv_rep_perpfr <-
	vht_ml$perpetrator_friends_q3_3f / vht_ml$violence_q3_1f

# share reported by neighbor

vht_ml$prop_ipv_rep_neigh <-
	vht_ml$neighbors_q3_3g / vht_ml$violence_q3_1f

# Total violence incidents

vht_ml$ipv_incidents <- NA
vht_ml$ipv_incidents[vht_ml$violence_q3_1f > 0] <-
	vht_ml$violence_q3_1f[vht_ml$violence_q3_1f > 0]
vht_ml$ipv_incidents[vht_ml$violence_q3_1f == 0 &
										 	vht_ml$viol_unreported_q3_2 == 2] <- 0
vht_ml$ipv_incidents[vht_ml$violence_q3_1f == 0 &
										 	vht_ml$viol_unreported_q3_2 == 1  &
										 	!is.na(vht_ml$viol_unreported_q3_3b)] <-
	vht_ml$viol_unreported_q3_3b[vht_ml$violence_q3_1f == 0 &
															 	vht_ml$viol_unreported_q3_2 == 1 &
															 	!is.na(vht_ml$viol_unreported_q3_3b)]

# vht_ml reported violence incidents

vht_mlcb$violence_q3_1f
vht_ml$reporting_vht_ipv <- vht_ml$violence_q3_1f
vht_ml$reported_ipv_incidents <- vht_ml$violence_q3_1f



# Categorical variable violence incidents (without reporting category)

# 0 = 0 reported incidents, 0 unreported incidents
# 1 = 0 reported incidents, unnreported incidents > 0
# 2 = reported incidents > 0

vht_ml$incidents_categorical <- NA
vht_ml$incidents_categorical[vht_ml$violence_q3_1f == 0 &
														 	vht_ml$viol_unreported_q3_2 == 2] <- 0
vht_ml$incidents_categorical[vht_ml$violence_q3_1f == 0 &
														 	vht_ml$viol_unreported_q3_2 == 1] <- 1
vht_ml$incidents_categorical[vht_ml$violence_q3_1f > 0] <- 2


# Categorical variable violence incidents (incl. reporting category as in PAP)

# 0 = 0 reported incidents, 0 unreported incidents
# 1 = 0 reported incidents, unnreported incidents > 0
# 2 = reported incidents > 0 & all cases reported by victim
# 3 = reported incidents > 0 & at least one case not reported by victim
vht_ml$incidents_reporting_categorical <- NA
vht_ml$incidents_reporting_categorical[vht_ml$violence_q3_1f == 0 &
																			 	vht_ml$viol_unreported_q3_2 == 2] <- 0
vht_ml$incidents_reporting_categorical[vht_ml$violence_q3_1f == 0 &
																			 	vht_ml$viol_unreported_q3_2 == 1] <- 1
vht_ml$incidents_reporting_categorical[vht_ml$violence_q3_1f > 0 &
																			 	(vht_ml$victim_q3_3a == vht_ml$violence_q3_1f)] <-
	2
vht_ml$incidents_reporting_categorical[vht_ml$violence_q3_1f > 0 &
																			 	(vht_ml$victim_q3_3a < vht_ml$violence_q3_1f)] <-
	3


# Unreported Violence

vht_mlcb$viol_nomedic_q3_5
vht_ml$unreported_ipv <- NA
vht_ml$unreported_ipv[vht_ml$viol_nomedic_q3_5 == 2] <- 0
vht_ml$unreported_ipv[vht_ml$viol_nomedic_q3_5 == 1] <- 1

# Norms and attitude outcomes

# IPV

vht_mlcb$intervene_q8_9
vht_ml$should_intervene <- NA
vht_ml$should_intervene[vht_ml$intervene_q8_9 == 2] <- 0
vht_ml$should_intervene[vht_ml$intervene_q8_9 == 1] <- 1

vht_mlcb$friends_fam_q16_3
vht_ml$ipv_efficacy <- NA
vht_ml$ipv_efficacy[vht_ml$friends_fam_q16_3 == 2] <- 0
vht_ml$ipv_efficacy[vht_ml$friends_fam_q16_3 == 1] <- 1


# vht_ml compliance ----------------------------------------------------------


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

vht_mlcb$films_q17_1
vht_mlcb$other_attend_q17_2

vht_ml$compliance[vht_ml$films_q17_1 %in% 1:6] <- 1
vht_ml$compliance[!(vht_ml$films_q17_1 %in% 1:6) &
										vht_ml$other_attend_q17_2 %in% 1:3] <- 2
vht_ml$compliance[vht_ml$films_q17_1 == 7 &
										(vht_ml$other_attend_q17_2 == 4 |
										 	is.na(vht_ml$other_attend_q17_2))] <- 3
vht_ml$compliance[vht_ml$films_q17_1 == 8 &
										(vht_ml$other_attend_q17_2 == 4 |
										 	is.na(vht_ml$other_attend_q17_2))] <- 4


vht_ml$compliance_label[vht_ml$compliance == 1] <- "Complier"
vht_ml$compliance_label[vht_ml$compliance == 2] <-
	"Indirect Complier"
vht_ml$compliance_label[vht_ml$compliance == 3] <-
	"Apprised Never-Taker"
vht_ml$compliance_label[vht_ml$compliance == 4] <- "Never-Taker"

