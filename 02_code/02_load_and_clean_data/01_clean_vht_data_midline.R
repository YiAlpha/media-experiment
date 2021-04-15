# Filter out one observation that is mostly missing
vht_ml <- filter(vht_ml, !is.na(id))

# Turn string into integer
vht_ml$machete_q6_2 <- as.numeric(vht_ml$machete_q6_2)

# Recoding IDs ------------------------------------------------------------

vht_ml$id[vht_ml$KEY == "uuid:43607c8d-2c76-4d61-a287-51c36fcba856"] <-
	"v404321"
vht_ml$id[vht_ml$KEY == "uuid:b5cb13cf-d93c-4d6f-b94c-a8014d3b810f"] <-
	"v404322"
vht_ml$id[vht_ml$KEY == "uuid:a052b4ae-397e-4e2a-9a85-58a9c07e6c14"] <-
	"v404323"
vht_ml$id[vht_ml$KEY == "uuid:886c8bbf-e245-4576-a6c5-c1179e1d01c1"] <-
	"v410241"
vht_ml$id[vht_ml$KEY == "uuid:f219c1f9-852f-4b94-966d-caa2943ef1da"] <-
	"v410242"
vht_ml$id[vht_ml$KEY == "uuid:c24adbad-69e3-4230-ab00-85e22b8ee706"] <-
	"v403451"
vht_ml$id[vht_ml$KEY == "uuid:64ad9964-c910-4dfd-b7d5-78f550affd61"] <-
	"v403452"
vht_ml$id[vht_ml$KEY == "uuid:22b693bf-bdec-49fa-a5ca-ba75d20cdb9d"] <-
	"v413372"
vht_ml$id[vht_ml$KEY == "uuid:3f351aa6-e5f6-4c68-a4fa-db51a8394958"] <-
	"v414303"
vht_ml$id[vht_ml$KEY == "uuid:1872d4f5-5e7c-4ffa-8c5c-588acc3334e8"] <-
	"v414304"
vht_ml$id[vht_ml$KEY == "uuid:367d81e1-2231-4c38-86f4-86af05676eaf"] <-
	"v306242"
vht_ml$id[vht_ml$KEY == "uuid:b6840a88-7bde-4091-a420-680af9c1142e"] <-
	"v417141"
vht_ml$id[vht_ml$KEY == "uuid:bc3e0b6e-f567-4491-90e5-1e71a68a84e4"] <-
	"v404273"
vht_ml$id[vht_ml$KEY == "uuid:99b0fafa-a9ee-4743-8962-b61e240f0db5"] <-
	"v405435"
vht_ml$id[vht_ml$KEY == "uuid:d9836322-6106-450d-bade-6bb867ec5db6"] <-
	"v413443"
vht_ml$id[vht_ml$KEY == "uuid:91493134-945f-46d7-bd7b-33ac969a8bca"] <-
	"v412403"
vht_ml$id[vht_ml$KEY == "uuid:e758e507-99dd-46b2-a50c-e4f60e483afd"] <-
	"v308131"
vht_ml$id[vht_ml$KEY == "uuid:f16beed5-859e-41d6-bc13-e8055ebf98a1"] <-
	"v306113"
vht_ml$id[vht_ml$KEY == "uuid:0fefed14-a685-42df-ad58-e010dba98eb5"] <-
	"v106061"
vht_ml$id[vht_ml$KEY == "uuid:35c1f18a-26e0-4acb-9499-b5f83fdcf845"] <-
	"v205092"
vht_ml$id[vht_ml$KEY == "uuid:efacfd7f-8fdf-4907-8272-abd4f4efbefa"] <-
	"v411381"
vht_ml$id[vht_ml$KEY == "uuid:55ebd091-40bb-4783-8c0e-655035b36d45"] <-
	"v401421"
vht_ml$id[vht_ml$KEY == "uuid:6121a868-5b42-4ead-ae00-e7def9b4105b"] <-
	"v401422"
vht_ml$id[vht_ml$KEY == "uuid:135a9fce-786f-4d2f-8be4-67ffd31ddbf4"] <-
	"v401423"
vht_ml$id[vht_ml$KEY == "uuid:cbc0d370-123e-4501-8dfc-656b95bb1352"] <-
	"v401424"
vht_ml$id[vht_ml$KEY == "uuid:a5d10355-fad4-49a6-9904-e8b88c05760c"] <-
	"v401351"
vht_ml$id[vht_ml$KEY == "uuid:a98f7960-70ae-438a-8276-2fbf99e1dfd9"] <-
	"v401352"
vht_ml$id[vht_ml$KEY == "uuid:6f6c6678-ae02-428c-8369-83737dab611a"] <-
	"V410471"
vht_ml$id[vht_ml$KEY == "uuid:3c5f544e-7843-4f21-a657-457246559c0f"] <-
	"V410472"
vht_ml$id[vht_ml$id == "V234567"] <- "V414411"
vht_ml$id[vht_ml$KEY == "uuid:5e68f04e-5185-4c01-8dfa-948fd666005d"] <-
	"V310252"
vht_ml$id[vht_ml$KEY == "uuid:535457bf-6327-46e7-927a-714c61b49fe9"] <-
	"V310253"
vht_ml$id[vht_ml$id == "V123455"] <- "V205151"
vht_ml$id[vht_ml$id == "V224466"] <- "V409111"
vht_ml$id[vht_ml$KEY == "uuid:0036960a-7d7d-4353-8337-28fc4f9ae6ea"] <-
	"V203112"

keys <- c(
	"uuid:ecaa1d3c-4481-4cf6-ad20-6ef4f197283a",
	"uuid:2df4c95c-1a75-4698-b203-e5a50b753555",
	"uuid:ffd0317b-10a3-43cc-afcc-fb322d626a1b",
	"uuid:ab13a13d-ce66-4568-82f0-805d9926d98a",
	"uuid:020cd1ac-b256-4203-a66c-ed4b4ce7e42f"
)

vht_ml$tc_id[vht_ml$KEY %in% keys] <- 89

vht_ml$tc_id[vht_ml$KEY %in% c(
	"uuid:b2ad80b3-08ef-4514-ba01-6531ecef3602",
	"uuid:004d8553-ae86-41e5-ab43-bcc95aa07c98"
)] <- 104
vht_ml$tc_id[vht_ml$KEY == "uuid:7f41cead-1be6-4d43-a661-3b83e48572a5"] <-
	69
vht_ml$tc_id[vht_ml$KEY == "uuid:388dfde3-88f5-4f72-be3b-ce54b3da2a5f"] <-
	56
vht_ml$tc_id[vht_ml$KEY == "uuid:33ae0666-003f-4446-a10a-674181ff5fbc"] <-
	93
vht_ml$tc_id[vht_ml$KEY == "uuid:efacfd7f-8fdf-4907-8272-abd4f4efbefa"] <-
	106

vht_ml$tc_id[vht_ml$id == "V414411"] <- 109

rm(keys)

# Fixing mistakes ---------------------------------------------------------
vht_ml$gender_q2_1[vht_ml$gender_q2_1 == 93] <- 1
vht_ml$gender_q2_1[vht_ml$gender_q2_1 == 41] <- 2

vht_ml$unit_q2_3vht_ml[vht_ml$unit_q2_3vht == 4] <- 1
vht_ml$unit_q2_3vht_ml[vht_ml$unit_q2_3vht == 89] <- 2
vht_ml$unit_q2_3naba[vht_ml$unit_q2_3naba == 4] <- 1
vht_ml$unit_q2_3naba[vht_ml$unit_q2_3naba == 89] <- 2

vht_ml$unit_q2_7[vht_ml$unit_q2_7 == 31] <- 1
vht_ml$unit_q2_7[vht_ml$unit_q2_7 == 71] <- 2
vht_ml$unit_q2_7[vht_ml$unit_q2_7 == 104] <- 3
vht_ml$unit_q2_7b[vht_ml$unit_q2_7b == 31] <- 1
vht_ml$unit_q2_7b[vht_ml$unit_q2_7b == 71] <- 2
vht_ml$unit_q2_7b[vht_ml$unit_q2_7b == 104] <- 3

vht_ml$visits_q2_9number[vht_ml$visits_q2_9 == "3 times a week"] <-
	3
vht_ml$visits_q2_9units[vht_ml$visits_q2_9 == "3 times a week"] <- 2

vht_ml$visits_q2_9number[vht_ml$visits_q2_9 == "3times a month"] <-
	3
vht_ml$visits_q2_9units[vht_ml$visits_q2_9 == "3times a month"] <- 3

vht_ml$visits_q2_9number[vht_ml$visits_q2_9 == "Once in a month"] <- 1
vht_ml$visits_q2_9units[vht_ml$visits_q2_9 == "Once in a month"] <- 3


vht_ml$visits_q2_9number[vht_ml$visits_q2_9 == "Once in two months"] <-
	6
vht_ml$visits_q2_9units[vht_ml$visits_q2_9 == "Once in two months"] <-
	4

# Fixing mistakes ---------------------------------------------------------

vht_ml$id[vht_ml$violence_q3_1f > 0 &
						!is.na(vht_ml$viol_unreported_q3_2)]

vht_ml$viol_unreported_q3_2[vht_ml$id == "V414411"] <- NA
vht_ml$abo_nomedic_q3_4[vht_ml$id == "V414411"] <- 2
