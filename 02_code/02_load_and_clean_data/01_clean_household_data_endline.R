time_stamps <- names(el)[grepl(pattern = "\\btime_", x = names(el))]
bookends <- c("starttime", "endtime", "date_time")

for (variable in time_stamps) {
	el[variable] <-
		as.POSIXct(el[, variable], format = "%Y-%b-%d %I:%M:%S", tz = "Etc/GMT+3")
}
for (variable in bookends) {
	el[, variable] <-
		as.POSIXct(el[, variable], format = "%b %d, %Y %I:%M:%S %p", tz = "Etc/GMT+3")
}

rm(time_stamps, bookends, variable)

# Make uniform variables  for new respondents and compliers ---------------

el$id[!is.na(el$id_c)] <- el$id_c[!is.na(el$id_c)]

# Drop observations from pre-test -----------------------------------------

el <-
	filter(el,
				 el$starttime >= as.POSIXct("2017-05-18", format = "%Y-%m-%d", tz = "Etc/GMT+3"))

# Removing trial and duplicate observations ---------------------------------------------

remove <- c(
	"uuid:c024b956-8c27-44b6-a0e2-48e9c741cca9",
	"uuid:78544162-342d-44ec-820b-4a92840cbdce",
	"uuid:f719eb2d-a04b-4524-b355-02412d475daa",
	"uuid:090b28cc-0f5f-40c5-b742-040a782ea08e",
	"uuid:0c27c6a4-4bc6-456f-a26c-eb41e7701900",
	"uuid:b7e5e76d-d3c8-491e-b285-02e3b518e775",
	"uuid:c3dbf91e-3324-43bb-ab27-16928f40609a",
	"uuid:370dbe0b-5409-410b-b65e-d3ea7e63cb7b",
	"uuid:6dffb853-c3a8-42a2-afcb-1267e22cd322",
	"uuid:aacf0f3d-9b3f-4603-817f-3041d389129e",
	"uuid:91004a0f-fb40-4c30-948d-5faded8f6f1a",
	"uuid:57fbd6a8-0ca2-47c7-b4bb-159f9b9e2f32",
	"uuid:fab9246f-0eff-4a40-a911-5c6d6e0f7644"
	
)

# Delete trials
el <- el[-which(el$KEY %in% remove), ]

rm(remove)

# Remove refusals ---------------------------------------------------------

el <- filter(el, consent == 1)

# Recode ID variables -----------------------------------------------------

el$id[el$KEY == "uuid:c8e1a506-dc6c-485a-9583-75c8b320656f"] <-
	"t41006041"
el$id[el$KEY == "uuid:bddec60f-b60f-4b9f-92bc-586ba2bd4eb4"] <-
	"t41601041"
el$id[el$KEY == "uuid:f24474ba-3bc6-45bb-a820-35a1665556d2"] <-
	"t40432021"
el$id[el$KEY == "uuid:9c7273f8-0044-492e-a31b-2bdece0c8046"] <-
	"X40543381"
el$id[el$KEY == "uuid:8c97dfca-6a17-4d53-9ed0-5231cc396ccd"] <-
	"X40825371"
el$id[el$KEY == "uuid:60ad2279-fca4-4784-bfa5-9171931f4c0f"] <-
	"X41020101"
el$id[el$KEY == "uuid:d7ec0ae4-8c63-451b-bb6a-5b58bff097a3"] <-
	"X41020051"
el$id[el$KEY == "uuid:d9a632e8-5e2c-4fc3-ad87-9b537e4cd1e6"] <-
	"X41610041"
el$id[el$KEY == "uuid:d6e44332-af68-43f5-90d7-cfb307f3582b"] <-
	"X40911211"
el$id[el$KEY == "uuid:7a2a11dd-0f1b-4e75-8c9e-34f063bdef7e"] <-
	"X30606311"
el$id[el$KEY == "uuid:de908679-e283-46fd-89ec-d249aa8a468a"] <-
	"X30607391"
el$id[el$KEY == "uuid:9cbd80f4-9833-4b2f-9614-008a4032e32e"] <-
	"X30607331"
el$id[el$KEY == "uuid:70a13fd9-9c15-4da1-8969-f332ec287a00"] <-
	"X30605151"
el$id[el$KEY == "uuid:f9b34eee-0570-4e2d-b0bc-102501fb3d35"] <-
	"X30605481"
el$id[el$KEY == "uuid:65970c6d-c3ff-44ff-88ce-d7787b48739d"] <-
	"X30624191"
el$id[el$KEY == "uuid:7d35a7f4-0cba-4b62-a585-335550063bdf"] <-
	"X30611141"
el$id[el$KEY == "uuid:f79a93e1-8ef5-48b5-9ebe-86f579d7d160"] <-
	"X30611451"
el$id[el$KEY == "uuid:ccdbdd36-a510-47c2-9477-bfe68469eb7f"] <-
	"X30611431"
el$id[el$KEY == "uuid:4adcff00-ff12-4c28-8ede-1f64682dd801"] <-
	"X30933161"
el$id[el$KEY == "uuid:5e515bec-0550-4504-8fbf-e9f59a897d5f"] <-
	"X30933491"
el$id[el$KEY == "uuid:b6952062-0985-4320-bb08-e94af37dc279"] <-
	"X30933501"
el$id[el$KEY == "uuid:507be73f-fa41-4350-8e7f-fb2c3d0e0c06"] <-
	"X30914471"
el$id[el$KEY == "uuid:1bdd8134-2bb3-4249-a4a6-84408cd46cdb"] <-
	"X30914271"
el$id[el$KEY == "uuid:24f61fd5-053b-4f1c-b36a-f71b85d6c7ca"] <-
	"X30914061"
el$id[el$KEY == "uuid:e62c45d2-d3af-4f75-aa46-76543de032d5"] <-
	"X30813191"
el$id[el$KEY == "uuid:efe534c0-c1de-4467-827e-210e9b1b14ee"] <-
	"X30818071"
el$id[el$KEY == "uuid:479f8664-2f8f-4bbb-93a3-6331c5c101f8"] <-
	"X30611411"
el$id[el$KEY == "uuid:5484fd19-b40e-4374-a13b-198053e12ba2"] <-
	"X30901091"
el$id[el$KEY == "uuid:26d4f45e-102a-4a03-b29b-f81b54f88a4d"] <-
	"X30317461"
el$id[el$KEY == "uuid:d62206a1-02a2-4c5a-946d-c0d9a8fcf60c"] <-
	"X30115161"
el$id[el$KEY == "uuid:efec10ae-4152-42e4-ab8a-ba939f1d60a1"] <-
	"w41104051"
el$id[el$id == "t10308012"] <- "t10308011"


# Fixing mistakes ---------------------------------------------------------

el$age_q2_3[el$KEY == "uuid:1710869b-65e1-493b-9eef-0453cfbccb54"] <-
	60
el$resp_status[el$KEY == "uuid:8c97dfca-6a17-4d53-9ed0-5231cc396ccd"] <-
	2

ids_dont_know <- c(
	"10301121",
	"10402131",
	"10411381",
	"20302201",
	"20403231",
	"30816121",
	"30203401",
	"41441141",
	"41623251"
)

el$organizers_q18_1[el$id %in% ids_dont_know] <- "Doesn’t know"

rm(ids_dont_know)


el$organizers_q18_1[el$id %in% c("20601241",
																 "41344181",
																 "40432191",
																 "30914431")] <- "Doesn’t remember"


el$organizers_q18_1[el$id %in% c("20311021",
																 "30408931",
																 "30521271",
																 "40621441",
																 "41006011",
																 "40911051")] <- "IPA"


el$organizers_q18_1[el$id %in% c("30901361",
																 "41036891")] <- "An NGO/organization"


el$organizers_q18_1[el$id %in% c("20515011")] <- "PIA"


# Recoding Location Info --------------------------------------------------

el$tc_id[el$KEY == "uuid:a5baf371-3cb0-4304-952e-583b70b5a792"] <-
	76
el$tc_id[el$KEY == "uuid:920a9bcb-f4a0-4473-9382-2fb611b2fc47"] <-
	73
el$tc_id[el$KEY %in% c(
	"uuid:9547797f-fc37-43fb-841d-761cd1ab23bf",
	"uuid:81626612-e2a2-473a-b405-0301b814d1d2"
)] <-  81
el$tc_id[el$KEY == "uuid:5af94329-1b89-4180-94c6-6642f6970e3f"] <-
	84
el$tc_id[el$KEY == "uuid:4014e624-466e-4a97-a23a-0df885eba55b"] <-
	67
el$tc_id[el$KEY == "uuid:8c97dfca-6a17-4d53-9ed0-5231cc396ccd"] <-
	103
el$tc_id[el$KEY == "uuid:693033e6-674a-4a72-9d95-8348570e77e4"] <-
	79
el$tc_id[el$KEY == "uuid:a8565271-f0dc-4b07-bb9f-25c0809a7e99"] <-
	109
el$tc_id[el$KEY == "uuid:42865ff6-fc1b-4e67-aa05-0cd04f0edffc"] <-
	93
el$tc_id[el$KEY == "uuid:5904f696-96b4-4c45-b0df-d1d25c8db417"] <-
	7

# Generate variable for respondent category -------------------------------
el$category_string <- substr(el$id, 1, 1)

el$respondent_category[el$resp_status == 1] <- "Complier"
el$respondent_category[el$category_string %in% c("t", "T")] <-
	"Parent of new teen"
el$respondent_category[el$category_string %in% c("x", "X")] <-
	"Parent of teen complier"
el$respondent_category[el$category_string %in% c("w", "W")] <-
	"New woman"
