# Reformatting date variables ---------------------------------------------

time_stamps <- names(ml)[grepl(pattern = "\\btime_", x = names(ml))]
bookends <- c("starttime", "endtime", "date_time")

for (variable in time_stamps) {
	ml[variable] <-
		as.POSIXct(ml[, variable], format = "%Y-%b-%d %I:%M:%S", tz = "Etc/GMT+3")
}
for (variable in bookends) {
	ml[, variable] <-
		as.POSIXct(ml[, variable], format = "%b %d, %Y %I:%M:%S %p", tz = "Etc/GMT+3")
}

rm(time_stamps, bookends, variable)

# Removing trial observations ---------------------------------------------

trials <- c(
	"uuid:491f1edc-69bc-4d43-9b53-5b2b9e179a50",
	"uuid:f2c36267-c8c7-4255-b984-ad6d3b3af29b",
	"uuid:3d3388dd-27f0-4146-896f-5611bdf11b6d",
	"uuid:f438f0a2-0e27-45d8-88b0-37dcccf9b023",
	"uuid:49743a8a-6b18-456c-8788-78dd0f578b13",
	"uuid:496860fb-e6dc-4c40-9f0a-b4f797e2798b",
	"uuid:4499291e-62f5-47d7-95c3-b25a6017ae72",
	"uuid:c6058aab-1d97-4dd2-b7d8-f3bcdcc88e30",
	"uuid:d630cffe-aef1-434f-a54c-02b5cb586e83",
	"uuid:321d6bc5-59a9-4ff2-8c97-0647c625cc52",
	"uuid:c22a129e-98fe-4f57-bc80-18d5a2b6f502",
	"uuid:6c790d4d-b9f7-4b5b-9189-40b284ac5993",
	"uuid:70221566-5a56-40fb-9044-5702e8e2a597",
	"uuid:f98ae107-0b9a-4aca-8e2a-c57406b18a32",
	"uuid:df97fdf8-4174-4ed6-bcbd-ecad33265dd4",
	"uuid:a3a5528e-c4d2-4e19-b3f2-7ad1b467b5db",
	"uuid:6ef5aaed-6ad6-4ae0-b4f8-577e0e40ca59",
	"uuid:0b32a63e-013c-483f-874a-70e1de6bcb71",
	"uuid:babbac3d-2eee-4390-a60d-b7db8a95eec2",
	"uuid:07ade1e9-a794-472b-890b-8ad084ae375b",
	"uuid:4b3412a4-71ba-4398-9958-759611ae45af",
	"uuid:5a00e801-9052-4caf-a275-f455e89c2135",
	"uuid:92f000cf-56f7-40e9-8e75-ef2bd834b119",
	"uuid:5cd8f810-7f1e-4225-9efe-9f08c2e10131",
	"uuid:e55f782d-58ed-41fd-bf8b-052582af1b1a",
	"uuid:7585f31c-0827-4e4f-b821-20816fe96809",
	"uuid:069b9c3b-0235-4adb-8919-2816e17075e9",
	"uuid:2d99ae1e-4b9c-4a95-9f7d-7898824310e8",
	"uuid:0e50dd1e-6c5e-4b1d-a2c6-27e7079c343e",
	"uuid:0d4a3839-d4eb-434a-8d63-7e2d9b60b51e",
	"uuid:d9724631-0841-434d-af63-7a681a8faef9",
	"uuid:f631f469-bc3d-4549-9711-e0bd03ae99cd",
	"uuid:cb17add9-5005-4459-b1d0-326ca494cee1",
	"uuid:7d4075af-e3e5-421e-9678-157071adc114",
	"uuid:a10411d5-e69a-4efd-90af-47a8e0e351c6",
	"uuid:121ed9ca-84ec-49ac-a953-07b4247bd3ad",
	"uuid:0e0e4e35-3599-44ba-af1c-e2810e0bffc7",
	"uuid:799ec207-1c6e-4286-aeee-970be894222b"
)

# Delete trials
ml <- ml[-which(ml$KEY %in% trials),]

rm(trials)

# Recoding IDs ------------------------------------------------------------

ml$id[ml$KEY == "uuid:30fb0480-6682-44a5-a7c7-52e39b573129"] <-
	40432121
ml$id[ml$KEY == "uuid:accd15a3-8584-4eec-85f0-a183e4bd0286"] <-
	40432411
ml$id[ml$KEY == "uuid:497d0372-1087-429e-8b48-894e270d4d75"] <-
	41047281
ml$id[ml$KEY == "uuid:7f0236ae-6247-4e1c-8fb4-23c5068c3e3f"] <-
	41047411
ml$id[ml$KEY == "uuid:986e7e79-77d7-4bab-9a6d-7a23ee4ce78a"] <-
	41020411
ml$id[ml$KEY == "uuid:4877718b-4d4b-49e1-b3dc-288efb1db07c"] <-
	41139511
ml$id[ml$KEY == "uuid:46e23605-7fa1-4059-a906-39185d94487f"] <-
	41139521
ml$id[ml$KEY == "uuid:6344771f-9407-4537-9544-6ca4d0acc19e"] <-
	41139531
ml$id[ml$KEY == "uuid:14a853df-7463-4eb2-a631-7012896ffc2c"] <-
	41139541
ml$id[ml$KEY == "uuid:bc118d1f-3141-4101-a0e9-d9e5551c2c51"] <-
	41139551
ml$id[ml$KEY == "uuid:3a221bbd-ca19-47b8-a433-e371fa73a3e0"] <-
	41139561
ml$id[ml$KEY == "uuid:b4fcff27-9059-40c7-ae08-4c519bfdc5f2"] <-
	41139571
ml$id[ml$KEY == "uuid:5d4ecd19-483e-44d1-83e0-3863c3c4dd8a"] <-
	41139581
ml$id[ml$KEY == "uuid:d4fd3b45-a1fd-4f76-8df9-940b8499f16c"] <-
	41139591
ml$id[ml$KEY == "uuid:184ce734-0a82-49c5-9a80-29fac62db076"] <-
	40427521
ml$id[ml$KEY == "uuid:c60be347-a534-464e-86f9-41fd08e310b2"] <-
	41115601
ml$id[ml$KEY == "uuid:3ff9435b-67b0-4cc8-a4d8-5de680a2757a"] <-
	41430291
ml$id[ml$KEY == "uuid:ae57fb09-8992-4547-9b75-7873f9ce2ab3"] <-
	40207511
ml$id[ml$KEY == "uuid:1aa58c60-baf3-46e5-b356-6e515fca9048"] <-
	40216311
ml$id[ml$KEY == "uuid:66c48d8a-4b91-4b6e-933f-359fed675aa5"] <-
	40216251
ml$id[ml$KEY == "uuid:3f8786c4-a2c3-4e05-9263-d961700f72c1"] <-
	41115901
ml$id[ml$KEY == "uuid:91d46b02-5849-43da-a692-0f228f654690"] <-
	41115921
ml$id[ml$KEY == "uuid:313ebfc7-83c9-4f56-bf5d-ce39728daa64"] <-
	40621261
ml$id[ml$KEY == "uuid:cbda9854-379e-46ef-b861-6fc3971003fa"] <-
	40621311
ml$id[ml$KEY == "uuid:8d61e70e-4571-4774-9f08-6a04e33f2d5e"] <-
	41139601
ml$id[ml$KEY == "uuid:1d52f74f-327c-4d24-a8e7-43d843f717eb"] <-
	41714351
ml$id[ml$KEY == "uuid:6b3f7427-fa37-430d-a148-2e4495b6e4de"] <-
	40926111
ml$id[ml$KEY == "uuid:586e5bbc-ddec-4170-9dee-210291ef9b34"] <-
	41006501
ml$id[ml$KEY == "uuid:9d945b61-f0ac-47c4-a735-1a9f756350fa"] <-
	41601481
ml$id[ml$KEY == "uuid:c32fe6d5-b177-4f1f-ada9-bfb5c486f509"] <-
	41601061
ml$id[ml$KEY == "uuid:6b3f7427-fa37-430d-a148-2e4495b6e4de"] <-
	40926111
ml$id[ml$KEY == "uuid:e899a473-65c2-4096-b73d-3e67aee169a3"] <-
	41115931
ml$id[ml$KEY == "uuid:ca78b892-5927-4dc6-a094-18f257343545"] <-
	41610611
ml$id[ml$KEY == "uuid:c8616dd0-d76f-41c9-8696-e66303bb9181"] <-
	41714421
ml$id[ml$KEY == "uuid:2c9c3999-671c-4073-aab4-5996e8a59c7e"] <-
	40346261
ml$id[ml$KEY == "uuid:825de3d6-deb4-4b23-9359-560034ed75f3"] <-
	40329181
ml$id[ml$KEY == "uuid:adef0a1d-69c2-43df-bcbe-048421f1b70f"] <-
	40329311
ml$id[ml$KEY == "uuid:636b5221-dc2f-43b2-b720-716f28657c43"] <-
	40345611
ml$id[ml$KEY == "uuid:cdcde092-fa4b-4c5a-9b29-53d4591c182f"] <-
	41610621
ml$id[ml$KEY == "uuid:c2cdd49c-a209-494d-944e-1d4d1c253fbd"] <-
	41610381
ml$id[ml$KEY == "uuid:2cbc1fbe-4ac6-4911-89ae-6461febdbd00"] <-
	41240511
ml$id[ml$KEY == "uuid:f006171c-521d-4856-bce1-5c1dbd6bb091"] <-
	41219481
ml$id[ml$KEY == "uuid:bd5f5f51-5f11-4404-a9ff-10a0a827a9c5"] <-
	41522511
ml$id[ml$KEY == "uuid:2c15ceb4-4b26-4f10-9b62-9d6f2b8e5485"] <-
	40345461
ml$id[ml$KEY == "uuid:a0aecb3c-1e74-450d-8afc-7ae1bca73dd2"] <-
	40135191
ml$id[ml$KEY == "uuid:64224ee8-d42e-4555-b351-5f8b4340d970"] <-
	40135041
ml$id[ml$KEY == "uuid:6d95dbc6-5cc8-4ebc-b155-9f39a400aed2"] <-
	40328261
ml$id[ml$KEY == "uuid:560922b9-1fa9-4847-999c-a2d74943b32e"] <-
	40328511
ml$id[ml$KEY == "uuid:3ae90603-6bac-4e5d-b94c-16e8893e2ad9"] <-
	30624331
ml$id[ml$KEY == "uuid:51928401-7177-4560-9f4f-bff535d0996d"] <-
	30624151
ml$id[ml$KEY == "uuid:3cae54af-eb6b-4745-b3c6-98a695dfa6cb"] <-
	31102281
ml$id[ml$KEY == "uuid:bfc543fb-072c-459b-b9e2-e61e71f59a7f"] <-
	31025481
ml$id[ml$KEY == "uuid:553f2091-d976-49d8-9831-fad825e29827"] <-
	30709061
ml$id[ml$KEY == "uuid:0db16bf3-837c-40a3-9b6f-a8ae19a6e1ae"] <-
	30606061
ml$id[ml$KEY == "uuid:27027d6f-f2a6-42ca-9748-321fe7b50b30"] <-
	30203481
ml$id[ml$KEY == "uuid:5d641683-e7f8-4942-85c7-18a69ad0eefc"] <-
	30203041
ml$id[ml$KEY == "uuid:5b955afa-905e-4390-8516-663f73a87cc3"] <-
	30712061
ml$id[ml$KEY == "uuid:311c2c2e-2dee-4392-b989-da3d01c68adf"] <-
	31132231
ml$id[ml$KEY == "uuid:c6de38b4-2fd6-4eb1-825a-b6dd84d26177"] <-
	31132011
ml$id[ml$KEY == "uuid:5328e531-7bdb-482b-b182-e177ca88103a"] <-
	31025301
ml$id[ml$KEY == "uuid:5908f375-8b4f-42c2-8bcd-3829916f77f3"] <-
	41709491
ml$id[ml$KEY == "uuid:28631fe2-cb42-4c1b-876e-3b072eb20d6c"] <-
	41709501
ml$id[ml$KEY == "uuid:7faea7ba-f2d6-42d6-86ba-e45c5eb46b26"] <-
	30818061
ml$id[ml$KEY == "uuid:880a816f-aaf9-4e11-9e6e-b2a30946e122"] <-
	30408191
ml$id[ml$KEY == "uuid:32cb3d24-b11d-4cec-87e1-83231e278944"] <-
	30529211
ml$id[ml$KEY == "uuid:6b7ba34f-4674-4ee5-88dc-c0926930e84d"] <-
	30317231
ml$id[ml$KEY == "uuid:ce863eb0-598c-46da-84c0-4e721fed1040"] <-
	30317171
ml$id[ml$KEY == "uuid:2dbe24ce-18ee-4700-9a36-16fd9f6399d1"] <-
	40908421
ml$id[ml$KEY == "uuid:92f82100-a115-4a38-954e-b6d1d53a28a8"] <-
	40534201
ml$id[ml$KEY == "uuid:46c9d1f9-b1df-4a76-93a0-453c3f468d25"] <-
	10312481
ml$id[ml$KEY == "uuid:a603fce3-8dae-41c8-9967-a111eedea944"] <-
	31028491
ml$id[ml$KEY == "uuid:f145b17d-cf22-43b5-833c-5cf3d0855a4c"] <-
	10308531
ml$id[ml$KEY == "uuid:2a36d515-e40e-4965-a414-cc55490106a4"] <-
	10214451
ml$id[ml$KEY == "uuid:e824bb61-2d48-4438-b8e1-9d54015d4494"] <-
	10713281
ml$id[ml$KEY == "uuid:c9e3d3bb-dfef-433c-8831-54390a97a579"] <-
	10710281
ml$id[ml$KEY == "uuid:a5f18e79-ee81-4932-97af-7940a1379f72"] <-
	20516331
ml$id[ml$KEY == "uuid:889224a1-ba6b-4e97-a3e4-fcd158e2fa85"] <-
	20516131
ml$id[ml$KEY == "uuid:a2e915da-acb9-4050-8eac-05b1593a29dd"] <-
	20505201
ml$id[ml$KEY == "uuid:aed6db20-2e7b-438c-8672-8fce6f7b848f"] <-
	20509391
ml$id[ml$KEY == "uuid:bb98d500-623a-436a-9716-84d71d404edf"] <-
	10402421
ml$id[ml$KEY == "uuid:d4da6618-689c-4c69-be8b-ea219de2fa91"] <-
	10214341
ml$id[ml$KEY == "uuid:b03d7641-e99a-4141-8af8-95690ce58f0d"] <-
	20403091
ml$id[ml$KEY == "uuid:fdffcca5-f903-4211-8314-1a7a946c1ed6"] <-
	20311441
ml$id[ml$KEY == "uuid:ad216561-45e7-4e60-a4a5-5fd3887877d6"] <-
	20311171
ml$id[ml$KEY == "uuid:55bdec99-f8b3-44d3-803e-bebf2db62273"] <-
	20212281
ml$id[ml$KEY == "uuid:7b105d43-7db5-422a-9a0c-8fcae2a3f898"] <-
	20212391
ml$id[ml$KEY == "uuid:26d87e33-1d06-40f8-810e-b550ec87bdbf"] <-
	20212411
ml$id[ml$KEY == "uuid:36fd1f35-1cf1-4f69-a4f9-60c8cc38af1b"] <-
	20413331
ml$id[ml$KEY == "uuid:60361280-6859-4b4b-b384-1996121efc7c"] <-
	20413141
ml$id[ml$KEY == "uuid:5fad8f69-9133-449c-809c-6d4e7f0a8af3"] <-
	20601181
ml$id[ml$KEY == "uuid:b55335ad-be4a-491f-ba66-140ac794078a"] <-
	40825911
ml$id[ml$KEY == "uuid:eda17908-e175-4e6f-a78b-15d29438ca3c"] <-
	41344931
ml$id[ml$KEY == "uuid:f850728b-1f4c-47d2-9946-837aa3968131"] <-
	30408891

# Recoding ‘other’ responses ----------------------------------------------

mlcb$rel_q2_4
ml$rel_q2_4[grepl(pattern = "Ali nakyeyombekedde|She's single|She single|She is single|single",
									x = ml$q2_4_oth,
									ignore.case = TRUE)] <- 1

ml$rel_q2_4[grepl(pattern = "School Dormitory",
									x = ml$q2_4_oth,
									ignore.case = TRUE)] <- 1

mlcb$status_q2_5
ml$status_q2_5[grepl(pattern = "widow",
										 x = ml$q2_5_oth,
										 ignore.case = TRUE)] <- 6


ml$lang_q2_23[ml$key == "uuid:2d1d7c2c-e167-4462-aa9a-66813cf565c5" |
								ml$key == "uuid:5de168bc-22fd-48e0-a501-acb5302b7c0d"] <- 26
ml$lang_q2_23[ml$key == "uuid:c351a962-359a-446b-88e9-a643f19342df"] <-
	3

mlcb$other_lang_q2_24
ml$other_lang_q2_24_4[ml$q2_24_oth == "Lwamba"] <- 24

ml$other_lang_q2_24_4[grepl(pattern = "Kakuua",
														x = ml$q2_24_oth,
														ignore.case = TRUE)] <- 9
ml$other_lang_q2_24_4[grepl(pattern = "Lululu|Luruuru",
														x = ml$q2_24_oth,
														ignore.case = TRUE)] <- 28

mlcb$tribe_q2_25
ml$tribe_q2_25[grepl(pattern = "Mulundi|Murundi", x = ml$q2_25_oth)] <-
	34
ml$tribe_q2_25[grepl(pattern = "Mukonjo|Bakonjo|Bakonjjo", x = ml$q2_25_oth)] <-
	17
ml$tribe_q2_25[grepl(pattern = "Mukakwa|Kakuua|Mukakwa", x = ml$q2_25_oth)] <-
	8
ml$tribe_q2_25[grepl(pattern = "Mululu|Muluulu|Muruuru", x = ml$q2_25_oth)] <-
	20
ml$tribe_q2_25[grepl(pattern = "Mulalo", x = ml$q2_25_oth)] <- 21
ml$tribe_q2_25[grepl(pattern = "Mukochi", x = ml$q2_25_oth)] <- 1

mlcb$activity_q3_1
ml$activity_q3_1[grepl(pattern = "Charcoal burning",
											 x = ml$q3_1_oth,
											 ignore.case = TRUE)] <- 4
ml$activity_q3_1[grepl(pattern = "Roasting meat|Drugshop|Selling clothes|Trading in crops|Trader|Drug shop|assistant working in a drug shop|Selling  cassavaq|Phone accesory|Selling electricals|Selling clothes in a boutique|Businessman in onions|Mutembeyi|Kusiika chapati|Produce trader|Boutique|Drug Shop keeper|Drug shop owner|Butchery|Butcher man|He is a butcher",
											 x = ml$q3_1_oth,
											 ignore.case = TRUE)] <- 7
ml$activity_q3_1[grepl(pattern = "Tailor|Sewing",
											 x = ml$q3_1_oth,
											 ignore.case = TRUE)] <- 11
ml$activity_q3_1[grepl(pattern = "Boda|Rider|driver",
											 x = ml$q3_1_oth,
											 ignore.case = TRUE)] <- 13
ml$activity_q3_1[grepl(pattern = "Teacher|Teaching",
											 x = ml$q3_1_oth,
											 ignore.case = TRUE)] <- 16

ml$activity_q3_1[grepl(pattern = "Hair Stylist|Hair dresser|Working in my salon|Hair cutting",
											 x = ml$q3_1_oth,
											 ignore.case = TRUE)] <- 7
ml$activity_q3_1[grepl(pattern = "saloon|Bar attendant|Selling drink|\\bbar\\b",
											 x = ml$q3_1_oth,
											 ignore.case = TRUE)] <- 9
ml$activity_q3_1[grepl(pattern = "Produce buying|Selling  cassava|Dealing in gold selling|Chapati making|Kusubula  bbirime|Making pancakes|Okutunda ebyokunywa|Produce trading|working in a drug shop|Music cutting and video library|Mobile money|Petrol station|Drug shop assistant",
											 x = ml$q3_1_oth,
											 ignore.case = TRUE)] <- 7
ml$activity_q3_1[grepl(pattern = "Musomesaw abantu bakulu|Teacher(Agriculture)",
											 x = ml$q3_1_oth,
											 ignore.case = TRUE)] <- 16
ml$activity_q3_1[grepl(pattern = "Muvubi|Fisherman",
											 x = ml$q3_1_oth,
											 ignore.case = TRUE)] <- 19
ml$activity_q3_1[grepl(pattern = "Repairing boats|Shoe repairer",
											 x = ml$q3_1_oth,
											 ignore.case = TRUE)] <- 10
ml$activity_q3_1[grepl(pattern = "Buider|builder",
											 x = ml$q3_1_oth,
											 ignore.case = )] <- 6

mlcb$religion_q14_1
ml$religion_q14_1[grepl(
	pattern = "No religion|Has no religion but believes in God",
	x = ml$religion_q14_1_ot,
	ignore.case = TRUE
)] <- 1
ml$religion_q14_1[grepl(
	pattern = "Christian for Mungu",
	x = ml$religion_q14_1_ot,
	ignore.case = TRUE
)] <- 9

ml$dwelling_q16_1[grepl(pattern = "Rental|Muzigo  ,rental|muzigo|Alin ku Muzigo|Apangisa muzigo|Entomba yakumizigo",
												x = ml$q16_1_oth,
												ignore.case = TRUE)] <- 2

mlcb$report_q16_1c
# This case was originally coded as none, but they respond they reported it to LC1:
ml$report_q16_1c[grepl(pattern = "2 5",
											 x = ml$report_q16_1c,
											 ignore.case = TRUE)] <- 2
ml$report_q16_1c[grepl(
	pattern = "Tewuliranga ko omuntu yena|No",
	x = ml$report_q16_1c_oth,
	ignore.case = TRUE
)] <- 4

mlcb$content_q17_1b
ml$content_q17_1b[grepl(
	pattern = "Recalls movies not treatments",
	x = ml$content_q17_1b_oth,
	ignore.case = TRUE
)] <- 5
ml$content_q17_1b_1[grepl(
	pattern = "Other project not social norms|She attended a different movieHe only watched one movie and found it ending. They dint remember anything else|Didn't get to know",
	x = ml$content_q17_1b_oth,
	ignore.case = TRUE
)] <- 4

mlcb$hear_q19_4
ml$hear_q19_4_1[grepl(
	pattern = "Didn't mind to ask|Didn't ask any body|Didn't hear anything about them|They didn't tell me|He only recalls about the lady that used to screen the movies|Don't know|Didn't bother|Didn't get time to ask them|He didn't ask about the content|They didn't discuss anything|999|Doesn't know|Was not told anything about the movies|Its only the husband who went and he didn't tell her anything about the movie|He did not watch any movie|The respondent  said that she didn't take an initiative to ask them what was in the movies|I did not ask them anything|Does not know of anything|Does not know any content|None|She wasnt told in details about the movie by the husband|Didn't hear anything about the content of the films|She doesn't know anything about them.|He didn't discuss with them anything, so didn't know about the content.|Doesn't know way we're they about|othing he heard  or knows|None|They didn't tell him anything|Didn't get time to ask them|She didn't hear of anything.|They didn't discuss anything with him.|He doesn't know and dint ask at the movies|Didn't bother|He didn't watch any movie|They didn't talk about them|Nothing he heard  or knows",
	x = ml$hear_q19_4_oth,
	ignore.case = TRUE
)] <- 7
ml$hear_q19_4_1[grepl(
	pattern = "She had about them but she cannot recall any thing",
	x = ml$hear_q19_4_oth,
	ignore.case = TRUE
)] <- 4


# Recoding area variables -------------------------------------------------

ml$county[grepl(pattern = "uuid:3eb540db-2685-4e15-8ee2-27d84364a475|uuid:5a3b1fb5-6335-46e1-b8a7-8804bc6fe134",
								x = ml$KEY,
								ignore.case = TRUE)] <- 5
ml$subcounty[grepl(pattern = "uuid:3eb540db-2685-4e15-8ee2-27d84364a475|uuid:5a3b1fb5-6335-46e1-b8a7-8804bc6fe134",
									 x = ml$KEY,
									 ignore.case = TRUE)] <- 27
ml$parish[grepl(pattern = "uuid:3eb540db-2685-4e15-8ee2-27d84364a475|uuid:5a3b1fb5-6335-46e1-b8a7-8804bc6fe134",
								x = ml$KEY,
								ignore.case = TRUE)] <- 68
ml$tc_id[grepl(pattern = "uuid:3eb540db-2685-4e15-8ee2-27d84364a475|uuid:5a3b1fb5-6335-46e1-b8a7-8804bc6fe134",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 74
ml$county[grepl(pattern = "uuid:02e96a04-80ba-4c3b-be34-e1713a0030f5",
								x = ml$KEY,
								ignore.case = TRUE)] <- 7
ml$subcounty[grepl(pattern = "uuid:02e96a04-80ba-4c3b-be34-e1713a0030f5",
									 x = ml$KEY,
									 ignore.case = TRUE)] <- 36
ml$parish[grepl(pattern = "uuid:02e96a04-80ba-4c3b-be34-e1713a0030f5",
								x = ml$KEY,
								ignore.case = TRUE)] <- 90
ml$tc_id[grepl(pattern = "uuid:02e96a04-80ba-4c3b-be34-e1713a0030f5",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 100
ml$parish[grepl(pattern = "uuid:34542561-ad9e-419e-9179-d9cf164931c0|uuid:0e786b45-0f67-4e8d-90f8-9e922e617d52|uuid:bfd53835-8427-4f0d-a63f-0a82bf7a9d19|uuid:014d4253-053b-469d-ae66-5f2469596398|uuid:3d1a1f04-5fd1-4910-9664-ac093e88442a|uuid:7369ba79-fded-4268-95a2-4b5cb9c75dda|uuid:5211dd3b-bb0e-46ab-ae80-071b725c9bb9|uuid:0b7ded7a-5300-4728-8a57-7ee9c4da502b|uuid:9651a1c4-2fb1-484c-a535-69763b8c13c8|uuid:04aa1766-cdca-4b1c-97c7-9660ca087abd|uuid:853dcfd3-7be5-46a0-a134-c56217ad0e2b|uuid:e310cc42-b340-42b2-93f9-8d9088782acc|uuid:3fc6c51e-31d2-4c3f-8b57-85849a572748|uuid:fb6da6ab-d0f1-47b5-b632-e59652a467d5|uuid:9b4c2395-c65d-4e2c-9ee4-80adc366cb93|uuid:bc92c2a0-4363-4707-a260-d657137be01b|uuid:0bf2d665-64d3-4c2d-b60a-187627f9224b|uuid:9779c5d6-7025-4aff-88a0-5a8712147926|uuid:644c7a00-e8f5-4201-bd08-451ca7d1cdbd",
								x = ml$KEY,
								ignore.case = TRUE)] <- 69
ml$tc_id[grepl(pattern = "uuid:34542561-ad9e-419e-9179-d9cf164931c0|uuid:0e786b45-0f67-4e8d-90f8-9e922e617d52|uuid:bfd53835-8427-4f0d-a63f-0a82bf7a9d19|uuid:014d4253-053b-469d-ae66-5f2469596398|uuid:3d1a1f04-5fd1-4910-9664-ac093e88442a|uuid:7369ba79-fded-4268-95a2-4b5cb9c75dda|uuid:5211dd3b-bb0e-46ab-ae80-071b725c9bb9|uuid:0b7ded7a-5300-4728-8a57-7ee9c4da502b|uuid:9651a1c4-2fb1-484c-a535-69763b8c13c8|uuid:04aa1766-cdca-4b1c-97c7-9660ca087abd|uuid:853dcfd3-7be5-46a0-a134-c56217ad0e2b|uuid:e310cc42-b340-42b2-93f9-8d9088782acc|uuid:3fc6c51e-31d2-4c3f-8b57-85849a572748|uuid:fb6da6ab-d0f1-47b5-b632-e59652a467d5|uuid:9b4c2395-c65d-4e2c-9ee4-80adc366cb93|uuid:bc92c2a0-4363-4707-a260-d657137be01b|uuid:0bf2d665-64d3-4c2d-b60a-187627f9224b|uuid:9779c5d6-7025-4aff-88a0-5a8712147926|uuid:644c7a00-e8f5-4201-bd08-451ca7d1cdbd",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 75
ml$tc_id[grepl(pattern = "uuid:0bf2d665-64d3-4c2d-b60a-187627f9224b|uuid:9b4c2395-c65d-4e2c-9ee4-80adc366cb93|uuid:bc92c2a0-4363-4707-a260-d657137be01b|uuid:644c7a00-e8f5-4201-bd08-451ca7d1cdbd|uuid:9779c5d6-7025-4aff-88a0-5a8712147926",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 76
ml$parish[grepl(pattern = "uuid:0f0cfbd0-70e2-433d-9596-8fa45bff7847",
								x = ml$KEY,
								ignore.case = TRUE)] <- 99
ml$tc_id[grepl(pattern = "uuid:0f0cfbd0-70e2-433d-9596-8fa45bff7847",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 109
ml$tc_id[grepl(pattern = "uuid:ab45c2a0-2987-45c2-a0a6-801ace86d03e",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 98
ml$tc_id[grepl(pattern = "uuid:f38ca3cd-7c43-4c18-b1ff-0b69b8ceb86f" ,
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 59
ml$parish[grepl(pattern = "uuid:f38ca3cd-7c43-4c18-b1ff-0b69b8ceb86f",
								x = ml$KEY,
								ignore.case = TRUE)] <- 55
ml$tc_id[grepl(pattern = "uuid:3f41ea6c-581e-4970-9901-106509ca06e2",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 56
ml$parish[grepl(pattern = "uuid:3f41ea6c-581e-4970-9901-106509ca06e2",
								x = ml$KEY,
								ignore.case = TRUE)] <- 52
ml$county[grepl(pattern = "uuid:a01e9aca-fae2-421a-951f-c9af92f2f5b7",
								x = ml$KEY,
								ignore.case = TRUE)] <- 5
ml$subcounty[grepl(pattern = "uuid:a01e9aca-fae2-421a-951f-c9af92f2f5b7",
									 x = ml$KEY,
									 ignore.case = TRUE)] <- 26
ml$parish[grepl(pattern = "uuid:a01e9aca-fae2-421a-951f-c9af92f2f5b7",
								x = ml$KEY,
								ignore.case = TRUE)] <- 65
ml$tc_id[grepl(pattern = "uuid:a01e9aca-fae2-421a-951f-c9af92f2f5b7",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 70
ml$tc_id[grepl(pattern = "uuid:eb5cf4fc-e391-4feb-9113-e367f3956945",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 59
ml$parish[grepl(pattern = "uuid:eb5cf4fc-e391-4feb-9113-e367f3956945",
								x = ml$KEY,
								ignore.case = TRUE)] <- 55

ids_of_kibuye_village <- c(
	"10707121",
	"10707341",
	"10707411",
	"10707161",
	"10707351",
	"10707361",
	"10707191",
	"10707441",
	"10707301",
	"10707311",
	"10707061",
	"10707491",
	"10707321",
	"10707261",
	"10707141",
	"10707111",
	"10707071",
	"10707021",
	"10707431",
	"10707231",
	"10707181",
	"10707031",
	"10707241",
	"10707091",
	"10707171",
	"10707151",
	"10707451",
	"10707391",
	"10707251",
	"10707331",
	"10707401",
	"10707271",
	"10707291",
	"10707221",
	"10707201",
	"10707471",
	"10707481",
	"10707461",
	"10707371",
	"10707211",
	"10707381",
	"10707281",
	"10707081",
	"10707041",
	"10707011",
	"10707421",
	"10707131",
	"10707101",
	"10707051"
)

ml$tc_id[ml$id %in% ids_of_kibuye_village] <- 12
ml$tc_id[grepl(pattern = "uuid:1eb6d3c9-7cbb-4b61-a2ff-1bbdeab10eb8",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 12
ml$tc_id[grepl(pattern = "uuid:2a73d6d0-3a1d-4987-9491-28ecb510a6a6|uuid:5541da77-3917-457d-a9ef-4ce02834b510|uuid:63b863b9-3230-4d63-9cd2-f7a711f97d60|uuid:a62f6ac8-063c-4417-a2b5-924706b6376a|uuid:b363213a-3423-454e-accf-682c34526a0f|uuid:84bf564d-b121-4baf-ad91-96e8baa13096",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 28
ml$parish[grepl(pattern = "uuid:2a73d6d0-3a1d-4987-9491-28ecb510a6a6|uuid:5541da77-3917-457d-a9ef-4ce02834b510|uuid:63b863b9-3230-4d63-9cd2-f7a711f97d60|uuid:a62f6ac8-063c-4417-a2b5-924706b6376a|uuid:b363213a-3423-454e-accf-682c34526a0f|uuid:84bf564d-b121-4baf-ad91-96e8baa13096",
								x = ml$KEY,
								ignore.case = TRUE)] <- 24
ml$tc_id[grepl(pattern = "uuid:aed6db20-2e7b-438c-8672-8fce6f7b848f",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 26
ml$tc_id[grepl(pattern = "uuid:d6baf787-8352-4164-bb95-66eda21891ba|uuid:001cccdd-76f3-4a50-8add-88785e8b8bcb|uuid:fd1959ff-d026-4d02-a24c-0be5c7768073|uuid:73456bbe-59d6-4c83-ae53-a84013b39b26|uuid:d12dbe86-1b44-4d88-843c-b80989ba7623|uuid:eaba4d81-0736-4a09-806e-6418c31a39af",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 26
ml$tc_id[grepl(pattern = "uuid:dc0e5182-f7dd-4ae0-b666-605b1f357583|uuid:9dc9efd5-ed26-49be-ac6e-dce12218a6d1",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 6
ml$parish[grepl(pattern = "uuid:da1bc67c-f555-4cb4-8f66-f86409d12347",
								x = ml$KEY,
								ignore.case = TRUE)] <- 12
ml$tc_id[grepl(pattern = "uuid:da1bc67c-f555-4cb4-8f66-f86409d12347",
							 x = ml$KEY,
							 ignore.case = TRUE)] <- 16

# Correction of tracking variable
mlcb$tracking
ml$tracking[ml$KEY == "uuid:1f3a38f1-f9e3-4996-9e1c-0d3fc8ca16b4" |
							ml$KEY == "uuid:b16d693a-cdea-4951-8ced-12756432c29e"] <- 1
ml$tracking[ml$enum != 11 &
							ml$enum != 41 & ml$enum != 13 &
							ml$enum != 3 & ml$enum != 36 & ml$enum != 37] <- 1

# GPS coordinates not recorded
ml$gps_longitude[grepl(pattern = "uuid:3e95d433-bb29-408f-ba35-8540359897df|uuid:42644443-2844-4032-b910-af17b544bb1f|uuid:9aff975a-6a88-4752-8f43-7825b01b231e|uuid:cb4b2109-c6df-450d-87fe-4398493e383f",
											 x = ml$KEY,
											 ignore.case = TRUE)] <- NA
ml$gps_latitude[grepl(pattern = "uuid:3e95d433-bb29-408f-ba35-8540359897df|uuid:42644443-2844-4032-b910-af17b544bb1f|uuid:9aff975a-6a88-4752-8f43-7825b01b231e|uuid:cb4b2109-c6df-450d-87fe-4398493e383f",
											x = ml$KEY,
											ignore.case = TRUE)] <- NA

# Enumerators selecting wrong names
ml$enum[grepl(pattern = "uuid:42481a49-e1dc-45bc-98f2-d530894daafb|uuid:d73cffd8-d3df-4771-9b9c-d3aad41aa793|uuid:19ca0525-7fc0-4949-bf71-ab379fc9864b",
							x = ml$KEY,
							ignore.case = TRUE)] <- 1
ml$enum[grepl(pattern = "uuid:07ab4640-f16b-4a09-89bc-acdff7f80389|uuid:dd28fcdc-9e60-4ad2-98d7-577b0d7bd6d4",
							x = ml$KEY,
							ignore.case = TRUE)] <- 6
ml$enum[grepl(pattern = "uuid:e32c5c65-c903-4775-8007-c7618e93f30b",
							x = ml$KEY,
							ignore.case = TRUE)] <- 21
ml$enum[grepl(pattern = "uuid:67877f6c-6d1d-4450-8790-a9fa01e43db3",
							x = ml$KEY,
							ignore.case = TRUE)] <- 12
ml$enum[grepl(pattern = "uuid:d82dcd4b-5823-494a-a07e-0db62939e5b3",
							x = ml$KEY,
							ignore.case = TRUE)] <- 21
ml$enum[grepl(pattern = "uuid:5e8a5c30-0e10-4043-943a-33a0bfb1ad3f",
							x = ml$KEY,
							ignore.case = TRUE)] <- 29
ml$enum[grepl(pattern = "uuid:b4c0fe9f-cd03-4823-8125-26e5bf858b6e",
							x = ml$KEY,
							ignore.case = TRUE)] <- 36
ml$enum[grepl(pattern = "uuid:000e0311-20d5-4e2f-ad60-a46497acc9af",
							x = ml$KEY,
							ignore.case = TRUE)] <- 37

# Enumerators entering wrong sex
ml$sex_q2_1[grepl(pattern = "uuid:e9152aab-b4f6-4514-afc5-a8f94199adb9",
									x = ml$KEY,
									ignore.case = TRUE)] <- 2
ml$sex_q2_1[grepl(pattern = "uuid:31d0389e-1b1c-4077-99ab-5246a49a75f0",
									x = ml$KEY,
									ignore.case = TRUE)] <- 1
ml$sex_q2_1[grepl(pattern = "uuid:c7851174-5fb1-46ae-b1f0-9f49da74cc14",
									x = ml$KEY,
									ignore.case = TRUE)] <- 1

# Merge in video hall distance data ---------------------------------------

ml <- ml %>% left_join(distance_data, by = "KEY")



