# Create codebook function ------------------------------------------------

make_ODK_codebook <-
	function(choices_sheet,
					 questions_sheet,
					 remove_non_questions = TRUE) {
		choices <- choices_sheet
		Qs <- questions_sheet
		
		codebook <- lapply(
			X = Qs$name,
			FUN = function(qname) {
				type <- Qs$type[which(Qs$name == qname)][1]
				is_select <-
					grepl(pattern = "select",
								x = type,
								ignore.case = T)
				
				if (is_select) {
					list_name <-
						unlist(strsplit(x = as.character(type), split = "\\s+"))[2]
					matches <-
						grep(
							pattern = paste0("\\b", list_name, "\\b"),
							x = choices$list_name,
							ignore.case = T
						)
					
					return_labels <- data.frame(integer = choices$name[matches],
																			label = choices$label.english[matches])
					
				} else {
					return_labels <- type
				}
				
				question <- Qs$label.english[which(Qs$name == qname)][1]
				
				return_labels <-
					list(question = question, answer = return_labels)
				
				return(return_labels)
			}
		)
		
		qnames <- gsub(pattern = "-",
									 replacement = ".",
									 x = Qs$name)
		names(codebook) <- qnames
		
		if (remove_non_questions) {
			is_not_question <- unlist(sapply(
				X = codebook,
				FUN = function(x) {
					(!is.data.frame(x$answer)) &
						grepl(pattern = "note|group|geopoint|calculate|audio|phonenumber|start|end",
									x = x$answer,
									ignore.case = TRUE)[1]
				}
			))
			
			codebook <- codebook[-which(is_not_question, arr.ind = T)]
			
		}
		
		return(codebook)
		
	}


# Create Codebooks Household Data -----------------------------------------

mlcb <- make_ODK_codebook(
	choices_sheet = read.csv("01_data/codebooks/midline_choices.csv", stringsAsFactors = F),
	questions_sheet = read.csv("01_data/codebooks/midline_Qs.csv", stringsAsFactors = F),
	remove_non_questions = TRUE
)


elcb <- make_ODK_codebook(
	choices_sheet = read.csv("01_data/codebooks/endline_choices.csv", stringsAsFactors = F),
	questions_sheet = read.csv("01_data/codebooks/endline_Qs.csv", stringsAsFactors = F),
	remove_non_questions = TRUE
)

# Create Codebooks VHT Data -----------------------------------------------

vht_mlcb <- make_ODK_codebook(
	choices_sheet = read.csv(
		"01_data/codebooks/vht_midline_choices.csv",
		stringsAsFactors = F
	),
	questions_sheet = read.csv("01_data/codebooks/vht_midline_Qs.csv", stringsAsFactors = F),
	remove_non_questions = TRUE
)

vht_elcb <- make_ODK_codebook(
	choices_sheet = read.csv(
		"01_data/codebooks/vht_endline_choices.csv",
		stringsAsFactors = F
	),
	questions_sheet = read.csv("01_data/codebooks/vht_endline_Qs.csv", stringsAsFactors = F),
	remove_non_questions = TRUE
)
