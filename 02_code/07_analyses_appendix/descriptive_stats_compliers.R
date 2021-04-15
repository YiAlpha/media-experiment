
panel_compliers <- el$id[el$respondent_category == "Complier"]

ml$compliance_cat <- NA
ml$compliance_cat[ml$compliance_label == "Complier" ] <- "All Compliers"
ml$compliance_cat[ml$id %in% panel_compliers] <- "Panel Compliers"
ml$compliance_cat[ml$compliance_label != "Complier" ] <- "Non-Compliers"
ml$compliance_label

ml$sex_q2_1

desc_tab_all_compliers <- ml %>% 
  filter(compliance_label == "Complier") %>% 
  dplyr::select(
    female,
    age,
    seven_years_edu_or_less,
    married_or_living_as_married,
    travel_big_city,
    news_everyday,
    phone_every_day,
    luganda_lang,
    catholic,
    protestant,
    muslim,
    pray_at_least_once_day,
    rooms,
    mud_wall,
    brick_wall,
    radio,
    tv,
    cellphone) %>% 
  summarize_each(funs =  funs(
    mean = paste0(round(mean(., na.rm = TRUE),2)," (n = ",sum(!is.na(.)),")")
    )
  )

desc_tab_non_compliers <- ml %>% 
  filter(compliance_label != "Complier") %>% 
  dplyr::select(
    female,
    age,
    seven_years_edu_or_less,
    married_or_living_as_married,
    travel_big_city,
    news_everyday,
    phone_every_day,
    luganda_lang,
    catholic,
    protestant,
    muslim,
    pray_at_least_once_day,
    rooms,
    mud_wall,
    brick_wall,
    radio,
    tv,
    cellphone) %>% 
  summarize_each(funs =  funs(
    mean = paste0(round(mean(., na.rm = TRUE),2)," (n = ",sum(!is.na(.)),")")
    )
  )

desc_tab_panel_compliers <- ml %>% 
  filter(id %in% panel_compliers) %>% 
  dplyr::select(
    female,
    age,
    seven_years_edu_or_less,
    married_or_living_as_married,
    travel_big_city,
    news_everyday,
    phone_every_day,
    luganda_lang,
    catholic,
    protestant,
    muslim,
    pray_at_least_once_day,
    rooms,
    mud_wall,
    brick_wall,
    radio,
    tv,
    cellphone) %>% 
  summarize_each(funs =  funs(
    mean = paste0(round(mean(., na.rm = TRUE),2)," (n = ",sum(!is.na(.)),")")
  )
  )

desc_tab <- t(rbind(
  desc_tab_non_compliers, desc_tab_all_compliers, desc_tab_panel_compliers
))


rownames(desc_tab) <- c("Woman",
                       "Age (in yrs)",
                       "Less than 8 yrs of education",
                       "Married or living as married",
                       "Ever been to big city",
                       "Consumes news every day",
                       "Uses mobile phone every day",
                       "Main language is Luganda",
                       "Catholic",
                       "Protestant",
                       "Muslim",
                       "Prays at least once a day",
                       "Number of rooms in house",
                       "Mud wall",
                       "Brick wall",
                       "Owns radio",
                       "Owns TV",
                       "Owns cell phone")



sink("03_tables/descriptive_stats.tex")
print(kable(desc_tab, col.names = c(
  "Mean among non-compliers",
  "Mean among all compliers",
  "Mean among panel compliers"),format = "latex",align = c("r","r","r")))
sink()
























