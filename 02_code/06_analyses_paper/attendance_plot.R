festival <- 
  festival %>% 
  mutate(
    date_1 = as.Date(date_1,format = "%d/%m/%Y"),
    date_2 = as.Date(date_2,format = "%d/%m/%y"),
    date_3 = as.Date(date_3,format = "%d/%m/%Y"),
    date_4 = as.Date(date_4,format = "%d/%m/%Y"),
    date_5 = as.Date(date_5,format = "%d/%m/%Y"),
    date_6 = as.Date(date_6,format = "%d/%m/%y"),
    movie_1 = as.character(movie_1),
    movie_2 = as.character(movie_2),
    movie_3 = as.character(movie_3),
    movie_4 = as.character(movie_4),
    movie_5 = as.character(movie_5),
    movie_6 = as.character(movie_6)
  )

plot_data <- with(festival,{
  data.frame(
    attendance = c(
      n_men_end1 + n_women_end1,
      n_men_end2 + n_women_end2, 
      n_men_end3 + n_women_end3, 
      n_men_end4 + n_women_end4, 
      n_men_end5 + n_women_end5, 
      n_men_end6 + n_women_end6 
  ),
  tc = rep(tc_id,6),
  IPV = factor(x = rep(IPV,6),levels = 0:1,labels = c("Control","Anti-IPV Campaign")),
  screening = rep(1:6,each = nrow(festival))
  )
})

attendance_plot <- 
	ggplot(plot_data,aes(x = screening, y = attendance,  color = IPV)) +
	geom_point(alpha = .3,size = 1,aes(group = tc)) +
	facet_grid(. ~ IPV) + 
	geom_smooth() +
	scale_y_continuous(name = "Adults in Audience",trans = "log1p",breaks = c(10,20,40,80,160,320)) +
	scale_x_continuous(
		name = "",breaks = 1:6,
		labels = c(
			"Pirates of the\nCarribean", "Slumdog\nMillionaire", "Spy", "The Fast and\nthe Furious 7", "Creed", "Oz The\nGreat" 
		)) +
	theme_bw() +
	theme(strip.background = element_blank(),legend.position = "none",panel.spacing = unit(2, "lines")) 

pdf("04_figures/attendance.pdf",width = 8.5,height = 2.6)
print(attendance_plot)
dev.off()


