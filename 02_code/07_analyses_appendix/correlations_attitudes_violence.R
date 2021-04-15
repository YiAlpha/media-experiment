
cormat <- with(subset(el,female == 1),cor(data.frame(ipv_attitude,intervene_index,any_violence)))
rownames(cormat) <- colnames(cormat) <- c("VAW Not Acceptable", "Reporting Index", "Any Incidents")

sink("03_tables/corr_mat_attitudes.tex")
print(xtable(cormat,digits = 2),floating = FALSE)
sink()



