megan = read.csv("MEGAN_c5h8_emissions_with_T.csv")
megan = megan %>% mutate(Temperature.C = Temperature - 273)
megan = megan %>% select(-Temperature) %>% mutate(Run = rep("T-Dependent", length(megan$Temperature.C)))
megan$Species = rep("Isoprene", length(megan$Run))
names(megan) = c("Emissions", "Temperature", "Run", "Species")
megan = megan[c("Run", "Species", "Emissions", "Temperature")]
write.table(megan, file = "MEGAN_emissions_T.csv", sep = ",", row.names = FALSE, quote = FALSE)
