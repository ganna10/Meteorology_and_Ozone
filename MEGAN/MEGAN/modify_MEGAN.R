# modify MEGAN code to make increase with T faster
# Version 0: Jane Coates 8/12/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//MEGAN/MEGAN/")

temperatures <- seq(289, 313, 2)
c5h8.emissions.list <- sapply(temperatures, C5H8_emissions, LDF = 1, EF = 10000, LAI = 1.48)
df.1 <- data.frame(Temperature = temperatures, C5H8.Emissions = c5h8.emissions.list, Case = "Original")

emissions.lai <- sapply(temperatures, C5H8_emissions, LDF = 1, EF = 10000, LAI = 12.5)
df.2 <- data.frame(Temperature = temperatures, C5H8.Emissions = emissions.lai, Case = "LAI")

plot.df = rbind(df.1, df.2)
ggplot(plot.df, aes(x = Temperature, y = C5H8.Emissions, colour = Case)) + geom_point() + geom_line()
