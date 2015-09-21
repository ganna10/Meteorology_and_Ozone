library(ggplot2)
library(dplyr)
library(grid)
library(Cairo)

d = read.table(file = "out_Temperature_06092015.csv", header = TRUE, sep = ",")
d = tbl_df(d)

mozart.data = d %>% filter(Mechanism == "MOZART" & Temperature == "293") %>% select(NOx, O3)
mozart.data$Mechanism = rep("MOZART-4", length(mozart.data$O3))
mcm.data = d %>% filter(Mechanism == "MCM" & Temperature == "293") %>% select(NOx, O3)
mcm.data$Mechanism = rep("MCMv3.2", length(mcm.data$O3))

mozart.data.emissions = d %>% filter(Mechanism == "MOZART" & Temperature == 293) %>% select(NOx.Emissions, O3)
mozart.data.emissions$Mechanism = rep("MOZART-4", length(mozart.data.emissions$O3))
mcm.data.emissions = d %>% filter(Mechanism == "MCM" & Temperature == "293") %>% select(NOx.Emissions, O3)
mcm.data.emissions$Mechanism = rep("MCMv3.2", length(mcm.data.emissions$O3))

df = rbind(mcm.data, mozart.data)
df.emissions = rbind(mcm.data.emissions, mozart.data.emissions)

p = ggplot(df, aes(x = NOx, y = O3))
p = p + geom_line()
p = p + facet_wrap(~ Mechanism)

CairoPDF(file = "O3_fn_of_NOx_temp_293.pdf")
print(p)
dev.off()

p = ggplot(df.emissions, aes(x = NOx.Emissions, y = O3))
p = p + geom_line()
p = p + facet_wrap(~ Mechanism)

CairoPDF(file = "O3_fn_of_NOx_emissions_temp_293.pdf")
print(p)
dev.off()
