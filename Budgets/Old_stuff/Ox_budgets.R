library(ggplot2)
library(ggthemes)
library(Cairo)

#peak data
cb05.data = read.csv(file = "CB05_Ox_production_T_311_NO_SF_1.0.csv")
cri.data = read.csv(file = "CRIv2_Ox_production_T_311_NO_SF_1.0.csv")
mcm.data = read.csv(file = "MCMv3.2_Ox_production_T_311_NO_SF_1.0.csv")
mozart.data = read.csv(file = "MOZART-4_Ox_production_T_311_NO_SF_1.0.csv")
radm2.data = read.csv(file = "RADM2_Ox_production_T_311_NO_SF_1.0.csv")

d = rbind(cb05.data, cri.data, mcm.data, mozart.data, radm2.data)
d$Condition = rep("Peak", length(d$Times))

#low NOx data
cb05.data = read.csv(file = "CB05_Ox_production_T_311_NO_SF_0.05.csv")
cri.data = read.csv(file = "CRIv2_Ox_production_T_311_NO_SF_0.05.csv")
mcm.data = read.csv(file = "MCMv3.2_Ox_production_T_311_NO_SF_0.05.csv")
mozart.data = read.csv(file = "MOZART-4_Ox_production_T_311_NO_SF_0.05.csv")
radm2.data = read.csv(file = "RADM2_Ox_production_T_311_NO_SF_0.05.csv")

d1 = rbind(cb05.data, cri.data, mcm.data, mozart.data, radm2.data)
d1$Condition = rep("Low NOx", length(d1$Times))

#high NOx data
cb05.data = read.csv(file = "CB05_Ox_production_T_311_NO_SF_7.0.csv")
cri.data = read.csv(file = "CRIv2_Ox_production_T_311_NO_SF_7.0.csv")
mcm.data = read.csv(file = "MCMv3.2_Ox_production_T_311_NO_SF_7.0.csv")
mozart.data = read.csv(file = "MOZART-4_Ox_production_T_311_NO_SF_7.0.csv")
radm2.data = read.csv(file = "RADM2_Ox_production_T_311_NO_SF_7.0.csv")

d2 = rbind(cb05.data, cri.data, mcm.data, mozart.data, radm2.data)
d2$Condition = rep("High NOx", length(d2$Times))

final.data = rbind(d, d1, d2)
final.data$Condition = factor(final.data$Condition, levels = c("Low NOx", "Peak", "High NOx"))

p = ggplot(final.data, aes(x = Times, y = Reaction.Rates, fill = Reactants))
p = p + geom_bar(position = "stack", stat = "identity")
p = p + facet_grid(Mechanism ~ Condition)

CairoPDF(file = "Ox_budgets.pdf", width = 7, height = 10)
print(p)
dev.off()
