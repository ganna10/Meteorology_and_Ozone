# compare oxidation rates of initial VOCs
# Version 0: Jane Coates 16/2/2016

get_data <- function (mechanism, run.label, date) {
  filename <- paste0(run.label, "/", mechanism, "_initial_VOC_oxidation_rate_", date, ".txt")
  data <- read.csv(filename)
  data$Run <- rep(run.label, length(data$Mechanism))
  return(data)
}

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//VOC_Oxidation/")
date <- "19022016"
mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "RADM2", "CB05")

# temperature dependent data
td.list <- lapply(mechanisms, get_data, run.label = "TD", date = date)
td.df <- do.call("rbind", td.list)
tbl_df(td.df)

# temperature independent data
ti.list <- lapply(mechanisms, get_data, run.label = "TI", date = date)
ti.df <- do.call("rbind", ti.list)
tbl_df(ti.df)

df <- rbind(td.df, ti.df)

assigned <- df %>%
  rowwise() %>%
  mutate(Temperature.C = Temperature - 273, NOx.Condition = get_NOx_condition(H2O2/HNO3), Group = set_gp(VOC)) %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition, Group) %>%
  summarise(Oxidation.Rate = mean(Oxidation.Rate), Emission.Rate = mean(Emission.Rate)) 
tbl_df(assigned)

fractional.loss <- assigned %>%
  select(-Emission.Rate) %>%
  group_by(Mechanism, Temperature.C, Run, NOx.Condition) %>%
  mutate(Total.Loss = sum(Oxidation.Rate), Fractional.Loss = Oxidation.Rate / Total.Loss) %>%
  select(-Oxidation.Rate, -Total.Loss)
tbl_df(fractional.loss)

# normed <- assigned %>%
#   mutate(Normalised = Oxidation.Rate / Emission.Rate) %>%
#   group_by(Mechanism, Temperature.C, NOx.Condition, Run, VOC) %>%
#   summarise(Normalised = sum(Normalised))
# 
# emissions <- assigned %>%
#   group_by(Mechanism, Temperature.C, Run, NOx.Condition, VOC) %>%
#   summarise(Emission.Rate = mean(Emission.Rate)) 
# tbl_df(emissions)

my.colours <- c("Alkanes" = "#6c254f", "Alkenes" = "#f9c500", "Isoprene" = "#0e5c28", "Terpenes" = "#2b9eb3", "Aromatics" = "#ef6638", "Alcohols" = "#0352cb", "Aldehydes" = "#b569b3", "Ketones" = "#ae4901", "Acids" = "#000000", "Chlorinated" = "#ba8b01", "Others" = "#77aecc")

# td plots
fractional.loss.td <- fractional.loss %>%
  filter(Run == "TD")

td.p <- ggplot(fractional.loss.td, aes(x = Temperature.C, y = Fractional.Loss, fill = Group))
td.p <- td.p + geom_bar(stat = "identity", size = 1)
td.p <- td.p + facet_grid(Mechanism ~ NOx.Condition)
td.p <- td.p + plot_theme()
td.p <- td.p + scale_fill_manual(values = my.colours)
td.p

# ti plots
fractional.loss.ti <- fractional.loss %>%
  filter(Run == "TI")

ti.p <- ggplot(fractional.loss.ti, aes(x = Temperature.C, y = Fractional.Loss, fill = Group))
ti.p <- ti.p + geom_bar(stat = "identity", size = 1)
ti.p <- ti.p + facet_grid(Mechanism ~ NOx.Condition)
ti.p <- ti.p + plot_theme()
ti.p <- ti.p + scale_fill_manual(values = my.colours)
ti.p

mcm.loss <- fractional.loss %>%
  filter(Run == "TI")
mcm <- ggplot(fractional.loss, aes(x = Temperature.C, y = Fractional.Loss, fill = Mechanism))
mcm <- mcm + geom_bar(stat = "identity", position = "dodge")
mcm <- mcm + facet_grid(NOx.Condition ~ Group)
mcm <- mcm + plot_theme()
mcm

set_gp <- function (species) {
  if (species == "C5H8" | species == "ISOP" | species == "ISO") {
    gp <- "Isoprene"
  } else if (species == "C2H6" | species == "C3H8" | species == "NC4H10" | species == "IC4H10" | species == "NC5H12" | species == "IC5H12" | species == "NEOP" | species == "NC6H14" | species == "M2PE" | species == "M3PE" | species == "CHEX" | species == "NC7H16" | species == "M2HEX" | species == "M3HEX" | species == "M22C4" | species == "M23C4" | species == "NC8H18" | species == "NC9H20" | species == "NC10H22" | species == "NC11H24" | species == "NC12H26" | species == "BIGALK" | species == "HC3" | species == "HC5" | species == "HC8" | species == "PAR" | species == "ETHA") {
    gp <- "Alkanes"
  } else if (species == "C2H4" | species == "C3H6" | species == "BUT1ENE" | species == "MEPROPENE" | species == "BIGENE" | species == "C4H6" | species == "CBUT2ENE" | species == "CPENT2ENE" | species == "HEX1ENE" | species == "ME2BUT1ENE" | species == "ME2BUT2ENE" | species == "ME3BUT1ENE" | species == "OL2" | species == "OLE" | species == "OLI" | species == "OLT" | species == "PENT1ENE" | species == "TBUT2ENE" | species == "TPENT2ENE" | species == "ETH" | species == "IOLE") {
    gp <- "Alkenes"
  } else if (species == "BENZENE" | species == "BENZAL" | species == "CSL" | species == "DIME35EB" | species == "EBENZ" | species == "IPBENZ" | species == "METHTOL" | species == "MXYL" | species == "OETHTOL" | species == "OXYL" | species == "PXYL" | species == "PETHTOL" | species == "PBENZ" | species == "PHENOL" | species == "STYRENE" | species == "TM123B" | species == "TM124B" | species == "TM135B" | species == "TOL" | species == "TOLUENE" | species == "XYL" | species == "DIET35TOL" | species == "AROH14") {
    gp <- "Aromatics"
  } else if (species == "APINENE" | species == "BPINENE" | species == "LIMONENE" | species == "C10H16" | species == "TERP") {
    gp <- "Terpenes"
  } else if (species == "CCL2CH2" | species == "CDICLETH" | species == "CH2CL2" | species == "CH3CCL3" | species == "CH3CH2CL" | species == "CH3CL" | species == "CHCL2CH3" | species == "CHCL3" | species == "TCE" | species == "TDICLETH" | species == "TRICLETH" | species == "VINCL") {
    gp <- "Chlorinated"
    #   } else if (species == "AACD" | species == "CH3CO2H" | species == "CH3COOH" | species == "FACD" | species == "HCOOH" | species == "ORA1" | species == "ORA2" | species == "PROPACID" | species == "ACO2H") {
    #     gp <- "Acids"
  } else if (species == "ACR" | species == "ALD" | species == "ALD2" | species == "ALDX" | species == "C2H5CHO" | species == "C3H7CHO" | species == "C4ALDB" | species == "C4H9CHO" | species == "CARB6" | species == "CH2O" | species == "CH3CHO" | species == "CH3COCHO" | species == "CH3OCHO" | species == "FORM" | species == "HCHO" | species == "IPRCHO" | species == "MACR" | species == "MGLY" | species == "MGLYOX" | species == "UCARB10") {
    gp <- "Aldehydes"
  } else if (species == "BUT2OL" | species == "C2H5OH" | species == "C6H5CH2OH" | species == "CH3OH" | species == "CYHEXOL" | species == "ETHGLY" | species == "ETOH" | species == "IBUTOL" | species == "IPEAOH" | species == "IPEBOH" | species == "IPECOH" | species == "IPROPOL" | species == "MBO" | species == "MEOH" | species == "NBUTOL" | species == "NPROPOL" | species == "PECOH" | species == "PROPGLY" | species == "TBUTOL") {
#     gp <- "Alcohols"
    gp <- "Others"
  } else if (species == "CH3COCH3" | species == "CYHEXONE" | species == "DIEK" | species == "HEX2ONE" | species == "HEX3ONE" | species == "KET" | species == "MEK" | species == "MIBK" | species == "MIBKAOH" | species == "MIPK" | species == "MPRK") {
    gp <- "Ketones"
  } else {
    gp <- "Others"
  }
  return(gp)
}
