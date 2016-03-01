# Plot initial conditions of emissions
# Version 0: Jane Coates 18/2/2016

get_data <- function (mechanism) {
  if (mechanism == "MCMv3.2") {
    file.bel <- paste0("Belgium_", mechanism, "_Total_NMVOC_Emissions.csv")
    file.nld <- paste0("Netherlands_", mechanism, "_Total_NMVOC_Emissions.csv")
    file.lux <- paste0("Luxembourg_", mechanism, "_Total_NMVOC_Emissions.csv")
    d.bel <- read.csv(file.bel)
    d.nld <- read.csv(file.nld)
    d.lux <- read.csv(file.lux)
    data <- data.frame(Species = d.bel$Species, Total = d.bel$Total + d.nld$Total + d.lux$Total)
    data$Mechanism <- rep(mechanism, length(data$Species))
  } else {
    filename <- paste0("Benelux_", mechanism, "_Total_NMVOC_Emissions.csv")
    data <- read.csv(filename)
    data$Mechanism <- rep(mechanism, length(data$Species))
  }
  final <- data %>%
    select(Species, Mechanism, Total)
  return(final)
}


setwd("~/Documents/Analysis/2015_Meteorology_and_Ozone/VOC_Oxidation/")
mechanisms <- c("CB05", "MCMv3.2", "CRIv2", "MOZART-4", "RADM2")

data.list <- lapply(mechanisms, get_data)
data.df <- do.call("rbind", data.list)
tbl_df(data.df)

frct <- data.df %>%
  group_by(Mechanism) %>%
  mutate(Sum = sum(Total), Fraction = Total/Sum)

frct$Species <- as.character(frct$Species)

named <- frct %>%
  rowwise() %>%
  mutate(Group = set_gp(Species)) %>%
  group_by(Mechanism, Group) %>%
  summarise(Fraction = sum(Fraction))
tbl_df(named)

named$Group <- factor(named$Group, levels = c("Alkanes", "Alkenes", "Isoprene", "Terpenes", "Aromatics", "Alcohols", "Aldehydes", "Ketones", "Chlorinated", "Others"))
my.colours <- c("Alkanes" = "#6c254f", "Alkenes" = "#f9c500", "Isoprene" = "#0e5c28", "Terpenes" = "#2b9eb3", "Aromatics" = "#ef6638", "Alcohols" = "#0352cb", "Aldehydes" = "#b569b3", "Ketones" = "#ae4901", "Acids" = "#000000", "Chlorinated" = "#ba8b01", "Others" = "#77aecc")

p <- ggplot(named, aes(x = Mechanism, y = Fraction, fill = Group))
p <- p + geom_bar(stat = "identity")
p <- p + plot_theme()
p <- p + scale_y_continuous(expand = c(0, 0), labels = percent)
p <- p + scale_x_discrete(expand = c(0, 0))
p <- p + scale_fill_manual(values = my.colours, limits = rev(levels(named$Group)))
p

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
    gp <- "Alcohols"
  } else if (species == "CH3COCH3" | species == "CYHEXONE" | species == "DIEK" | species == "HEX2ONE" | species == "HEX3ONE" | species == "KET" | species == "MEK" | species == "MIBK" | species == "MIBKAOH" | species == "MIPK" | species == "MPRK") {
    gp <- "Ketones"
  } else {
    gp <- "Others"
  }
  return(gp)
}