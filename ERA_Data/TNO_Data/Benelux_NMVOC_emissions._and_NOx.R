# Extract Benelux NMVOC emissions for each SNAP category, lump all sub-categores of SNAP 7, express as emissions/m2/s
# Version 0: Jane Coates 20/8/2015
# Version 1: Jane Coates 31/8/2015 not summing all SNAP 7 sub categories together

setwd("/work/users/jco//EU_Average_Sensitivity//Emissions")
#read in TNO-MACC-III emission data from 2011
data = read.table(file = "/data/emissions/MACC/TNO/TNO_MACC_III/text/TNO_MACC_III_emissions_v1_1_2011.txt", header = TRUE)
data = tbl_df(data)

#function to calculate emission fluxes from input data frame, returns data.frame
get.number.grid.points = function (data) {
    grid.areas = data %>% filter(SourceType == "A") %>% select(Lat, Lon)
    grid.areas = grid.areas %>% distinct(Lat, Lon)
    return(grid.areas)
}

get.emission.fluxes = function (data, grid.areas) {
    data = data %>% group_by(SNAP) %>% mutate(Tonnes.NMVOC = sum(NMVOC)) %>% distinct(SNAP) %>% select(-SourceType, -NMVOC)
    #get individual SNAP 7 sub-categories to calculate total of SNAP 7 emissions
    #NMVOC.snap7 = data %>% filter(SNAP == "73" | SNAP == "71" | SNAP == "75" | SNAP == "72" | SNAP == "74")
    #remove sub-categories of SNAP 7 and replace with total SNAP 7 emissions
    #data = data %>% filter(SNAP <= 34)
    #data = rbind(data, c(7, sum(NMVOC.snap7$Tonnes.NMVOC)))
    #add columns with emissions conversion from Tonnes to grams/cm2/s
    data = data %>% mutate(Tonnes.per.km2 = Tonnes.NMVOC/(length(grid.areas$Lat)*49))
    data = data %>% mutate(grams.per.cm2 = Tonnes.per.km2 * 1000 * 1000 / (1000 * 100 * 1000 * 100))
    data = data %>% mutate(grams.per.cm2.per.s = grams.per.cm2 / (365 * 24 * 60 * 60))
    return(data)
}

#extract emissions using country codes
belgium = data %>% filter( ISO3 == "BEL" ) %>% select(-PM10, -PM2_5, -SO2, -NH3, -Year)
luxembourg = data %>% filter( ISO3 == "LUX" ) %>% select(-PM10, -PM2_5, -SO2, -NH3, -Year)
netherlands = data %>% filter( ISO3 == "NLD" ) %>% select(-PM10, -PM2_5, -SO2, -NH3, -Year)

#keep only NMVOC emissions and sum all emissions for each SNAP category
NMVOC.belgium = belgium %>% select(-CH4, -CO, -NOX, -Lon, -Lat)
# belgium.grids = get.number.grid.points(belgium)
# length(belgium.grids$Lat)*49
# NMVOC.belgium = get.emission.fluxes(NMVOC.belgium, belgium.grids)

NMVOC.luxembourg = luxembourg %>% select(-CH4, -CO, -NOX, -Lon, -Lat)
# luxembourg.grids = get.number.grid.points(luxembourg)
# length(luxembourg.grids$Lat)*49
# NMVOC.luxembourg = get.emission.fluxes(NMVOC.luxembourg, luxembourg.grids)

NMVOC.netherlands = netherlands %>% select(-CH4, -CO, -NOX, -Lon, -Lat)
# netherlands.grids = get.number.grid.points(netherlands)
(length(netherlands.grids$Lat) + length(belgium.grids$Lat) + length(luxembourg.grids$Lat))*49
# NMVOC.netherlands = get.emission.fluxes(NMVOC.netherlands, netherlands.grids)

benelux.NMVOC = rbind(NMVOC.belgium, NMVOC.luxembourg, NMVOC.netherlands)
sum(benelux.NMVOC$NMVOC)

#keep only NOx emissions and sum all emissions for each SNAP category
NOx.belgium = belgium %>% select(-CH4, -CO, -NMVOC, -Lon, -Lat)
# belgium.grids = get.number.grid.points(belgium)
# length(belgium.grids$Lat)*49
# NOx.belgium = get.emission.fluxes(NOx.belgium, belgium.grids)

NOx.luxembourg = luxembourg %>% select(-CH4, -CO, -NMVOC, -Lon, -Lat)
# luxembourg.grids = get.number.grid.points(luxembourg)
# length(luxembourg.grids$Lat)*49
# NOx.luxembourg = get.emission.fluxes(NOx.luxembourg, luxembourg.grids)

NOx.netherlands = netherlands %>% select(-CH4, -CO, -NMVOC, -Lon, -Lat)
# netherlands.grids = get.number.grid.points(netherlands)
# length(netherlands.grids$Lat)*49
# NOx.netherlands = get.emission.fluxes(NOx.netherlands, netherlands.grids)

benelux.NOx = rbind(NOx.belgium, NOx.luxembourg, NOx.netherlands)
sum(benelux.NOx$NOX)

# convert Tonnes to NO emissions in molecules cm-2 s-1
sprintf("%.2e", sum(benelux.NOx$NOX) * 6.022e23 * 1e6 / (30.01*365*24*60*60*83692*1000*1000*100*100))
# write.table(NMVOC.belgium, file = "belgium_NMVOC_Emissions.csv", quote = FALSE, row.names = FALSE, col.names = TRUE, sep = ",")
# write.table(NMVOC.luxembourg, file = "luxembourg_NMVOC_Emissions.csv", quote = FALSE, row.names = FALSE, col.names = TRUE, sep = ",")
# write.table(NMVOC.netherlands, file = "netherlands_NMVOC_Emissions.csv", quote = FALSE, row.names = FALSE, col.names = TRUE, sep = ",")
