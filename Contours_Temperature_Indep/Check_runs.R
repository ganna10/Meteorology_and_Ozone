args = commandArgs(trailingOnly = TRUE)

filename = paste0("out_Temperature_NOx_", args[[1]], ".csv") 
d = read.csv(filename)
NOx = d %>% select(Mechanism, NOx.Emissions, NOx, Temperature)
#new = NOx %>% group_by(Mechanism, Temperature) %>% summarise(Max.NOx.Emissions = max(NOx.Emissions), Max.NOx = max(NOx), Min.NOx.Emissions = min(NOx.Emissions), Min.NOx = min(NOx))
new = NOx %>% group_by(Mechanism, Temperature) %>% summarise(Max.NOx.Emissions = max(NOx.Emissions), Min.NOx.Emissions = min(NOx.Emissions))
print.data.frame(new)
