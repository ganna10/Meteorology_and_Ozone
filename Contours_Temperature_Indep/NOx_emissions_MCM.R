library(dplyr, warn.conflicts=FALSE)
library(tidyr)
library(ggplot2)

d = read.csv(file = "out_Temperature_NOx_11102015.csv")
mcm = d %>% filter(Mechanism == "MCM") %>% select(NOx.Emissions, Temperature)
mcm = tbl_df(mcm)

filter.temps = function (temp) { d = mcm %>% filter(Temperature == temp) 
    min = min(d$NOx.Emissions)
    max = max(d$NOx.Emissions)
    list = c(min, max, temp)
    return(list)
}
NOx.data = lapply(temp.levels, filter.temps)
df <- do.call("rbind", NOx.data)
df = as.data.frame(df)
names(df) = c("Min.NOx", "Max.NOx", "Temperature")

plot.data = df %>% gather(Variable, NOx.Emissions, -Temperature)

p = ggplot(plot.data, aes(x = Temperature, y = NOx.Emissions, colour = Variable))
p = p + geom_point()
