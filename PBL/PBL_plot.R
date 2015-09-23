library(ncdf)
library(ggplot2)
library(ggthemes)
library(Cairo)

d = open.ncdf("ZMBL.nc")
zmbl = get.var.ncdf(d)
close.ncdf(d)

time = seq(0, 2880, 20)
time = time / 1440 #convert mins to days

data = data.frame(time, zmbl)

p = ggplot(data, aes(x = time, y = zmbl))
p = p + geom_line()
p = p + theme_tufte()
p = p + xlab("Time (days)") + ylab("PBL Height (m)")
p = p + scale_x_continuous(expand = c(0, 0))
p = p + scale_y_continuous(expand = c(0, 0), limits = c(0, 600))
p = p + theme(axis.line = element_line(colour = "black"))
p = p + theme(axis.title = element_text(face = "bold"))

CairoPDF(file = "PBL_time_series.pdf", width = 7, height = 5)
print(p)
dev.off()
