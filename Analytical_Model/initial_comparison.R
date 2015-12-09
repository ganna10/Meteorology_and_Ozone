# comparing model results to analytical model from Pusede:2014
# Version 0: Jane Coates 04/12/2015

setwd("~/Documents//Analysis//2015_Meteorology_and_Ozone//Analytical_Model")
date = "09122015"
# mechanisms <- c("MCMv3.2", "CRIv2", "MOZART-4", "CB05", "RADM2")
mechanisms <- c("CB05")

# data
data.list <- lapply(mechanisms, get_analytical_model_data, Run.Label = "Modified_MEGAN", Date = date)
df <- do.call("rbind", data.list)
tbl_df(df)

data <- df %>%
  rowwise() %>%
  mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3), Temperature.C = Temperature - 273, NO.NOx = NO/(NO + NO2), VOCR.NOxR = VOCR/NOxR, PHOx = PHOx * 1e12, O3 = O3 * 1e9, PO3 = PO3 * 1e9) %>%
  select(-H2O2, -HNO3, -Temperature) %>%
  group_by(Mechanism, Temperature.C, NOx.Condition) %>%
  summarise(VOCR = mean(VOCR), PHOx = mean(PHOx), NO.NOx = mean(NO.NOx), NO = mean(NO), NO2 = mean(NO2), O3 = mean(O3), NOxR = mean(NOxR), PO3 = mean(PO3), VOCR.NOxR = mean(VOCR.NOxR))
data$NOx.Condition <- factor(data$NOx.Condition, levels = c("High-NOx", "Maximal-O3", "Low-NOx"))

my.colours <- c("MCMv3.2" = "#6c254f", "CRIv2" = "#ef6638", "MOZART-4" = "#2b9eb3", "CB05" = "#0e5c28", "RADM2" = "#f9c500")
plot.lines <- list(
  geom_point(size = 1, aes(colour = Mechanism)),
  facet_wrap( ~ NOx.Condition),
  plot_theme(),
  scale_colour_manual(values = my.colours),
  theme(legend.title = element_blank()),
  theme(legend.position = "top"),
  xlab(expression(bold(paste("Temperature (", degree, "C)")))),
  scale_x_continuous(expand = c(0, 0)),
  scale_y_continuous(expand = c(0, 0)),
  theme(panel.margin = unit(4, "mm")))

# VOCR Plot
vocr <- ggplot(data, aes(x = Temperature.C, y = VOCR))
vocr <- vocr + plot.lines
vocr <- vocr + ylab("VOCR (s-1)")
vocr <- vocr + scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, 5), expand = c(0, 0))
vocr <- vocr + ggtitle("Total VOC OH Reactivity")
vocr <- vocr + theme(legend.position = "top")
vocr + theme(panel.grid = element_line(colour = "grey"))

# NOxR Plot
noxr <- ggplot(data, aes(x = Temperature.C, y = NOxR))
noxr <- noxr + plot.lines
noxr <- noxr + ylab("NOxR (s-1)")
# noxr <- noxr + scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.5), expand = c(0, 0))
noxr <- noxr + ggtitle("Total NOx OH Reactivity")
noxr

# VOCR/NOxR Plot
vocr.noxr <- ggplot(data, aes(x = Temperature.C, y = VOCR.NOxR))
vocr.noxr <- vocr.noxr + plot.lines
vocr.noxr <- vocr.noxr + ylab("VOCR/NOxR")
vocr.noxr <- vocr.noxr + ggtitle("VOCR/NOxR OH Reactivity")
vocr.noxr <- vocr.noxr + facet_wrap(~ NOx.Condition, scales = "free")
vocr.noxr

# PHOx Plot
phox <- ggplot(data, aes(x = Temperature.C, y = PHOx))
phox <- phox + plot.lines
phox <- phox + ylab("HOx Production (ppt/s)")
phox <- phox + ggtitle("HOx (= OH + HO2 + RO2) Production Rate")
# phox <- phox + scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5), expand = c(0, 0))
phox

# NO/NOx Plot
no.nox <- ggplot(data, aes(x = Temperature.C, y = NO.NOx))
no.nox <- no.nox + plot.lines
no.nox <- no.nox + ylab("NO / NOx")
no.nox <- no.nox + ggtitle("NO/NOx Mixing Ratios")
# no.nox <- no.nox + scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 0.1), expand = c(0, 0))
no.nox

# O3 Plot
o3 <- ggplot(data, aes(x = Temperature.C, y = O3))
o3 <- o3 + plot.lines
o3 <- o3 + ylab("O3 (ppbv)")
o3 <- o3 + ggtitle("O3 Mixing Ratios")
# o3 <- o3 + scale_y_continuous(limits = c(40, 60), breaks = seq(40, 60, 10), expand = c(0, 0))
o3

# PO3 plot
po3 <- ggplot(data, aes(x = Temperature.C, y = PO3))
po3 <- po3 + plot.lines
po3 <- po3 + ylab("PO3 (ppb/h)")
po3 <- po3 + ggtitle("Ozone Production Rate")
# po3 <- po3 + scale_y_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, 0.05), expand = c(0, 0))
po3

g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
my.legend <- g_legend(vocr)

grid.arrange(arrangeGrob(vocr + theme(legend.position = "none"), 
                         noxr + theme(legend.position = "none"),
                         vocr.noxr + theme(legend.position = "none"),
                         no.nox + theme(legend.position = "none"),
                         phox + theme(legend.position = "none"),
                         po3 + theme(legend.position = "none"),
                         o3 + theme(legend.position = "none"),
                         my.legend,
                         ncol = 2))

CairoPDF(file = "Analytical_model_comparison.pdf", width = 10, height = 7)
print(grid.arrange(my.legend, 
                   arrangeGrob(vocr + theme(legend.position = "none"), 
                               phox + theme(legend.position = "none"),
                               no.nox + theme(legend.position = "none"),
                               o3 + theme(legend.position = "none"),
                               nrow = 2), 
                   nrow = 2, 
                   heights = c(1, 6)))
dev.off()

CairoPDF(file = "PO3_model.pdf")
print(po3)
dev.off()
