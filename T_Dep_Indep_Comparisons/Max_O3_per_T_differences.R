# Calculate differences in max O3 at each Temperature for different NOx conditions, determined on H2O2/HNO3 ratio. Each mechanism and run
# Version 0: Jane Coates 24/10/2015

runs = c("Dependent", "Independent")
mechanisms = c("CB05", "RADM2")

get_NOx_condition = function (x) {
    if (x > 0.5) {
        condition = "Low-NOx"
    } else if (x < 0.08) {
        condition = "High-NOx"
    } else {
        condition = "Maximal-O3"
    }
    return (condition)
}

mechanism_data_frame = function (mechanism, dataframe) {
    data = dataframe %>%    filter(Mechanism == mechanism) %>% 
                            mutate(H2O2.HNO3.Ratio = H2O2/HNO3, Temperature.C = Temperature - 273) %>% 
                            select(Mechanism, NOx.Emissions, Temperature.C, O3, H2O2.HNO3.Ratio) %>%
                            rowwise() %>% 
                            mutate(NOx.Condition = get_NOx_condition(H2O2.HNO3.Ratio))
    summarised.data = data %>%  group_by(Mechanism, Temperature.C, NOx.Condition) %>%
                                summarise(Max.O3 = max(O3))
    summarised.data$NOx.Condition = factor(summarised.data$NOx.Condition, levels = c("Low-NOx", "Maximal-O3", "High-NOx"))
    return(summarised.data)
}

get_data = function (run) {
    filename = paste0("Temperature_", run, "_data.csv")
    d = read.csv(filename)
    data = lapply(mechanisms, mechanism_data_frame, dataframe = d)
    df = do.call("rbind", data)
    df$Run = rep(paste("Temperature", run, "\nIsoprene Emissions"), length(df$NOx.Condition)) 
    return(df)
}
list.data = lapply(runs, get_data)
indep = tbl_df(list.data[[2]])
dep = tbl_df(list.data[[1]])

Abs.O3.Diff = dep$Max.O3 - indep$Max.O3
Rel.O3.Diff = Abs.O3.Diff / dep$Max.O3
df = data.frame(Abs.O3.Diff, Rel.O3.Diff, Mechanism = dep$Mechanism, Temperature.C = dep$Temperature.C, NOx.Condition = dep$NOx.Condition)

my.colours = c("MCMv3.2" = "#000000", "CB05" = "#0e5c28", "RADM2" = "#e6ab02", "MOZART-4" = "#6c254f", "CRIv2" = "#ef6638")
plot.lines  = function () {
    list(   geom_point(),
            xlab(expression(bold(paste("Temperature (", degree, "C)")))),
            theme_tufte(),
            theme(axis.line = element_line(colour = "black")),
            theme(axis.title = element_text(face = "bold")),
            theme(strip.text = element_text(face = "bold")),
            theme(legend.position = "top"),
            theme(legend.title = element_blank()),
            scale_colour_manual(values = my.colours)
    )
}

p = ggplot(df, aes(x = Temperature.C, y = Abs.O3.Diff, colour = Mechanism))
p = p + facet_wrap( ~ NOx.Condition)
p = p + ylab("Absolute Difference in Max O3 (ppbv)")
p = p + plot.lines()

CairoPDF(file = "Absolute_Difference_O3_T_NOx_condition.pdf", width = 10, height = 7)
print(p)
dev.off()

p = ggplot(df, aes(x = Temperature.C, y = Rel.O3.Diff, colour = Mechanism))
p = p + facet_wrap( ~ NOx.Condition)
p = p + ylab("Percent Difference in Max O3 Relative to Temperature Dependent Run")
p = p + scale_y_continuous(labels = percent)
p = p + plot.lines()

CairoPDF(file = "Relative_Difference_O3_T_NOx_condition.pdf", width = 10, height = 7)
print(p)
dev.off()
