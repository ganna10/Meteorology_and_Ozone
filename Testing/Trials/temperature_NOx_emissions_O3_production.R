# Plots of NOx emissions and Temperature vs O3 production for each mechanism. date of file (ddmmyyyy) is the input arg
# Version 0: Jane Coates 28/9/2015

library(reshape2)
library(methods)
library(grid)
library(quadprog)
library(proto)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(akima)
library(directlabels)
library(Cairo)
library(ggthemes)
library(scales)

args = commandArgs(trailingOnly = TRUE)

filename = paste0("out_Temperature_NOx_", args[[1]], ".csv")
d = read.table(file = filename, header = TRUE, sep  = ",")
d = tbl_df(d)

species = c("O3.Production")

## Modify "draw.rects"
draw.rects.modified <- function(d,...){
  if(is.null(d$box.color))d$box.color <- NA
  if(is.null(d$fill))d$fill <- "white"
  for(i in 1:nrow(d)){
    with(d[i,],{
      grid.rect(gp = gpar(col = box.color, fill = fill),
                vp = viewport(x, y, w, h, "cm", c(hjust, vjust), angle=rot))
    })
  }
  d
} 

## Load "far.from.others.borders" 
far.from.others.borders <- function(all.groups,...,debug=FALSE){
  group.data <- split(all.groups, all.groups$group)
  group.list <- list()
  for(groups in names(group.data)){
    ## Run linear interpolation to get a set of points on which we
    ## could place the label (this is useful for e.g. the lasso path
    ## where there are only a few points plotted).
    approx.list <- with(group.data[[groups]], approx(x, y))
    if(debug){
      with(approx.list, grid.points(x, y, default.units="cm"))
    }
    group.list[[groups]] <- data.frame(approx.list, groups)
  }
  output <- data.frame()
  for(group.i in seq_along(group.list)){
    one.group <- group.list[[group.i]]
    ## From Mark Schmidt: "For the location of the boxes, I found the
    ## data point on the line that has the maximum distance (in the
    ## image coordinates) to the nearest data point on another line or
    ## to the image boundary."
    dist.mat <- matrix(NA, length(one.group$x), 3)
    colnames(dist.mat) <- c("x","y","other")
    ## dist.mat has 3 columns: the first two are the shortest distance
    ## to the nearest x and y border, and the third is the shortest
    ## distance to another data point.
    for(xy in c("x", "y")){
      xy.vec <- one.group[,xy]
      xy.mat <- rbind(xy.vec, xy.vec)
      lim.fun <- get(sprintf("%slimits", xy))
      diff.mat <- xy.mat - lim.fun()
      dist.mat[,xy] <- apply(abs(diff.mat), 2, min)
    }
    other.groups <- group.list[-group.i]
    other.df <- do.call(rbind, other.groups)
    for(row.i in 1:nrow(dist.mat)){
      r <- one.group[row.i,]
      other.dist <- with(other.df, (x-r$x)^2 + (y-r$y)^2)
      dist.mat[row.i,"other"] <- sqrt(min(other.dist))
    }
    shortest.dist <- apply(dist.mat, 1, min)
    picked <- calc.boxes(one.group[which.max(shortest.dist),])
    ## Mark's label rotation: "For the angle, I computed the slope
    ## between neighboring data points (which isn't ideal for noisy
    ## data, it should probably be based on a smoothed estimate)."
    left <- max(picked$left, min(one.group$x))
    right <- min(picked$right, max(one.group$x))
    neighbors <- approx(one.group$x, one.group$y, c(left, right))
    slope <- with(neighbors, (y[2]-y[1])/(x[2]-x[1]))
    picked$rot <- 180*atan(slope)/pi
    output <- rbind(output, picked)
  }
  output
}

get.labels = function (break.points, orig.data, digits) {
    labels = lapply(break.points,
                    function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits )
            )
    return (labels)
}

get.data = function (mechanism, spc, dataframe) {
    data = dataframe %>% filter(Mechanism == mechanism)
    data = data %>% filter(NOx.Emissions <= 7.5e9)
    data = data %>% mutate(Scaled.Temperature = (Temperature - min(Temperature))/(max(Temperature) - min(Temperature)), Scaled.NOx.Emissions = (NOx.Emissions - min(NOx.Emissions))/(max(NOx.Emissions) - min(NOx.Emissions)))
    if (spc == "HOx") {
        data = data %>% mutate(HOx = OH + HO2) %>% select(-OH, -HO2)
    }

    colnum = match(spc, names(data))
    fld = with(data, interp(x = Scaled.Temperature, y = Scaled.NOx.Emissions, z = data[[colnum]], duplicate = "strip"))
    df = melt(fld$z, na.rm = TRUE)
    names(df) = c("x", "y", "O3.Production")
    df$Temperature = fld$x[df$x]
    df$NOx.Emissions = fld$y[df$y]
    
    if (mechanism == "MOZART") {
        df$Mechanism = rep("MOZART-4", length(df$O3.Production))
    } else if (mechanism == "MCM") {
        df$Mechanism = rep("MCMv3.2", length(df$O3.Production))
    } else if (mechanism == "CRI") {
        df$Mechanism = rep("CRIv2", length(df$O3.Production))
    } else {
        df$Mechanism = rep(mechanism, length(df$O3.Production))
    }
    return (df)
}

get.plot = function (spc, data) { 
    if (spc == "HOx") {
        columns = c("Mechanism", "OH", "HO2", "NOx.Emissions", "Temperature")
    } else {
        columns = c("Mechanism", spc, "NOx.Emissions", "Temperature")
    }
    column.numbers = match(columns, names(data))
    data = data %>% select(column.numbers)
    
    mechanisms = c("MCM", "MOZART", "CRI", "RADM2", "CB05")
    mechanism.data = lapply(mechanisms, get.data, spc = spc, dataframe = data) #returns list of dataframes
    
    df = do.call("rbind", mechanism.data) #combining into 1 data frame
    
    mozart.data = data %>% filter(Mechanism == "MOZART") #to get labels
    temperature.break.points = seq(0, 1, 0.2)
    temperature.labels = get.labels(temperature.break.points, mozart.data$Temperature, digits = 2) 
    NOx.Emissions.break.points = seq(0, 1, 0.2)
    NOx.Emissions.labels = get.labels(NOx.Emissions.break.points, mozart.data$NOx.Emissions, digits = 2)
    NOx.Emissions.labels = lapply(NOx.Emissions.labels, function (i) sprintf("%0.2e", i))

    title = "Cumulative O3 Production as Function of Temperature and Cumulative NOx Emissions"
    
    p = ggplot(df, aesx = Temperature, y = NOx.Emissions, z = O3.Production))
    p = p + stat_contour(aes(colour = ..level..)) 
    p = p + facet_wrap(~ Mechanism, scales = "free_x") 
    p = p + theme_tufte() 
    p = p + ggtitle(title)
    p = p + theme(plot.title = element_text(face = "bold"))
    p = p + theme(axis.line = element_line(colour = "black")) 
    p = p + theme(strip.text = element_text(face = "bold")) 
    p = p + theme(panel.margin = unit("5", "mm"))
    p = p + xlab("Temperature (K)") 
    p = p + ylab("NOx emissions (molecules(NOx) cm-3 s-1)") 
    p = p + theme(axis.title = element_text(face = "bold")) 
    p = p + scale_colour_continuous(name = "Cumulative O3 production")
    p = p + scale_x_continuous(breaks = temperature.break.points, labels = temperature.labels)
    p = p + scale_y_continuous(breaks = NOx.Emissions.break.points, labels = NOx.Emissions.labels)

    #angled.boxes <- list("far.from.others.borders", "calc.boxes", "enlarge.box", "draw.rects.modified")
    
    filename = paste0("plot_temperature_NOx_emissions_", spc, ".pdf")
    CairoPDF(file = filename, width = 10, height = 7)
    print(direct.label(p, cex = .7))
    dev.off()
}
lapply(species, get.plot, data = d)
