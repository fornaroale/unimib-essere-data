
rm(list = ls())
base.dir <- "C:\\Users\\ale\\Desktop\\WDIR_PUB\\script_R\\"
setwd(base.dir)

library(dplyr)
library(Kendall)
library(ggplot2)

# --> Import sources
source("utils.R")


# ------------------------------
# --> Arcan analysis
# ------------------------------


# Extract data
arcan.data = import_data_arcan(paste(base.dir, "/data/arcan/", sep=""))

# Manipulate data (create report)
arcan.ds1 = extract_data_arcan(arcan.data[1])
arcan.ds2 = extract_data_arcan(arcan.data[2])
arcan.ds3 = extract_data_arcan(arcan.data[3])
arcan.ds4 = extract_data_arcan(arcan.data[4])
arcan.ds5 = extract_data_arcan(arcan.data[5])

# Put data together
arcan.ds = dplyr::bind_rows(arcan.ds1, 
                            arcan.ds2,
                            arcan.ds3,
                            arcan.ds4,
                            arcan.ds5)
arcan.ds[is.na(arcan.ds)] <- 0

# Compute statistics of architectural smells
compute_metrics_statistics(arcan.ds)

# Run Mann-Kendall on arc. smells computed by Arcan
for (metric in names(arcan.ds)) {
  if(metric != "number.of.AS" & metric != "LOC"){
    metric.values <- data.frame(arcan.ds[[metric]])
    names(metric.values) <- metric
    print(metric)
    print(MannKendall(metric.values[,1]))
  }
}

# ------------------------------
# --> DesigniteJava analysis
# ------------------------------


# Extract data
designite.data = import_data_designite(paste(base.dir, "/data/designite/", sep=""))

# Manipulate data (create report)
designite.ds1 = extract_data_designite(designite.data[1])
designite.ds2 = extract_data_designite(designite.data[2])
designite.ds3 = extract_data_designite(designite.data[3])
designite.ds4 = extract_data_designite(designite.data[4])
designite.ds5 = extract_data_designite(designite.data[5])

# Put data together
designite.ds = dplyr::bind_rows(designite.ds1, 
                                designite.ds2,
                                designite.ds3,
                                designite.ds4,
                                designite.ds5)
designite.ds[is.na(designite.ds)] <- 0
designite.ds

# Compute statistics of architectural smells
compute_metrics_statistics(designite.ds)

# Run Mann-Kendall on arc. smells computed by Designite
for (smell in names(designite.ds)) {
  if(smell != "number.of.AS" & smell != "LOC"){
    smell.values <- data.frame(designite.ds[[smell]])
    names(smell.values) <- smell
    print(smell)
    print(MannKendall(smell.values[,1]))
  }
}

# Create report of design smells
designite.cs.ds1 = extract_cs_designite(designite.data[1])
designite.cs.ds2 = extract_cs_designite(designite.data[2])
designite.cs.ds3 = extract_cs_designite(designite.data[3])
designite.cs.ds4 = extract_cs_designite(designite.data[4])
designite.cs.ds5 = extract_cs_designite(designite.data[5])
designite.cs.ds.design = dplyr::bind_rows(designite.cs.ds1[1], 
                                          designite.cs.ds2[1],
                                          designite.cs.ds3[1],
                                          designite.cs.ds4[1],
                                          designite.cs.ds5[1])
designite.cs.ds.design <- designite.cs.ds.design %>% relocate(Total, .after = last_col())
designite.cs.ds.design[is.na(designite.cs.ds.design)] <- 0
designite.cs.ds.design.mk <- designite.cs.ds.design
designite.cs.ds.design["Total" ,] <- colSums(designite.cs.ds.design)
designite.cs.ds.design

# Run Mann-Kendall on design smells computed by Designite
for (smell in names(designite.cs.ds.design.mk)) {
  if(smell != "Total"){
    smell.values <- data.frame(designite.cs.ds.design.mk[[smell]])
    names(smell.values) <- smell
    print(smell)
    print(MannKendall(smell.values[,1]))
  }
}

# Create report of implementation smells
designite.cs.ds.impl   = dplyr::bind_rows(designite.cs.ds1[2], 
                                          designite.cs.ds2[2],
                                          designite.cs.ds3[2],
                                          designite.cs.ds4[2],
                                          designite.cs.ds5[2])
designite.cs.ds.impl <- designite.cs.ds.impl %>% relocate(Total, .after = last_col())
designite.cs.ds.impl[is.na(designite.cs.ds.impl)] <- 0
designite.cs.ds.impl.mk <- designite.cs.ds.impl
designite.cs.ds.impl["Total" ,] <- colSums(designite.cs.ds.impl)
designite.cs.ds.impl

# Run Mann-Kendall on implementation smells detected by Designite
for (smell in names(designite.cs.ds.impl.mk)) {
  if(smell != "Total"){
    smell.values <- data.frame(designite.cs.ds.impl.mk[[smell]])
    names(smell.values) <- smell
    print(smell)
    print(MannKendall(smell.values[,1]))
  }
}

# Plot the evolution of no. of implementation smells found by Designite
plot_ImplSmells_evolution <- function(data){
  data <- data %>% as.data.frame(row.names = 1:nrow(.))
  plot.data <- data %>% subset(select=c("Total")) %>%
    mutate(version=as.numeric(rownames(.)))
  ggplot(plot.data, aes(x=version,y=Total)) + geom_line() +
    geom_point() +
    labs(x = "Version")
}
plot_ImplSmells_evolution(designite.cs.ds.impl[1:5,])
