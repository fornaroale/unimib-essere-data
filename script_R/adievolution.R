
library(dplyr)
library(ggplot2)
library(stringr)
library(Kendall)

rm(list = ls())
base.dir <- "C:\\Users\\ale\\Desktop\\WDIR_PUB\\script_R\\"
setwd(base.dir)

# ------------------------------
# --> Arcan ADI evolution analysis
# ------------------------------


plot_ADI_evolution <- function(data){
  plot.data <- data %>% subset(select=c("ADI","version","commit")) %>% 
                        group_by(version) %>% 
                        summarise(ADI=first(ADI), commit=first(commit)) %>%
                        mutate(commit=as.numeric(rownames(.)))
  
  ggplot(plot.data, aes(x=version,y=ADI, group=1)) + geom_line() +
    geom_point() +
    labs(x = "Version") +
    geom_text(aes(label = ADI), nudge_y=-0.5, nudge_x=-0.2)
}

plot_ADI_broader_evolution <- function(data){
  plot.data <- data %>% subset(select=c("ADI","commit")) %>%
    mutate(version=as.numeric(rownames(.)))
  
  ggplot(plot.data, aes(x=version,y=ADI)) + geom_line() +
    geom_point() +
    labs(x = "Version")
}

import_ADI_values <- function(csv.path){
  adi.csv <- read.csv(csv.path, header = TRUE)
  projects.names <- unique(adi.csv$project)
  adi.data <- list()
  for (p in projects.names) {
    adi.data[[p]] <- adi.csv[adi.csv$project==p,]
    adi.data[[p]] <- adi.data[[p]] %>% as.data.frame(row.names = 1:nrow(.))
  }
  
  return (adi.data)
}

# Plot actual ADI data
ADI.data <- import_ADI_values("data/adi_values.csv")
plot_ADI_evolution(ADI.data[[1]]) # teastore
plot_ADI_evolution(ADI.data[[2]]) # sitewhere
plot_ADI_evolution(ADI.data[[3]]) # sharebike
plot_ADI_evolution(ADI.data[[4]]) # trainticket

# Run Mann-Kendall on ADI
MannKendall(ADI.data[[1]]$ADI) # teastore
MannKendall(ADI.data[[2]]$ADI) # sitewhere
MannKendall(ADI.data[[3]]$ADI) # sharebike
MannKendall(ADI.data[[4]]$ADI) # trainticket

####### OLD CODE:
# # Consider a broader set of values
# ADI.broader.data <- import_ADI_values("data/adi_values_broader.csv")
# plot_ADI_broader_evolution(ADI.broader.data[[1]]) # teastore
# plot_ADI_broader_evolution(ADI.broader.data[[2]]) # sitewhere
# plot_ADI_broader_evolution(ADI.broader.data[[3]]) # sharebike
# plot_ADI_broader_evolution(ADI.broader.data[[4]]) # trainticket
# 
# # Run Mann-Kendall on ADI
# MannKendall(ADI.broader.data[[1]]$ADI) # teastore
# MannKendall(ADI.broader.data[[2]]$ADI) # sitewhere
# MannKendall(ADI.broader.data[[3]]$ADI) # sharebike
# MannKendall(ADI.broader.data[[4]]$ADI) # trainticket
