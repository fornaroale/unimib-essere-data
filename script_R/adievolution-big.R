
rm(list = ls())
base.dir <- "C:\\Users\\ale\\Desktop\\WDIR_PUB\\script_R\\"
setwd(base.dir)

# --> Import sources
source("utils.R")

plot_ADI_evolution <- function(data){
  plot.data <- data %>% subset(select=c("ADI","Commit")) %>%
    mutate(version=as.numeric(rownames(.)))
  
  ggplot(plot.data, aes(x=version,y=ADI)) + geom_line() +
    geom_point() +
    labs(x = "Version")
}

project.name <- "sitewhere" # change to project name
ADI.data <- import_data_arcan(paste(base.dir, "/evol_data", sep=""))
setwd(base.dir)

# Consider only smell-characteristics file to retrieve ADIs total
ADI.data <- ADI.data[[project.name]]
ADI.data.sc <- ADI.data[[3]] # extract smells characteristics
ADI.data.sc.sub <- subset(ADI.data.sc, select = c("project", "versionId", 
                                                  "versionDate", "smellType", 
                                                  "ATDI", "ATDI_WEIGHTED", 
                                                  "Severity", "Shape", "Size", 
                                                  "AffectedElements"))
# Calculate sum of ATDI_WEIGHTED by 
data.aggregated <- aggregate(cbind(ADI.data.sc.sub$ATDI_WEIGHTED), 
                             by=list(date=ADI.data.sc.sub$versionDate,
                                     version=ADI.data.sc.sub$versionId), 
                             FUN=sum)
data.aggregated <- data.aggregated[order(as.Date(data.aggregated$date)),] # order by date
data.aggregated$Project <- ADI.data.sc.sub$project[[1]]
names(data.aggregated) <- c("Date", "Commit", "ADI", "Project")
data.aggregated <- data.aggregated %>% as.data.frame(row.names = 1:nrow(.))
data.aggregated

# Plot ADI evolution
plot_ADI_evolution(data.aggregated)

# Run Mann-Kendall on ADI
MannKendall(data.aggregated$ADI)

# Export CSV data
export.data <- subset(data.aggregated, select = c("Project", "Commit", "ADI"))
export.data$Commit <- substr(data.aggregated$Commit, 0, 8)
write.csv(export.data, 
          paste(getwd(),"data",paste("adi-evol-",project.name,".csv",sep=""),sep="/"), 
          row.names=FALSE)
