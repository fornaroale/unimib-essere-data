
import_data_arcan <- function(path) {
  
  # Read data folders
  setwd(path)
  returnData <- list()
  
  for (dir in list.dirs()[-1]) {
    # get into the sub directory
    setwd(dir)
    full.dir <- getwd()
    
    # Retrieve folder name
    project.name <- basename(full.dir)
    
    # Load csv files
    files.name = list.files(full.dir, "*.csv", all.files = FALSE, full.names = FALSE)
    files.num = length(files.name)
    ls <- list()
    if(files.num){
      for (j in 1:files.num){
        ls[[files.name[j]]] = read.csv(paste(full.dir,files.name[j],sep = "/"), header = TRUE)
      }
    }
    
    # save project data
    returnData[[project.name]] <- ls
    
    # pop back up to the parent directory
    setwd("../")
  }
  
  return (returnData)
}


extract_data_arcan <- function(dataset){
  project.name <- names(dataset)[1]
  dataset <- dataset[[1]]
  
  # AS.package  <- subset(dataset[["smell-characteristics.csv"]], 
  #                       AffectedComponentType==as.character("CONTAINER"))
  AS.package  <- dataset[["smell-characteristics.csv"]]
    
  LOC.metrics <- subset(dataset[["component-metrics.csv"]], 
                        ComponentType==as.character("PACKAGE"))
  LOC         <- sum(LOC.metrics$LinesOfCode)
  
  number.of.AS <- as.numeric(nrow(AS.package))
  AS.by.type   <- table(AS.package[["smellType"]])
  
  returnData        <- as.vector(AS.by.type)
  names(returnData) <- names(AS.by.type)
  
  # Create data
  data <- cbind(data.frame(number.of.AS, LOC),t(returnData))
  rownames(data) <- project.name
  
  return (data)
}


compute_metrics_statistics <- function(data){
  df <- data[1:ncol(data)]
  statistics <- as.data.frame(matrix(nrow = 4, ncol = ncol(df)))
  names(statistics) <- names(df)
  rownames(statistics) <- c("mean", "sd", "min", "max")
  
  for (i in colnames(df)){
    mean <- mean(df[,i])
    sd   <- sd(df[,i])
    min  <- min(df[,i])
    max  <- max(df[,i])
    statistics[,i] <- c(mean, sd, min, max)
  }
  
  return(statistics)
}


import_data_designite <- function(path) {
  
  # Read data folders
  setwd(path)
  returnData <- list()
  
  for (dir in list.dirs()[-1]) {
    # get into the sub directory
    setwd(dir)
    full.dir <- getwd()
    
    # Retrieve folder name
    project.name <- basename(full.dir)
    
    # Load csv files
    files.name = list.files(full.dir, "*.csv", all.files = FALSE, full.names = FALSE)
    files.num = length(files.name)
    ls <- list()
    if(files.num){
      for (j in 1:files.num){
        if(files.name[j] == "ArchitectureSmells.csv" || files.name[j] == "TypeMetrics.csv" ||
           files.name[j] == "ImplementationSmells.csv" || files.name[j] == "DesignSmells.csv"){
          
          readData <- read.csv(paste(full.dir,files.name[j],sep = "/"), sep=",", header = TRUE, row.names=NULL)
          
          if(!is.na(readData$Project.Name[1]) && readData$Project.Name[1] != project.name){
            names(readData)[1:(ncol(readData)-1)] <- names(readData)[2:ncol(readData)]
          }
          
          readData <- readData[grep(project.name, readData$Project.Name),]
          ls[[files.name[j]]] <- readData 
        }
      }
    }
    
    # save project data
    returnData[[project.name]] <- ls
    
    # pop back up to the parent directory
    setwd("../")
  }
  
  return (returnData)
}


extract_data_designite <- function(dataset){
  project.name <- names(dataset)[1]
  dataset <- dataset[[1]]
  
  # extract number of Architectural Smells & count LOC
  number.of.AS <- as.numeric(nrow(dataset[["ArchitectureSmells.csv"]]))
  metrics      <- dataset[["TypeMetrics.csv"]]
  LOC          <- sum(metrics$LOC)
  
  # group smell by type
  AS.by.type   <- table(dataset[["ArchitectureSmells.csv"]][["Architecture.Smell"]])
  
  # create data to return
  returnData        <- as.vector(AS.by.type)
  names(returnData) <- names(AS.by.type)
  
  # prepare return data
  data <- cbind(data.frame(number.of.AS, LOC),t(returnData))
  rownames(data) <- project.name
  
  return (data)
}


extract_cs_designite <- function(dataset){
  project.name <- names(dataset)[1]
  dataset <- dataset[[1]]
  
  # extract number of Design Smells & Implementation Smells
  number.of.DS <- as.numeric(nrow(dataset[["DesignSmells.csv"]]))
  number.of.IS <- as.numeric(nrow(dataset[["ImplementationSmells.csv"]]))
  
  # group smell by type
  DS.by.type <- table(dataset[["DesignSmells.csv"]][["Design.Smell"]])
  IS.by.type <- table(dataset[["ImplementationSmells.csv"]][["Implementation.Smell"]])
  
  # create data to return for design smells
  DS.returnData        <- as.vector(DS.by.type)
  names(DS.returnData) <- names(DS.by.type)
  
  # create data to return for implementation smells
  IS.returnData        <- as.vector(IS.by.type)
  names(IS.returnData) <- names(IS.by.type)
  
  # prepare return data
  DS.data <- data.frame(t(DS.returnData))
  rownames(DS.data) <- project.name
  IS.data <- data.frame(t(IS.returnData))
  rownames(IS.data) <- project.name
  
  # calculate totals of design & implementation Smells
  DS.data <- cbind(DS.data, Total = rowSums(DS.data))
  IS.data <- cbind(IS.data, Total = rowSums(IS.data))
  
  # prepare return list (design smells & implementation smells)
  list.data <- list(DS.data, IS.data)
  
  return (list.data)
}

