library(dplyr)
rm(list = ls())
base.dir <- "C:\\Users\\ale\\Desktop\\WDIR_PUB\\script_R\\"
setwd(base.dir)

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

# Compute statistics of architectural smells
compute_metrics_statistics(designite.ds)

# Create report of design and implementation smells
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
designite.cs.ds.impl   = dplyr::bind_rows(designite.cs.ds1[2], 
                                          designite.cs.ds2[2],
                                          designite.cs.ds3[2],
                                          designite.cs.ds4[2],
                                          designite.cs.ds5[2])
designite.cs.ds.impl <- designite.cs.ds.impl %>% relocate(Total, .after = last_col())
designite.cs.ds.impl[is.na(designite.cs.ds.impl)] <- 0


