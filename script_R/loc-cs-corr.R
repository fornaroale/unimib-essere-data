
rm(list = ls())
base.dir <- "C:\\Users\\ale\\Desktop\\WDIR_PUB\\script_R\\"
setwd(base.dir)

# --> Is there a correlation between LOC and no. of 
#     implementation smells detected by Designite?

data <- read.csv(paste(base.dir,"data/cs_loc_values.csv",sep = "/"), header = TRUE)

# Visualize data using scatter plots
library("ggpubr")
ggscatter(data, x = "LOC", y = "ImplSmell", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Lines of Code", ylab = "No. of Implementation Smells")

# Note how distributions are skewed (LOC!) --> data isn't normally distributed
ggdensity(data$LOC)
shapiro.test(data$LOC)       # NOT normally distributed
ggdensity(data$ImplSmell)    # distribution is a bit skewed
shapiro.test(data$ImplSmell) # even if ImplSmell suggests to be normally distributed

# Run Kendall's rank correlation tau to check correlation
cor.test(data$LOC, data$ImplSmell,  method="kendall")
