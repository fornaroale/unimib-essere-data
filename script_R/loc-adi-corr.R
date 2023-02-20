
rm(list = ls())
base.dir <- "C:\\Users\\ale\\Desktop\\WDIR_PUB\\script_R\\"
setwd(base.dir)

# --> Is there a correlation between LOC and ATDI computed by Arcan?

data <- read.csv(paste(base.dir,"data/adi_loc_values.csv",sep = "/"), header = TRUE)

# Visualize data using scatter plots
library("ggpubr")
ggscatter(data, x = "LOC", y = "ADI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Lines of Code", ylab = "ADI (computed by Arcan)")

# Note how distributions are skewed (LOC!) --> data isn't normally distributed
ggdensity(data$LOC)
shapiro.test(data$LOC)    # NOT normally distributed
ggdensity(data$ADI)       # distribution is a bit skewed
shapiro.test(data$ADI)    # even if ImplSmell suggests to be normally distributed

# Run Kendall's rank correlation tau to check correlation
cor.test(data$LOC, data$ADI,  method="kendall")
