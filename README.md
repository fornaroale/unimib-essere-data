# Evolution of Software Systems and Reverse Engineering
## Progetto d'Esame - Dataset e Script R

This folder contains the R scripts and datasets discussed in the exam project.
- **./analysis_arcan_archive** contains the raw data produced by the projects analysis conducted via Arcan
- **./analysis_arcan_filters** contains the filters used to analyze the projects with Arcan
- **./analysis_designite_archive** contains the raw data produced by the projects analysis conducted via DesigniteJava
- **./script_R** contains the R scripts necessary to:
    * extract data from raw files, compute metrics statistics and run Mann-Kendall tests (`main.R`)
    * study the evolution of ADI values for each project considering only the 5 versions object of study (`adievolution.R`)
    * study the evolution of ADI values for each project considering more versions (`adievolution-big.R`)

Note: to ensure that the R script to aggregate the data produced by Designite works correctly, the folder containing the data must have the same name indicated in the -o flag when Designite has been started to analyze the project.