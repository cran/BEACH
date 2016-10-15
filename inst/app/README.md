Biometric Exploratory Analysis Creation House (BEACH) is a shiny app that provides automation platform for users.

Before running BEACH, please make sure your computer is connected to internet and the following packages are installed.

dep.packages <- c("shiny", "DT", "haven", "xtable", "rtf", "plyr", "sas7bdat", "WriteXLS", "SASxport", "rJava");
na.packages <- dep.packages[!dep.packages %in% installed.packages()]
if (length(na.packages)>0) install.packages(na.packages);

if(!"sas7bdat.parso" %in% installed.packages()) devtools::install_github('BioStatMatt/sas7bdat.parso', force=TRUE)



Please set up your default internet browser as google chrome
Then, in your R console, please run the following code to run BEACH locally.

library(shiny);
runGitHub("BEACH", "DanniYuGithub");


To install the package from R cran, please check the link https://cran.r-project.org/web/packages/BEACH/index.html
