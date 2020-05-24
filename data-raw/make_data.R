library(jsonlite)
library(devtools)
library(readr)

# QSF Template 
qsf0 <- read_json("frame-template.qsf")
sq0 <- read_json("sq-template.qsf")

use_data(qsf0, sq0, overwrite=TRUE, internal=TRUE)


svy_df <- read_csv2("sq-example.csv")
svy_qsf <- read_json("sq_example.qsf")

use_data(svy_df,svy_qsf, overwrite=TRUE)

