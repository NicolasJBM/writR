# Create a list of journalswhich a template is available
library(dplyr)
library(tibble)


journals <- read.csv("inst/scripts/journals.csv", stringsAsFactors = FALSE)
save(journals, file="data/journals.RData")



jelcodes <- read.csv("inst/scripts/jelcodes.csv", stringsAsFactors = FALSE)
save(jelcodes, file="data/jelcodes.RData")



charts <- read.csv("inst/scripts/charts.csv", stringsAsFactors = FALSE)
save(charts, file="data/charts.RData")
