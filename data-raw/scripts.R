# Create a list of journalswhich a template is available
library(dplyr)
library(tibble)

dat_journals <- read.csv("data-raw/journals.csv", stringsAsFactors = FALSE)
save(dat_journals, file = "data/dat_journals.RData")

dat_jelcodes <- read.csv("data-raw/jelcodes.csv", stringsAsFactors = FALSE)
save(dat_jelcodes, file = "data/dat_jelcodes.RData")
