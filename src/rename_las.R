setwd("/media/ziegler6/Volume/DataFP/Q780")

old_names <- list.files(path = ".", recursive = TRUE)
new_names <- gsub(" - ", "_", old_names, fixed = TRUE)

file.rename(old_names, new_names)
