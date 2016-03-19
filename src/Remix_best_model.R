inpath <- "/media/aziegler/Volume/data_div/"

spec_r2_lst <- list.files(inpath, pattern = "spec_r2")

##get all r2 from all different models per arthropod
spec_r2_entries <- lapply(spec_r2_lst, function(x){
  #string manipulation for name
  csv_str <- strsplit(x, "__")[[1]][2]
  name_mod <- strsplit(csv_str, "\\.")[[1]][1]
  spec_r2_csv <- read.csv(paste0(inpath, x), header=T)
  spec_r2_csv$spec <- as.character(spec_r2_csv$spec)
  spec_r2_csv$spec[spec_r2_csv$spec == "Hym_excl_ants"] <- "Hymenoptera"
  spec_r2_csv$spec[spec_r2_csv$spec == "Hym_ants"] <- "Formicideae"
  spec_r2_csv$spec <- as.factor(spec_r2_csv$spec)
  colnames(spec_r2_csv)[which(colnames(spec_r2_csv)=="r_squared")] <- paste0(name_mod, "_r2")
  spec_r2_srt <- spec_r2_csv[with(spec_r2_csv, order(spec_r2_csv$spec), decreasing = F),]
})

spec_r2_all <- do.call(cbind, spec_r2_entries)
spec_r2_col <- spec_r2_all[,-seq(3, ncol(spec_r2_all), by = 2)]
spec_r2 <- cbind(spec_r2_col[1], spec_r2_col[6:ncol(spec_r2_col)], spec_r2_col[2:5])

##find which model is best suited for each arthropod
max_mod <- lapply(seq(nrow(spec_r2)), function(x){
  print(x)
  col_num <- which(sapply(spec_r2, function(y)
    any(y == (max(spec_r2[x,2:ncol(spec_r2)])))
  ))
  best_name <- colnames(spec_r2)[col_num]
  best_value <- spec_r2[x, col_num]
  best_col <- cbind(best_name, best_value)
})
best_mod_col <- do.call(rbind, max_mod)

best_mod <- cbind(spec_r2, best_mod_col)

write.csv(best_mod, file = paste0(inpath, "best_mod_16_03_17.csv"))


