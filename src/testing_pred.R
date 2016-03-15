setwd("/media/aziegler/Volume/data_div/") ###alz: wenn sich diese Zeile nicht ausführen lässt: Volume mounten

mod_date <- "16_02_09"
load(paste0("gpm_models_rf_", mod_date, ".rda"))

library(gpm)
library(xlsx)
###testing###

var_imp <- compVarImp(models)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp, xlab = "Species", ylab = "Band")

###resort variable importance values for further calculation
imp_resp <-lapply(seq(var_imp[[1]]$mean), function(i){
  imp_j <- lapply((var_imp), function(j){
    sngl_entry <- j$mean[[i]]
  })
  imp_j <- do.call("cbind", imp_j)
})
imp_resp <- do.call("rbind", imp_resp)
imp_resp <- cbind(imp_resp, rowMeans(imp_resp))
###label variale importance df
#colnames
col_nm <- lapply(seq(var_imp), function (spec){
  levels(var_imp[[spec]]$RESPONSE)
})
col_nm <- do.call("rbind", col_nm)
colnames(imp_resp) <- c(col_nm, "var_imp_mean")
#rownames
row_nm <- levels(var_imp[[1]]$VARIABLE)
rownames(imp_resp) <- row_nm
imp_resp_df <- as.data.frame(imp_resp)

var_imp_srt <- imp_resp_df[with(imp_resp_df, order(imp_resp_df$var_imp_mean, decreasing = T)), ]


#####R2 tests for the models
tests <- compRegrTests(models, per_model = TRUE, per_selector = TRUE,
                       sub_selectors = c(1,3), details = TRUE)

#tests_pairs <- aggregate(tests, by = list(tests$pairs), FUN = function(x){x[1]})
tests_agg <- aggregate(tests, by=list(tests$model_response, tests$model_selector), FUN="mean")

# #create a list where all landuse types are rearranged in less groups
# agg_lnduse <- as.data.frame(unique(tests$model_selector))
# colnames(agg_lnduse) <- c("plot_lnduse")
# agg_lnduse$plot_lnduse <- as.character(agg_lnduse$plot_lnduse)
# agg_lnduse$lnduse_sum <- agg_lnduse$plot_lnduse
# agg_lnduse$lnduse_sum[agg_lnduse$plot_lnduse == "flm" |
#                         agg_lnduse$plot_lnduse == "foc" ] <- "forest"
# agg_lnduse$lnduse_sum[agg_lnduse$plot_lnduse == "fod" ] <- "forest_disturbed"


tests$model_selector <- as.character(tests$model_selector)
tests$model_select_sum <- tests$model_selector
tests$model_select_sum[tests$model_select_sum == "flm" |
                         tests$model_select_sum == "foc"] <- "forest"
tests$model_select_sum[tests$model_select_sum == "fod"] <- "forest_disturbed"

tests_agg_sum <- aggregate(tests, by=list(tests$model_response, tests$model_select_sum), FUN="mean")

write.csv(tests_agg_sum, file = "tests_agg_sum.csv", row.names = F)

tests_agg_land <- aggregate(tests_agg_sum, by=list(tests_agg_sum$Group.2), FUN = "mean")
tests_agg_spec <- aggregate(tests_agg_sum, by=list(tests_agg_sum$Group.1), FUN = "mean")

#tests sorted after highest R2
tests_srt <- tests_agg[with(tests_agg, order(tests_agg$r_squared, decreasing = T)), ]
tests_srt_sum <- tests_agg_sum[with(tests_agg_sum, order(tests_agg_sum$r_squared, decreasing = T)), ]

tests_srt_land <- tests_agg_land[with(tests_agg_land, order(tests_agg_land$r_squared, decreasing = T)), ]
tests_srt_spec <- tests_agg_spec[with(tests_agg_spec, order(tests_agg_spec$r_squared, decreasing = T)), ]

####prepare and clean for write out############################################
#tests_srt
colnames(tests_srt)[1:2] <- c("spec", "land")
tests_srt <- tests_srt[,c(((which(colnames(tests_srt) == "spec")):
                           (which(colnames(tests_srt) == "land"))),
                         ((which(colnames(tests_srt) == "testing_response")):
                           (which(colnames(tests_srt) == "residuals"))))]
#tests_srt_sum
colnames(tests_srt_sum)[1:2] <- c("spec", "land_sum")
tests_srt_sum <- tests_srt_sum[,c(((which(colnames(tests_srt_sum) == "spec")):
                             (which(colnames(tests_srt_sum) == "land_sum"))),
                          ((which(colnames(tests_srt_sum) == "testing_response")):
                             (which(colnames(tests_srt_sum) == "residuals"))))]
all_r2 <- mean(tests_srt_sum$r_squared)
max_r2 <- max(tests_srt_sum$r_squared)
min_r2 <- min(tests_srt_sum$r_squared)
#tests_srt_land
colnames(tests_srt_land)[1] <- c("land_sum")
tests_srt_land <- tests_srt_land[,c((which(colnames(tests_srt_land) == "land_sum")),
                                   ((which(colnames(tests_srt_land) == "testing_response")):
                                      (which(colnames(tests_srt_land) == "residuals"))))]
#tests_srt_spec
colnames(tests_srt_spec)[1] <- c("spec")
tests_srt_spec <- tests_srt_spec[,c((which(colnames(tests_srt_spec) == "spec")),
                                    ((which(colnames(tests_srt_spec) == "testing_response")):
                                       (which(colnames(tests_srt_spec) == "residuals"))))]

# write.csv(tests_srt, file = "tests_srt_16_02_10.csv", row.names = F)
# write.csv(tests_srt_sum, file = "tests_srt_sum_16_02_10.csv", row.names = F)
# write.csv(tests_srt_land, file = "tests_srt_land_16_02_10.csv", row.names = F)
# write.csv(tests_srt_spec, file = "tests_srt_spec_16_02_10.csv", row.names = F)
###############################################################################
###different stuff from testing phase###
###############################################################################
###write out as excel workbook
###von: https://statmethods.wordpress.com/2014/06/19/quickly-export-multiple-r-objects-to-an-excel-workbook/
# #Funktion
save.xlsx <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}
# ##Aufruf
save.xlsx(paste0("stats_modell_", mod_date ,".xlsx"), tests_srt_sum, tests_srt_land,
          tests_srt_spec, all_r2, max_r2, min_r2, var_imp_srt)





#########################################################################################################

#plot single plots
sngl_nm_rspns <- "Acari"
sngl_reg <- lm(tests$testing_response[which(tests$model_response == sngl_nm_rspns)] ~
                 tests$testing_predicted[which(tests$model_response == sngl_nm_rspns)], main = sngl_nm_rspns)
plot(tests$testing_response[which(tests$model_response == sngl_nm_rspns)]
     ~ tests$testing_predicted[which(tests$model_response == sngl_nm_rspns)],
     main = sngl_nm_rspns)
abline(sngl_reg)

#plot number of pairs to r²
pair_r2_reg <- lm(tests$r_squared ~ tests$pairs)
plot(tests$r_squared ~ tests$pairs)
abline(pair_r2_reg)
summary(pair_r2_reg)

pair_agg_r2_reg <- lm(tests_agg$r_squared ~ tests_agg$pairs)
plot(tests_agg$r_squared ~ tests_agg$pairs)
abline(pair_agg_r2_reg)
summary(pair_agg_r2_reg)

pair_agg_sum_r2_reg <- lm(tests_agg_sum$r_squared ~ tests_agg_sum$pairs)
plot(tests_agg_sum$r_squared ~ tests_agg_sum$pairs)
abline(pair_agg_sum_r2_reg)
summary(pair_agg_sum_r2_reg)

#plot histogram of pairs
hist(tests$pairs, breaks=50) # breaks = 5 als Abbildung
qntl_pairs <- quantile(tests$pairs, probs = seq(0, 1, 0.20))
#clip df to only variables with more than 100 pairs
tests_clp <- tests_agg[which(tests_agg$pairs>= 30),]

# check regression again
pair_r2_reg_clp <- lm(tests_clp$r_squared ~ tests_clp$pairs)
plot(tests_clp$r_squared ~ tests_clp$pairs)
abline(pair_r2_reg_clp)
summary(pair_r2_reg_clp)
