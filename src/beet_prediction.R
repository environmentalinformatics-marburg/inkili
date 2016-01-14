setwd("/media/aziegler/Volume/data_div/") ###alz: wenn sich diese Zeile nicht ausführen lässt: Volume mounten

# Libraries --------------------------------------------------------------------
#library(gpm)
library(grid)

# Read and adjust data from S. Schlauss, level 300 -----------------------------
###hier Daten gemittelt auf site und Round (Stand: 14.12.2015)
grnd_ldr <- read.table("grnd_ldr.csv", 
                         header = TRUE, sep = ",", dec = ".")

beet_ldr <- grnd_ldr[,c((which(colnames(grnd_ldr) == "max_hght")):(which(colnames(grnd_ldr) == "ldr_radius")), 
                        (which(colnames(grnd_ldr) == "plotID_beet")):(which(colnames(grnd_ldr) == "rich_beet")), 
                        (which(colnames(grnd_ldr) == "polename")):(which(colnames(grnd_ldr) == "y_pnt")))]
  
###filtering plots by scan angle
beet_ldr <- beet_ldr[which(grnd_ldr$max_angl <= 25),]
###
beet_ldr <- beet_ldr[complete.cases(beet_ldr), ]
#grnd_ldr$rich_insct <- as.numeric(grnd_ldr$rich_insct)

# meta_data <- createGPMMeta(grnd_ldr, type = "input",
#                            selector = 1, response = c(30:63, 67:403), 
#                            independent = c(4:6, 8:18), meta = c(2,3,7,20:30, 64:66))
meta_data <- createGPMMeta(beet_ldr, type = "input",
                           selector = 1, 
                           #response and independent from "colname" to "colname"
                           #- better handling, when new columns are made
                           response = c((which(colnames(beet_ldr) == "total_insct")):
                                          (which(colnames(beet_ldr) == "ttl_insct_rdc")),
                                        (which(colnames(beet_ldr) == "Agri0001")):
                                          (which(colnames(beet_ldr) == "rich_beet"))), 
                           independent = c((which(colnames(beet_ldr) == "max_hght")):
                                             (which(colnames(beet_ldr) == "mdn")), 
                                           (which(colnames(beet_ldr) == "max_rtrn")):
                                             (which(colnames(beet_ldr) == "sd_per_rtrn_2"))), 
                           meta = c((which(colnames(beet_ldr) == "crdnt_x")),
                                    (which(colnames(beet_ldr) == "crdnt_y")),
                                    (which(colnames(beet_ldr) == "max_angl")),
                                    (which(colnames(beet_ldr) == "ldr_radius")),
                                    (which(colnames(beet_ldr) == "plot_rnd")):
                                      (which(colnames(beet_ldr) == "y_pnt")),
                                    (which(colnames(beet_ldr) == "plotID_beet")):
                                      (which(colnames(beet_ldr) == "rnd_beet"))))

beet_ldr_df <- beet_ldr
beet_ldr <- gpm(beet_ldr, meta_data)
# save(grnd_ldr, file = "processed/grnd_ldr.rda")

# Select responses occuring at least across 20 unique selector values on average
# load("processed/grnd_ldr.rda")
plotid <- beet_ldr@data$input[,beet_ldr@meta$input$SELECTOR]
observations <- beet_ldr@data$input[, beet_ldr@meta$input$RESPONSE]
min_occ <- 0
min_rsmpl <- 100
min_thv <- 20 ##################################################standardwert hier war 20
min_occurence <- minimumOccurence(x = observations, selector = plotid,
                                  occurence = min_occ, 
                                  resample = min_rsmpl, thv = min_thv)
common_response_variables <- min_occurence[[1]]
common_response <- data.frame(min_occurence[[2]])
common_response$RESPONSE <- rownames(common_response)
names(common_response)[1] <- "OCCURENCE"
rownames(common_response) <- NULL
# save(common_response_variables, file = "gpm_common_response_variables.rda")
# save(common_response, file = "gpm_common_response.rda")


# Compile model evaluation dataset ---------------------------------------------
beet_ldr_resamples <- resamplingsByVariable(x = beet_ldr@data$input, 
                                              selector = plotid, 
                                              resample = 100)
# save(beet_ldr_resamples, file = "gpm_beet_ldr_resamples.rda")




# Split dataset into testing and training samples for each individual species --
# load("processed/beet_ldr.rda")
# load("processed/common_response_variables.rda")
# load("processed/beet_ldr_resamples.rda")
col_response <- common_response_variables
beet_ldr_trte <- splitMultResp(x = beet_ldr@data$input, 
                                 response = col_response,
                                 resamples = beet_ldr_resamples)
# save(beet_ldr_trte, file = "gpm_beet_ldr_trte.rda")
# beet_ldr_trte[[10]][[2]]$testing$SAMPLES #rownumber
# beet_ldr@data$input[beet_ldr_trte[[10]][[100]]$testing$SAMPLES, beet_ldr_trte[[10]][[100]]$testing$RESPONSE] #value



# Evaluate prediction models ---------------------------------------------------
# load("processed/beet_ldr.rda")
# load("processed/common_response_variables.rda")
# load("processed/beet_ldr_trte.rda")
response <- common_response_variables
independent <- beet_ldr@meta$input$INDEPENDENT



models <- trainModel(x = beet_ldr,
                     response = response, independent = independent,
                     resamples = beet_ldr_trte, n_var = seq(1,9,1),
                     response_nbr = c(7), resample_nbr = c(55), ###bei min_occ_thv = 20 stand hier: (1:11); (1:100)
                     mthd = "rf", cv_nbr = 10)

# models <- trainModel(x = beet_ldr,
#                      response = response, independent = independent,
#                      resamples = beet_ldr_trte, n_var = seq(1,9,1),
#                      response_nbr = c(1:2), resample_nbr = c(1:2),
#                      mthd = "rf", cv_nbr = 5)



var_imp <- compVarImp(models)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp, xlab = "Species", ylab = "Band")

tests <- compRegrTests(models, per_model = TRUE, per_selector = TRUE, 
                       sub_selectors = c(1,3), details = TRUE)

tests_pairs <- aggregate(tests, by = list(tests$pairs), FUN = function(x){x[1]})
tests_agg <- aggregate(tests, by=list(tests$model_response, tests$model_selector), FUN="mean")
tests_srt <- tests_agg[with(tests_agg, order(tests_agg$r_squared, decreasing = T)), ]

# compRegrTests(models, per_model = TRUE, per_selector = FALSE, 
#               sub_selectors = c(1,3), details = FALSE)
info <- append(paste0 ("response_", response),  ##########################################wie geht das schlauer?
               paste0("independent_", independent))
info <- append(info, 
               paste0("radius_ldr_", beet_ldr@data$input$ldr_radius[1]))
info <- append(info, 
               paste0("min_occurence__occ_", min_occ, "_rsmpl_", min_rsmpl, "_thv_", min_thv))

save(models, tests, info, file = "gpm_models_rf_2016_01_06.rda")
#load("gpm_models_rf_2015_12_22.rda") ###which model does what: data_div/gpm_models_readme.txt


plot_all <- lapply(seq(response), function(a){
  plot(tests$testing_response[which(tests$model_response == response[a])] ~ 
         tests$testing_predicted[which(tests$model_response == response[a])], main = response[a])
})
######################################################################################
###different stuff from testing phase###
######################################################################################

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


#
# tests <- compContTests(models)
# 
# tstat_mean <- lapply(tests, function(x){
#   data.frame(RESPONSE = x$RESPONSE[1], 
#              KAPPA_MEAN = mean(x$Kappa, na.rm = TRUE),
#              POD_MEAN = mean(x$POD, na.rm = TRUE),
#              FAR_MEAN = mean(x$FAR, na.rm = TRUE), 
#              POFD_MEAN = mean(x$POFD, na.rm = TRUE),
#              ACCURACY_MEAN = mean(x$ACCURACY, na.rm = TRUE),
#              SR_MEAN = mean(x$SR, na.rm = TRUE),
#              TS_MEAN = mean(x$TS, na.rm = TRUE),
#              ETS_MEAN = mean(x$ETS, na.rm = TRUE),
#              HK_MEAN = mean(x$HK, na.rm = TRUE))
# })
# tstat_mean <- do.call("rbind", tstat_mean)
# tstat_mean <- merge(tstat_mean, common_response, by = "RESPONSE")
# tstat_mean[order(tstat_mean$KAPPA_MEAN, decreasing = TRUE),]
# corrplot(cor(tstat_mean[, -1]))
