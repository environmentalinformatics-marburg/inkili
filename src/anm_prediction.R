###abbreviations
#sum = summarized

setwd("/media/aziegler/Volume/data_div/") ###alz: wenn sich diese Zeile nicht ausführen lässt: Volume mounten

# Libraries --------------------------------------------------------------------
library(gpm)
library(grid)

mod_date <-"16_02_03_c"

# Read and adjust data from S. Schlauss, level 300 -----------------------------
###hier Daten gemittelt auf site und Round (Stand: 14.12.2015)
grnd_ldr <- read.table(paste0(mod_date, "grnd_ldr.csv"),
                         header = TRUE, sep = ",", dec = ".")


###filtering plots by scan angle
grnd_ldr <- grnd_ldr[which(grnd_ldr$max_angl <= 25),]
###

#grnd_ldr$rich_insct <- as.numeric(grnd_ldr$rich_insct)

# meta_data <- createGPMMeta(grnd_ldr, type = "input",
#                            selector = 1, response = c(30:63, 67:403),
#                            independent = c(4:6, 8:18), meta = c(2,3,7,20:30, 64:66))
meta_data <- createGPMMeta(grnd_ldr, type = "input",
                           selector = 1,
                           #response and independent from "colname" to "colname"
                           #- better handling, when new columns are made
                           response = c((which(colnames(grnd_ldr) == "total_insct")):
                                          (which(colnames(grnd_ldr) == "ttl_insct_rdc")),
                                        (which(colnames(grnd_ldr) == "Agri0001")):
                                          (which(colnames(grnd_ldr) == "rich_beet"))),
                           independent = c((which(colnames(grnd_ldr) == "max_hght")):
                                             (which(colnames(grnd_ldr) == "mdn")),
                                           (which(colnames(grnd_ldr) == "max_rtrn")):
                                             (which(colnames(grnd_ldr) == "sd_per_rtrn_2")),
                                           (which(colnames(grnd_ldr) == "hght_asl"))),
                           meta = c((which(colnames(grnd_ldr) == "crdnt_x")),
                                    (which(colnames(grnd_ldr) == "crdnt_y")),
                                    (which(colnames(grnd_ldr) == "max_angl")),
                                    (which(colnames(grnd_ldr) == "ldr_radius")),
                                    (which(colnames(grnd_ldr) == "plot_rnd")):
                                      (which(colnames(grnd_ldr) == "y_pnt")),
                                    (which(colnames(grnd_ldr) == "plotID_beet")):
                                      (which(colnames(grnd_ldr) == "rnd_beet"))))

grnd_ldr_df <- grnd_ldr
grnd_ldr <- gpm(grnd_ldr, meta_data)
# save(grnd_ldr, file = "processed/grnd_ldr.rda")

# Select responses occuring at least across 20 unique selector values on average
# load("processed/grnd_ldr.rda")
plotid <- grnd_ldr@data$input[,grnd_ldr@meta$input$SELECTOR]
observations <- grnd_ldr@data$input[, grnd_ldr@meta$input$RESPONSE]
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
grnd_ldr_resamples <- resamplingsByVariable(x = grnd_ldr@data$input,
                                              selector = plotid,
                                              resample = 100)
# save(grnd_ldr_resamples, file = "gpm_grnd_ldr_resamples.rda")




# Split dataset into testing and training samples for each individual species --
# load("processed/grnd_ldr.rda")
# load("processed/common_response_variables.rda")
# load("processed/grnd_ldr_resamples.rda")
col_response <- common_response_variables
grnd_ldr_trte <- splitMultResp(x = grnd_ldr@data$input,
                                 response = col_response,
                                 resamples = grnd_ldr_resamples)
# save(grnd_ldr_trte, file = "gpm_grnd_ldr_trte.rda")
# grnd_ldr_trte[[10]][[2]]$testing$SAMPLES #rownumber
# grnd_ldr@data$input[grnd_ldr_trte[[10]][[100]]$testing$SAMPLES, grnd_ldr_trte[[10]][[100]]$testing$RESPONSE] #value



# Evaluate prediction models ---------------------------------------------------
# load("processed/grnd_ldr.rda")
# load("processed/common_response_variables.rda")
# load("processed/grnd_ldr_trte.rda")
response <- common_response_variables
independent <- grnd_ldr@meta$input$INDEPENDENT

######model calculation####################################################
#load("gpm_models_rf_16_02_02_a.rda") ###which model does what: data_div/gpm_models_readme.txt
models <- trainModel(x = grnd_ldr,
                     response = response, independent = independent,
                     resamples = grnd_ldr_trte, n_var = seq(1,9,1),
                     response_nbr = c(1:11), resample_nbr = c(1:100), ###bei min_occ_thv = 20 stand hier: (1:11); (1:100)
                     mthd = "rf", cv_nbr = 10)

# compRegrTests(models, per_model = TRUE, per_selector = FALSE,
#               sub_selectors = c(1,3), details = FALSE)
info <- append(paste0 ("response_", response),  ##########################################wie geht das schlauer?
               paste0("independent_", independent))
info <- append(info,
               paste0("radius_ldr_", grnd_ldr@data$input$ldr_radius[1]))
info <- append(info,
               paste0("min_occurence__occ_", min_occ, "_rsmpl_", min_rsmpl, "_thv_", min_thv)) ######außerdem noch datum des modelllaufs einfügen

save(models, info, file = paste0("gpm_models_rf_", mod_date, ".rda"))
####################################################################################



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
