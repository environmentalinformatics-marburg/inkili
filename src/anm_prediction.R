setwd("/media/aziegler/Volume/data_div/")

# Libraries --------------------------------------------------------------------
library(gpm)
library(grid)

# Read and adjust data from S. Schlauss, level 300 -----------------------------
grnd_ldr <- read.table("grnd_ldr.csv", 
                         header = TRUE, sep = ",", dec = ".")


###filtering plots by scan angle
grnd_ldr <- grnd_ldr[-which(grnd_ldr$max_angl > 25),]
###

grnd_ldr$rich_insct <- as.numeric(grnd_ldr$rich_insct)
meta_data <- createGPMMeta(grnd_ldr, type = "input",
                      selector = 1, response = c(24:57, 61:119), 
                      independent = c(4:12), meta = c(2,3,13:23, 58:60))
grnd_ldr <- gpm(grnd_ldr, meta_data)
# save(grnd_ldr, file = "processed/grnd_ldr.rda")


# Select responses occuring at least across 20 unique selector values on average
# load("processed/grnd_ldr.rda")
plotid <- grnd_ldr@data$input[,grnd_ldr@meta$input$SELECTOR]
observations <- grnd_ldr@data$input[, grnd_ldr@meta$input$RESPONSE]
min_occurence <- minimumOccurence(x = observations, selector = plotid,
                                  occurence = 0, 
                                  resample = 100, thv = 20)
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

#grnd_ldr@data

models <- trainModel(x = grnd_ldr@data$input,
                     response = response, independent = independent,
                     resamples = grnd_ldr_trte, n_var = seq(1,9,1),
                     response_nbr = c(1:10), resample_nbr = c(1:100),
                     mthd = "rf", cv_nbr = 10)

save(models, file = "gpm_models_rf_2015_12_08.rda")
# load("gpm_models_rf_2015_12_08.rda")

var_imp <- compVarImp(models)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp, xlab = "Species", ylab = "Band")

tests <- compRegrTests(models)

R2_mean <- lapply(seq(tests), function(x){
  R2 <- mean(tests[[x]]$R2)
})

R2_df <- as.data.frame(cbind(unlist(R2_mean)), row.names = response)
colnames(R2_df) <- "R2"

#write.csv(R2_df, file = "2015_12_08_R2_df.csv")



# 
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
