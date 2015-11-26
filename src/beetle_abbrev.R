
inpath <- ("/media/aziegler/Volume/Bodendaten/data")
outpath <- ("/media/aziegler/Volume/data_div")
#Beispiel Nutzung: writeChar(prjNew,paste0(outPath,"/",layer,".prj"))
setwd(inpath)

beet_spl <- read.csv ("SP7_PTabund_beetles_samples.csv", header=T, sep=";")

#only execute once
#generate table with beetle abbreviations
beet_spl$spec_name <- paste0(beet_spl[,2], "_", beet_spl[,3], "_",
                                   beet_spl[,4], "_", beet_spl[,5], "_",
                                   beet_spl[,6], "_", beet_spl[,7], "_",
                                   beet_spl[,8], "_", beet_spl[,9])
beet_abbr <- cbind(beet_spl[1], beet_spl[ncol(beet_spl)])
rownames(beet_abbr) <- NULL
write.csv(beet_abbr, file=paste0(outpath, "/", "beet_abbr.csv"))
