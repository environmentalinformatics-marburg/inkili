#Varnames:
#insct = insct abundance of insct in general
#S=site;
#R = Round(~Durchgang);
#T = Trap;
#beet = beetle;
#beet_abbr = beetle abreviation
#eco = ecology;
#spl = samples
#O = Ordnung; UK=Unterklasse; K=Klasse; TO=Teilordnung; UO=Unterordnung; F=Familie; x=Sonderfall
#srt = sorted
#trans = transponiert
#clp = clip
#abbr = abbreviation
#agg = aggregate
#tmp = temporary variable
#anm = animals (includes beetles and other insct)
#fin = final
#rich = richnes
#tec = technische informationen
#ttl = total
#rdc = reduced (hier keine Ameisen und Collembola)

library(base)
library(inkili)

inpath <- ("/media/aziegler/Volume/Bodendaten/data")
outpath <- ("/media/aziegler/Volume/data_div")
#Beispiel Nutzung: writeChar(prjNew,paste0(outPath,"/",layer,".prj"))
setwd(inpath)

###read data
#Varnames:
#abun = abundance; S=site; R = Round (?Durchgang); T = Trap;
#beet=beetle; eco=ecology; spl=samples
insct_SRT <- read.csv ("SP7_PTabund_per_site_and_trap_and_rnd.csv", header=T, sep=";")
insct_SR <- read.table("SP7_PTabund_means_per_site_and_rnd.txt", header=T, sep=",")
insct_S <- read.table("SP7_PTabund_means_per_site.txt", header=T, sep=",")
beet_spl <- read.csv ("SP7_PTabund_beetles_samples.csv", header=T, sep=";")

beet_eco <- read.csv ("SP7_PTabund_beetles_ecology.csv", header=T, sep=";")


###compile data table from "abun_SR"
#sort abun_SR and delete unnecessary columns
insct_SR_srt <- insct_SR[, c(1,(ncol(insct_SR)-1), ncol(insct_SR), 3, (ncol(insct_SR)-3), 4:(ncol(insct_SR)-5))]

#change column date in format date
insct_SR_srt$date_coll <- strptime(insct_SR_srt$date_coll, format = "%Y-%m-%d")
insct_SR_srt$date_coll <- as.Date(insct_SR_srt$date_coll)
insct_SR_srt$plt_rnd <- paste0(insct_SR_srt$plotID, "_", insct_SR_srt$rnd)
insct_SR_srt <- cbind(insct_SR_srt[1:3], insct_SR_srt[ncol(insct_SR_srt)], insct_SR_srt[5:ncol(insct_SR_srt)-1])
colnames(insct_SR_srt)[which(colnames(insct_SR_srt)=="Hym_ants")] <- "Formicideae" # means ants without army ants
colnames(insct_SR_srt)[which(colnames(insct_SR_srt)=="Hym_excl_ants")] <- "Hymenoptera" #means hymenoptera without any ants

###adjust "beet_spl" to be included in insct_SR
#continue editing table "beet_spl"
rownames(beet_spl) <- as.list(as.character(beet_spl$spec_short))
beet_spl$spec_short <- NULL
beet_spl$superfamily <- NULL
beet_spl$family <- NULL
beet_spl$subfamily <- NULL
beet_spl$genus <- NULL
beet_spl$subgenus <- NULL
beet_spl$species <- NULL
beet_spl$genus_work <- NULL
beet_spl$species_work <- NULL
beet_spl$spec_name <- NULL
beet_spl$total <- NULL

beet_trans <- t(beet_spl)
beet_trans <- as.data.frame(beet_trans)
beet_trans$total <- rowSums(beet_trans)

#create table with plot information from the rownames of "beet_trans"
beet_info <- as.data.frame(cbind(rownames(beet_trans)))
colnames(beet_info)[1] <- "orig_name"
beet_info$plotID <- substr(beet_info$orig_name, 1, 4)
beet_info$date_coll <- substr(beet_info$orig_name, 6, 15)
beet_info$date_coll <- strptime(beet_info$date_coll, format = "%Y.%m.%d")
beet_info$date_coll <- as.Date(beet_info$date_coll)
beet_info$trap <- substr(beet_info$orig_name, 18, 20)

# join "beet_trans" with "beet_info"
beet_srt <- cbind(beet_info[2:4], beet_trans[ncol(beet_trans)], beet_trans[1:(ncol(beet_trans)-1)])
rownames(beet_srt) <- NULL

### aggregate beet_srt by plot and date (=round)
beet_SR_srt <- as.data.frame(aggregate(beet_srt[5], by = list(beet_srt$plotID, beet_srt$date_coll), FUN="mean"))
for (i in (6:ncol(beet_srt))){
  tmp_mean <- aggregate(beet_srt[i], by = list(beet_srt$plotID, beet_srt$date_coll), FUN="mean")
  beet_SR_srt <- cbind(beet_SR_srt, tmp_mean[3])
}
colnames(beet_SR_srt)[1] <- "plotID"
colnames(beet_SR_srt)[2] <- "date_coll"
beet_SR_srt$rnd <- paste0("jun", tolower(format(beet_SR_srt$date_coll, "%Y")))
beet_SR_srt$plt_rnd <- paste0(beet_SR_srt$plotID, "_", beet_SR_srt$rnd)
beet_SR_srt <- cbind(beet_SR_srt[1:2],
                     beet_SR_srt[ncol(beet_SR_srt)-1],
                     beet_SR_srt[ncol(beet_SR_srt)],
                     beet_SR_srt[3:ncol(beet_SR_srt)-2])
colnames(beet_SR_srt)[1] <- "plotID_beet"
colnames(beet_SR_srt)[2] <- "date_coll_beet"
colnames(beet_SR_srt)[3] <- "rnd_beet"

###merge both aggregated Tables insct_SR_srt und beet_SR_srt
all_SR <- merge(insct_SR_srt, beet_SR_srt, by = "plt_rnd", all=T)
colnames(all_SR)[2:3] <- c("plotID", "date_coll")

write.csv(all_SR, file=paste0(outpath, "/", "anm_SR.csv"), row.names=F)

###create one table only for insects without the beetle species
insct_SR_fin <- all_SR[, 1:38]
colnames(insct_SR_fin)[1:6] <- c("plot_rnd", "plotID", "date_coll_insct", "rnd", "elevation", "total_insct")

### create new collums with species richness for insct and total number of
###insects without ants and Collembola (Springschwänze)
insct_SR_spec <- insct_SR_fin[,7:ncol(insct_SR_fin)]
insct_SR_spec$rich_insct <- rowSums(insct_SR_spec>0)
insct_SR_spec$ttl_insct_rdc <- rowSums(insct_SR_spec
                                           [ -which(colnames(insct_SR_spec)%in%
                                                      c("Collembola",
                                                        "Hym_army_ants",
                                                        "Hym_ants",
                                                        "rich_insct"))])
insct_SR_fin <- cbind(insct_SR_fin, insct_SR_spec$rich_insct,
                      insct_SR_spec$ttl_insct_rdc)
colnames(insct_SR_fin)[ncol(insct_SR_fin)-1] <- c("rich_insct")
colnames(insct_SR_fin)[ncol(insct_SR_fin)] <- c("ttl_insct_rdc")

###analog to the prior:
###create one table for beetles without the insect species
beet_SR_fin <- all_SR[, c(1:5, 39:41, 44:ncol(all_SR))]
colnames(beet_SR_fin)[c(1:5)] <- c("plot_rnd", "plotID", "date_coll",
                                         "rnd", "elevation")

###create new collum with species richness for beetles
beet_SR_spec <- beet_SR_fin[, 12:ncol(beet_SR_fin)]
beet_SR_spec$rich_beet <- rowSums(beet_SR_spec>0)
beet_SR_fin <- cbind(beet_SR_fin, beet_SR_spec$rich_beet)
colnames(beet_SR_fin)[ncol(beet_SR_fin)] <- c("rich_beet")

###write out table with species richness for insct
# write.csv(insct_SR_fin, file=paste0(outpath, "/", "insct_SR_fin.csv"), row.names=F)
# write.csv(beet_SR_fin, file=paste0(outpath, "/", "beet_SR_fin.csv"), row.names=F)

###create one table with only technical/abiotic information
tec_info <- all_SR[,c(1:5)]
#write.csv(tec_info, file=paste0(outpath, "/", "tec_info.csv"),row.names=F)

