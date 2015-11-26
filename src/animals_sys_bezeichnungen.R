inpath <- ("/media/aziegler/Volume/Bodendaten/data")
outpath <- ("/media/aziegler/Volume/Bodendaten/data/output") 
#Beispiel Nutzung: writeChar(prjNew,paste0(outPath,"/",layer,".prj"))
setwd(inpath)

###read data
#Varnames: 
#abun = abundance; S=site; R = Round (?Durchgang); T = Trap; 
#beet=beetle; eco=ecology; spl=samples
abun_SRT <- read.csv ("SP7_PTabund_per_site_and_trap_and_rnd.csv", header=T, sep=";")
abun_SR <- read.table("SP7_PTabund_means_per_site_and_rnd.txt", header=T, sep=",")
abun_S <- read.table("SP7_PTabund_means_per_site.txt", header=T, sep=",")

### table for names
names_multi <- (unique(c(colnames(abun_S), colnames(abun_SR), colnames(abun_SRT))))
names <- c(names_multi[4:16], names_multi[20:38], names_multi[44:47])
# O= Ordnung; UK=Unterklasse; K=Klasse; TO=Teilordnung; UO=Unterordnung; F=Familie; x=Sonderfall
sys <- c("O", "UK", "Fx", "F", "O", "O", "O", "O", "K", "K", "TO", "O?", "O", 
         "O", "O", "O", "O", "O", "O", "O", "O", "O", "UO", "O", "O", "Ox", "O", 
         "O", "O", "O", "O", "O", "O", "K", "K", "K")
deu <- c("springschwaenze", "milben", "treiberameisen", "ameisen_ohne_treiberameisen", "kaefer", "webspinnen", 
         "asseln", "heuschrecken", "hundertfuesser", "doppelfuesser", 
         "termiten", "schaben?", "ohrwuermer", "doppelschwänze", 
         "felsenspringer", "fischchen", "beintastler", "weberknechte", 
         "pseudoskorpione", "skorpione", "walzenspinnen", "schnabelkerfe", 
         "wanzen", "zweifluegler", "tarsenspinner", "hautflügler_ohne_ameisen", "schmetterlinge", 
         "fangschrecken", "staublaeuse", "fransenfluegler", "tierlaeuse", 
         "floehe", "geisselskorpione", "amphibie", "reptil", "saeugetier")
names_sys <- cbind(names, sys, deu)

write.csv(names_sys, file=paste0(outpath, "/", "systematische_bezeichnungen.csv"))
