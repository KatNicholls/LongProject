
# 001 Getting TE locations from EDTA file --------------------------------------

library(dplyr)
setwd("/home/irh25/katherine/005TEs/002EDTA")
raw_files <- data.frame(filename=list.files('~/katherine/005TEs/002EDTA', pattern="gff3"))
raw_files <- raw_files %>% mutate(name_csv = gsub("\\..*",".csv",raw_files$filename)) 

for (i in 1:nrow(raw_files)){
  
  setwd("/home/irh25/katherine/005TEs/002EDTA")
  
  x <- read.delim2(raw_files[i,1], header = FALSE)
  
  #correcting for when chromosome isnt simple Chr1->5 to allow for next step
  x$V1 = gsub("SUPER_", "Chr", x$V1, perl=TRUE)
  x$V1 = gsub("_RagTag_Rag", "", x$V1, perl=TRUE)
  x$V1 = gsub("scaffold_","Chr", x$V1, perl=TRUE)
  
  TEs <- x %>% filter(V3 %in% c("LTR_retrotransposon", "Copia_LTR_retrotransposon", "Gypsy_LTR_retrotransposon")) %>% select(chrm = V1, start = V4, stop = V5, name = V3)
  
  setwd("/home/irh25/katherine/009REPEAT/001TE_Locations")
  
  write.csv(TEs, raw_files$name_csv[i], quote=FALSE, row.names=FALSE)
  
}


