
#STEP 1 GETTING THE LOCATIONS OF TEs from EDTA 
library(dplyr)
raw_files <- data.frame(filename=list.files('~/katherine/005TEs/002EDTA'))
raw_files <- raw_files %>% mutate(name_csv = gsub("\\..*",".csv",raw_files$filename)) 


for (i in 1:nrow(raw_files)){
  x <- read.delim2(raw_files[i,1], header = FALSE)
  
  #correcting for when chromosome isnt simple Chr1->5 to allow for next step
  x$V1 = gsub("SUPER_", "Chr", x$V1, perl=TRUE)
  x$V1 = gsub("_RagTag_Rag", "", x$V1, perl=TRUE)
  x$V1 = gsub("scaffold_","Chr", x$V1, perl=TRUE)
  
  TEs <- x %>% filter(V3 %in% c("LTR_retrotransposon", "Copia_LTR_retrotransposon", "Gypsy_LTR_retrotransposon")) %>% select(chrm = V1, start = V4, stop = V5, name = V3)
  
    write.csv(TEs, raw_files$name_csv[i], quote=FALSE, row.names=FALSE)
  
 }


