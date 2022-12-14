#STEP 3 - GETTING ATHILA SEQUENCES
library(seqinr)
library(Biostrings)
library(dplyr)

setwd("/home/irh25/katherine/002Athila")
#read in this csv with the ATHILA locations
TE_with_flanks_scored <- read.csv('TE_with_flanks_scored.csv')

#filtering the above file for just intact Athila and creating a separate csv file for each accession 
TE_with_flanks_scored <- TE_with_flanks_scored %>% mutate(new_name=gsub("\\..*","",TE_with_flanks_scored$accession_fasta))
TE_intact <- TE_with_flanks_scored %>% filter(quality == "intact") %>% select(chromosome,genome_left_coord_FL,genome_right_coord_FL,centromere,new_name,combo_fam)
Accession.names <- vector(length=length(unique(TE_intact$new_name)), mode ="character")
for (i in 1:66){
  Accession.names[i] <- paste(unique(TE_intact$new_name)[i],".csv",sep="")
  TE_intact %>% filter(new_name == unique(new_name)[i])

  write.csv(TE_intact, Accession.names[i], quote=FALSE, row.names=FALSE)
}

#listing the accessions
mycsv <- list.files(pattern="*.csv")
mycsv <- mycsv[1:66]
#listing the fasta files
setwd('/home/irh25/katherine/004Genomes/004Athaliana_NEW')
myfiles=list.files(pattern="*.fa")

setwd("/home/irh25/katherine/002Athila")
#files match straight away !

mycsv <- gsub("\\.csv","",mycsv)

#now getting the sequences of the TEs
#also maybe change this 66 to nrow(mycsv)
for (i in 1:66) {
  myname=mycsv[i]
  mycsvi=paste(mycsv[i], ".csv", sep="")
  
  mycsvi <- read.csv(mycsvi)
  
  setwd("/home/irh25/katherine/004Genomes/004Athaliana_NEW")
  myfastafile=paste(myfiles[i])
  myfasta <- readDNAStringSet(myfastafile)
  myfasta <- myfasta[1:5]
  myfasta=myfasta[sort(names(myfasta))]
  names(myfasta) <- c("Chr1", "Chr2", "Chr3", "Chr4", "Chr5")
  setwd("/home/irh25/katherine/002Athila")
  
  Chrm <- mycsvi[,1]
  
  fasta.names <- vector(length=nrow(mycsvi), mode ="character")
  fasta.seqs <- list()
  
  for (j in 1:nrow(mycsvi)){
    Chr = mycsvi[j,1]
    Start = mycsvi[j,2]
    Stop = mycsvi[j,3]
    Length = Stop - Start
    TE = mycsvi[j,6]
    Accession = myname
   
  if (mycsvi[j,4] == "out"){
      preference = "Centrophobic"
    }
    else if (mycsvi[j,4] == "in"){
      preference = "Centrophilic"
    }
    
    fasta.names[j] = paste(c(Chr,Start,Stop,Length,TE,preference, Accession), collapse="_")
    fasta.seqs[[j]] = subseq(myfasta[[Chrm[j]]], start = Start, end = Stop)
  }
  
  #sorting out names
  #my.out <- paste(myfiles[i], "_TE_Seq", sep="")
  my.out <- gsub("\\..*","_Athila_Seq",myfiles[i])
  
  write.fasta(sequences = fasta.seqs, names = fasta.names, file.out = my.out, open ="w", nbchar=60, as.string = TRUE)
  
}

