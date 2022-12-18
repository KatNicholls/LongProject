#ALYRATA - STEPS
library(seqinr)
library(Biostrings)
library(dplyr)

#Locations of TEs from EDTA (already done at same time as AThaliana)

#Create a fasta file of TE sequences

#List of filtered accessions
mycsv=list.files(path="~/katherine/005TEs/003EDTA_Filt/", pattern="*csv")
#only lyrata
mycsv=mycsv[c(57,62,68)]

#Sequence data - only have MN47 and NT1 atm
myfiles=list.files(pattern="*.fasta")

## Doing mn47 and nt1 to start with - from above no number 68
mycsv = mycsv[c(1,2)]

#now getting the sequences of the TEs
#also maybe change this 66 to nrow(mycsv)
for (i in 1:2) {
  myname=mycsv[i]
  myname=gsub("\\.csv","",myname)
  mycsvi=paste(mycsv[i])
  
  setwd("~/katherine/005TEs/003EDTA_Filt/")
  mycsvi <- read.csv(mycsvi)
  
  setwd("/home/irh25/katherine/004Genomes/002Alyrata")
  
  
  ##NEED TO FIX THAT A. LYRATA HAS MORE CHROMOSOMES
  #need to check that this is what they say...
  mycsvi$chrm <- gsub("\\_RagT","",mycsvi$chrm)
  mychrom=c("Chr1", "Chr2", "Chr3", "Chr4", "Chr5", "Chr6", "Chr7", "Chr8")
  mycsvi=mycsvi[mycsvi$chrm%in%mychrom,]
  
  myfastafile=paste(myfiles[i])
  myfasta <- readDNAStringSet(myfastafile)
  myfasta <- myfasta[1:8]
  myfasta=myfasta[sort(names(myfasta))]
  names(myfasta) <- c("Chr1", "Chr2", "Chr3", "Chr4", "Chr5", "Chr6", "Chr7", "Chr8")
  
  Chrm <- mycsvi[,1]
  
  fasta.names <- vector(length=nrow(mycsvi), mode ="character")
  fasta.seqs <- list()
  
  for (j in 1:2){
    Chr = mycsvi[j,1]
    Start = mycsvi[j,2]
    Stop = mycsvi[j,3]
    Length = Stop - Start
    TE = mycsvi[j,4]
    Accession = myname
    
    fasta.names[j] = paste(c(Chr,Start,Stop,Length,TE,Accession), collapse="_")
    fasta.seqs[[j]] = subseq(myfasta[[Chrm[j]]], start = Start, end = Stop)
  }
  
  #sorting out names
  #my.out <- paste(myfiles[i], "_TE_Seq", sep="")
  my.out <- gsub("\\..*","_TE_Seq",myfiles[i])
  
  write.fasta(sequences = fasta.seqs, names = fasta.names, file.out = my.out, open ="w", nbchar=60, as.string = TRUE)
  
}

