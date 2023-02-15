# Libraries
library(seqinr)
library(Biostrings)
library(dplyr)

#creating a list of the accession sequences
setwd("~/katherine/66_genomes_datafiles")
myfiles=list.files(pattern="*.fa")

#creating a list of the filtered accession for TEs and removing the Alyrata sequences, so just have the 66 Athaliana sequences
mycsv=list.files(path="~/katherine/009REPEAT/001TE_Locations/", pattern="*csv")

#removing Alyrata sequences from csv files and fasta files
mycsv=mycsv[-c(57,62,68)]
myfiles=myfiles[-c(57,62,68)]

#Found a mismatch between the names of the two files so need to fix it
#removing all but the names
myfiles2=myfiles
myfiles2=gsub("\\.fa", "", myfiles2)
myfiles2=gsub("\\.ragtag_scaffolds","", myfiles2)
myfiles2=gsub("\\.patch.scaffold.Chr","",myfiles2)
myfiles2=gsub("\\.scaffolds.bionano.final","",myfiles2)
myfiles2=gsub("\\.1.primary","",myfiles2)


mycsv=gsub("\\.csv", "", mycsv)

#lets try match the order
#creating mycsv2 to be able to match
mycsv2=mycsv

#fixing all the name issues
mycsv2=gsub("at", "", mycsv2)
mycsv2=gsub("C-0","Cat-0",mycsv2)

#lets see if they now match - know they now match so don't need to run
#myfiles2[myfiles2%in%mycsv2] #yes
#myfiles2[!(myfiles2%in%mycsv2)] #no

#reorder csv file list to match order of fasta
mycsv=mycsv[order(match(mycsv2,myfiles2))]

#find the alphabetical order...
#mycsv2 <- mycsv2 %>% order()
#check match
#mycsv2 == myfiles


#now getting the sequences of the TEs
#also maybe change this 66 to nrow(mycsv)
for (i in 1:66) {
  myname=mycsv[i]
  mycsvi=paste(mycsv[i], ".csv", sep="")
  
  setwd("~/katherine/009REPEAT/001TE_Locations/")
  mycsvi <- read.csv(mycsvi)
  
  
  #removing anything that is Chr1 etc... getting rid of ptg 
  mychrom=c("Chr1", "Chr2", "Chr3", "Chr4", "Chr5")
  mycsvi=mycsvi[mycsvi$chrm%in%mychrom,]
  
  setwd("~/katherine/66_genomes_datafiles")
  
  myfastafile=paste(myfiles[i])
  myfasta <- readDNAStringSet(myfastafile)
  myfasta <- myfasta[1:5]
  myfasta=myfasta[sort(names(myfasta))]
  names(myfasta) <- c("Chr1", "Chr2", "Chr3", "Chr4", "Chr5")
  
  Chrm <- mycsvi[,1]
  
  fasta.names <- vector(length=nrow(mycsvi), mode ="character")
  fasta.seqs <- list()
  
  for (j in 1:nrow(mycsvi)){
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
  
  
  setwd("/home/irh25/katherine/009REPEAT/002Fasta")
  
  write.fasta(sequences = fasta.seqs, names = fasta.names, file.out = my.out, open ="w", nbchar=60, as.string = TRUE)
  
}


