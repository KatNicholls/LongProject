## ALYRATA
#Locations of TEs from EDTA (already done at same time as AThaliana)
#Create a fasta file of TE sequences

setwd("~/katherine/009REPEAT/001TE_Locations")
#List of filtered accessions
mycsv=list.files(pattern="*csv")
#only lyrata
mycsv=mycsv[c(57,62,68)]

#Sequence data - only have MN47 and NT1 atm
setwd("~/katherine/009REPEAT/002Fasta/003Alyrata")

myfiles=list.files(pattern="*.fasta")

## Doing mn47 and nt1 to start with - from above no number 68
mycsv = mycsv[c(1,2)]

#FIND WHERE TO PUT IN CENTROMERE IN OR OUT 
centromere_coordinates <- read_csv("manual_centromere_coordinates_lyrataApril28th.csv")

setwd("~/katherine/009REPEAT/001TE_Locations")
MN47 <- read_csv("MN47_23122021.csv")
NT1 <- read_csv("NT1_030222.csv")

MN47 %>% mutate(centromere = NA)
NT1 %>% mutate(centromere = NA)
NT1$chrm <- gsub("_RagT","", NT1$chrm)

# filter(MN47$start%in%region & MN47$stop%in%region)
#CENTROMERE REGIONS MN47
region1 <- c(centromere_coordinates$start[1]:centromere_coordinates$end[1])
region2a <- c(centromere_coordinates$start[2]:centromere_coordinates$end[2])
region2b <- c(centromere_coordinates$start[3]:centromere_coordinates$end[3])
region3 <- c(centromere_coordinates$start[4]:centromere_coordinates$end[4])
region4 <- c(centromere_coordinates$start[5]:centromere_coordinates$end[5])
region5 <- c(centromere_coordinates$start[6]:centromere_coordinates$end[6])
region6 <- c(centromere_coordinates$start[7]:centromere_coordinates$end[7])
region7 <- c(centromere_coordinates$start[8]:centromere_coordinates$end[8])
region8 <- c(centromere_coordinates$start[9]:centromere_coordinates$end[9])

#CENTROMERE REGIONS NT1
region9 <- c(centromere_coordinates$start[10]:centromere_coordinates$end[10])
region10 <- c(centromere_coordinates$start[11]:centromere_coordinates$end[11])
region11 <- c(centromere_coordinates$start[12]:centromere_coordinates$end[12])
region12 <- c(centromere_coordinates$start[13]:centromere_coordinates$end[13])
region13 <- c(centromere_coordinates$start[14]:centromere_coordinates$end[14])
region14 <- c(centromere_coordinates$start[15]:centromere_coordinates$end[15])
region15 <- c(centromere_coordinates$start[16]:centromere_coordinates$end[16])
region16 <- c(centromere_coordinates$start[17]:centromere_coordinates$end[17])

#Centromere MN47
for(i in 1:nrow(MN47)){
  if(MN47$chrm[i] == "Chr1" & MN47$start[i]%in%region1 & MN47$stop[i]%in%region1){
    MN47$centromere[i] <- paste("IN")
  }
  else if(MN47$chrm[i] == "Chr2" & MN47$start[i]%in%region2a & MN47$stop[i]%in%region2a | MN47$chrm[i] == "Chr2" & MN47$start[i]%in%region2b & MN47$stop[i]%in%region2b){
    MN47$centromere[i] <- paste("IN")
  }
  else if(MN47$chrm[i] == "Chr3" & MN47$start[i]%in%region3 & MN47$stop[i]%in%region3){
    MN47$centromere[i] <- paste("IN")
  }
  else if(MN47$chrm[i] == "Chr4" & MN47$start[i]%in%region4 & MN47$stop[i]%in%region4){
    MN47$centromere[i] <- paste("IN")
  }
  else if(MN47$chrm[i] == "Chr5" & MN47$start[i]%in%region5 & MN47$stop[i]%in%region5){
    MN47$centromere[i] <- paste("IN")
  }
  else if(MN47$chrm[i] == "Chr6" & MN47$start[i]%in%region6 & MN47$stop[i]%in%region6){
    MN47$centromere[i] <- paste("IN")
  }
  else if(MN47$chrm[i] == "Chr7" & MN47$start[i]%in%region7 & MN47$stop[i]%in%region7){
    MN47$centromere[i] <- paste("IN")
  }
  else if(MN47$chrm[i] == "Chr8" & MN47$start[i]%in%region8 & MN47$stop[i]%in%region8){
    MN47$centromere[i] <- paste("IN")
  }
  else{
    MN47$centromere[i] <- paste("OUT")
  }
}

#Centromere NT1
for(i in 1:nrow(NT1)){
  if(NT1$chrm[i] == "Chr1" & NT1$start[i]%in%region9 & NT1$stop[i]%in%region9){
    NT1$centromere[i] <- paste("IN")
  }
  else if(NT1$chrm[i] == "Chr2" & NT1$start[i]%in%region10 & NT1$stop[i]%in%region10){
    NT1$centromere[i] <- paste("IN")
  }
  else if(NT1$chrm[i] == "Chr3" & NT1$start[i]%in%region11 & NT1$stop[i]%in%region11){
    NT1$centromere[i] <- paste("IN")
  }
  else if(NT1$chrm[i] == "Chr4" & NT1$start[i]%in%region12 & NT1$stop[i]%in%region12){
    NT1$centromere[i] <- paste("IN")
  }
  else if(NT1$chrm[i] == "Chr5" & NT1$start[i]%in%region13 & NT1$stop[i]%in%region13){
    NT1$centromere[i] <- paste("IN")
  }
  else if(NT1$chrm[i] == "Chr6" & NT1$start[i]%in%region14 & NT1$stop[i]%in%region14){
    NT1$centromere[i] <- paste("IN")
  }
  else if(NT1$chrm[i] == "Chr7" & NT1$start[i]%in%region15 & NT1$stop[i]%in%region15){
    NT1$centromere[i] <- paste("IN")
  }
  else if(NT1$chrm[i] == "Chr8" & NT1$start[i]%in%region16 & NT1$stop[i]%in%region16){
    NT1$centromere[i] <- paste("IN")
  }
  else{
    NT1$centromere[i] <- paste("OUT")
  }
}


#now getting the sequences of the TEs
#also maybe change this 66 to nrow(mycsv)
#change code so is MN47 and NT1 

  mychrom=c("Chr1", "Chr2", "Chr3", "Chr4", "Chr5", "Chr6", "Chr7", "Chr8")
  MN47=MN47[MN47$chrm%in%mychrom,]
  NT1=NT1[NT1$chrm%in%mychrom,]
  
  setwd("~/katherine/009REPEAT/002Fasta/003Alyrata")
  
  #MN47
  myfastafile=paste(myfiles[1])
  myfasta <- readDNAStringSet(myfastafile)
  myfasta <- myfasta[1:8]
  myfasta=myfasta[sort(names(myfasta))]
  names(myfasta) <- c("Chr1", "Chr2", "Chr3", "Chr4", "Chr5", "Chr6", "Chr7", "Chr8")
  
  Chrm <- MN47[,1]
  
  fasta.names <- vector(length=nrow(MN47), mode ="character")
  fasta.seqs <- list()
  
    for (j in 1:nrow(MN47)){
    
    Chr = MN47[j,1]
    Start = MN47[j,2]
    Start <- as.numeric(Start)
    Stop = MN47[j,3]
    Stop <- as.numeric(Stop)
    Length = Stop - Start
    TE = MN47[j,4]
    centromere = MN47[j,5]
    Accession = "MN47"
    
    if(MN47$chrm[j] == "Chr1"){
      k <- 1
    }
    else if(MN47$chrm[j] == "Chr2"){
      k <- 2
    }
    else if(MN47$chrm[j] == "Chr3"){
      k <- 3
    }
    else if(MN47$chrm[j] == "Chr4"){
      k <- 4
    }
    else if(MN47$chrm[j] == "Chr5"){
      k <- 5
    }
    else if(MN47$chrm[j] == "Chr6"){
      k <- 6
    }
    else if(MN47$chrm[j] == "Chr7"){
      k <- 7
    }
    else{
      k <- 8
    }
    
    fasta.names[j] = paste(c(Chr,Start,Stop,Length,TE,centromere,Accession), collapse="_")
    fasta.seqs[[j]] = subseq(myfasta[k], start = Start, end = Stop)
  }
  
  my.out <- gsub("\\..*","_TE_Seq.fa",myfiles[1])
  write.fasta(sequences = fasta.seqs, names = fasta.names, file.out = my.out, open ="w", nbchar=60, as.string = TRUE)
  
  
  #NT1#
  myfastafile=paste(myfiles[2])
  myfasta <- readDNAStringSet(myfastafile)
  myfasta <- myfasta[1:8]
  myfasta=myfasta[sort(names(myfasta))]
  names(myfasta) <- c("Chr1", "Chr2", "Chr3", "Chr4", "Chr5", "Chr6", "Chr7", "Chr8")
  
  Chrm <- NT1[,1]
  
  fasta.names <- vector(length=nrow(NT1), mode ="character")
  fasta.seqs <- list()
  
  for (j in 1:nrow(NT1)){
    
    Chr = NT1[j,1]
    Start = NT1[j,2]
    Start <- as.numeric(Start)
    Stop = NT1[j,3]
    Stop <- as.numeric(Stop)
    Length = Stop - Start
    TE = NT1[j,4]
    centromere = NT1[j,5]
    Accession = "NT1"
    
    if(NT1$chrm[j] == "Chr1"){
      k <- 1
    }
    else if(NT1$chrm[j] == "Chr2"){
      k <- 2
    }
    else if(NT1$chrm[j] == "Chr3"){
      k <- 3
    }
    else if(NT1$chrm[j] == "Chr4"){
      k <- 4
    }
    else if(NT1$chrm[j] == "Chr5"){
      k <- 5
    }
    else if(NT1$chrm[j] == "Chr6"){
      k <- 6
    }
    else if(NT1$chrm[j] == "Chr7"){
      k <- 7q
    }
    else{
      k <- 8
    }
    
    fasta.names[j] = paste(c(Chr,Start,Stop,Length,TE,centromere,Accession), collapse="_")
    fasta.seqs[[j]] = subseq(myfasta[k], start = Start, end = Stop)
  }
  
  my.out <- gsub("\\..*","_TE_Seq.fa",myfiles[2])
  write.fasta(sequences = fasta.seqs, names = fasta.names, file.out = my.out, open ="w", nbchar=60, as.string = TRUE)

  ### NOW ADDING THE ALYRATA ATHILA
  ##MN47 / NT1
  setwd("~/katherine/009REPEAT/002Fasta/003Alyrata")
  ATHILA <- read.delim("Alyrata_ATHILA.txt")
  ATHILA_useful <- ATHILA %>% select(TE_ID, chromosome, genome_left_coord_FL, genome_right_coord_FL, quality, length_FL)
  
  #NEED TO FILTER SO JUST HAVE INTACT
  ATHILA_useful_intact <- ATHILA_useful %>% filter(quality == "intact")
  
  #WANT TO MUTATE NAMES SO KNOW WHCIH ACCESSION
  Acc <- data.frame(str_split_fixed(ATHILA_useful_intact$TE_ID, "_", n=5)) %>% select(X2)
  ATHILA_useful_intact <- ATHILA_useful_intact %>% mutate(accession = Acc)
  
  #WORK OUT WHICH ATHILA FAMILY
  ATHILA_Families <- ATHILA %>% filter(quality == "intact") %>% select(FL_fam)
  ATHILA_Families[,1] <- gsub("_v1","",ATHILA_Families[,1])
  ATHILA_Families[,1] <- gsub("_v2","",ATHILA_Families[,1])
  ATHILA_Families[,1] <- gsub("_general","",ATHILA_Families[,1])
  ATHILA_useful_intact <- ATHILA_useful_intact %>% mutate(family = ATHILA_Families)
  
  #SORT OUT NAMES OF THE CHROMOSOME
  ATHILA_useful_intact$chromosome <- gsub("_RagTag","", ATHILA_useful_intact$chromosome) 
  ATHILA_useful_intact$chromosome <- gsub("scaffold_","Chr", ATHILA_useful_intact$chromosome)
  ATHILA_useful_intact$chromosome <- gsub("scafold_","Chr", ATHILA_useful_intact$chromosome)
  
  #separate into MN47 and NT1
  MN47_ATHILA <- ATHILA_useful_intact %>% filter(accession == "MN47")
  NT1_ATHILA <-  ATHILA_useful_intact %>% filter(accession == "NT1")

  #WORK OUT IF IN THE CENTROMERE
  #MN47 CENTROMERE IN/OUT
  centromere <- c()
  for(i in 1:nrow(MN47_ATHILA)){
    if(MN47_ATHILA$chromosome[i] == "Chr1" & MN47_ATHILA$genome_left_coord_FL[i]%in%region1 & MN47_ATHILA$genome_right_coord_FL[i]%in%region1){
      MN47_ATHILA$centromere[i] <- paste("IN")
    }
    else if(MN47_ATHILA$chromosome[i] == "Chr2" & MN47_ATHILA$genome_left_coord_FL[i]%in%region2a & MN47_ATHILA$genome_right_coord_FL[i]%in%region2a | MN47_ATHILA$chromosome[i] == "Chr2" & MN47_ATHILA$genome_left_coord_FL[i]%in%region2b & MN47_ATHILA$genome_right_coord_FL[i]%in%region2b){
      MN47_ATHILA$centromere[i] <- paste("IN")
    }
    else if(MN47_ATHILA$chromosome[i] == "Chr3" & MN47_ATHILA$genome_left_coord_FL[i]%in%region3 & MN47_ATHILA$genome_right_coord_FL[i]%in%region3){
      MN47_ATHILA$centromere[i] <- paste("IN")
    }
    else if(MN47_ATHILA$chromosome[i] == "Chr4" & MN47_ATHILA$genome_left_coord_FL[i]%in%region4 & MN47_ATHILA$genome_right_coord_FL[i]%in%region4){
      MN47_ATHILA$centromere[i] <- paste("IN")
    }
    else if(MN47_ATHILA$chromosome[i] == "Chr5" & MN47_ATHILA$genome_left_coord_FL[i]%in%region5 & MN47_ATHILA$genome_right_coord_FL[i]%in%region5){
      MN47_ATHILA$centromere[i] <- paste("IN")
    }
    else if(MN47_ATHILA$chromosome[i] == "Chr6" & MN47_ATHILA$genome_left_coord_FL[i]%in%region6 & MN47_ATHILA$genome_right_coord_FL[i]%in%region6){
      MN47_ATHILA$centromere[i] <- paste("IN")
    }
    else if(MN47_ATHILA$chromosome[i] == "Chr7" & MN47_ATHILA$genome_left_coord_FL[i]%in%region7 & MN47_ATHILA$genome_right_coord_FL[i]%in%region7){
      MN47_ATHILA$centromere[i] <- paste("IN")
    }
    else if(MN47_ATHILA$chromosome[i] == "Chr8" & MN47_ATHILA$genome_left_coord_FL[i]%in%region8 & MN47_ATHILA$genome_right_coord_FL[i]%in%region8){
      MN47_ATHILA$centromere[i] <- paste("IN")
    }
    else{
      MN47_ATHILA$centromere[i] <- paste("OUT")
    }
  }
  MN47_ATHILA <- MN47_ATHILA %>% mutate(centromere = centromere)
  
  
  ##NT1 CENTROMERE IN/OUT
  centromere <- c()
  for(i in 1:nrow(NT1_ATHILA)){
    if(NT1_ATHILA$chromosome[i] == "Chr1" & NT1_ATHILA$genome_left_coord_FL[i]%in%region9 & NT1_ATHILA$genome_right_coord_FL[i]%in%region9){
      NT1_ATHILA$centromere[i] <- paste("IN")
    }
    else if(NT1_ATHILA$chromosome[i] == "Chr2" & NT1_ATHILA$genome_left_coord_FL[i]%in%region10 & NT1_ATHILA$genome_right_coord_FL[i]%in%region10){
      NT1_ATHILA$centromere[i] <- paste("IN")
    }
    else if(NT1_ATHILA$chromosome[i] == "Chr3" & NT1_ATHILA$genome_left_coord_FL[i]%in%region11 & NT1_ATHILA$genome_right_coord_FL[i]%in%region11){
      NT1_ATHILA$centromere[i] <- paste("IN")
    }
    else if(NT1_ATHILA$chromosome[i] == "Chr4" & NT1_ATHILA$genome_left_coord_FL[i]%in%region12 & NT1_ATHILA$genome_right_coord_FL[i]%in%region12){
      NT1_ATHILA$centromere[i] <- paste("IN")
    }
    else if(NT1_ATHILA$chromosome[i] == "Chr5" & NT1_ATHILA$genome_left_coord_FL[i]%in%region13 & NT1_ATHILA$genome_right_coord_FL[i]%in%region13){
      NT1_ATHILA$centromere[i] <- paste("IN")
    }
    else if(NT1_ATHILA$chromosome[i] == "Chr6" & NT1_ATHILA$genome_left_coord_FL[i]%in%region14 & NT1_ATHILA$genome_right_coord_FL[i]%in%region14){
      NT1_ATHILA$centromere[i] <- paste("IN")
    }
    else if(NT1_ATHILA$chromosome[i] == "Chr7" & NT1_ATHILA$genome_left_coord_FL[i]%in%region15 & NT1_ATHILA$genome_right_coord_FL[i]%in%region15){
      NT1_ATHILA$centromere[i] <- paste("IN")
    }
    else if(NT1_ATHILA$chromosome[i] == "Chr8" & NT1_ATHILA$genome_left_coord_FL[i]%in%region16 & NT1_ATHILA$genome_right_coord_FL[i]%in%region16){
      NT1_ATHILA$centromere[i] <- paste("IN")
    }
    else{
      NT1_ATHILA$centromere[i] <- paste("OUT")
    }
  }
  NT1_ATHILA <- NT1_ATHILA %>% mutate(centromere = centromere)
  #THEN FOLLOW SIMILAR PATTERN TO PREVIOUS
  ##MN47 ATHILA
  
  #READ IN MN47 FASTA / SORT OUT NAMES
  MN47_fasta <- readDNAStringSet("MN47_23122021.fasta")
  MN47_fasta <- MN47_fasta[1:8]
  MN47_fasta=MN47_fasta[sort(names(MN47_fasta))]
  names(MN47_fasta) <- c("Chr1", "Chr2", "Chr3", "Chr4", "Chr5", "Chr6", "Chr7", "Chr8")
  
  # Chrm <- MN47_ATHILA$chromosome
  
  fasta.names <- vector(length=nrow(MN47_ATHILA), mode ="character")
  fasta.seqs <- list()
  
  for (j in 1:nrow(MN47_ATHILA)){
    
    Chr = MN47_ATHILA$chromosome[j]
    Start = MN47_ATHILA$genome_left_coord_FL[j]
    Start <- as.numeric(Start)
    Stop = MN47_ATHILA$genome_right_coord_FL[j]
    Stop <- as.numeric(Stop)
    Length = Stop - Start
    TE = MN47_ATHILA$family[j,]
    centromere = MN47_ATHILA$centromere[j]
    Accession = "MN47"
    
    if(MN47_ATHILA$chromosome[j] == "Chr1"){
      k <- 1
    }
    else if(MN47_ATHILA$chromosome[j] == "Chr2"){
      k <- 2
    }
    else if(MN47_ATHILA$chromosome[j] == "Chr3"){
      k <- 3
    }
    else if(MN47_ATHILA$chromosome[j] == "Chr4"){
      k <- 4
    }
    else if(MN47_ATHILA$chromosome[j] == "Chr5"){
      k <- 5
    }
    else if(MN47_ATHILA$chromosome[j] == "Chr6"){
      k <- 6
    }
    else if(MN47_ATHILA$chromosome[j] == "Chr7"){
      k <- 7
    }
    else{
      k <- 8
    }
    
    fasta.names[j] = paste(c(Chr,Start,Stop,Length,TE,centromere,Accession), collapse="_")
    fasta.seqs = c(fasta.seqs, subseq(MN47_fasta[[k]], start = Start, end = Stop))
  }
  
  my.out <- paste("MN47_ATHILA_050223.fa")
  write.fasta(sequences = fasta.seqs, names = fasta.names, file.out = my.out, open ="w", nbchar=60, as.string = TRUE)
  
  ##NT1 ATHILA
  NT1_fasta <- readDNAStringSet("NT1_030222.fasta")
  NT1_fasta <- NT1_fasta[1:8]
  NT1_fasta=NT1_fasta[sort(names(NT1_fasta))]
  names(NT1_fasta) <- c("Chr1", "Chr2", "Chr3", "Chr4", "Chr5", "Chr6", "Chr7", "Chr8")
  
  fasta.names <- vector(length=nrow(NT1_ATHILA), mode ="character")
  fasta.seqs <- list()
  
  for (j in 1:nrow(NT1_ATHILA)){
    
    Chr = NT1_ATHILA$chromosome[j]
    Start = NT1_ATHILA$genome_left_coord_FL[j]
    Start <- as.numeric(Start)
    Stop = NT1_ATHILA$genome_right_coord_FL[j]
    Stop <- as.numeric(Stop)
    Length = Stop - Start
    TE = NT1_ATHILA$family[j,]
    centromere = NT1_ATHILA$centromere[j]
    Accession = "NT1"
    
    if(NT1_ATHILA$chromosome[j] == "Chr1"){
      k <- 1
    }
    else if(NT1_ATHILA$chromosome[j] == "Chr2"){
      k <- 2
    }
    else if(NT1_ATHILA$chromosome[j] == "Chr3"){
      k <- 3
    }
    else if(NT1_ATHILA$chromosome[j] == "Chr4"){
      k <- 4
    }
    else if(NT1_ATHILA$chromosome[j] == "Chr5"){
      k <- 5
    }
    else if(NT1_ATHILA$chromosome[j] == "Chr6"){
      k <- 6
    }
    else if(NT1_ATHILA$chromosome[j] == "Chr7"){
      k <- 7
    }
    else{
      k <- 8
    }
    
    fasta.names[j] = paste(c(Chr,Start,Stop,Length,TE,centromere,Accession), collapse="_")
    fasta.seqs = c(fasta.seqs, subseq(NT1_fasta[[k]], start = Start, end = Stop))
  }
  
  my.out <- paste("NT1_ATHILA_050223.fa")
  write.fasta(sequences = fasta.seqs, names = fasta.names, file.out = my.out, open ="w", nbchar=60, as.string = TRUE)
  
  
  ##transdecoder + pfam...
  
  
  ###FILTER INTEGRASES A. LYRATA
  #For retrieving only Integrase Pfam
  AllPfam<- read.table(file="Alyrata_050223.stdout", header=F, fill=T)
  
  # Dunno which one would be better
  AllPeptides = read.fasta("Alyrata_ALL_TE_050223.fa.transdecoder.pep", seqtype="AA")
  
  #Modify table concatenating names
  AllPfam$Joined = paste(AllPfam$V19, AllPfam$V20, AllPfam$V21, AllPfam$V22, AllPfam$V23)
  AllPfam$V19 = NULL
  AllPfam$V20 = NULL
  AllPfam$V21 = NULL
  AllPfam$V22 = NULL
  AllPfam$V23 = NULL
  write.table(AllPfam, file= "AllPfam_TableA") 
  
  
  #For retrieving only Integrase Pfam
  Integrase_Pfam = AllPfam[AllPfam$Joined == "Integrase core domain  ", ]
  #making the correct names
  row.names(Integrase_Pfam) = 1:nrow(Integrase_Pfam)
  
  names_Integrase = as.character(Integrase_Pfam$V3)
  Integrase_proteins = AllPeptides[names_Integrase]
  Integrase_proteins = unique(Integrase_proteins)
  
  write.fasta(getSequence(Integrase_proteins), getName(Integrase_proteins), file.out="Integrase_Alyrata_050223.fa")
  
  # BRASSICA
  ## GET TE LOCATIONS
  
  #scp some files
  #Set-Location -Path "C:\Users\krnic\OneDrive - University of Cambridge\Documents\II\LONG PROJECT\LONG PROJECT\"
  #scp gddh.fasta irh25@hydrogen.plantsci.cam.ac.uk:/home/irh25/katherine/004Genomes/005Brassica
  
  mycsv <- read.delim2("a12.fasta.mod.EDTA.intact.gff3", header=F)
  
  #work on matching chromosome names... keep NC for now..
  #STEP 1 GETTING LOCATIONS OF TEs FROM EDTA
  
  library(dplyr)
  raw_files <- data.frame(filename=list.files(pattern="*gff3"))
  raw_files <- raw_files %>% mutate(name_csv = gsub("\\..*",".csv",raw_files$filename)) 
   or LTR/Copia
  
  for (i in 1:nrow(raw_files)){
    x <- read.delim2(raw_files[i,1], header = FALSE)
    
    TEs <- x %>% filter(V3 %in% c("LTR_retrotransposon", "Copia_LTR_retrotransposon", "Gypsy_LTR_retrotransposon")) %>% select(chrm = V1, start = V4, stop = V5, name = V3)
    
    write.csv(TEs, raw_files$name_csv[i], quote=FALSE, row.names=FALSE)
    
  }
  
  
  
  ## MAKE INTO FASTA
  #step 2
  setwd("~/katherine/009REPEAT/002Fasta/004Brassica")
  myfiles=list.files(pattern="*.fasta")
  
  setwd("~/katherine/009REPEAT/001TE_Locations/001Brassica")
  mycsv=list.files(pattern="*csv")
  
  #now getting the sequences of the TEs
  for (i in 1:2) {
    myname=mycsv[i]
    mycsvi=mycsv[i]
    
    setwd("~/katherine/009REPEAT/001TE_Locations/001Brassica")
    mycsvi <- read.csv(mycsvi)
    
    #get rid of ptg
    mycsvi=mycsvi[grep("NC",mycsvi$chrm),]
    
    setwd("~/katherine/009REPEAT/002Fasta/004Brassica")
    myfastafile=paste(myfiles[i])
    myfasta <- readDNAStringSet(myfastafile)
    myfasta= myfasta[grep("NC",names(myfasta)),]
    
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
    my.out <- gsub("\\..*","_TE_Seq.fa",myfiles[i])
    
    write.fasta(sequences = fasta.seqs, names = fasta.names, file.out = my.out, open ="w", nbchar=60, as.string = TRUE)
    
  }
  
  
  ## COMBINE GENOMES
  ##THEN TRANSDECODER / pFAM
  
  
  #FILTER INTEGRASES BRASSICA
  #STEP 7 FILTERING FOR THE INTEGRASE CORE DOMAIN
  #For retrieving only Integrase Pfam
  AllPfam<- read.table(file="WholeBrassica_191222.stdout", header=F, fill=T)
  
  # Dunno which one would be better
  AllPeptides = read.fasta("All_Brassica_18.12.22.transdecoder.pep", seqtype="AA")
  #setwd("~/katherine/004Genomes/002Alyrata/Alyrata_All_attempt2_18.12.22.fa.transdecoder_dir")
  #AllPeptides = read.fasta("longest_orfs.pep", seqtype="AA")
  #setwd("~/katherine/004Genomes/002Alyrata")
  
  #Modify table concatenating names
  AllPfam$Joined = paste(AllPfam$V19, AllPfam$V20, AllPfam$V21, AllPfam$V22, AllPfam$V23)
  AllPfam$V19 = NULL
  AllPfam$V20 = NULL
  AllPfam$V21 = NULL
  AllPfam$V22 = NULL
  AllPfam$V23 = NULL
  write.table(AllPfam, file= "AllPfam_TableA") 
  
  
  #For retrieving only Integrase Pfam
  Integrase_Pfam = AllPfam[AllPfam$Joined == "Integrase core domain  ", ]
  #making the correct names
  row.names(Integrase_Pfam) = 1:nrow(Integrase_Pfam)
  
  names_Integrase = as.character(Integrase_Pfam$V3)
  Integrase_proteins = AllPeptides[names_Integrase]
  Integrase_proteins = unique(Integrase_proteins)
  
  write.fasta(getSequence(Integrase_proteins), getName(Integrase_proteins), file.out="Integrase_Brassica_191222.fa")
  

 
