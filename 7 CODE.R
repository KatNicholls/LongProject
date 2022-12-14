#STEP 7 FILTERING FOR THE INTEGRASE CORE DOMAIN
#For retrieving only Integrase Pfam
AllPfam<- read.table(file="WholeTransdecoder_081222.stdout", header=F, fill=T)

#where does this file come from ??
AllPeptides = read.fasta("Whole ...pep", seqtype="AA")


### Modify table: concatenating names
AllPfam$Concatenated = paste(AllPfam$V19, AllPfam$V20, AllPfam$V21, AllPfam$V22, AllPfam$V23)
AllPfam$V19 = NULL
AllPfam$V20 = NULL
AllPfam$V21 = NULL
AllPfam$V22 = NULL
AllPfam$V23 = NULL

#write.table(AllPfam, file= "AllPfam_TableA") 

#For retrieving only Integrase Pfam
Integrase_Pfam = AllPfam[AllPfam$Concatenated == "Integrase core domain  ", ]

#making the coorect names
row.names(Integrase_Pfam) = 1:nrow(Integrase_Pfam)


names_Integrase = Integrase_Pfam$V3
Integrase_proteins = AllPeptides[names_Integrase]
Integrase_proteins = unique(Integrase_proteins)

write.fasta(getSequence(Integrase_proteins), getName(Integrase_proteins), file.out="Integrase_Allproteins_141222.fa")