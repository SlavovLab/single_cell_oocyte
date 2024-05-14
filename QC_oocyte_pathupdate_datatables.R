
library("readxl")
# load in meta data 
meta <- read_excel("Oocyte_paper_sets.xlsx", sheet = 1)
meta$Run_label <- paste0(meta$File_name_short, "_", meta$Label)

# looking at oocyte data  
ev <- read.delim("Oocytes_report.tsv")

# filter on Q.value
x.fil <- ev %>% dplyr::filter(Lib.Q.Value <= 0.01 & Lib.PG.Q.Value <= 0.01)
x.fil$SeqCharge <- paste0(x.fil$Modified.Sequence, x.fil$Precursor.Charge)

# count the number of unique protein groups per run
x.count1 <- x.fil %>% dplyr::group_by(Run) %>% dplyr::summarise(n_Genes = length(unique(Genes)))
# plot the number of counts 
ggplot(data = x.count1, aes(x = reorder(Run, n_Genes), y = n_Genes)) + 
  geom_bar(stat="identity", fill = "darkolivegreen") + 
  #geom_text(aes(label = n_Genes), vjust = -0.3, size = 3.5) + 
  labs(title = "Number of Proteins by GeneNames after Q.value filter",
       y = "Number of Genes", 
       x = "Single Cell Sets") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20),
        axis.text.y = element_text(size = 20), 
        axis.text.x = element_blank())

# shorten run names
x.fil$Run_short <- sub("_.*", "", x.fil$Run)

# obtain the mtraq label for each precursor
x.fil$label <- sub( ".*\\(","", (sub(").*", "", x.fil$Modified.Sequence)))
unique(x.fil$label)
# shorten the label to d0, d4, d8
x.fil$label_short <- paste0("d", sub(".*-", "", x.fil$label))
unique(x.fil$label_short)
# paste together the Run and the mtraq label used 
x.fil$Run_Label <- paste0(x.fil$Run_short, "_", x.fil$label_short )
unique(x.fil$Run_Label)
# put associated sample id into raw data dataframe 
x.fil$sample_id <- meta$Patient_ID[match(x.fil$Run_Label, meta$Run_label)]
unique(x.fil$sample_id)
# paste together the run, label, and sample id as one characteristic 
x.fil$Run_sample_sampleid <- paste0(x.fil$Run_Label, "_", x.fil$sample_id)
unique(x.fil$Run_sample_sampleid)

# count the number of unique precursors per sample 
x.count2 <- x.fil %>% 
  dplyr::group_by(Run_sample_sampleid) %>% 
  dplyr::summarise(n_Precursors = length(unique(SeqCharge)))

ggplot(data = x.count2, aes(x = reorder(Run_sample_sampleid, n_Precursors), y = n_Precursors)) + 
  geom_bar(stat="identity", fill = "darkolivegreen") + 
  labs(title = "Number of Precursors after Q.value filter",
       y = "Number of Precursors (Seq Charge)", 
       x = "Single Oocytes / Sample") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20),
        axis.text.y = element_text(size = 20), 
        axis.text.x = element_blank())


# filter out run that has low amount of protein identified 
x.count1.fil <- x.count1 %>% dplyr::filter(n_Genes > 2000)
x.fil1 <- x.fil %>% dplyr::filter(Run %in% x.count1.fil$Run)
unique(x.fil1$Run)


# change zeroes to NA
x.fil1[x.fil1 == 0] <- NA

# rename SeqCharge with Stripped Sequence
x.fil1$SeqCharge <- paste0(x.fil1$Stripped.Sequence, x.fil1$Precursor.Charge)

# quantify the number of times the precursor has been quantified per plexDIA run
precursorcount <- x.fil1 %>% 
  dplyr::group_by(SeqCharge) %>% 
  dplyr::summarise(numRuns = length(unique(Run_short)))

# filter for precursors in at least 50% of the runs
precursorcount.fil <- precursorcount %>% 
  dplyr::filter(numRuns >= length(unique(x.fil1$Run_short))/2)

# filter the data for only these precursors AND for protetypic 
x.fil2 <- x.fil1 %>% 
  dplyr::filter(SeqCharge %in% precursorcount.fil$SeqCharge) %>% 
  dplyr::filter(Proteotypic == 1)


# count the number of proteins after filtering for peptide missingness
x.count3 <- x.fil2 %>% dplyr::group_by(Run_sample_sampleid) %>% dplyr::summarise(n_Genes = length(unique(Genes)))
# plot the number of counts 
ggplot(data = x.count3, aes(x = reorder(Run_sample_sampleid, n_Genes), y = n_Genes)) + 
  geom_bar(stat="identity", fill = "darkolivegreen") + 
  labs(title = "Number of Proteins by GeneNames after Q.value filter & missing data",
       y = "Number of Genes", 
       x = "Single Oocytes") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20),
        axis.text.y = element_text(size = 20), 
        axis.text.x = element_blank())


# now we can go onto normalization  (each row is a precursor, that is specific to each run and each sample )
# median normalize each specific sample
x.fil3 <- x.fil2 %>% 
  dplyr::group_by(Run_sample_sampleid) %>% 
  dplyr::mutate(q1 = Precursor.Translated / median(Precursor.Translated, na.rm = T))

# normalize each peptide by the mean across all single cells 
x.fil4 <- x.fil3 %>% 
  dplyr::group_by(SeqCharge) %>% 
  dplyr::mutate(q2 = q1 / mean(q1, na.rm = T))

# pull out the leading protein match by leading Uniprot ID
x.fil4$leading.protein <- sub(";.*", "", x.fil4$Protein.Group)

# obtain CVs for each protein in each sample, and plot density plot
CV <- x.fil4 %>% 
  dplyr::group_by(Protein.Group, Run_sample_sampleid) %>% 
  dplyr::summarise(cv = sd(q2, na.rm = T) / mean(q2, na.rm = T))
# obtain the median CV per sample 
CV.median <- CV %>% 
  dplyr::group_by(Run_sample_sampleid) %>% 
  dplyr::summarise(medianCV = median(cv, na.rm = T))
CV.median$sample <- ifelse(grepl("NEG", CV.median$Run_sample_sampleid), "Negative", "SingleCell")
ggplot(data = CV.median, aes(x = medianCV, fill = sample)) + 
  geom_density(alpha = 0.8) + 
  geom_vline(xintercept = 0.7, linetype = "dashed") + 
  theme_bw() +
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20), 
        legend.text = element_text(size = 20), legend.title = element_text(size = 20)) + 
  labs(x = "Quantification Variability, CV", 
       fill = "Sample Type") 

# filter the peptide dataframe for samples that pass our CV threshold and then normalize 
CV.median.pass <- CV.median %>% dplyr::filter(medianCV < 0.7)
x.fil4a <- x.fil4 %>% 
  dplyr::filter(Run_sample_sampleid %in% CV.median.pass$Run_sample_sampleid)


# collapse each protein in each sample by the median of peptides mapping to that protein  
x.fil5 <- x.fil4a %>% 
  dplyr::group_by(Run_sample_sampleid, leading.protein) %>% 
  dplyr::summarise(q3 = median(q2, na.rm = T))

# column and row normalize the protein matrix  
x.fil6 <- x.fil5 %>%
  dplyr::group_by(Run_sample_sampleid) %>% 
  dplyr::mutate(q4 = q3 / median(q3, na.rm = T))
x.fil7 <- x.fil6 %>% 
  dplyr::group_by(leading.protein) %>% 
  dplyr::mutate(q5 = q4 / median(q4, na.rm = T))

# reshape the matrix ( samples as columns, proteins as rows)
x.fil8 <- reshape2::dcast(leading.protein ~ Run_sample_sampleid, data = x.fil7, value.var = "q5")

x.fil8[,2:ncol(x.fil8)] <- log2(x.fil8[,2:ncol(x.fil8)])

# save this as a csv file
#write.csv(x.fil8, "2022_07_28_Oocyte_ProteinsXcells.csv", row.names = FALSE)

