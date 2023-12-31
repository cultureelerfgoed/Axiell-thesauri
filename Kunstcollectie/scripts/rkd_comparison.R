### compare RKD ID's from first effort with Openrefine effort

library(data.table)
library(stringi)
library(stringr)
library(xlsx)

setwd("C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\Kunstcollectie\\data")

vervaardigers <- fread("Vervaardigers-Objecten-in-eigen-beheer(KC)_reconciled.tsv")

setDT(vervaardigers)

# extract rkd_id_digital_reference uit digital_reference

vervaardigers$rkd_id_digital_reference <- str_extract(vervaardigers$digital_reference, "(?<=artists/).+")

# vergelijk de twee ID's: de ene vanuit Openrefine, de andere die er al in stonden ("digital_reference")

vervaardigers[!is.na(rkd_id_digital_reference), .N] # 5625 rkd_id in digital_reference

vervaardigers[!is.na(rkd_id_reconciliation), .N] # 1774 # via openrefine

# maak rkd_id_match variabele om twee RKD matches te vergelijken 

vervaardigers[!is.na(rkd_id_digital_reference) & !is.na(rkd_id_reconciliation), 
              rkd_id_match := ifelse(rkd_id_reconciliation == rkd_id_digital_reference, TRUE, FALSE)] 

#vergelijk beide matches

vervaardigers[, .N, by = rkd_id_match] # 659 komen overeen met digital_reference, 199 niet

vervaardigers[!is.na(rkd_id_reconciliation) & is.na(rkd_id_digital_reference), .N] # 916 nieuwe matches gevonden met Openrefine

# wegschrijven resultaten

write.xlsx(vervaardigers, "Vervaardigers-Objecten-in-eigen-beheer(KC)_resultaten.xlsx")

fwrite(vervaardigers, "Vervaardigers-Objecten-in-eigen-beheer(KC)_resultaten.csv")

