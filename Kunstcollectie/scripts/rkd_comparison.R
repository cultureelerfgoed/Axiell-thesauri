### compare RKD ID's from first effort with Openrefine effort

library(data.table)
library(stringi)
library(stringr)

setwd("C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\Kunstcollectie\\data")

vervaardigers <- fread("Vervaardigers-Objecten-in-eigen-beheer(KC)_reconciled.tsv")

setDT(vervaardigers)

# betere versie van rkd_id_digital_reference

vervaardigers$rkd_id_digital_reference <- str_extract(vervaardigers$digital_reference, "(?<=artists/).+")


# vergelijk de twee ID's: de ene vanuit Openrefine, de andere die er al in stonden ("digital_reference")


vervaardigers[!is.na(rkd_id_digital_reference), .N] # 5625 rkd_id in digital_reference

vervaardigers[!is.na(rkd_id_reconciliation), .N] # 1774 # via openrefine

vervaardigers[!is.na(rkd_id_digital_reference) & !is.na(rkd_id_reconciliation), 
              rkd_id_match := ifelse(rkd_id_reconciliation == rkd_id_digital_reference, TRUE, FALSE)] # 659

vervaardigers[, .N, by = rkd_id_match] # 659 komen overeen, 199 niet

vervaardigers[!is.na(rkd_id_reconciliation) & is.na(rkd_id_digital_reference), .N] # 916 nieuwe matches gevonden

