### Axiell thesauri analysis

# Load required libraries
library(stringdist)
library(dplyr)
library(data.table)
library(foreach)
library(doParallel)
library(progress)
library(textTinyR)

setwd("C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri")

# Sample list of concepts
thesaurus <- fread("Thesarus-export(B&AC).csv", encoding = "UTF-8")
setDT(thesaurus)

#x <- thesaurus[sample(nrow(thesaurus), 10000), ]
#concepts <- x$term

# split by soort.term

geografisch <- thesaurus[term.soort == "geografisch trefwoord", .N, by = term][order(-N)]
onderwerp <- thesaurus[term.soort == "onderwerp", .N, by = term][order(-N)]
rechten <- thesaurus[term.soort == "rechten", .N, by = term][order(-N)]
collectie <- thesaurus[term.soort == "collectie", .N, by = term][order(-N)]


# overview of term.soort

soorten <- thesaurus[, ., list(term.soort, use_count)][order(-N)]

fwrite(soorten, "thesaurus_soort.csv")

summary_table <- thesaurus[, .(Total_use_count = sum(use_count)), by = .(term.soort)]

fwrite(summary_table, "thesaurus_soort_usecount.csv")

# unique number of term.soort per term

termsoort <- thesaurus[, .(Distinct_Term_Soort_Count = uniqueN(term.soort),
                  Concatenated_Term_Soort = paste(unique(term.soort), collapse = ", ")),
              by = term]

#overlap
plaats <- thesaurus[term.soort == "geografisch trefwoord#plaats", .N, by = term][order(-N)]

overlap <- plaats[term %in% geografisch$term, ]

# use count zero

zero_use <- thesaurus[use_count == 0, list(term, term.soort)]

fwrite(zero_use, "thesaurus_zero_use.csv")

# split ow

terms_ow <- thesaurus[term.soort == "onderwerp" & use_count != 0, .N, list(term, use_count)]
setDT(terms_ow)

#duplicated

dups_ow <- terms_ow[duplicated(term),]
fwrite(dups_ow, "thesaurus_onderwerp_dubbel.csv")

# compounded terms
ow_compounded <- terms_ow[grepl(";", term), ]
fwrite(ow_compounded, "thesaurus_onderwerp_gedeeld.csv")

# find similar terms
ow_matching <- terms_ow[!grepl(";", term) & !duplicated(term), ]
fwrite(ow_matching, "thesaurus_onderwerp_matchingset.csv")

terms <- gedeelde$term

#sample

textiel <- gedeelde[grepl("textiel", term),]
pijpen <- gedeelde[grepl("pijpen", term),]
bouw <- gedeelde[grepl("bouw", term),]

# import matched terms for onderwerp using fuzzymatcher.py

matches_ow <- fread("thesaurus_onderwerp_matches.csv", encoding = "UTF-8")

concepts <- data.frame(c(matches_ow$`Term in CSV`, matches_ow$`Other Term in CSV`))
colnames(concepts)
setnames(concepts, "c.matches_ow..Term.in.CSV...matches_ow..Other.Term.in.CSV..", "concept")

setDT(concepts)
concepts <- concepts[!duplicated(concept),]

# shoot them back into ow_matching to combine matched concepts with use count

ow_matching <- fread("thesaurus_onderwerp_matchingset.csv")
setDT(ow_matching)
ow_matching[term %in% concepts$concept, potential_matched_concept := TRUE,]
ow_matching[is.na(potential_matched_concept), potential_matched_concept := FALSE,]

ow_matching <- ow_matching[order(-use_count),]
ow_matching[potential_matched_concept == FALSE, .N] # 16899 potential unique concepts in thesaurus 
ow_matching[potential_matched_concept == FALSE, sum(use_count),] # 314095 objects can be potentially enriched 
ow_matching[potential_matched_concept == TRUE, sum(use_count),] # 292582 objects cannot be potentially enriched 

314095 / (314095 + 292582) # 51% of B&AC Collection objects can be potentially enriched relatively easily

fwrite(ow_matching, "thesaurus_onderwerp_matchingresult.csv")

