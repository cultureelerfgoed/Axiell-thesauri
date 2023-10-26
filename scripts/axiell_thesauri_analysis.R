### Axiell thesauri analysis

# Load required libraries
library(stringdist)
library(dplyr)
library(data.table)
library(foreach)
library(doParallel)
library(progress)
library(textTinyR)

setwd("C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\data")

# load B&AC thesaurus
thesaurus <- fread("Thesarus-export(B&AC).csv", encoding = "UTF-8")
setDT(thesaurus)

#x <- thesaurus[sample(nrow(thesaurus), 10000), ]
#concepts <- x$term

# term status

thesaurus[, .N, by = term.status]

# use count zero

zero_use <- thesaurus[use_count == 0, list(term, term.soort)]

# duplicates, all removed

dups <- thesaurus[duplicated(thesaurus$term) & use_count != 0 | duplicated(thesaurus$term, fromLast = TRUE) & use_count != 0][order(term)]

# flag dups
thesaurus[duplicated(term)| duplicated(thesaurus$term, fromLast = TRUE), dubbel := TRUE,]
thesaurus[is.na(dubbel), dubbel := FALSE, ]

thesaurus[use_count !=0, .N, by = .(dubbel)]

# split by soort.term if not zero use and non-duplicate

geografisch <- thesaurus[use_count != 0 & dubbel == FALSE & grepl("geografisch", term.soort),]
plaats <- thesaurus[use_count != 0 & dubbel == FALSE & grepl("plaats", term.soort),]
onderwerp <- thesaurus[use_count != 0 & dubbel == FALSE & grepl("onderwerp", term.soort),]

# overview of term.soort

soorten <- thesaurus[,  .N, list(term.soort)][order(-N)]

fwrite(soorten, "thesaurus_soort.csv")

summary_table <- thesaurus[, .(Total_use_count = sum(use_count)), by = .(term.soort)]

fwrite(summary_table, "thesaurus_soort_usecount.csv")

# unique number of term.soort per term

termsoort <- thesaurus[, .(Distinct_Term_Soort_Count = uniqueN(term.soort),
                  Concatenated_Term_Soort = paste(unique(term.soort), collapse = ", ")),
              by = term]

# compounded terms
compounded <- thesaurus[use_count != 0 & dubbel == FALSE & grepl(";", term), ]
fwrite(ow_compounded, "thesaurus_gedeelde_termen.csv")

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

# compare fuzzy with levenshtein output with same threshold score of 85%

fuzzy <- fread("thesaurus_onderwerp_matches.csv", encoding = "UTF-8")

leven <- fread("thesaurus_onderwerp_matches_v2.csv", encoding = "UTF-8")

setDT(fuzzy)

`%!in%` = Negate(`%in%`)

fuzzy[`Term in CSV` %!in% leven$`Term in CSV`, .N, by = `Term in CSV`] # 3279
leven[`Term in CSV` %!in% fuzzy$`Term in CSV`, .N] # 0


# = all terms we find with levenshtein are in fuzzy, but far from vice versa. 

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


# unieke lijst exporteren met eventuele uri's en die checken (broken?) en inhoudelijk checken



