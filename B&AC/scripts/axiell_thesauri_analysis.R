### Axiell thesauri analysis

# Load required libraries
library(stringdist)
library(dplyr)
library(data.table)
library(foreach)
library(doParallel)
library(progress)
library(textTinyR)

setwd("C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\B&AC\\data")

# load B&AC thesaurus
thesaurus <- fread("Thesarus-export(B&AC).csv", encoding = "UTF-8")
setDT(thesaurus)

#x <- thesaurus[sample(nrow(thesaurus), 10000), ]
#concepts <- x$term

# term status

thesaurus[, .N, by = term.status]

# use count zero

zero_use <- thesaurus[use_count == 0, list(term, term.soort)]

fwrite(zero_use, "thesaurus_termen_ongebruikt.csv")

# duplicates, all removed & not zero use

dups <- thesaurus[duplicated(thesaurus$term) & use_count != 0 | duplicated(thesaurus$term, fromLast = TRUE) & use_count != 0][order(term)]

fwrite(dups, "thesaurus_dubbelingen.csv")

# flag dups
thesaurus[duplicated(term)| duplicated(thesaurus$term, fromLast = TRUE), dubbel := TRUE,]
thesaurus[is.na(dubbel), dubbel := FALSE, ]

thesaurus[use_count !=0, .N, by = .(dubbel)]

# split by soort.term if not zero use and non-duplicate

geografisch <- thesaurus[use_count != 0 & dubbel == FALSE & grepl("geografisch", term.soort),]
plaats <- thesaurus[use_count != 0 & dubbel == FALSE & grepl("plaats", term.soort),]
onderwerp <- thesaurus[use_count != 0 & dubbel == FALSE & grepl("onderwerp", term.soort),]

overig <- thesaurus[use_count != 0 & dubbel == FALSE & !grepl("onderwerp", term.soort),]

fwrite(onderwerp, "thesaurus_onderwerp.csv")

# compounded terms
compounded <- thesaurus[use_count != 0 & dubbel == FALSE & grepl(";", term), ]

fwrite(ow_compounded, "thesaurus_gedeelde_termen.csv")

# create set to find similar terms used as "onderwerp"

ow_matching <- onderwerp[, list(term) ]

fwrite(ow_matching, "thesaurus_onderwerp_matchingset.csv")

# compare matched terms for onderwerp using fuzzymatcher.py

fuzzy_ow <- fread("thesaurus_onderwerp_fuzzymatches.csv", encoding = "UTF-8")

onderwerp[term %in% fuzzy_ow$term_in_csv, .N] # 6755

25110 - onderwerp[term %in% fuzzy_ow$term_in_csv, .N]

# make openrefine set voor onderwerpstrefwoorden

"%ni%" <- Negate("%in%")

openrefine_ow <- onderwerp[term %ni% fuzzy_ow$term_in_csv,  ]

fwrite(openrefine_ow, "thesaurus_onderwerp_openrefineset.csv")

# count how many object we can already refine

openrefine_ow[, sum(use_count)]

thesaurus[, sum(use_count),  ]

# van alle termen in het veld onderwerp kan maximaal 54% procent direct worden verrijkt:

openrefine_ow[, sum(use_count)] / thesaurus[grepl("onderwerp", term.soort), sum(use_count),  ] # 54,4 %


