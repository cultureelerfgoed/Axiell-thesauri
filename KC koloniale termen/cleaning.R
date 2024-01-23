# clean Term

library(data.table)
library(stringi)

data <- fread("C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\KC koloniale termen\\Termen_KC.csv") 

setDT(data)

data$Term <- as.character(data$Term)

data[, Term_clean := stri_trans_general(data$Term, "Latin-ASCII"),]

data[, Term_clean := gsub("/", " ", Term_clean),]

data[, Term_clean := gsub("\\", " ", Term_clean, fixed = TRUE),]

df <- data[!is.na(data$Term_clean) & data$Term_clean != "", ]

write.csv(df, "C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\KC koloniale termen\\Termen_KC_clean.csv", fileEncoding = "UTF-8", row.names = FALSE))
