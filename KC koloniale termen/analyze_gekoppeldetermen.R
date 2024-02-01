# analyze koppeling termen Koloniaal Verleden aan thesaurustermen KC

library(data.table)
library(tidyr)
library(dplyr)

setwd("C:\\Users\\Ruben\\Documents\\05. RCE\\Axiell thesauri\\KC koloniale termen")

data <- readxl::read_excel("EXPORT_Adlib_Termen-KC-reconciled_2.xlsx", sheet = "Blad1")
setdt(data)

# switch to long format to get all trefwoorden en onderwerpen per record

data <- mutate_all(data, as.character)

long_df <- pivot_longer(data, 
                        cols = -c("Objectnummer"), 
                        names_to = "Variable", 
                        values_to = "Value")

# remove NA

setDT(long_df)
long_df <- long_df[!is.na(Value),]

long_df <- as.data.frame(long_df)

# load koloniaal verleden preflabels

koloniaal <- readxl::read_excel("koloniaal_verleden.xlsx", sheet = "Blad1")

koloniaal <- as.data.frame(koloniaal)

koloniaal$preflabel <- tolower(koloniaal$preflabel)
koloniaal$altabel <- tolower(koloniaal$altlabel)

# select colonial terms from Value

setDT(long_df)
long_df$Value <- tolower(long_df$Value)

long_df[Value %in% koloniaal$preflabel, .N]

long_df[Value %in% koloniaal$preflabel, colonial := TRUE]
long_df[Value %in% koloniaal$altlabel, colonial := TRUE]

# count koloniaal verleden matches by koloniaal term

results <- long_df[!is.na(colonial), .N, by = Value][order(-N)]

# give objects where at least two koloniaal verleden are matched

results2 <- long_df[colonial == TRUE, .N, by = Objectnummer][order(-N)]

# NB why do i miss objectnumbers between original and results2? > because there is no match term included in the EXPORT yet...
`%nin%` = Negate(`%in%`)

missings <- long_df[Objectnummer %nin% results2$Objectnummer, list(unique(Objectnummer)) ]

# let's try it without the top-5 generic terms from koloniaal verleden

results3 <- long_df[colonial == TRUE & Value != "keramiek" & Value != "porselein" & Value != "koper" & Value != "textiel" & Value != "katoen", 
                    .N, by = Objectnummer][order(-N)]









