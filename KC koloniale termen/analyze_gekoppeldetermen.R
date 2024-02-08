# analyze koppeling termen Koloniaal Verleden aan thesaurustermen KC

library(data.table)
library(tidyr)
library(dplyr)
library(zoo)

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

# give objects where at least one koloniaal verleden term is matched

results2 <- long_df[colonial == TRUE, .N, by = Objectnummer][order(-N)]

# NB why do i miss objectnumbers between original and results2? 
#  because there is no match term included in the EXPORT yet...

`%nin%` = Negate(`%in%`)

missings <- long_df[Objectnummer %nin% results2$Objectnummer, list(unique(Objectnummer)) ] 

# let's try it without the top-5 generic terms from koloniaal verleden

results3 <- long_df[colonial == TRUE & Value != "keramiek" & Value != "porselein" & Value != "koper" & Value != "textiel" & Value != "katoen", 
                    .N, by = Objectnummer][order(-N)]

# to do: 
# 1: top5 plus ten minste 1 ander trefwoord

top5 <- long_df[Value == "keramiek" | Value == "porselein" | Value == "koper" | Value == "textiel" | Value == "katoen", 
                .N, by = Objectnummer][order(-N)]

merged <- merge(top5, results3, by = "Objectnummer", all.x = TRUE, all.y =  TRUE)

setnames(merged, "N.x", "top5_trefwoorden")
setnames(merged, "N.y", "overige_trefwoorden")

# alleen trefwoorden wanneer ze niet top-5 zijn of samen met top-5 voorkomen

#results4 <- merged[!is.na(overige_trefwoorden) & is.na(top5_trefwoorden), ]

#subset <- results3[Objectnummer %in% top5$Objectnummer, ]

#results4[Objectnummer %in% results3$Objectnummer, .N]

# subset plus results4 = results3...

#merg2 <- merge(subset, results4, by = "Objectnummer", all.x = T, all.y =  T)

#merg2[Objectnummer %in% results3$Objectnummer, .N]

# 2: lijst uit 1 controleren met de ca. 1600 op Cn.nl

# methode controleren met handmatig en wat je mist aanvullen aan trefwoorden aan koloniaal verleden
# wat voor percentage overlap is voldoende om deze methode aan te raden? 

export <- readxl::read_excel("vc-sporen-van-slavernij-edm-strict-records-xml.xlsx")
setDT(export)
export$id <- basename(export$`rdf:RDF - edm:ProvidedCHO - rdf:about`)
#export_objects <- export[, list(`rdf:RDF - edm:ProvidedCHO - dc:identifier`)]
#export_objects <- export_objects[!is.na(`rdf:RDF - edm:ProvidedCHO - dc:identifier`)]

# Fill missing values in the first column
export$`rdf:RDF - edm:ProvidedCHO - rdf:about` <- na.locf(export$`rdf:RDF - edm:ProvidedCHO - rdf:about`, na.rm = FALSE)

export$id <- basename(export$`rdf:RDF - edm:ProvidedCHO - rdf:about`)

# get objects from collectiennl that are not matched

export[id %nin% results3$Objectnummer, .N, by = id ] # 636 objecten niet gevonden (wel in cn.nl, niet in deze resultaten)

nomatch <- export[id %nin% results3$Objectnummer, ]

# see which terms we might have missed

missedsubjects <- nomatch[, .N, list(`rdf:RDF - edm:ProvidedCHO - dc:subject`)][order(-N)]

missedmaterials <- nomatch[, .N, list(`rdf:RDF - edm:ProvidedCHO - dcterms:medium`)][order(-N)]

missedtypes <- nomatch[, .N, list(`rdf:RDF - edm:ProvidedCHO - dc:type`)][order(-N)]

# did we find extra items from the shortlist?

results3[Objectnummer %in% export$id] # 1063 overlap (zowel in cn.nl als deze resultaten)

bonus <- results3[Objectnummer %nin% export$id] # 1544 objecten extra gevonden (niet in cn.nl, wel in deze resultaten)

})

