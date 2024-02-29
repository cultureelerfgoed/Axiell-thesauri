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

export[id %nin% results3$Objectnummer, .N, by = id ] # 636 objecten niet gevonden (wel in cn.nl deelcollectie, niet in deze resultaten)

nomatch <- export[id %nin% results3$Objectnummer, ]

# see which terms we might have missed

missedsubjects <- nomatch[, .N, list(`rdf:RDF - edm:ProvidedCHO - dc:subject`)][order(-N)]

missedmaterials <- nomatch[, .N, list(`rdf:RDF - edm:ProvidedCHO - dcterms:medium`)][order(-N)]

missedtypes <- nomatch[, .N, list(`rdf:RDF - edm:ProvidedCHO - dc:type`)][order(-N)]

# did we find extra items from the shortlist?

results3[Objectnummer %in% export$id] # 1063 overlap (zowel in cn.nl als deze resultaten)

bonus <- results3[Objectnummer %nin% export$id] # 1544 objecten extra gevonden (niet in cn.nl, wel in deze resultaten)

# wat is verschil tussen 1544 en 23767 ook alweer 
# via handreiking, NDE datawerkplaatsen en code online

# zit alles van koloniale deelcollectie dan al in data vanuit export? 
# NEE. Dus je begint al met missen van ruim 300 objecten vanuit termenlijst t.o.v. handmatig
setDT(export)
export[, uniqueN(id),] # 1699 objecten koloniaal
export[id %in% long_df$Objectnummer, .N] # 1324 daarvan in subset

1700 - 1324

##### Poging 2 direct vanuit de volledige axiell export KC met alle termen, thema's en locaties

adlib <- readxl::read_excel("Export_KC_2024-02-29(2).xlsx", sheet = "Objecten")

# naar long tabel

data <- mutate_all(adlib, as.character)

long_df <- pivot_longer(data, 
                        cols = -c("Inventarisnr."), 
                        names_to = "Kolom_adlib_export", 
                        values_to = "waarde")
setDT(long_df)

long_df <- long_df[!is.na(waarde),]

# load koloniaal verleden preflabels

koloniaal <- readxl::read_excel("koloniaal_verleden.xlsx", sheet = "Blad1")

koloniaal <- as.data.frame(koloniaal)

koloniaal$preflabel <- tolower(koloniaal$preflabel)
koloniaal$altabel <- tolower(koloniaal$altlabel)

# koloniaal to long

data <- mutate_all(koloniaal, as.character)

kol_long <- pivot_longer(data, 
                        cols = -c("s"), 
                        names_to = "kolom", 
                        values_to = "label")
setDT(kol_long)

kol_long <- kol_long[!is.na(label),]
kol_long <- kol_long[!duplicated(kol_long$label, fromLast = TRUE), ]

# select colonial terms from Value

setDT(long_df)
long_df$waarde <- tolower(long_df$waarde)



long_df[waarde %in% kol_long$label, .N] # 34372 treffers in totaal
long_df[waarde %in% kol_long$label, .N, by = Inventarisnr.] # 23570 objecten met een trefwoord in koloniaal verleden

# zonder de top-5 termen van koloniaal (zie hierboven)

# filteren van matches!

# eerst de top5 termen die voor ons nu weinig toegevoegde waarde hebben (zie boven) aan een var toewijzen

long_df <- long_df[waarde == "keramiek" | waarde == "porselein" | waarde == "koper" | waarde == "textiel" | waarde == "katoen", 
                top5term := TRUE]

long_df <- long_df[is.na(top5term), top5term := FALSE,]

# vervolgens aangeven of er een match is met de koloniaal verleden woordenlijst (per waarde, niet per object)

long_df[waarde %in% kol_long$label, match_koloniaal_woordenlijst := TRUE]
long_df[waarde %in% kol_long$label, match_koloniaal_woordenlijst := TRUE]

# wat houden we dan over aan treffers met koloniaal verleden als we de top5 weglaten?

long_df[top5term == FALSE & match_koloniaal_woordenlijst == TRUE, .N, by = Inventarisnr.] # 6122 matches

# hoeveel van die matches zaten al in de handmatige actie (thema "verband houdend met")

thema_matches <- long_df[waarde == "verband houdend met het koloniale verleden", .N, list(Inventarisnr.)] # 1771 objecten met dit thema (de handmatige selectie)

long_df[Inventarisnr. %in% thema_matches$Inventarisnr. & match_koloniaal_woordenlijst == TRUE, .N, by = Inventarisnr.] # 1356 gevonden via koloniaal verleden (incl top 5!)

long_df[Inventarisnr. %in% thema_matches$Inventarisnr. & match_koloniaal_woordenlijst == TRUE & top5term == FALSE, .N, by = Inventarisnr.] # 1170 als we de top5 termen buiten beschouwing laten

# wat zijn nu de soort trefwoorden van de ca. 600 die we nu gemist hebben met de excl. top 5 methode?

positives <- long_df[Inventarisnr. %in% thema_matches$Inventarisnr. & match_koloniaal_woordenlijst == TRUE & top5term == FALSE, .N, list(Inventarisnr.)]

missed_matches <- long_df[Inventarisnr. %in% thema_matches$Inventarisnr. & Inventarisnr. %nin% positives$Inventarisnr. , .N, by = Inventarisnr.]

# welke termen horen er dan bij die 601 missed matches?

missed_trefwoorden <- long_df[Inventarisnr. %in% missed_matches$Inventarisnr., .N, by = waarde][order(-N)]

# welke objecten vinden we dan extra?

extra_objecten <- long_df[Inventarisnr. %nin% thema_matches$Inventarisnr. & match_koloniaal_woordenlijst == TRUE & top5term == FALSE, .N, by = Inventarisnr.][order(-N)]

# en welke trefwoorden gaat het hier om?

long_df[Inventarisnr. %nin% thema_matches$Inventarisnr. & match_koloniaal_woordenlijst == TRUE & top5term == FALSE, .N, by = waarde][order(-N)]


# het klopt nog niet, maar dat ligt aan de data, niet de methode
# zie C108! Wel verband met koloniaal volgens cn.nl, niet volgens de export. 
# mogelijk omdat niet alle thema's meekomen?

# vergelijk dan maar met cn.nl data

long_df[Inventarisnr. %in% export$id, .N, by = Inventarisnr.] # 1699

cnmatches <- long_df[Inventarisnr. %in% export$id & match_koloniaal_woordenlijst == TRUE & top5term == FALSE, .N, by = Inventarisnr.]

gekke <- extra_objecten[Inventarisnr. %nin% cnmatches$Inventarisnr., .N, by = Inventarisnr.]

# dus de gekke 21 objecten staan niet als koloniaal thema geidentificeerd in adlib, maar zijn het wel volgens cn.nl
# en die hebben we dus ook gevonden

# er zijn dus ook nog 97 objecten die WEL in Adlib koloniaal zijn, maar dan weer niet op CN.nl staan!
laatste <- long_df[Inventarisnr. %nin% export$id & waarde == "verband houdend met het koloniale verleden" , .N, by = Inventarisnr.]

# welke hebben we daarvan gevonden? > 42
long_df[Inventarisnr.%in% laatste$Inventarisnr. & match_koloniaal_woordenlijst == TRUE & top5term == FALSE , .N, by = Inventarisnr.]

