
source("source/read and convert.R")

df_Kinopreise <- df_Eintritt|>
  distinct(Platzkategorie, .keep_all = T)|>
  select(Platzkategorie, Verkaufspreis)

df_Kinopreise

#########################################################################################################
# Einnahmen und Abgaben von mehreren Events verhältnismässig nach Besucherzahlen 
# auf die gelinkten Filme aufteilen (Link im Excel file: .../Kinoklub/Input/Verleiherabgaben.xlsx ) 
#########################################################################################################
df_Abrechnung <- df_Eintritt|>
  distinct(Datum, `Suisa Nummer`, .keep_all = TRUE)|>
  select(-(4:8))|>
  select(-Adresse, -PLZ, -Ort, -Titel, -Verleiher)|>
  left_join( # Verleiherrechnungen 
    Einnahmen_und_Ausgaben[["Ausgaben"]]|>
      filter(Kategorie == Einnahmen_und_Ausgaben[["dropdown"]]$`drop down`[5])|> # suchen nach den Verleiher Einträgen
      select(-Kategorie,-Datum, -Bezeichnung)|>
      select(1:4)|>
      rename(Verleiherrechnungsbetrag = Betrag,
             Datum = Spieldatum),
    by = join_by(Datum)
  )#|>
  # mutate(`Kinoförderer gratis?` = if_else(`Kinoförderer gratis?` == "nein",F,T))
df_Abrechnung

#########################################################################################################
# join `Kinoförderer gratis?`
df_Eintritt <- df_Abrechnung|>
  select(Datum, `Suisa Nummer`, Filmtitel, `Kinoförderer gratis?`)|>
  left_join(df_Eintritt,
            by = join_by(Datum, `Suisa Nummer`, Filmtitel, `Kinoförderer gratis?`))

df_Eintritt <- df_Eintritt|>
  mutate(`Kinoförderer gratis?` = if_else(`Kinoförderer gratis?` == "nein", F, T),
         Zahlend = if_else(Verkaufspreis>0, T, F))

# Kinoförderer müssen abgerechnet werden für "Netto 3" Umsatz
df_Eintritt <- bind_rows(
  df_Eintritt|>
  filter(!`Kinoförderer gratis?`)|>
  mutate(`Umsatz für Netto3` = if_else(Platzkategorie == df_P_kat_verechnen$Kinoförderer,
                                       Anzahl * df_P_kat_verechnen$Verkaufspreis,
                                       Umsatz )
  )|>
  select(Datum, `Suisa Nummer`, Filmtitel, `Kinoförderer gratis?`, `Link Datum`, Platzkategorie, Verkaufspreis, Anzahl, Umsatz, `Umsatz für Netto3`, Zahlend),
  
  df_Eintritt|>
    filter(`Kinoförderer gratis?`)|>
    mutate(`Umsatz für Netto3` = Umsatz)|>
    select(Datum, `Suisa Nummer`, Filmtitel, `Kinoförderer gratis?`, `Link Datum`, Platzkategorie, Verkaufspreis, Anzahl, Umsatz, `Umsatz für Netto3`, Zahlend)
)



#########################################################################################################
# Verleiherrechnung

df_Abrechnung <- df_Abrechnung|>
  left_join(df_Eintritt|>
              group_by(Datum, `Suisa Nummer`)|>
              reframe(Umsatz = sum(Umsatz),
                      `Umsatz für Netto3` = sum(`Umsatz für Netto3`)),
            by = join_by(Datum, `Suisa Nummer`)
            )


#########################################################################################################
# Gemeinsame Abrechnung 

df_Abrechnung <- df_Abrechnung |>
  left_join(
    df_Abrechnung |>
      group_by(`Suisa Nummer`) |>
      filter(!is.na(`Link Datum`)) |>
      reframe(
        Verleiherrechnungsbetrag_ = sum(Verleiherrechnungsbetrag, na.rm = T)
      ),
    join_by(`Suisa Nummer`)
  )|>
  mutate(
    Verleiherrechnungsbetrag = if_else(
      is.na(Verleiherrechnungsbetrag),
      Verleiherrechnungsbetrag_ ,
      Verleiherrechnungsbetrag
    )
  ) |>
  select(-Verleiherrechnungsbetrag_) |>
  mutate(`keine Verleiherrechnung` = if_else(is.na(Verleiherrechnungsbetrag), T, F),
         `Kinoförderer gratis?` = if_else(`Kinoförderer gratis?` == "nein", F, T))
df_Abrechnung

#########################################################################################################
# Berechung des Verteilerproduktes
df_Abrechnung <- df_Abrechnung|>
  left_join(
    df_Abrechnung|>
      filter(!is.na(`Link Datum`))|>
      group_by(`Suisa Nummer`)|>
      reframe(Umsatz_ = sum(Umsatz),
              `Umsatz für Netto3_` = sum(`Umsatz für Netto3`)),
    by = join_by(`Suisa Nummer`)
  )|>
  mutate(Verteilprodukt = if_else(!is.na(Umsatz_), Umsatz / Umsatz_, 1),
         Verteilprodukt = if_else(`Kinoförderer gratis?`, Umsatz, `Umsatz für Netto3` /  `Umsatz für Netto3_`), # Kinoförderer gratis?
         Verleiherrechnungsbetrag_verteilt = Verleiherrechnungsbetrag * Verteilprodukt)|>
  mutate(Umsatz_ = if_else(is.na(Umsatz_),
                           Umsatz,
                           Umsatz_),
  `Umsatz für Netto3` = if_else(is.na(!`Umsatz für Netto3_`),
                                `Umsatz für Netto3`,
                                `Umsatz für Netto3_`)
  )|>
  mutate(`Umsatz für Netto3` = if_else(is.na(`Umsatz für Netto3_`),
                                        `Umsatz für Netto3`,
                                        `Umsatz für Netto3_`)
  )|>
  arrange(desc(Datum))

# Netto 3, suisa-vorabzug
df_Abrechnung <-  df_Abrechnung|>
  mutate(Verteilprodukt = if_else(!is.na(`Kinoförderer gratis?`), 
                                  Umsatz / Umsatz_, 
                                  `Umsatz für Netto3` / `Umsatz für Netto3_`),
         Verleiherrechnungsbetrag_verteilt = Verleiherrechnungsbetrag * Verteilprodukt,
         `Umsatz für Netto3_` = NULL,
         `SUISA-Vorabzug [CHF]` = `Umsatz für Netto3` * (`SUISA-Vorabzug` / 100),
         Verleiherabgaben = if_else(is.na(`Abzug [%]`),
                                    `Abzug fix [CHF]`- `SUISA-Vorabzug [CHF]`,
                                    (`Umsatz für Netto3` - `SUISA-Vorabzug [CHF]`)* `Abzug [%]` / 100)
         )|>
  arrange(desc(Datum))

########################################################################
# Gewinn/Verlust Eintitt
########################################################################
df_Abrechnung <- df_Abrechnung|>
  mutate(MWST = if_else(`keine Verleiherrechnung`,
                        round5Rappen(Verleiherabgaben - (Verleiherabgaben / (1 + (c_MWST / 100)))),
                        Verleiherabgaben * c_MWST / 100),
         `Gewinn/Verlust [CHF]` = (Umsatz_ - Verleiherabgaben - MWST) * Verteilprodukt # Der Gewinn wird verteilt
         )|>
  left_join(df_show,
            by = join_by(Datum, Filmtitel, `Suisa Nummer`))|>
  mutate(`Sonstige Kosten [CHF]` = Verleiherrechnungsbetrag - MWST)
  

names(df_Abrechnung)

df_Abrechnung|>
  select(Datum ,Filmtitel,
         `Suisa Nummer`, "Minimal Abzug" ,"Abzug [%]","Abzug fix [CHF]", Umsatz, `Umsatz für Netto3`,`SUISA-Vorabzug [CHF]`, Verleiherabgaben )

df_GV_Eintritt <- df_Abrechnung

#############################################################
# error handling
# keine Rechnung vorhanden
df_keine_Rechnnung <- df_Abrechnung|>
  filter(`keine Verleiherrechnung`)
df_keine_Rechnnung

# error handling, keine Verleiherrechnung
if(nrow(df_keine_Rechnnung)>0) {
  warning(paste0("\nAchtung für die diesen Film \"", df_keine_Rechnnung$Filmtitel,"\" am ",
                 day(df_keine_Rechnnung$Datum),".",month(df_keine_Rechnnung$Datum),".", lubridate::year(df_keine_Rechnnung$Datum)," gibt es keine Verleiherrechnung.",
                 "\nBitte korrigieren in der Datei:",
                 "\n.../Kinokulb/input/Einnahmen und Ausgaben.xlsx\n"
  )
  )  
}

#############################################################
# error handling
# Verleiherrechnungbetrag ist kleiner als minimaler Abzug.
df_temp <- df_Abrechnung|>
  mutate(`Minimal Abzug unterschritten` = `Minimal Abzug`> Verleiherrechnungsbetrag,
         `Minimal Abzug unterschritten` = if_else(is.na(`Minimal Abzug unterschritten`), F, `Minimal Abzug unterschritten`)
         )|>
  select(Datum, Filmtitel, `Minimal Abzug unterschritten`)|>
  filter(`Minimal Abzug unterschritten`)

# error handling, keine Verleiherrechnung
if(nrow(df_temp)>0) {
  warning(paste0("\nAchtung für die diesen Film \"", df_temp$Filmtitel,"\" am ",
                 day(df_temp$Datum),".",month(df_temp$Datum),".", lubridate::year(df_temp$Datum)," gibt es keine Verleiherrechnung.",
                 "\nBitte korrigieren in der Datei:",
                 "\n.../Kinokulb/input/Einnahmen und Ausgaben.xlsx\n"
                 )
          )  
}



########################################################################
# error handling
# Es darf nur einen Eintrag pro Film geben in der Abrechnung
df_temp <- df_Abrechnung|>
  group_by(Datum)|>
  reframe(n = n())|>
  left_join(df_Abrechnung|>
              select(Datum, Filmtitel, `Suisa Nummer`),
            by = join_by(Datum))|>
  filter(n > 1)

if(nrow(df_temp) > 1) {
  print(df_temp)
  stop("In der Datei .../input/Verleiherabgaben.xlsx gibt es mehrere Filme am selben Datum")}


########################################################################
# Gewinn Kiosk
########################################################################

ii <- 3
l_GV_Kiosk <- list()
for (ii in 1:length(c_Date)) {
  df_temp <- df_show|>filter(Datum == c_Date[ii])
  
  l_GV_Kiosk[[ii]] <- df_Kiosk|>
    filter(Datum %in% c(c_Date[ii]))|>
    reframe(Kassiert = sum(Kassiert, na.rm = T),
            Gewinn = sum(Gewinn, na.rm = T))|>
    mutate(Datum = c_Date[ii],
           `Suisa Nummer` = c_suisa_nr[ii]
    )
  l_GV_Kiosk[[ii]]
}
l_GV_Kiosk

df_GV_Kiosk <- l_GV_Kiosk|>
  bind_rows()|>
  left_join(df_show, by = join_by(Datum, `Suisa Nummer`))|>
  select( Datum,Anfang, `Suisa Nummer`, Filmtitel, Kassiert, Gewinn)

df_GV_Kiosk


########################################################################
# Gewinn Filmvorführung
########################################################################

ii <- 12
c_Date[ii]

l_GV_Vorfuehrung <- list()
l_verteilprodukt <- list()
for (ii in 1:length(c_Date)) {
  l_GV_Vorfuehrung[[ii]] <- df_Abrechnung|>
    filter(Datum == c_Date[ii])
  l_verteilprodukt[[ii]] <- df_Abrechnung|>
    filter(Datum == c_Date[ii])|>
    select(Verteilprodukt)|>
    pull()
}

df_GV_Vorfuehrung <- l_GV_Vorfuehrung |>
  bind_rows() 

df_GV_Vorfuehrung
########################################################################
# write to Excel
########################################################################


df_s_Eintritt <- df_Eintritt|>
  group_by(Datum, Filmtitel, `Suisa Nummer`)|>
  reframe(Besucher = sum(Anzahl))
df_s_Eintritt

list(Shows = df_show,
     Eintritte = df_Eintritt,
     `Abrechnung Werbung` = df_s_Eintritt,
     # `Gewinn Verlust Eintritt` = df_GV_Eintritt
     Kiosk = df_Kiosk,
     `Gewinn Kiosk` = df_GV_Kiosk,
     `Gewinn Verlust Vorführung` = df_GV_Vorfuehrung,
     Verleiherabgaben  = df_verleiherabgaben,
     Einkaufspreise = df_Einkaufspreise,
     Spezialpreisekiosk = Spezialpreisekiosk,
     Ticketpreise = df_Kinopreise,
     Ausgaben = Einnahmen_und_Ausgaben[["Ausgaben"]],
     Einnahmen = Einnahmen_und_Ausgaben[["Einnahmen"]]
)|>
  write.xlsx(file="output/Data/Auswertung.xlsx", asTable = TRUE)

# remove(l_Eintritt,  m, c_raw, l_GV, l_GV_Kiosk, c_Besucher,  df_Eventausgaben,
#        c_suisaabzug, c_Gratis, c_Umsatz, l_GV_Vorfuehrung,ii, c_Eventausgaben,df_P_kat_verechnen, c_lenght, c_Brutto,
#        convert_data_Film_txt, c_file, c_Verleiherrechnung, c_sheets, c_Kinofoerder_gratis, c_MWST_Abzug, c_Netto3, 
#        c_Verleiher_garantie, c_Verleiherabzug,n_Film,n_kiosk,
#        c_verleiherabzug_prozent)


########################################################################
# user interaction
########################################################################
writeLines("Berechnungen erfolgt")

