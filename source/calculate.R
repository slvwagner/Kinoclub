
source("source/read and convert.R")

#########################################################################################################
# Kinotickes
#########################################################################################################
df_Kinopreise <- df_Eintritt|>
  distinct(Platzkategorie, .keep_all = T)|>
  select(Platzkategorie, Verkaufspreis)
df_Kinopreise

#########################################################################################################
# Ticketabrechnung vorbereiten
#########################################################################################################
df_Abrechnung <- df_Eintritt|>
  distinct(Datum, `Suisa Nummer`, .keep_all = TRUE)|>
  select(-(4:8))|>
  left_join(df_show|>
              select(Datum, `Suisa Nummer`, Anfang, Ende),
            by = join_by(Datum, `Suisa Nummer`))|>
  left_join( # Verleiherrechnungen 
    Einnahmen_und_Ausgaben[["Ausgaben"]]|>
      filter(Kategorie == Einnahmen_und_Ausgaben[["dropdown"]]$`drop down`[5])|> # suchen nach den Verleiher Einträgen
      select(-Kategorie,-Datum, -Bezeichnung)|>
      select(1:2)|>
      rename(Verleiherrechnungsbetrag = Betrag,
             Datum = Spieldatum),
    by = join_by(Datum)
  )
df_Abrechnung

#########################################################################################################
# Je nach Verleiher müssen die Kinoförderer als Umsatz abgerechnet werden. 
# Für die Berechnung von "Netto 3" müssen die Kinoförder als Umsatz verrechnet werden.
# Netto 3 = Umsatz minus SUISA-Vorabzug.
#########################################################################################################
df_Eintritt <- bind_rows(
  df_Eintritt|>
    filter(!`Kinoförderer gratis?`)|>
    mutate(`Umsatz für Netto3` = if_else(Platzkategorie == df_P_kat_verechnen$Kinoförderer,
                                         Anzahl * df_P_kat_verechnen$Verkaufspreis,
                                         Umsatz
                                         )
    ),
  df_Eintritt|>
    filter(`Kinoförderer gratis?`)|>
    mutate(`Umsatz für Netto3` = Umsatz)
)|>
  arrange(desc(Datum))

#########################################################################################################
# Abrechnungsperiode erstellen
#########################################################################################################
l_abrechnung <- list()
for (ii in 1:length(c_Date)) {
  l_abrechnung[[ii]] <- list(Abrechnung = df_Abrechnung|>
                               filter(Datum %in% c(c_Date[ii], df_Abrechnung$`Link Datum`[ii])),
                             Tickets = df_Eintritt|>
                               filter(Datum %in% c(c_Date[ii], df_Abrechnung$`Link Datum`[ii])),
                             Kiosk = tibble(),
                             Eventeinnahmen = tibble(),
                             Eventausgaben = tibble()
                             )
}
names(l_abrechnung) <- c_Date

#########################################################################################################
# Einnahmen und Abgaben von mehreren Events verhältnismässig nach Umsatzzahlen 
# auf die gelinkten Filme aufteilen (Link im Excel file: .../Kinoklub/Input/Verleiherabgaben.xlsx ) 
#########################################################################################################
ii <- 1

for (ii in 1:length(c_Date)) {
  ########################################################################
  # Tickets
  ########################################################################
  # Verleiherrechnung vorhanden?
  if(l_abrechnung[[ii]]$Abrechnung$Verleiherrechnungsbetrag|>sum(na.rm = T) > 0) {
    # Gemeinsame Abrechnung (Datum und link Datum): Verleiherrechnung wir auch auf dem Linkdatum eingetragen
    c_Verleiherrechnungsbetrag <- l_abrechnung[[ii]]$Abrechnung$Verleiherrechnungsbetrag[!is.na(l_abrechnung[[ii]]$Abrechnung$Verleiherrechnungsbetrag)]
    l_abrechnung[[ii]]$Abrechnung$Verleiherrechnungsbetrag <- rep(c_Verleiherrechnungsbetrag, length(l_abrechnung[[ii]]$Abrechnung$Verleiherrechnungsbetrag))
  }else{
    # Error handling: Keine Verleiherrechnung vorhanden
    warning(paste0("\nAchtung für die diesen Film \"", l_abrechnung[[ii]]$Abrechnung$Filmtitel,"\" am ",
                   day(l_abrechnung[[ii]]$Abrechnung$Datum),".",month(l_abrechnung[[ii]]$Abrechnung$Datum),".", lubridate::year(l_abrechnung[[ii]]$Abrechnung$Datum)," gibt es keine Verleiherrechnung.",
                   "\nBitte korrigieren in der Datei:",
                   "\n.../Kinokulb/input/Einnahmen und Ausgaben.xlsx\n")
            )
  }
  
  # Berechnung Umsatz für Netto3 (für gemeinsame Abrechnung verwendet)
  l_abrechnung[[ii]]$Abrechnung <- 
    bind_cols(
      l_abrechnung[[ii]]$Abrechnung,
      l_abrechnung[[ii]]$Tickets|>
        group_by(Datum)|>
        reframe(Umsatz = sum(Umsatz),
                `Umsatz für Netto3` = sum(`Umsatz für Netto3`))|>
        select(-Datum)
    )  
  
  # Ticketumsatz für Verteilprodukt berechnen
  l_abrechnung[[ii]]$Abrechnung <- 
    bind_cols(l_abrechnung[[ii]]$Abrechnung,
              l_abrechnung[[ii]]$Abrechnung|>
                reframe(`Umsatz total`= sum(Umsatz),
                        `Umsatz für Netto3 total`= sum(`Umsatz für Netto3`))
              )

  c_suisa_vorabzug <- 
    l_abrechnung[[ii]]$Tickets|>
    distinct(`SUISA-Vorabzug`)|>
    pull()
  c_suisa_vorabzug
  
  c_minimal_abzug <- 
    l_abrechnung[[ii]]$Tickets|>
    distinct(`Minimal Abzug`)|>
    pull()
  c_minimal_abzug
  
  c_abzug_prozent <- 
    l_abrechnung[[ii]]$Tickets|>
    distinct(`Abzug [%]`)|>
    pull()
  c_abzug_prozent
  
  c_abzug_fix <- 
    l_abrechnung[[ii]]$Tickets|>
    distinct(`Abzug fix [CHF]`)|>
    pull()
  c_abzug_fix
  
  c_Verleiherrechnungsbetrag <- l_abrechnung[[ii]]$Abrechnung|>
    distinct(Verleiherrechnungsbetrag)|>
    pull()
  
  ########################################################################
  # Berechnen Verteilerproduckt (Verteilprodukt ist vom Netto3 abhängig)
  # Netto3 = Umsatz - suisa vorabzug
  # MWST
  # Verleiherabgaben  
  ########################################################################
  
  if(l_abrechnung[[ii]]$Abrechnung$`Abzug fix [CHF]`|>sum(na.rm = T) > 0){
    l_abrechnung[[ii]]$Abrechnung <-  l_abrechnung[[ii]]$Abrechnung|>
      mutate(Verteilprodukt = `Umsatz für Netto3` / `Umsatz für Netto3 total`,
             Netto3 = `Umsatz für Netto3 total`- (`Umsatz für Netto3 total` * `SUISA-Vorabzug` / 100),
             Verleiherabgaben = if_else(is.na(c_abzug_prozent), # Prozentualer oder fixer Verleiherabzug
                                        c_abzug_fix,
                                        Netto3[1] * c_abzug_prozent / 100 # Prozentualerabzug
             ),
             `MWST [CHF]` = if_else(is.na(Verleiherrechnungsbetrag),
                                    (Verleiherabgaben * c_MWST /100)* Verteilprodukt,
                                    (Verleiherabgaben - Verleiherabgaben / (1 + (c_MWST / 100))) * Verteilprodukt
             ),
             `Sonstige Kosten [CHF]` = (Verleiherrechnungsbetrag - (Verleiherabgaben - `MWST [CHF]`)) * Verteilprodukt,
             `Gewinn/Verlust [CHF]` = (Umsatz - (Verleiherabgaben * Verteilprodukt)) * Verteilprodukt
      )
  }else{
    l_abrechnung[[ii]]$Abrechnung  <- l_abrechnung[[ii]]$Abrechnung|>
      mutate(Verteilprodukt = `Umsatz für Netto3` / `Umsatz für Netto3 total`,
             Netto3 = `Umsatz für Netto3 total`- (`Umsatz für Netto3 total` * `SUISA-Vorabzug` / 100),
             Verleiherabgaben = if_else(is.na(c_abzug_prozent), # Prozentualer oder fixer Verleiherabzug
                                        c_abzug_fix,
                                        Netto3[1] * c_abzug_prozent / 100 # Prozentualerabzug
             ),
             Verleiherabgaben = if_else(Verleiherabgaben > c_minimal_abzug,
                                        Verleiherabgaben * Verteilprodukt,
                                        c_minimal_abzug * Verteilprodukt
             ),
             `MWST [CHF]` = if_else(is.na(Verleiherrechnungsbetrag),
                                    (Verleiherabgaben * c_MWST /100)* Verteilprodukt,
                                    (Verleiherabgaben - Verleiherabgaben / (1 + (c_MWST / 100))) * Verteilprodukt
             ),
             `Sonstige Kosten [CHF]` = (Verleiherrechnungsbetrag - (Verleiherabgaben - `MWST [CHF]`)) * Verteilprodukt,
             `Gewinn/Verlust [CHF]` = (Umsatz - (Verleiherabgaben * Verteilprodukt)) * Verteilprodukt
      )
  }

  df_Verteilprodukt <- l_abrechnung[[ii]]$Abrechnung|>
    select(Datum, Verteilprodukt)
  df_Verteilprodukt

  ########################################################################
  # Eventeinnahmen (werden verteilt bei gemeinsamer Abrechnung)
  ########################################################################
  l_abrechnung[[ii]]$Eventeinnahmen <-
    Einnahmen_und_Ausgaben$Einnahmen|>
    filter(Datum == c_Date[ii])|>
    mutate(Betrag = df_Verteilprodukt|>
             filter(Datum %in% c(df_Verteilprodukt$Datum ,c_Date[ii]))|>
             select(Verteilprodukt)|>
             pull() * Betrag
           )

  ########################################################################
  # Eventausgaben (werden verteilt bei gemeinsamer Abrechnung)
  ########################################################################
  l_abrechnung[[ii]]$Eventausgaben <-
    Einnahmen_und_Ausgaben$Ausgaben |>
    filter(Datum %in% c(df_Verteilprodukt$Datum ,c_Date[ii]))|>
    mutate(Betrag = df_Verteilprodukt|>
             filter(Datum == c_Date[ii])|>
             select(Verteilprodukt)|>
             pull() * Betrag
           )
  
  ########################################################################
  # Gewinn Kiosk (wird nie verteilt, da der Verkauf pro Datum erfolgt)
  ########################################################################
  l_abrechnung[[ii]]$Kiosk <- 
    df_Kiosk|>
    filter(Datum %in% c(c_Date[ii]))|>
    reframe(Kassiert = sum(Kassiert, na.rm = T),
            Gewinn = sum(Gewinn, na.rm = T))|>
    mutate(Datum = c_Date[ii],
           `Suisa Nummer` = c_suisa_nr[ii]
    )|>
    left_join(df_show|>
                select(Datum, Filmtitel),
              by = join_by(Datum)
              )
  
  ########################################################################
  # Gewinn Filmvorführung
  ########################################################################
  l_abrechnung[[ii]]$Abrechnung <- l_abrechnung[[ii]]$Abrechnung|>
    mutate(Datum = l_abrechnung[[ii]]$Abrechnung$Datum,
           `Suisa Nummer` = l_abrechnung[[ii]]$Abrechnung$`Suisa Nummer`,
           Filmtitel = l_abrechnung[[ii]]$Abrechnung$Filmtitel,
           `Gewinn Tickets [CHF]`= l_abrechnung[[ii]]$Abrechnung$`Gewinn/Verlust [CHF]`,
           `Gewinn Kiosk [CHF]` = l_abrechnung[[ii]]$Kiosk$Gewinn,
           Eventeinnahmen = l_abrechnung[[ii]]$Eventeinnahmen|>
             reframe(sum(Betrag))|>
             pull() * df_Verteilprodukt$Verteilprodukt,
           Eventausgaben = l_abrechnung[[ii]]$Eventausgaben|>reframe(sum(Betrag))|>
             pull() * df_Verteilprodukt$Verteilprodukt,
           `Gewinn/Verlust [CHF]` = `Gewinn Tickets [CHF]` + `Gewinn Kiosk [CHF]`+ Eventeinnahmen - Eventausgaben
    )|>
    filter(Datum == c_Date[ii])
}


########################################################################
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
# summary Eintritt
########################################################################
df_s_Eintritt <- df_Eintritt|>
  group_by(Datum, Filmtitel, `Suisa Nummer`)|>
  reframe(Besucher = sum(Anzahl))
df_s_Eintritt

########################################################################
# write to Excel
########################################################################
df_test <- l_abrechnung|>
  lapply(function(x){
    x$Abrechnung
  })|>
  bind_rows()

list(Shows = df_show,
     Eintritte = df_Eintritt,
     `Abrechnung Werbung` = df_s_Eintritt,
     `Gewinn Verlust Eintritt` = 
       l_abrechnung|>
       lapply(function(x){
         x$Abrechnung
       })|>
       bind_rows()|>
       distinct(Datum, `Suisa Nummer`,.keep_all = T),
     `Gewinn Kiosk` = 
       l_abrechnung|>
       lapply(function(x){
         x$Kiosk
       })|>
       bind_rows()|>
       left_join(df_show,
                 by = join_by(Datum, `Suisa Nummer`)
                 )|>
       select(-Saal,-Version,-Alter),
     `Gewinn Verlust Vorführung` = 
       l_abrechnung|>
       lapply(function(x){
         x$Abrechnung
         })|>
       bind_rows(),
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

