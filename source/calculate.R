
source("source/read and convert.R")

#########################################################################################################
# Je nach Verleiher müssen die Kinoförderer als Umsatz abgerechnet werden. 
# Für die Berechnung von "Netto 3" müssen die Kinoförder als Umsatz verrechnet werden.
# Netto 3 = Umsatz minus SUISA-Vorabzug.
#########################################################################################################
df_Eintritt <- bind_rows(
  df_Eintritt|>
    filter(!`Kinoförderer gratis?`)|>
    mutate(
      `Umsatz für Netto3 [CHF]` = if_else(Platzkategorie %in% df_P_kat_verechnen$Kinoförderer,
                                    Anzahl * df_P_kat_verechnen$Verkaufspreis,
                                    Umsatz
                                    ),
      `Verkaufspreis Abgerechnet [CHF]` = `Umsatz für Netto3 [CHF]` / Anzahl
    ),
  df_Eintritt|>
    filter(`Kinoförderer gratis?`)|>
    mutate(`Umsatz für Netto3 [CHF]` = Umsatz)
)|>
  arrange(desc(Datum))

df_Eintritt|>
  filter(Datum == as.Date("2024-10-12"))

#########################################################################################################
# Abrechnungsperiode erstellen
#########################################################################################################
l_keineRechnung <- list()
l_abrechnung <- list()
ii<-19
for (ii in 1:length(c_Date)) {
  l_abrechnung[[ii]] <- list(Abrechnung = df_Abrechnung|>
                               filter(Datum %in% c(c_Date[ii], df_Abrechnung$`Link Datum`[ii]))|>
                               select(Datum, `Link Datum`, Anfang, Ende, Filmtitel, `Suisa Nummer`, Verleiher,`Verleiherrechnungsbetrag [CHF]`, 
                                      `SUISA-Vorabzug [%]`, `Link Datum`, `Minimal Abzug [CHF]`, `Abzug [%]`, `Abzug fix [CHF]`, `Kinoförderer gratis?`),
                             Tickets = df_Eintritt|>
                               filter(Datum %in% c(c_Date[ii], df_Abrechnung$`Link Datum`[ii]))|>
                               select(Datum, Filmtitel, `Suisa Nummer`, Platzkategorie, Verkaufspreis, Anzahl, Umsatz, `Verkaufspreis Abgerechnet [CHF]`,`Umsatz für Netto3 [CHF]`)
                             )
  
  ########################################################################
  # error handling
  # Verleiherrechnung vorhanden?
  ########################################################################
  if(l_abrechnung[[ii]]$Abrechnung$`Verleiherrechnungsbetrag [CHF]`|>sum(na.rm = T) > 0) {
    # Gemeinsame Abrechnung (Datum und link Datum): Verleiherrechnung wir auch auf dem Linkdatum eingetragen
    c_Verleiherrechnungsbetrag <- l_abrechnung[[ii]]$Abrechnung$`Verleiherrechnungsbetrag [CHF]`[!is.na(l_abrechnung[[ii]]$Abrechnung$`Verleiherrechnungsbetrag [CHF]`)]
    l_abrechnung[[ii]]$Abrechnung$`Verleiherrechnungsbetrag [CHF]` <- rep(c_Verleiherrechnungsbetrag, length(l_abrechnung[[ii]]$Abrechnung$`Verleiherrechnungsbetrag [CHF]`))
  }else{
    # Error handling: Keine Verleiherrechnung vorhanden
    warning(paste0("\nAchtung für den Film \"", l_abrechnung[[ii]]$Abrechnung$Filmtitel,"\" am ",
                   day(l_abrechnung[[ii]]$Abrechnung$Datum),".",month(l_abrechnung[[ii]]$Abrechnung$Datum),".", lubridate::year(l_abrechnung[[ii]]$Abrechnung$Datum)," gibt es keine Verleiherrechnung.",
                   "\nBitte korrigieren in der Datei:",
                   "\n.../Kinokulb/input/Einnahmen und Ausgaben.xlsx\n")
    )
    # Rechnungen vorhanden (wird im Bericht verwendet)
    l_keineRechnung[[ii]] <- tibble(Datum = l_abrechnung[[ii]]$Abrechnung$Datum ,
                                    Filmtitel = l_abrechnung[[ii]]$Abrechnung$Filmtitel)
    
  }
  ########################################################################
  # Berechnung Umsatz für Netto3 (für gemeinsame Abrechnung verwendet)
  ########################################################################
  l_abrechnung[[ii]]$Abrechnung <- 
    bind_cols(
      l_abrechnung[[ii]]$Abrechnung,
      l_abrechnung[[ii]]$Tickets|>
        group_by(Datum)|>
        reframe(Umsatz = sum(Umsatz),
                `Umsatz für Netto3 [CHF]` = sum(`Umsatz für Netto3 [CHF]`))|>
        select(-Datum)
    )
}
names(l_abrechnung) <- c_Date

df_keine_Rechnnung <- l_keineRechnung|>
  bind_rows()
df_keine_Rechnnung

#########################################################################################################
# Einnahmen und Abgaben von mehreren Events verhältnismässig nach Umsatzzahlen 
# auf die gelinkten Filme aufteilen (Link im Excel file: .../Kinoklub/Input/Verleiherabgaben.xlsx ) 
#########################################################################################################
l_abrechnung[["2024-10-12"]]
l_abrechnung[["2024-10-12"]]$Abrechnung|>
  as.data.frame()|>
  print()

c_Date[17:18]
ii <- 18

for (ii in 1:length(c_Date)) {
  ############
  # umsatz- und Netto3 Umsatz-Berechnung
  ############
  l_abrechnung[[ii]]$Abrechnung <- 
    l_abrechnung[[ii]]$Abrechnung|>
    mutate(
      Verteilprodukt =  Umsatz / sum(Umsatz),
      `SUISA-Vorabzug [CHF]` = if_else(`Kinoförderer gratis?`, # Der Suisa-Vorabzug muss anders berechnet werden wenn die Kinoförderer verrechnet werden 
                                       sum(Umsatz) * (`SUISA-Vorabzug [%]` /100) * Verteilprodukt,
                                       sum(`Umsatz für Netto3 [CHF]`) * (`SUISA-Vorabzug [%]` / 100) *  Verteilprodukt),
      `Netto3 [CHF]` = if_else(`Kinoförderer gratis?`, # Der Suisa-Vorabzug muss anders berechnet werden wenn die Kinoförderer verrechnet werden müssen
                       (sum(Umsatz) - sum(`SUISA-Vorabzug [CHF]`)) * Verteilprodukt,
                       (sum(`Umsatz für Netto3 [CHF]`) - sum(`SUISA-Vorabzug [CHF]`)) * Verteilprodukt
      )
    )
  l_abrechnung[[ii]]$Abrechnung  
  ############
  # Je nach dem ob ein fixer betrag oder Prozentualeabgaben mit dem Verleiher vereinbart wurden muss anders gerechnet werden. 
  ############
  if((!is.na(l_abrechnung[[ii]]$Abrechnung$`Abzug fix [CHF]`[1]))>0){ 
    # fixer Betrag inklusive Mehrwertsteuer mit dem Verleiher vereinbart! 
    l_abrechnung[[ii]]$Abrechnung <- 
      l_abrechnung[[ii]]$Abrechnung|>
      mutate(
        `Verleiherabzug [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                                         `Abzug fix [CHF]`[1] * Verteilprodukt,     # keine Verleiherrechnung vorhanden
                                         `Verleiherrechnungsbetrag [CHF]` * Verteilprodukt  # Verleiherrechnung ist vorhanden
                                         ),
        `MWST [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                               sum(`Verleiherabzug [CHF]`) * (c_MWST / 100) * Verteilprodukt,
                               (`Verleiherrechnungsbetrag [CHF]`[1] - (`Verleiherrechnungsbetrag [CHF]`[1] / (1+(c_MWST/100)))) * Verteilprodukt
        )
      )
  }else{ 
    # Prozentualerabzug mit dem Verleiher vereinbart! 
    l_abrechnung[[ii]]$Abrechnung <- 
      l_abrechnung[[ii]]$Abrechnung|>
      mutate(

        `Verleiherabzug [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                                         sum(`Netto3 [CHF]`) * (`Abzug [%]`[1] / 100) * Verteilprodukt, # keine Verleiherrechnung vorhanden
                                         `Verleiherrechnungsbetrag [CHF]` * Verteilprodukt              # Verleiherrechnung ist vorhanden
                                         ), 
        `Verleiherabzug [CHF]` = if_else(`Verleiherabzug [CHF]` > (`Minimal Abzug [CHF]`[1] * Verteilprodukt),
                                         sum(`Verleiherabzug [CHF]`) * Verteilprodukt,
                                         `Minimal Abzug [CHF]`[1] * Verteilprodukt
        ),
        `MWST [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                               sum(`Verleiherabzug [CHF]`) * (c_MWST / 100) * Verteilprodukt,
                               (`Verleiherrechnungsbetrag [CHF]`[1] - (`Verleiherrechnungsbetrag [CHF]`[1] / (1+(c_MWST/100)))) * Verteilprodukt
        )
      )
  }
  
  l_abrechnung[["2024-10-12"]]$Abrechnung|>
    as.data.frame()|>
    print()
  
  ############
  # Gewinn/Verlust Tickets
  ############
  l_abrechnung[[ii]]$Abrechnung <- 
    l_abrechnung[[ii]]$Abrechnung|>
    mutate(`Gewinn/Verlust Tickets [CHF]` = if_else(is.na(`Verleiherrechnungsbetrag [CHF]`),
                                                    Umsatz - ((`SUISA-Vorabzug [CHF]` + `Verleiherabzug [CHF]` + `MWST [CHF]`) * Verteilprodukt) ,
                                                    Umsatz - ((`SUISA-Vorabzug [CHF]` + `Verleiherrechnungsbetrag [CHF]`) * Verteilprodukt)
                                                    ) 
           )

  l_abrechnung[["2024-10-12"]]$Abrechnung|>
    as.data.frame()|>
    print()
  
  
  ########################################################################
  # Extract Verteilprodukt
  ########################################################################
  df_Verteilprodukt <- l_abrechnung[[ii]]$Abrechnung|>
    select(Datum, Verteilprodukt)
  df_Verteilprodukt

  ########################################################################
  # Eventeinnahmen (Jede Einnahme wird verteilt bei gemeinsamer Abrechnung)
  ########################################################################
  l_abrechnung[[ii]]$Eventeinnahmen <-
    Einnahmen_und_Ausgaben$Einnahmen|>
    filter(Kategorie == "Event",
           Datum == c_Date[ii])|>
    mutate(Betrag = df_Verteilprodukt|>
             filter(Datum %in% c(df_Verteilprodukt$Datum ,c_Date[ii]))|>
             select(Verteilprodukt)|>
             pull() * Betrag
           )
  l_abrechnung[[ii]]$Eventeinnahmen
  
  ########################################################################
  # Eventausgaben (Jede Ausgabe wird verteilt bei gemeinsamer Abrechnung)
  ########################################################################
  l_abrechnung[[ii]]$Eventausgaben <-
    Einnahmen_und_Ausgaben$Ausgaben |>
    filter(Kategorie == "Event",
           Spieldatum %in% c(df_Verteilprodukt$Datum ,c_Date[ii]))|>
    mutate(Betrag = df_Verteilprodukt|>
             filter(Datum == c_Date[ii])|>
             select(Verteilprodukt)|>
             pull() * Betrag
           )|>
    mutate(Datum = NULL)|>
    rename(Datum = Spieldatum)
  l_abrechnung[[ii]]$Eventausgaben
  
  ########################################################################
  # Gewinn Kiosk (wird nie verteilt, da der Verkauf pro Datum erfolgt)
  ########################################################################
  l_abrechnung[[ii]]$Kiosk <- 
    df_Kiosk|>
    filter(Datum == c_Date[ii])|>
    reframe(Kassiert = sum(Kassiert, na.rm = T),
            Gewinn = sum(Gewinn, na.rm = T))|>
    mutate(Datum = c_Date[ii],
           `Suisa Nummer` = c_suisa_nr[ii]
    )|>
    left_join(df_show|>
                select(Datum, Filmtitel),
              by = join_by(Datum)
              )|>
    select(Datum, `Suisa Nummer`, Filmtitel, Kassiert, Gewinn)
  l_abrechnung[[ii]]$Kiosk
  
  ########################################################################
  # Manko und Überschuss Kiosk 
  ########################################################################
  
  l_abrechnung[[ii]]$`Manko oder Überschuss [CHF]` <- df_manko_uerberschuss|>
    filter(Datum == c_Date[ii])

  ########################################################################
  # Verteilen für gemeinsame Abrechnung
  ########################################################################
  l_abrechnung[[ii]]$Abrechnung <- l_abrechnung[[ii]]$Abrechnung|>
    mutate(`SUISA-Vorabzug [CHF]` = sum(`SUISA-Vorabzug [CHF]`) *  Verteilprodukt,
           `Verleiherabzug [CHF]` = sum(`Verleiherabzug [CHF]`) *  Verteilprodukt,
           `MWST [CHF]` = sum(`MWST [CHF]`) * Verteilprodukt,
           `Gewinn/Verlust Tickets [CHF]` = sum(`Gewinn/Verlust Tickets [CHF]`) *  Verteilprodukt ,
           `Gewinn/Verlust Kiosk [CHF]` = l_abrechnung[[ii]]$Kiosk$Gewinn , # Kiosk wird nicht verteilt
           `Überschuss / Manko Kiosk [CHF]` = l_abrechnung[[ii]]$`Manko oder Überschuss [CHF]`$`Überschuss / Manko`, # Manko, Überschuss wird nicht verteilt
           `Eventeinnahmen [CHF]` = sum(l_abrechnung[[ii]]$Eventeinnahmen$Betrag), # Eventeinnahmen wurden bereits mit Verteilprodukt berechnet
           `Eventausgaben [CHF]` = sum(l_abrechnung[[ii]]$Eventausgaben$Betrag), # Eventausgaben wurden bereits mit Verteilprodukt berechnet
           `Gewinn/Verlust Filmvorführungen [CHF]` = (`Gewinn/Verlust Tickets [CHF]` + `Gewinn/Verlust Kiosk [CHF]`+ `Überschuss / Manko Kiosk [CHF]` + `Eventeinnahmen [CHF]` - `Eventausgaben [CHF]`)
           )
  
  l_abrechnung[[ii]]$Abrechnung|>
    select(7,15:ncol(l_abrechnung[[ii]]$Abrechnung))  
  
  ########################################################################
  # Nur Abrechnung für aktuelles Datum behalten
  ########################################################################
  l_abrechnung[[ii]]$Abrechnung <- l_abrechnung[[ii]]$Abrechnung|>
    filter(Datum == c_Date[ii])
}


l_abrechnung[["2024-10-12"]]$Abrechnung|>
  as.data.frame()|>
  print()

l_abrechnung[["2024-10-11"]]$Abrechnung|>
  as.data.frame()|>
  print()

l_abrechnung[["2024-10-11"]]
l_abrechnung[["2024-10-12"]]

##############################################################################
# Abrechnung Filmvorführung erstellen (für Berichte verwendet)
# Runden aller [CHF]  Beträge
##############################################################################
df_Abrechnung <- bind_cols(
  l_abrechnung|>
    lapply(function(x){
      x$Abrechnung|>
        select(!ends_with("[CHF]"))
    })|>
    bind_rows(),
  l_abrechnung|>
    lapply(function(x){
      c_temp <- x$Abrechnung|>
        select(ends_with("[CHF]"))|>
        as_vector()|>
        round5Rappen()
      as.data.frame(c_temp)|>
        t()|>
        as_tibble()
    })|>
    bind_rows()
)|>
  rename(`Umsatz [CHF]` = Umsatz)

##############################################################################
# Abrechnung Tickets erstellen (für Berichte verwendet)
##############################################################################
df_Abrechnung_tickes <- l_abrechnung|>
  lapply(function(x){
    x$Tickets
  })|>
  bind_rows()|>
  rename(`Verkaufspreis [CHF]` = Verkaufspreis,
         `Umsatz [CHF]` = Umsatz,
         )|>
  left_join(df_show|>
              select(`Suisa Nummer`, Datum, Anfang, Ende),
            by = join_by(Datum, `Suisa Nummer`))

df_Abrechnung_tickes

##############################################################################
# Abrechnung Kiosk erstellen  (für Berichte verwendet) 
##############################################################################
df_Abrechnung_kiosk <- l_abrechnung|>
  lapply(function(x){
    x$Kiosk
  })|>
  bind_rows()|>
  rename(`Kassiert [CHF]` = Kassiert,
         `Gewinn [CHF]` = Gewinn
         )|>
  left_join(df_show|>
              select(`Suisa Nummer`, Datum, Anfang, Ende),
            by = join_by(Datum, `Suisa Nummer`)
            )
df_Abrechnung_kiosk

##############################################################################
# Abrechnung Events erstellen (für Berichte verwendet)
##############################################################################
df_Abrechnung_Eventeinnahmen <- l_abrechnung|>
  lapply(function(x){
    x$Eventeinnahmen
  })|>
  bind_rows()|>
  select(Datum, Bezeichnung, Betrag)|>
  rename(`Betrag [CHF]` = Betrag)
df_Abrechnung_Eventeinnahmen

df_Abrechnung_Eventausgaben <- l_abrechnung|>
  lapply(function(x){
    x$Eventausgaben
  })|>
  bind_rows()|>
  select(Datum, Bezeichnung, Betrag)|>
  rename(`Betrag [CHF]` = Betrag)
df_Abrechnung_Eventausgaben

########################################################################
# summary Eintritt (für Berichte verwendet)
########################################################################
df_Besucherzahlen <- df_Eintritt|>
  group_by(Datum, Filmtitel, `Suisa Nummer`)|>
  reframe(Besucher = sum(Anzahl))
df_Besucherzahlen

########################################################################
# write to Excel
########################################################################
list(`Werbung` = df_Besucherzahlen,
     `Tickets` = df_Abrechnung_tickes,
     `Kiosk` = df_Abrechnung_kiosk,
     `Eventeinnahmen` = df_Abrechnung_Eventeinnahmen,
     `Eventausgaben` = df_Abrechnung_Eventausgaben,
     `Überschuss Manko` = df_manko_uerberschuss,
     `Filmvorführung` = df_Abrechnung
       )|>
  write.xlsx(file="output/Data/Auswertung.xlsx", asTable = TRUE, overwrite = TRUE)



########################################################################
# user interaction
########################################################################
writeLines("Berechnungen erfolgt")

ii <- 18