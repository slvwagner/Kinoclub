
source("source/read and convert.R")


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
                               filter(Datum %in% c(c_Date[ii], df_Abrechnung$`Link Datum`[ii]))|>
                               select(Datum, `Link Datum`, Anfang, Ende, Filmtitel, `Suisa Nummer`, Verleiherrechnungsbetrag, 
                                      `SUISA-Vorabzug`, `Link Datum`, `Minimal Abzug`, `Abzug [%]`, `Abzug fix [CHF]`, `Kinoförderer gratis?`),
                             Tickets = df_Eintritt|>
                               filter(Datum %in% c(c_Date[ii], df_Abrechnung$`Link Datum`[ii]))|>
                               select(Datum, Filmtitel, `Suisa Nummer`, Platzkategorie, Verkaufspreis, Anzahl, Umsatz, `Umsatz für Netto3`)
                             )
  
  ########################################################################
  # error handling
  # Verleiherrechnung vorhanden?
  ########################################################################
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
  ########################################################################
  # Berechnung Umsatz für Netto3 (für gemeinsame Abrechnung verwendet)
  ########################################################################
  l_abrechnung[[ii]]$Abrechnung <- 
    bind_cols(
      l_abrechnung[[ii]]$Abrechnung,
      l_abrechnung[[ii]]$Tickets|>
        group_by(Datum)|>
        reframe(Umsatz = sum(Umsatz),
                `Umsatz für Netto3` = sum(`Umsatz für Netto3`))|>
        select(-Datum)
    )
}
names(l_abrechnung) <- c_Date

#########################################################################################################
# Einnahmen und Abgaben von mehreren Events verhältnismässig nach Umsatzzahlen 
# auf die gelinkten Filme aufteilen (Link im Excel file: .../Kinoklub/Input/Verleiherabgaben.xlsx ) 
#########################################################################################################
ii <- 28
c_Date[ii]
for(ii in 1:length(c_Date)) if(c_Date[ii] == as.Date("2024-01-25")) break
ii
c_Date[ii]

c_Date

l_abrechnung[[ii]]

for (ii in 1:length(c_Date)) {
  
  l_abrechnung[[ii]]$Abrechnung <- l_abrechnung[[ii]]$Abrechnung|>
    mutate(Verteilprodukt = if_else(`Kinoförderer gratis?`, # Das Verteilprodukt muss anders berechnet werden wenn die Kinoförderer verrechnet werden müssen
                                    Umsatz / sum(Umsatz),
                                    `Umsatz für Netto3` /  sum(`Umsatz für Netto3`)
                                    ),
           `SUISA-Vorabzug [CHF]` = if_else(`Kinoförderer gratis?`, # Der Suisa-Vorabzug muss anders berechnet werden wenn die Kinoförderer verrechnet werden müssen
                                            sum(Umsatz) * `SUISA-Vorabzug` /100,
                                            sum(`Umsatz für Netto3`) * `SUISA-Vorabzug` / 100
                                            ),
           Netto3 = if_else(`Kinoförderer gratis?`, # Der Suisa-Vorabzug muss anders berechnet werden wenn die Kinoförderer verrechnet werden müssen
                            sum(Umsatz) - `SUISA-Vorabzug [CHF]`,
                            sum(`Umsatz für Netto3`) - `SUISA-Vorabzug [CHF]`
                            ),
           `Verleiherabzug [CHF]` = if_else(is.na(`Abzug fix [CHF]`), # ist ein prozentualer oder fixer betrag mit dem Verleiher vereinbart? 
                                            Netto3 * `Abzug [%]` / 100,
                                            `Abzug fix [CHF]`
                                            ),
           `MWST [CHF]` = if_else(is.na(Verleiherrechnungsbetrag),
                                  `Verleiherabzug [CHF]` * (c_MWST / 100),
                                  Verleiherrechnungsbetrag - (Verleiherrechnungsbetrag / (1+(c_MWST/100)))
                                  ),
           `Sonstige Kosten [CHF]` = Verleiherrechnungsbetrag - `MWST [CHF]` - `Verleiherabzug [CHF]`,
           `Gewinn/Verlust Tickets [CHF]` = sum(Umsatz)- `SUISA-Vorabzug [CHF]` - `Verleiherabzug [CHF]` - `MWST [CHF]`
           )
  l_abrechnung[[ii]]$Abrechnung|>
    select(7,11:ncol(l_abrechnung[[ii]]$Abrechnung))

  ########################################################################
  # Extract Verteilprodukt
  ########################################################################
  df_Verteilprodukt <- l_abrechnung[[ii]]$Abrechnung|>
    select(Datum, Verteilprodukt)
  df_Verteilprodukt

  ########################################################################
  # Eventeinnahmen (werden verteilt bei gemeinsamer Abrechnung)
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
  # Eventausgaben (werden verteilt bei gemeinsamer Abrechnung)
  ########################################################################
  l_abrechnung[[ii]]$Eventausgaben <-
    Einnahmen_und_Ausgaben$Ausgaben |>
    filter(Kategorie == "Event",
           Datum %in% c(df_Verteilprodukt$Datum ,c_Date[ii]))|>
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
  l_abrechnung[[ii]]

  l_abrechnung[[ii]]$Abrechnung <- l_abrechnung[[ii]]$Abrechnung|>
    mutate(`Umsatz für Netto3` = NULL,
           `SUISA-Vorabzug [CHF]` = `SUISA-Vorabzug [CHF]` * Verteilprodukt,
           `Verleiherabzug [CHF]` = `Verleiherabzug [CHF]` * Verteilprodukt,
           `MWST [CHF]` = `MWST [CHF]` * Verteilprodukt,
           `Sonstige Kosten [CHF]` = `Sonstige Kosten [CHF]` * Verteilprodukt,
           `Gewinn/Verlust Tickets [CHF]` = `Gewinn/Verlust Tickets [CHF]` * Verteilprodukt,
           `Gewinn/Verlust Kiosk [CHF]` = sum(l_abrechnung[[ii]]$Kiosk$Gewinn) , # Kiosk wird nicht verteilt
           `Überschuss / Manko Kiosk [CHF]` = l_abrechnung[[ii]]$`Manko oder Überschuss [CHF]`$`Überschuss / Manko`, # Manko, Überschuss wird nicht verteilt
           `Eventeinnahmen [CHF]` = sum(l_abrechnung[[ii]]$Eventeinnahmen$Betrag), # Eventeinnahmen wurden bereits mit Verteilprodukt berechnet
           `Eventausgaben [CHF]` = sum(l_abrechnung[[ii]]$Eventausgaben$Betrag), # Eventausgaben wurden bereits mit Verteilprodukt berechnet
           `Gewinn/Verlust Filmvorführungen [CHF]` = `Gewinn/Verlust Tickets [CHF]` + `Gewinn/Verlust Kiosk [CHF]`+ `Überschuss / Manko Kiosk [CHF]` + `Eventeinnahmen [CHF]` - `Eventausgaben [CHF]`
           )
  
  l_abrechnung[[ii]]$Abrechnung|>
    select(7,15:ncol(l_abrechnung[[ii]]$Abrechnung))  
  
  ########################################################################
  # Nur Abrechnung für aktuelles Datum behalten
  ########################################################################
  l_abrechnung[[ii]]$Abrechnung <- l_abrechnung[[ii]]$Abrechnung|>
    filter(Datum == c_Date[ii])
}

l_abrechnung[[1]]
l_abrechnung[[12]]
l_abrechnung[[29]]

# Runden aller [CHF]  Beträge
df_Abrechnung <- bind_cols(
  l_abrechnung|>
    lapply(function(x){
      x$Abrechnung|>
        select(1:14)
    })|>
    bind_rows(),
  l_abrechnung|>
    lapply(function(x){
      c_temp <- x$Abrechnung|>
        select((ncol(x$Abrechnung)-10):ncol(x$Abrechnung))|>
        as_vector()|>
        round5Rappen()
      as.data.frame(c_temp)|>
        t()|>
        as_tibble()
    })|>
    bind_rows()
)


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
     `Gewinn Verlust Vorführung` = df_Abrechnung,
     Verleiherabgaben  = df_verleiherabgaben,
     Einkaufspreise = df_Einkaufspreise,
     Spezialpreisekiosk = Spezialpreisekiosk,
     Ticketpreise = df_Kinopreise,
     Ausgaben = Einnahmen_und_Ausgaben[["Ausgaben"]],
     Einnahmen = Einnahmen_und_Ausgaben[["Einnahmen"]]
     )|>
  write.xlsx(file="output/Data/Auswertung.xlsx", asTable = TRUE)



########################################################################
# user interaction
########################################################################
remove(df_temp, df_test, df_Verteilprodukt, c_Verleiherrechnungsbetrag, file_datum, p, 
       c_test, c_sheets, c_suisa_nr, c_path, c_names, c_raw, c_lenght, c_files, c_fileDate, c_file)

writeLines("Berechnungen erfolgt")

