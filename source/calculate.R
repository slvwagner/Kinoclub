
source("source/read and convert.R")

########################################################################
# Einnahmen und Abgaben von mehreren Events verhältnismässig nach Besucherzahlen 
# auf die gelinkten Filme aufteilen (Link im Excel file: .../Kinoklub/Input/Verleiherabgaben.xlsx ) 
########################################################################
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
  )|>
  mutate(`Kinoförderer gratis?` = if_else(`Kinoförderer gratis?` == "nein",F,T))
df_Abrechnung

# Verleiherrechnung für gemeinsame Abrechnung 
df_Abrechnung <- left_join(
  df_Abrechnung,
  df_Abrechnung|>
    group_by( `Suisa Nummer`)|>
    filter(!is.na(`Link Datum`))|>
    reframe(Verleiherrechnungsbetrag_ = sum(Verleiherrechnungsbetrag, na.rm = T)),
  join_by(`Suisa Nummer`))|>
  mutate(Verleiherrechnungsbetrag = if_else(is.na(Verleiherrechnungsbetrag),Verleiherrechnungsbetrag_ , Verleiherrechnungsbetrag))|>
  select(-Verleiherrechnungsbetrag_)|>
  mutate(`keine Verleiherrechnung` = if_else(is.na(Verleiherrechnungsbetrag), T, F))
df_Abrechnung

df_Abrechnung <- df_Eintritt|>
  group_by(Datum, `Suisa Nummer`, Filmtitel, `Link Datum`)|>
  reframe(Umsatz = sum(Umsatz))|>
  left_join(df_Abrechnung,
            by = join_by(Datum, `Suisa Nummer`, Filmtitel, `Link Datum`)
            )
df_Abrechnung

# Berechung des Verteilerproduktes
df_Abrechnung <- df_Abrechnung|>
  left_join(
    df_Abrechnung|>
      filter(!is.na(`Link Datum`))|>
      group_by(`Suisa Nummer`)|>
      reframe(Umsatz_ = sum(Umsatz)),
    by = join_by(`Suisa Nummer`)
  )

df_Abrechnung <- df_Abrechnung|>
  mutate(Verteilprodukt = if_else(!is.na(Umsatz_), Umsatz / Umsatz_, 1),
         Verleiherrechnungsbetrag_verteilt = Verleiherrechnungsbetrag * Verteilprodukt)|>
  arrange(desc(Datum))|>
  mutate(`SUISA-Vorabzug [CHF]` = Umsatz_ * (`SUISA-Vorabzug`/100)*Verteilprodukt,
         `SUISA-Vorabzug [CHF]` = if_else(is.na(`SUISA-Vorabzug [CHF]`),
                                          Umsatz * (`SUISA-Vorabzug`/100),
                                          `SUISA-Vorabzug [CHF]`
                                          ),
         `Netto 3` = (Umsatz - `SUISA-Vorabzug [CHF]`)*Verteilprodukt
         )

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
# Gewinn/Verlust Eintitt
########################################################################
df_Kinopreise <- df_Eintritt|>
  distinct(Platzkategorie,.keep_all = T)|>
  select(Platzkategorie, Verkaufspreis)
df_Kinopreise

c_ticket_ermässigt <- df_Kinopreise|>
  select(Platzkategorie, Verkaufspreis)|>
  filter(Platzkategorie == "Ermässigt")|>
  pull()
c_ticket_ermässigt

l_GV <- list()
l_Abgaben <- list()
ii <- 12
for (ii in 1:length(c_Date)) {
  # Einfache Abrechnung
  df_temp <- df_Eintritt |>
    filter(Datum == c_Date[ii])
  df_temp
  
  # Soll gemeinsam mit einem anderen Datum abgerechnet werden?
  c_linkDatum <- distinct(df_temp, `Link Datum`)|>pull()
  c_linkDatum
  if(!is.na(c_linkDatum)){ # gemeinsam abrechnen
    df_temp <- df_Eintritt |>
      filter(Datum %in% c(c_Date[ii], c_linkDatum))|>
      arrange(Datum)
  }
  df_temp
  
  # Kinoförderer dürfen nicht bei jedem Verleiher als gratis abgerechnet werden und müssen anders behandelt werden.
  #  
  c_Kinofoerder_gratis <- df_Abrechnung|>
    filter(Datum == c_Date[ii])|>
    select(`Kinoförderer gratis?`)|>
    pull()
  c_Kinofoerder_gratis
  
  if(!c_Kinofoerder_gratis){ # Kinoföderer müssen abgerechnet werden 
    df_temp <- df_temp |>
      filter(Datum %in% c(c_Date[ii], c_linkDatum),Zahlend) |>
      bind_rows(
        df_temp |>
          filter(Datum %in% c(c_Date[ii], c_linkDatum),
                 Platzkategorie %in% df_P_kat_verechnen$Kinoförderer )|>
          mutate(Abrechnungspreis = df_Kinopreise|>
                   filter(Platzkategorie == "Ermässigt")|> # Kategorieren werden mit Ermässigt ersetzt
                   select(Verkaufspreis)|>pull()
          )
      )|>
      mutate(Umsatz = if_else(is.na(Abrechnungspreis), Umsatz, Abrechnungspreis *  Anzahl))|>
      select(-Abrechnungspreis)|>
      arrange(Datum)
    df_temp
    
    ## Brutto wir mit ticket Umsatz berechnet
    c_Brutto <- df_temp |>
      filter(Datum %in% c(c_Date[ii], c_linkDatum))|>
      reframe(Umsatz = sum(Umsatz)) |>
      pull()
    
  } else { # Kinoföderer sind gratis und müssen nicht durch den Verleiher verrechnet werden
    
    ##Brutto
    c_Brutto <- df_Abrechnung|>
      filter(Datum %in% c(c_Date[ii])) |>
      reframe(Umsatz = sum(Umsatz)) |>
      pull()
  } 
  
  df_temp
  c_Brutto
  
  c_Umsatz <- df_Eintritt |>
    filter(Datum %in% c(c_Date[ii])) |>
    reframe(Umsatz = sum(Umsatz)) |>
    pull()
  c_Umsatz
  
  c_Besucher <- df_Eintritt|>
    filter(Datum %in% c(c_Date[ii]))|>
    reframe(Anzahl = sum(Anzahl))|>
    pull()
  c_Besucher
  
  c_Gratis <- df_Eintritt |>
    filter(Datum %in% c(c_Date[ii]), !Zahlend)|>
    reframe(Anzahl = sum(Anzahl))|>
    pull() 
  c_Gratis  
  
  c_suisaabzug <- df_Abrechnung|>
    filter(Datum == c_Date[ii])|>
    select(`SUISA-Vorabzug`)|>
    pull()
  c_suisaabzug

  # Abgaben
  l_Abgaben[[ii]] <- df_temp|>
    filter(Datum %in% c(c_Date[ii]))
  l_Abgaben[[ii]]
  
  ## Netto 3
  c_Netto3 <- c_Brutto - round5Rappen(c_Brutto * (c_suisaabzug/100))
  c_Netto3
  
  # minimale Abgaben an den Verleiher
  # gemeinsame Abrechnung ?
  if(!is.na(c_linkDatum)){
    c_Verleiher_garantie <- df_Abrechnung|>
      filter(Datum %in% c(c_Date[ii], c_linkDatum))|>
      select(`Minimal Abzug`)|>
      pull()
    c_Verleiher_garantie
    c_Verleiher_garantie <- c_Verleiher_garantie[1]
    
  }else{
    c_Verleiher_garantie <- df_verleiherabgaben |>
      filter(Datum == c_Date[ii])|>
      select(`Minimal Abzug`)|>
      pull()
  }
  c_Verleiher_garantie
  
  # error handling
  if(length(c_Verleiher_garantie) > 1) {
    print(df_verleiherabgaben |>
            filter(Datum == c_Date[ii]))
    stop("In der Datei .../input/Verleiherabgaben.xlsx gibt es mehrere Filme am selben Datum")}
  
  # prozentual Abgabe von Netto 3 an den Verleiher
  c_verleiherabzug_prozent <-
    df_Abrechnung|>
    filter(Datum == c_Date[ii])|>
    select(`Abzug [%]`)|> 
    pull() / 100
  c_verleiherabzug_prozent
  
  ##################################################
  # Abzug fix oder prozentual 
  # Definition in der Datei: .../Kinoklub/Input/Verleiherabgaben.xlsx
  if(is.na(c_verleiherabzug_prozent)) { # Abzug fix
    c_Verleiherabzug <- df_Abrechnung|>
      filter(Datum %in% c(c_Date[ii]))|>
      select(`Abzug fix [CHF]`)|>
      pull()
    
  }else{ # prozentualer Abzug
    # Abgabe an den Verleiher
    c_Verleiherabzug <- c_Netto3 * c_verleiherabzug_prozent
    
    ### Wenn die Abgabe von Netto 3 kleiner als der definierte minimal Abzug ist wird dieser eingesetzt
    if (c_Verleiherabzug < c_Verleiher_garantie) {
      c_Verleiherabzug <- c_Verleiher_garantie
    }
  }
  
  
  ##################################################
  # Berechnung der Abgaben
  # Verleiherrechnung vorhanden ?
  c_Verleiherrechnung <- df_Abrechnung|>
    distinct(Datum,`Suisa Nummer`,.keep_all = T)|> 
    filter(Datum %in% c(c_Date[ii]))|>
    select(`keine Verleiherrechnung`)|>
    pull()
  c_Verleiherrechnung
  
  if(sum(c_Verleiherrechnung, na.rm = T)) {
    c_Verleiherrechnung <-  TRUE
  }
  c_Verleiherrechnung
  
  # Wenn keine Verleiherrechnung vorhanden ist muss die MWST der Verleiherrechnung berechnet werden.
  if (c_Verleiherrechnung) {
    # Mehrwertsteuer auf der Verleiherrechnung
    c_MWST_Abzug <-
      round5Rappen(c_Verleiherabzug - (c_Verleiherabzug / (1 + (c_MWST / 100)))
      )
    c_MWST_Abzug
    
  }else {
    c_MWST_Abzug <- round5Rappen(c_Verleiherabzug * (c_MWST / 100))
  }
  c_MWST_Abzug
  
  # Error handling Verleiherabgaben
  if(is.na(c_MWST_Abzug)) {
    error <- df_Eintritt|>
      filter(Datum == c_Date[ii])|>
      distinct(Datum,.keep_all = T)|>
      select(Datum, `Suisa Nummer`, Filmtitel)|>
      mutate(Datum = paste0(day(Datum),".",month(Datum),".",year(Datum)))
    error <- str_c("\nFür den Film ", error$Filmtitel," am ", error$Datum, " mit Suisa-Nummer ", error$`Suisa Nummer`, " wurde keine",
                   "\nVerleiherabgabe im file .../Kinoklub/input/Verleiherabgaben.xlsx definiert.")
    stop(error)
  }
  
  # Verleiherrechnungsbetrag
  c_Verleiherrechnung <- df_Abrechnung |> 
    filter(Datum %in% c(c_Date[ii]))|>
    select(Verleiherrechnungsbetrag_verteilt)|>
    pull()
  c_Verleiherrechnung
  
  if(length(c_Verleiherrechnung) > 1){
    c_Verleiherrechnung <- c_Verleiherrechnung[!is.na(c_Verleiherrechnung)]
    if(length(c_Verleiherrechnung) > 1) c_Verleiherrechnung <- c_Verleiherrechnung[1]
  }
  c_Verleiherrechnung
  
  ##############################################################################################################
  # Gewinn berechnung
  ##############################################################################################################
  l_GV[[ii]] <- tibble(Datum = c_Date[ii],
                       `Suisa Nummer` = c_suisa_nr[ii],
                       Brutto = c_Umsatz, 
                       Verleiherrechnung = c_Verleiherrechnung,
                       `SUISA-Abzug [%]` = c_suisaabzug,
                       `SUISA-Abzug [CHF]` = round5Rappen(c_Brutto * (c_suisaabzug / 100)), 
                       `Netto 3` = c_Netto3,
                       `Verleiher-Abzug [%]` = c_verleiherabzug_prozent*100,
                       `Verleiher-Abzug [CHF]` = c_Verleiherabzug,
                       `MWST Satz auf die Verleiherrechnung [%]` = c_MWST,
                       `MWST auf die Verleiherrechnung [CHF]` =  c_MWST_Abzug)|>
    mutate(## Gewinn berechnung
      `Sonstige Kosten [CHF]` = (c_Verleiherrechnung - c_MWST_Abzug) - `Verleiher-Abzug [CHF]`,
      `Gewinn/Verlust [CHF]` = round5Rappen(Brutto - sum(`Verleiher-Abzug [CHF]`,
                                                         `Sonstige Kosten [CHF]`, `MWST auf die Verleiherrechnung [CHF]`,
                                                         na.rm = T)))|>
    left_join(df_show, by = join_by(Datum, `Suisa Nummer`))
}

df_GV_Eintritt <- l_GV|>
  bind_rows()
df_GV_Eintritt


########################################################################
# Abgaben 
########################################################################

df_Abgaben <- l_Abgaben|>
  bind_rows()|>
  bind_rows(df_Eintritt|> # 
              filter(!Zahlend, !(Platzkategorie %in% df_P_kat_verechnen$Kinoförderer )))|>
  mutate(Verkaufspreis = if_else(Platzkategorie == df_P_kat_verechnen$Kinoförderer, df_P_kat_verechnen$Verkaufspreis, Verkaufspreis)
  )
df_Abgaben


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
  # Soll gemeinsam mit einem anderen Datum abgerechnet werden?
  c_linkDatum <- df_Eintritt |>
    filter(Datum == c(c_Date[ii]), Zahlend)|>
    distinct(`Link Datum`)|>
    pull()
  c_linkDatum
  
  c_verteilprodukt <- df_Abrechnung|>
    filter(Datum == c_Date[ii])|>
    select(Verteilprodukt)|>
    pull()
  c_verteilprodukt
  
  # Eventausgaben
  df_Eventausgaben <- Einnahmen_und_Ausgaben$Ausgaben |>
    filter(Kategorie == Einnahmen_und_Ausgaben$dropdown$`drop down`[1])|> # Kategorie Event
    filter(Spieldatum %in% c(c_Date[ii], c_linkDatum))|>
    mutate(Betrag = Betrag * c_verteilprodukt)
  df_Eventausgaben
  
  # Sind Event Ausgaben vorhanden
  if(nrow(df_Eventausgaben) < 1) {
    c_Eventausgaben <- 0
  } else {
    c_Eventausgaben <- sum(df_Eventausgaben$Betrag, na.rm = T)
  }
  
  # Eventeinnahmen
  df_Eventeinnahmen <- Einnahmen_und_Ausgaben$Einnahmen |>
    filter(Kategorie == Einnahmen_und_Ausgaben$dropdown$`drop down`[1])|> # Kategorie Event
    filter(Datum %in% c(c_Date[ii], c_linkDatum))|>
    mutate(Betrag = Betrag * c_verteilprodukt)
  df_Eventeinnahmen
  
  # Sind Event einnahmen vorhanden
  if(nrow(df_Eventeinnahmen) < 1) {
    c_Eventeinnahmen <- 0
  } else {
    c_Eventeinnahmen <- sum(df_Eventeinnahmen$Betrag, na.rm = T)
  }
  c_Eventeinnahmen
  
  df_temp <- df_show|>
    filter(Datum == c_Date[ii])
  df_temp
  
  l_GV_Vorfuehrung[[ii]] <- tibble(
    Datum = c_Date[ii],
    Anfang = df_temp$Anfang,
    Ende = df_temp$Ende,
    `Suisa Nummer` = c_suisa_nr[ii],
    Filmtitel = df_temp$Filmtitel,
    `Gewinn/Verlust [CHF]` = round5Rappen(
      l_GV_Kiosk[[ii]]$Gewinn + l_GV[[ii]]$`Gewinn/Verlust [CHF]` + pull(df_manko_uerberschuss|>filter(Datum == c_Date[ii])) - c_Eventausgaben + c_Eventeinnahmen
    )
  )
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

remove(l_Eintritt,  m, c_raw, l_GV, l_GV_Kiosk, c_Besucher,  df_Eventausgaben,
       c_suisaabzug, c_Gratis, c_Umsatz, l_GV_Vorfuehrung,ii, c_Eventausgaben,df_P_kat_verechnen, c_lenght, c_Brutto,
       convert_data_Film_txt, c_file, c_Verleiherrechnung, c_sheets, c_Kinofoerder_gratis, c_MWST_Abzug, c_Netto3, 
       c_Verleiher_garantie, c_Verleiherabzug,n_Film,n_kiosk,
       c_verleiherabzug_prozent)


########################################################################
# user interaction
########################################################################
writeLines("Berechnungen erfolgt")

