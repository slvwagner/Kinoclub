library(tidyverse)
library(rebus)
library(openxlsx)
library(lubridate)

file.remove("error.log")|>suppressWarnings()

source("source/functions.R")

if(!r_is.defined(c_MWST)){
  c_MWST <- 8.1
}

if (!r_is.defined(df_P_kat_verechnen)) {
  df_P_kat_verechnen <- tibble(Kinoförderer = c("Kinoförderer"),
                               Verkaufspreis =  c(13))
}

c_openfiles <- list.files(paste0("Input/"),"~")
if(length(c_openfiles) > 0) stop(paste0("\nFile: ", c_openfiles ," ist geöffnet und muss geschlossen werden!"))
remove(c_openfiles)

########################################################################
# Einnahmen und Ausgaben einlesen aus Excel 
########################################################################

c_file <- "Einnahmen und Ausgaben.xlsx"
c_sheets <- readxl::excel_sheets(paste0("Input/",c_file))

Einnahmen_und_Ausgaben <- lapply(c_sheets, function(x) {
  readxl::read_excel(paste0("Input/", c_file),
                     sheet = x)
})
names(Einnahmen_und_Ausgaben) <- c_sheets
Einnahmen_und_Ausgaben


Einnahmen_und_Ausgaben[["Ausgaben"]] <- Einnahmen_und_Ausgaben[["Ausgaben"]]|>
  mutate(Spieldatum = as.Date(Spieldatum),
         Datum = as.Date(Datum))

Einnahmen_und_Ausgaben[["Einnahmen"]] <- Einnahmen_und_Ausgaben[["Einnahmen"]]|>
  mutate(Datum = as.Date(Datum))

# Datachecking 
df_temp <- Einnahmen_und_Ausgaben[["Ausgaben"]]|>
  filter(Kategorie %in% c("Event","Verleiher"))|>
  mutate(error = is.na(Spieldatum))|>
  filter(error)

if(nrow(df_temp)>0) { 
  for (ii in 1:nrow(df_temp)) {
    warning((paste("\nFür die Kategorie \"Event\" oder \"Verleiher\" muss in der Datei \"Einnahmen und Ausgaben.xlsx\" ein Eventdatum definiert werden.",
                   "\n\nKategorie\tSpieldatum\tBezeichnung\n",df_temp$Kategorie[ii],"\t\t", df_temp$Spieldatum[ii], "\t\t", df_temp$Bezeichnung[ii])))
  }
}

########################################################################
# Funktion zur Eintrittabrechnung für Advanced Tickets files
########################################################################
convert_data_Film_txt <- function(c_fileName) {
  l_data <- list()
  for(kk in 1:length(c_fileName)){
    # read in data
    c_raw <- suppressWarnings(readLines(c_fileName[kk]))
    c_raw
    l_temp <- list()
    
    # Extract suisa
    p <- START%R%DGT%R%DGT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT #suisa
    index <- c_raw|>
      str_detect(p)
    
    c_temp <- c_raw[index]|>
      str_split("\t")|>
      unlist()
    
    ii <- 1
    
    l_temp[[ii]] <- c_temp[1]
    names(l_temp)[ii] <- "Suisa"
    ii <- ii+1
    
    # Extract Filmtitel
    l_temp[[ii]] <- c_temp[2]
    names(l_temp)[ii] <- "Filmtitel"
    ii <- ii+1
    
    # Extract Datum
    p <- "\t"%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DOT%R%DGT%R%DGT%R%DGT%R%DGT #Datum
    index <- c_raw|>
      str_detect(p)
    index
    
    c_temp <- c_raw[index]|>
      str_split("\t")|>
      unlist()
    c_temp
    
    l_temp[[ii]] <- c_temp[2]
    names(l_temp)[ii] <- "Datum"
    ii <- ii+1
    
    # Extract suisa-Vorabzug
    p <- DGT%R%DOT%R%one_or_more(DGT)%R%"%"%R%SPC%R%"SUISA" #Datum
    index <- c_raw|>
      str_detect(p)
    index
    
    c_temp <- c_raw[index]|>
      str_split("%"%R%SPC)|>
      unlist()
    c_temp
    
    l_temp[[ii]] <- c_temp[1]|>as.numeric()
    names(l_temp)[ii] <- "SUISA-Vorabzug"
    ii <- ii+1
    
    # Extract Tabelle
    p <- "Platzkategorie" #Tabellenanfang
    p1 <- "Brutto" # Tabellenende
    
    index <- c_raw|>
      str_detect(p)
    index1 <- c_raw|>
      str_detect(p1)
    
    for (jj in 1:length(c_raw)) {
      if(index[jj]== TRUE) {
        index <- jj 
        break
      }
    }
    for (jj in 1:length(c_raw)) {
      if(index1[jj]== TRUE) {
        index1 <- jj-2 
        break
      }
    }
    df_data <- c_raw[(index+1):index1]|>
      str_split("\t")|>
      bind_cols()|>
      as.matrix()|>
      t()|>
      as.data.frame()|>
      suppressMessages()
    
    names(df_data) <- c_raw[index]|>
      str_split("\t")|>
      unlist()
    
    df_data <- df_data|>
      mutate(Preis = as.numeric(Preis),
             Tax = as.numeric(Tax),
             Anzahl = as.numeric(Anzahl),
             Umsatz= Preis*Anzahl
      )|>
      tibble()
    
    l_temp[[ii]] <- df_data|>
      tibble()
    names(l_temp)[ii] <- "Abrechnung"
    
    l_data[[kk]] <- l_temp[[ii]] |>
      mutate(`Suisa Nummer` = l_temp[[1]],
             Filmtitel = l_temp[[2]],
             Datum_ = l_temp[[3]],
             `SUISA-Vorabzug` = l_temp[[4]]
      )
  }
  return(l_data)
}

########################################################################
# Eintritt aus Advanced Tickets
########################################################################

c_files <- list.files(pattern = "Eintritte", recursive = T)
l_Eintritt <- convert_data_Film_txt(c_files)

names(l_Eintritt) <- c_files|>
  str_extract(one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT))

# error handling 
# check file datum vs in file datum found
df_Eintritt <- l_Eintritt|>
  bind_rows(.id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum),
         Datum_ = lubridate::dmy(Datum_)
         )

df_temp <- df_Eintritt|>
  filter(!Datum%in%Datum_)|>
  distinct(Datum,.keep_all = T)

if(nrow(df_temp)>0){
  stop(paste0("Im file: .../Kinoklub/Input/advance tickets/Eintritt ",day(df_temp$Datum),".",month(df_temp$Datum),".", year(df_temp$Datum), 
              " wurde ein anderes Datum gefunden: ", day(df_temp$Datum_),".",month(df_temp$Datum_),".", year(df_temp$Datum_))
       )
}

df_Eintritt

# create data frame
df_Eintritt <- l_Eintritt|>
  bind_rows(.id = "Datum")|>
  mutate(Datum = lubridate::dmy(Datum),
         Datum_ = NULL,
         Verkaufspreis = Preis ,
         Tax = NULL, 
         Zahlend = if_else(Verkaufspreis == 0, F, T))|>
  select(Datum, Filmtitel,`Suisa Nummer`,Platzkategorie,Zahlend,Verkaufspreis, Anzahl,Umsatz,`SUISA-Vorabzug`)

df_Eintritt

########################################################################
# Filmvorführungen
########################################################################

df_Flimvorfuerungen <- l_Eintritt|>
  lapply( function(x){ 
    distinct(x, Datum_,`Suisa Nummer`)
  })|>
  bind_rows()|>
  mutate(Datum = Datum_|>dmy()|>as.Date())
df_Flimvorfuerungen

c_Date <- df_Flimvorfuerungen$Datum
c_suisa_nr <- df_Flimvorfuerungen$`Suisa Nummer`

########################################################################
# Eventausgaben
########################################################################

Einnahmen_und_Ausgaben[["dropdown"]]$`drop down`

Einnahmen_und_Ausgaben[["Ausgaben"]]|>
  filter(Kategorie == "Event")


########################################################################
# Kioskabrechnungen
########################################################################
source("source/Kiosk.R")

n_kiosk <- df_Kiosk|>distinct(Datum, .keep_all = T)
n_Film <- df_Eintritt|>distinct(Datum, .keep_all = T )

#############
# Error handling
if(n_kiosk|>nrow() > n_Film|>nrow()){
  df_temp <- anti_join(n_kiosk,n_Film, by = "Datum")|>
    select(Datum)
  
  stop(paste0("\nEs fehlt einen Kinoabrechnung für das Datum:\n",
              day(df_temp$Datum),".",month(df_temp$Datum), ".",year(df_temp$Datum)))
}else if(df_Kiosk|>distinct(Datum)|>nrow() < df_Eintritt|>distinct(Datum)|>nrow()){
  
  df_temp <- anti_join(n_Film, n_kiosk, by = "Datum")|>
    select(1:3)
  stop(paste0("\nEs fehlt einen Kioskabrechnug zum Film:\n", 
              df_temp$Filmtitel, " am ", day(df_temp$Datum),".",month(df_temp$Datum), ".",year(df_temp$Datum)
  ))
}

########################################################################
# show times
########################################################################

# error handling file not found
try(c_raw <- list.files(pattern = "Shows", recursive = T)|>
      readLines()|>
      suppressWarnings(),
    outFile = "error.log"
)
if(length(list.files(pattern = "error.log"))>0) {
  stop("\nEs sind nicht alle shows vorhanden: \nDatei .../Kinoklub/input/advance tickets/Shows.txt nicht gefunden. \nBitte herunterladen und abspeichern.")
  }

c_select <- tibble(found = str_detect(c_raw, "Tag"))|>
  mutate(index = row_number(),
         index = if_else(found, index, NA))|>
  filter(!is.na(index))|>
  arrange(index)|>
  slice(1)|>
  pull()
c_select

m <- c_raw[c_select:length(c_raw)]|>
  str_split("\t", simplify = T)
m
c_names <- m[1,m[1,] != ""]

m <- m[2:nrow(m),m[1,] != ""]
colnames(m) <- c_names
m

df_show <- m|>
  as_tibble()|>
  mutate(Datum = Tag|>lubridate::ymd(),
         Anfang = parse_time(Anfang),
         Ende = parse_time(Ende))|>
  select(Datum,Anfang, Ende, Saal, Titel, Version, Alter)|>
  rename(Filmtitel = Titel)|>
  arrange(Datum)

df_show <- df_show|>
  left_join(df_Eintritt|>
              distinct(Datum, `Suisa Nummer`),
            by = c("Datum" = "Datum")
  )|>
  arrange(Datum)

df_show <- df_show|>
  filter(!is.na(`Suisa Nummer`))

## error handling 
df_temp <- df_Eintritt|>distinct(Datum, `Suisa Nummer`)|>
  anti_join(df_show, by = join_by(Datum, `Suisa Nummer`))|>
  left_join(df_Eintritt, by = join_by(Datum, `Suisa Nummer`))|>
  distinct(Datum, .keep_all = T)
df_temp

if(nrow(df_temp) != 0) {
  stop(paste0(
    "\nFür den Film: ",df_temp$Filmtitel, " am ", 
    day(df_temp$Datum),".",month(df_temp$Datum),".",year(df_temp$Datum), 
    " gibt es keinen Eintrag in der Datei .../Kinoklub/Input/advance tickets/show.txt\nBitte herunterladen und abspeichern")
  )}

########################################################################
# Abos und Kinogutscheine
########################################################################
source("source/Verkauf_Abos_Gutscheine.R")


########################################################################
# Verleiherabgaben einlesen
########################################################################

c_file <- "input/Verleiherabgaben.xlsx"
c_sheets <- readxl::excel_sheets(c_file)
c_sheets

df_verleiherabgaben <- readxl::read_excel(c_file,c_sheets[1])|>
  mutate(Datum = as.Date(Datum),
         `Link Datum` = as.Date(`Link Datum`))|>
  left_join(readxl::read_excel(c_file,c_sheets[2]), by = "Verleiher")

df_Eintritt <- df_Eintritt|>
  left_join(df_verleiherabgaben|>
              select(-Titel, -Adresse, -PLZ, -Ort),
            by = c(`Suisa Nummer` = "Suisa", "Datum"))|>
  mutate(`Kinoförderer gratis?` = if_else(`Kinoförderer gratis?` == "nein", F, T),
         Zahlend = if_else(Verkaufspreis>0, T, F))
df_Eintritt

########################################################################
## Errorhandling 

# kein prozentualer noch fixer abzug definiert
df_temp <- df_Eintritt|>
  filter(is.na(`Abzug [%]`) & is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){ 
  stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
              "wurde kein Abzug definiert.",
              "\nBitte korrigieren im File:",
              "\n.../Kinoklub/input/Verleiherabgaben.xlsx korrigieren.")
  )
}

# kein minimal Abzug definiert (Es muss kein minimaler Abzug definiert werden falls ein Abzug definiert wurde)
df_temp <- df_Eintritt|>
  filter(is.na(`Minimal Abzug`) & !is.na(`Abzug [%]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0) stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
                                "\nwurde werder kein Minimal Abzug definiert.",
                                "\nBitte korrigieren im File:",
                                "\n.../Kinoklub/input/Verleiherabgaben.xlsx korrigieren.")
)

# Prozentualer und Fixer Abzug definiert
df_temp <- df_Eintritt|>
  filter(!is.na(`Abzug [%]`) & !is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){ 
  stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
              "\nwurde ein Prozentualer und ein Fixer Abzug definiert, nur eine Definition ist möglich!",
              "\nBitte korrigieren im File:",
              "\n.../Kinoklub/input/Verleiherabgaben.xlsx korrigieren.")
  )
}

# minimal und Fixer Abzug definiert
df_temp <- df_Eintritt|>
  filter(!is.na(`Minimal Abzug`) & !is.na(`Abzug fix [CHF]`))|>
  distinct(Filmtitel,.keep_all = T)
df_temp

if(nrow(df_temp)>0){
  stop(paste0("\nFür den Film ",df_temp$Filmtitel, " am ", paste0(day(df_temp$Datum),".", month(df_temp$Datum),".", year(df_temp$Datum)),
              "\nwurde ein minimal Abzug und ein Fixer Abzug definiert, nur eine Definition ist möglich!",
              "\nBitte korrigieren im File",
              "\n.../Kinoklub/input/Verleiherabgaben.xlsx")
  )
}

remove(l_Eintritt, l_Abos, l_raw,
       m, n_kiosk,n_Film, df_spez_preis_na 
       )

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
# Kinotickes
#########################################################################################################
df_Kinopreise <- df_Eintritt|>
  distinct(Platzkategorie, .keep_all = T)|>
  select(Platzkategorie, Verkaufspreis)
df_Kinopreise

########################################################################
# user interaction
########################################################################
writeLines("Daten eingelesen")

