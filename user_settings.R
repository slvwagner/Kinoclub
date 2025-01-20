#############################################################################################################################################
# Bitte beachte das README.md und die Dokumentation im Verzeichniss ".../doc"
# Diesen Script erstellt alle Berichte für den Kinoklub.
# 
# Autor: Florian Wagner
# florian.wagner@wagnius.ch

# 2024 V1.0 Go Live mit Stefan Jablonski, Nadia und Florian Wagner
# 2024 V1.1 Verkauf von Abos und Gutscheinen wird in der Jahresabarechnung berücksichtigt  
# 2024 V1.2 Abrechnung für Kinowerbung hinzugefügt:..../output/Auswertung.xlsx und Prognosen in der Statistik überarbeitet
# 2024 V1.3 Neuer Bericht Statistik_DT hinzugefügt. Interaktives durchsuchen aller Tabellen 
# 2024 V1.4 Jahresbarechnung detailed entfernt
# 2024 V1.5 Merge Verkaufsartikel "Popcorn frisch", "Popcorn Salz" zu "Popcorn frisch"
# 2024 V1.6 Statistik: Wochentaganalyse
# 2024 V1.7 Statistik ohne Datatable gelöscht
# 2024 V1.8 Dokumentations update 
# 2024 V1.9 Filmvorschläge from Wordpress 
# 2024 V1.10 PowerBi script
# 2024 V1.11 WordPress Filmvorschläge auswerten
# 2024 V1.12 Verleiherrechnung nur erstellen falls nötig (Kinoförder Gratis => nein, in Verleiherabgaben.xlsx)
# 2024 V1.13 Gemeinsame Abrechnung über Link Datum in Excel file "Verleiherabgaben.xlsx"
# 2024 V1.14 GUI Graphical user interface 
# 2024 V1.15 Fake Suisa Nummer von Advanced Tickets kann nun auch verarbeitet werden 
# 2024 V1.16 Introduction of envirnonments to run GUI
# 2025 V1.17 Data typ for excel files are definded by column type database 

#############################################################################################################################################
# Vorbereiten / Installieren
#############################################################################################################################################
rm(list = ls())
source("source/functions.R")
c_script_version <- "2025 V1.17"

#############################################################################################################################################
# Benutzereinstellungen 
#############################################################################################################################################

# Abrechnungsjahr import
Abrechungsjahr <- str_split(c_script_version,SPC, simplify = T)[,1]|>as.integer()

# Wiel lange dauer die Sommerpause
sommerpause <- 65 # Tage

# Sollen Inhaltsverzeichnisse erstellt werden
toc <- TRUE

# Mehrwertsteuersatz
c_MWST <- 8.1 #%

# Platzkategorien die für gewisse Verleiherabgerechnet werden müssen
df_P_kat_verechnen <- tibble(Kinoförderer = c("Kinoförderer","Kinofördererkarte"), 
                             Verkaufspreis =  c(13,13))

# Ausgabeformate
# 1 = only html
# 2 = only docx
# 3 = only pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
# 4 = html and docx
# 5 = html and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
# 6 = docx and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
# 7 = html, docx and pdf (Achtung für pdf install Latex for Windows (Miktex) for Mac (MacTex))
c_render_option <- "1" 


#############################################################################################################################################
# Versionierung
#############################################################################################################################################
# Einlesen template der Verleiherabrechnung
c_raw <- readLines("doc/README.Rmd")

# Index where to find
c_index <- (1:length(c_raw))[c_raw|>str_detect("Script Version")]
c_index <- c_index[length(c_index)]
c_raw[c_index+1]

################################################
# Dokumentation anpassen falls neue Version
if(c_raw[c_index+1] != c_script_version){ 
  ######################################
  # Aktuelle Version ermitteln
  c_raw[c_index+1] <- c_script_version
  
  # neues file schreiben
  c_raw|>
    writeLines("doc/README.Rmd")
  
  ######################################
  # Scrip Versionshistorie ermitteln  
  c_raw <- readLines("Erstelle Abrechnung.R")
  p <- "#"%R%SPC%R%one_or_more(DGT)%R%SPC%R%"V"%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT)
  c_Version_hist <- c_raw[str_detect(c_raw, p)]|>
    str_remove("# ")|>
    paste0("  \\")
  c_Version_hist
  
  p <- one_or_more(DGT)%R%SPC%R%"V"%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT)
  
  if(c_Version_hist[length(c_Version_hist)]|>str_extract(p) != c_script_version) stop("\nDer letzte Eintrag der Versionshistorie stimmt nicht mit der Variable c_script_version überein.\nBitte korrigieren")
  
  # Versionshistorie in Template einfügen
  c_raw <- readLines("doc/README.Rmd")
  c_raw[1:3]
  # Titel suchen
  index <- (1:length(c_raw))[c_raw|>str_detect("# Versionshistorie")]
  index
  # Ändern des Templates
  c(c_raw[1:(index + 1)], c_Version_hist,"\n")|>
    writeLines("doc/README.Rmd")
  
  source("doc/create Readme and Docu.R")
}


source("source/calculate.R")

#############################################################################################################################################
# remove temp files 
#############################################################################################################################################
list.files(pattern = "temp", recursive = TRUE)|>
  file.remove()

#############################################################################################################################################
# User Interaktion
#############################################################################################################################################
print(clc)
paste0("****************************************\n",
       "Script Version:  ", c_script_version,
       "\n\nAlles wurde korrekt ausgeführt.", if(warnings()|>length()>0) {"\nEs Fehlen noch Datensätze. \nBitte beachte die Fehlermeldungen unten in orange."},"\n\n",
       paste0("Dateinen wurden im folgenden Verzeichniss erstellt:\n", getwd(), "/output/"),
       "\n****************************************\n")|>
  writeLines()
