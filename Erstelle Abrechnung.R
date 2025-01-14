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

#############################################################################################################################################
# Vorbereiten / Installieren
#############################################################################################################################################
rm(list = ls())

c_script_version <- "2024 V1.16"

# Define libraries to be installed
packages <- c("rmarkdown", "rebus", "openxlsx", "tidyverse", "lubridate","DT")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


#############################################################################################################################################
# Functions
#############################################################################################################################################
source("source/functions.R")

mapping <- function(c_Date) {
  df_mapping <- tibble(Datum = c_Date)|>
    mutate(user_Datum = paste0(day(Datum),".", month(Datum),".", year(Datum)),
           index = row_number())
  
  ############################################################################################
  # Soll die Verleiherabrechnung erzeugt werden?
  c_file <- "input/Verleiherabgaben.xlsx"
  c_sheets <- readxl::excel_sheets(c_file)
  
  df_verleiherabgaben <- readxl::read_excel(c_file,c_sheets[1])|>
    mutate(Datum = as.Date(Datum))|>
    left_join(readxl::read_excel(c_file,c_sheets[2]), by = "Verleiher")
  
  df_mapping <- df_verleiherabgaben|>
    select(Datum, `Kinoförderer gratis?`)|>
    right_join(df_mapping, by = join_by(Datum))|>
    mutate(CreateReportVerleiherabrechnung = if_else(`Kinoförderer gratis?` == "ja",F,T),
           `Kinoförderer gratis?` = NULL)|>
    arrange(index)
  return(df_mapping)
}

#############################################################################################################################################
# Benutzereinstellungen 
#############################################################################################################################################
Abrechungsjahr <- str_split(c_script_version,SPC, simplify = T)[,1]|>as.integer()

sommerpause = 65 # Tage

# Sollen für jede Vorführung eine Abrechnung erstellt werden?
c_run_single <- TRUE

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

# create Site Map 
c_SiteMap <- TRUE

#############################################################################################################################################
# Script start
#############################################################################################################################################

#########
# package installation 
#########
if(c_SiteMap){ # Wenn Site-Maps erstellen aktiviert wurden dann müssen noch weitere Libraries installiert werden.
  # Package names
  packages <- c("magick", "webshot")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
}

##########
# Vorlage für Diagramme (Bei einer Änderung soll auch das css (".../source/Kinokulub_dark.css") geändert werden)
#########
my_template <-
  theme_bw() +
  theme(
    panel.background = element_rect(
      fill = "#322f3b",
      colour = "#322f3b",
      linewidth = 0.5,
      linetype = "solid"
    ),
    plot.background = element_rect(fill = "#322f3b"),
    axis.title = element_text(colour = "#f4cccc", size  = 15),
    axis.text = element_text(colour = "#f4cccc"),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill = "#322f3b", color = "black"),
    legend.text = element_text(color = "#f4cccc"),
    legend.title = element_text(size = 12),
    title = element_text(color = "#f4cccc", size  = 22)
  )

#########
# Ausgabeformat(e)
#########
df_Render <- switch (
  c_render_option,
  "1" = tibble::tibble(Render  = c("html_document"),
                       fileExt = c(".html")),
  "2" = tibble::tibble(Render  = c("word_document"),
                       fileExt = c(".docx")),
  "3" = tibble::tibble(Render  = c("pdf_document"),
                       fileExt = c(".pdf")),
  "4" = tibble::tibble(Render  = c("html_document","word_document"),
                       fileExt = c(".html", ".docx")),
  "5" = tibble::tibble(Render  = c("html_document","pdf_document"),
                       fileExt = c(".html", ".pdf")),
  "6" = tibble::tibble(Render  = c("word_document","pdf_document"),
                       fileExt = c(".docx", ".pdf")),
  "7" = tibble::tibble(Render  = c("html_document","word_document","pdf_document"),
                       fileExt = c(".html", ".docx", ".pdf")),
  stop("\nDie verwendete Renderoption is nicht definiert")
)

#########
# erstellen von Verzeichnissen
#########
dir.create("output/") |> suppressWarnings()
dir.create("output/data/") |> suppressWarnings()


#########
# löschen aller files im output folder
#########
c_path <- "output"

if(dir.exists(c_path)){
  c_files <- paste0(c_path,"/",list.files(c_path))
  c_files
  file.remove(c_files)|>suppressWarnings()
}

#########
# löschen aller files im output/data folder
#########
if(dir.exists(c_path)){
  c_files <- paste0(c_path,"/data",list.files(c_path))
  c_files
  file.remove(c_files)|>suppressWarnings()
}

#############################################################################################################################################
# Versionskontrolle
#############################################################################################################################################
if(!file.exists("version control.ini")) { # ist kein versions kontrolle vorhanden?
  #versions kontrolle schreiben
  write(c_script_version, "version control.ini")
  
  # Löschen aller output files 
  c_path <- "output"
  c_files <- list.files(c_path, pattern = "html", full.names = T)
  c_files
  file.remove(c_files)|>suppressWarnings()
  
  c_path <- "output/pict"
  c_files <- list.files(c_path, pattern = "html", full.names = T)
  c_files
  file.remove(c_files)|>suppressWarnings()
  
  c_path <- "output/webserver"
  c_files <- list.files(c_path, pattern = "html", full.names = T)
  c_files
  file.remove(c_files)|>suppressWarnings()
  
  
  
}else{
  x <- read_file("version control.ini")|>
    str_remove("\r")|>
    str_remove("\n")
  x
  if(x != c_script_version){ # ist es nicht die aktuelle Version?
    # Löschen aller output files 
    c_path <- "output"
    c_files <- list.files(c_path, pattern = "html", full.names = T)
    c_files
    file.remove(c_files)|>suppressWarnings()
    
    c_path <- "output/pict"
    c_files <- list.files(c_path, pattern = "html", full.names = T)
    c_files
    file.remove(c_files)|>suppressWarnings()
    
    c_path <- "output/webserver"
    c_files <- list.files(c_path, pattern = "html", full.names = T)
    c_files
    file.remove(c_files)|>suppressWarnings()
    #versions kontrolle schreiben
    write(c_script_version, "version control.ini")
    
    # Löschen aller Daten die mit einer anderen Version erstellt wurden
    file.remove("environment.RData")|>
      suppressWarnings()
  }
}

#############################################################################################################################################
# Filmvorschläge auswerten
#############################################################################################################################################
data_env <- new.env()
source("source/read_and_convert_wordPress.R", local = data_env)
ls(envir = data_env)

#############################################################################################################################################
# Daten einlesen und konvertieren
#############################################################################################################################################
data_env <- new.env()
source("source/calculate.R", local = data_env)
ls(envir = data_env)

#############################################################################################################################################
# Was für Berichte müssen erstellt werden
#############################################################################################################################################
df_mapping <- mapping(data_env$c_Date)

#############################################################################################################################################
# Statistik-Bericht erstellen
#############################################################################################################################################
print(clc)
paste("Bericht: \nStatistik erstellen!")|>
  writeLines()

# Einlesen
c_raw <- readLines("source/Statistik.Rmd")

# Inhaltsverzeichnis
if(toc){# neues file schreiben mit toc
  c_raw|>
    r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
    writeLines(paste0("source/temp.Rmd"))
}else {# neues file schreiben ohne toc
  c_raw|>
    writeLines(paste0("source/temp.Rmd"))
}

# Render
rmarkdown::render(input = paste0("source/temp.Rmd"),
                  output_format  = df_Render$Render,
                  output_file = paste0("Statistik",df_Render$fileExt),
                  output_dir = paste0(getwd(), "/output"),
                  envir = data_env
                  )

paste("Bericht: \nStatistik erstellt")|>
  writeLines()

#############################################################################################################################################
# Jahresrechnung-Bericht erstellen
#############################################################################################################################################
print(clc)
paste("Bericht: \nJahresrechnung erstellen!")|>
  writeLines()
# Einlesen
c_raw <- readLines("source/Jahresrechnung.Rmd")

# Inhaltsverzeichnis
if(toc){# neues file schreiben mit toc
  c_raw|>
    r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
    writeLines(paste0("source/temp.Rmd"))
}else {# neues file schreiben ohne toc
  c_raw|>
    writeLines(paste0("source/temp.Rmd"))
}

# Render
rmarkdown::render(input = paste0("source/temp.Rmd"),
                  output_format = df_Render$Render,
                  output_file = paste0("Jahresrechnung",df_Render$fileExt),
                  output_dir = paste0(getwd(), "/output"),
                  envir = data_env
                  )

paste("Bericht: \nJahresrechnung erstellt")|>
  writeLines()

#############################################################################################################################################
# Bericht(e) Abrechnung pro Filmforführung erstellen
#############################################################################################################################################
print(clc)
paste("Bericht:\nJahresrechnung erstellt")|>
  writeLines()
ii <- 1
for(ii in 1:nrow(df_mapping)){
  # Einlesen template der Abrechnung
  c_raw <- readLines("source/Abrechnung.Rmd")
  
  # Ändern des Templates mit user eingaben (ii <- ??) verwendet für Datum
  index <- (1:length(c_raw))[c_raw|>str_detect("variablen")]
  c_raw[(index+1)] <- c_raw[(index+1)]|>str_replace(one_or_more(DGT), paste0(ii))
  
  # Ändern des Templates Titel Filmname
  index <- (1:length(c_raw))[c_raw|>str_detect("Abrechnung Filmvorführung")]
  c_temp1 <- data_env$df_Abrechnung|>
    filter(Datum == data_env$df_Abrechnung$Datum[ii])|>
    mutate(Anfang = paste0(lubridate::hour(Anfang),":", lubridate::minute(Anfang)|>as.character()|>formatC(format = "0", width = 2)|>str_replace(SPC,"0")),
           Datum = paste0(day(Datum),".",month(Datum),".",year(Datum))
    )|>
    rename(`Total Gewinn [CHF]`= `Gewinn/Verlust Filmvorführungen [CHF]`)|>
    select(Filmtitel)|>
    pull()
  
  c_temp <- c_raw[(index)]|>
    str_split("\"", simplify = T)|>
    as.vector()
  
  c_temp <- c_temp[1:2]
  c_temp <- paste0(c(c_temp), collapse = "\"")
  c_temp <- paste0(c(c_temp, " "), collapse = "")
  c_temp <- paste0(c(c_temp, c_temp1), collapse = "")
  c_raw[(index)] <- paste0(c(c_temp, "\""), collapse = "")
  
  # Inhaltsverzeichnis
  if(toc){# neues file schreiben mit toc
    c_raw|>
      r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
      writeLines(paste0("source/temp.Rmd"))
  }else {# neues file schreiben ohne toc
    c_raw|>
      writeLines(paste0("source/temp.Rmd"))
  }
  
  ls(envir = data_env)
  # Render
  rmarkdown::render(input = paste0("source/temp.Rmd"),
                    output_format = df_Render$Render,
                    output_file = paste0("Abrechnung ",df_mapping$user_Datum[ii],df_Render$fileExt),
                    output_dir = "output",
                    envir = data_env
  )

  # user interaction
  paste("Bericht: \nFilmabrechnung vom", df_mapping$user_Datum[ii], "erstellt")|>
    writeLines()
  
  ############################################################################################
  # Muss eine Verleiherrechnung erstellt werden?
  
  if(df_mapping$CreateReportVerleiherabrechnung[ii]){
    
    # Einlesen template der Verleiherabrechnung
    c_raw <- readLines("source/Verleiherabrechnung.Rmd")
    c_raw
    
    # Ändern des Templates mit user eingaben (ii <- ??) verwendet für Datum
    index <- (1:length(c_raw))[c_raw|>str_detect("variablen")]
    index
    c_raw[(index+1)] <- c_raw[(index+1)]|>str_replace(one_or_more(DGT), paste0(ii))
    
    # neues file schreiben
    writeLines(c_raw, "Verleiherabrechnung.Rmd")
    
    # Render
    rmarkdown::render(input = "Verleiherabrechnung.Rmd",
                      output_format = df_Render$Render,
                      output_file  = paste0("Verleiherabrechnung ", df_mapping$user_Datum[ii], df_Render$fileExt),
                      output_dir = "output",
                      envir = data_env
                      )
    
    # user interaction
    paste("Bericht: \nVerleiherabrechnung vom", df_mapping$user_Datum[ii], "erstellt")|>
      writeLines()
    
    # remove file
    file.remove("Verleiherabrechnung.Rmd")
  }
}
  
remove(c_raw, index,ii)
print(clc)

paste("Bericht: \nAlle Abrechnungen für Filmvorführungen wurden erstellt.")|>
  writeLines()

#############################################################################################################################################
# Create Site-Map and webserver data 
#############################################################################################################################################
source("source/create_webserver_data.R", local = data_env)

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
