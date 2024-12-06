# Lade das Shiny-Paket
library(tidyverse)
library(shiny)

paste0("\n********************************************",
       "\nGUI",
       "\n********************************************")|>
  writeLines()

# Set locale to German
Sys.setlocale("LC_TIME", "de_DE.UTF-8") # Ensure your system supports this locale


rm(list = ls())

source("source/functions.R")

mapping <- function(c_Datum) {
  #############################################################################################################################################
  # Index pro Suisa-Nummer und Datum erstellen
  #############################################################################################################################################
  df_mapping <- tibble(Datum = c_Datum)|>
    mutate(user_Datum = paste0(day(Datum),".", month(Datum),".", year(Datum)),
           index = row_number())
  
  ############################################################################################
  # Soll die Verleiherabrechnung erzeugt werden?
  
  c_file <- "input/Verleiherabgaben.xlsx"
  c_sheets <- readxl::excel_sheets(c_file)
  c_sheets
  
  df_verleiherabgaben <- readxl::read_excel(c_file,c_sheets[1])|>
    mutate(Datum = as.Date(Datum))|>
    left_join(readxl::read_excel(c_file,c_sheets[2]), by = "Verleiher")
  
  df_mapping <- df_verleiherabgaben|>
    select(Datum, `Kinoförderer gratis?`)|>
    right_join(df_mapping, by = join_by(Datum))|>
    mutate(CreateReportVerleiherabrechnung = if_else(`Kinoförderer gratis?` == "ja",F,T),
           `Kinoförderer gratis?` = NULL)|>
    arrange(index)
}


#############################################################################################################################################
# Benutzereinstellungen 
#############################################################################################################################################

sommerpause = 65 # Tage

# Sollen für jede Vorführung eine Abrechnung erstellt werden?
c_run_single <- TRUE

# Sollen Inhaltsverzeichnisse erstellt werden
toc <- TRUE

# Mehrwertsteuersatz
c_MWST <- 8.1 #%

# Platzkategorien die für gewisse Verleiherabgerechnet werden müssen
df_P_kat_verechnen <- tibble(Kinoförderer = "Kinoförderer", Verkaufspreis =  13)

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

if(c_SiteMap){ # Wenn Site-Maps erstellen aktiviert wurden dann müssen noch weitere Libraries installiert werden.
  # Package names
  packages <- c("magick")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Package names
  packages <- c("webshot")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
    webshot::install_phantomjs()
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
}

##########
# Vorlage für Diagramme (Bei einer Änderung soll auch das css (".../source/Kinokulub_dark.css") geändert werden)

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
dir.create("output/") |> suppressWarnings()
dir.create("output/data/") |> suppressWarnings()


#########
# löschen aller files im output folder
c_path <- "output"

if(dir.exists(c_path)){
  c_files <- paste0(c_path,"/",list.files(c_path))
  c_files
  file.remove(c_files)|>suppressWarnings()
}

#########
# löschen aller files im output folder
if(dir.exists(c_path)){
  c_files <- paste0(c_path,"/data",list.files(c_path))
  c_files
  file.remove(c_files)|>suppressWarnings()
}


#############################################################################################################################################
# 
#############################################################################################################################################
# Variable, um Warnings zu speichern
calculate_warnings <- NULL

# Daten berechnen und laden, Warnings auffangen
tryCatch({
  # Warnings abfangen
  calculate_warnings <- capture.output({
    source("source/calculate.R")
  }, type = "message")
}, error = function(e) {
  stop("Fehler beim Laden von 'source/calculate.R': ", e$message)
})

# Sicherstellen, dass df_show$Datum existiert und korrekt formatiert ist
if (!exists("df_show") || !"Datum" %in% colnames(df_show)) {
  stop("Die DataFrame 'df_show' oder die Spalte 'Datum' existiert nicht.")
}

# Vektor mit Datumseinträgen
datum_vektor <- df_show$Datum
c_Date_Gui <- NULL

ls()|>
  print()

###########################################################################################################
# UI-Definition
###########################################################################################################
ui <- fluidPage(
  titlePanel("Kinoklub GUI"),
  
  sidebarLayout(
    sidebarPanel(
      # # Kalender zur Auswahl von Start- und Enddatum mit Schweizer Datumsformat
      # dateInput("start_datum", "Wähle ein Startdatum:", value = min(datum_vektor),
      #           min = min(datum_vektor), max = max(datum_vektor), format = "dd.mm.yyyy"),
      # dateInput("end_datum", "Wähle ein Enddatum:", value = max(datum_vektor),
      #           min = min(datum_vektor), max = max(datum_vektor), format = "dd.mm.yyyy"),
      dateRangeInput(
        inputId = "dateRange", 
        label = "Wählen Sie einen Datumsbereich aus:",
        start = Sys.Date() - 7,  # Default start date (one week ago)
        end = max(datum_vektor), # Default end date (last show)
        min = min(datum_vektor), # Earliest selectable date
        max = max(datum_vektor), # Latest selectable date
        format = "dd.mm.yyyy",   # Set input format to German (DD.MM.YYYY)
        separator = " bis "      # Separator for the two dates in German
      ),
      
      # Button zum Ausführen des Codes
      actionButton("ausfuehren", "Berichte erstellen")
    ),
    
    mainPanel(
      renderText("Fehlermeldunge:"),
      # Rückmeldung
      verbatimTextOutput("ausgabe"),
      # Bereich, um Warnings darzustellen
      verbatimTextOutput("warnings_output"),
      # Example: display a table of the days in the selected range
      tableOutput("dateTable")
    )
  )
)

###########################################################################################################
# Server-Logik
###########################################################################################################
server <- function(input, output, session) {
  
  # Variable, um Status zu speichern
  ausgabe_text <- reactiveVal("Wähle ein Start- und Enddatum und klicke auf 'Code ausführen'.")
  
  ###########################################################################################################
  # Überwachung des Buttons
  ###########################################################################################################
  observeEvent(input$ausfuehren, {
    start_datum <- input$dateRange|>min()
    end_datum <- input$dateRange|>max()
    
    # start_datum <- as.Date("2024-11-1")
    # end_datum <- as.Date("2024-11-30")
        
    # Überprüfen, ob beide Daten gültig sind
    if (start_datum <= end_datum) {
      # Aktion ausführen
      ausgabe_text(paste("Die Berichte für den Zeitraum von", 
                         format(start_datum, "%d.%m.%Y"), "bis", 
                         format(end_datum, "%d.%m.%Y"), "wurden erstellt"))
      
      ##############################################
      # Filmabrechnungen erstellen mit dateRange user input
      ##############################################
      tryCatch({
        print(clc)
        print("Erstelle Abrechnung")

        df_mapping__ <- mapping(c_Date)|>
          filter(between(Datum, start_datum, end_datum))
        
        for(ii in df_mapping__$index){
          # Template der Abrechnung einlesen
          c_raw <- readLines("source/Abrechnung.Rmd")
          c_raw
          
          # Ändern des Templates: Variable im Template ii wird gesetzt. c_Date[ii] wird verwendet um das korrekte Datum für die Bereichterstellung auszuwählen.
          index <- (1:length(c_raw))[c_raw|>str_detect("variablen")]
          c_raw[(index+1)] <- c_raw[(index+1)]|>str_replace(one_or_more(DGT), paste0(ii))
          
          # Ändern des Templates Titel Filmname
          index <- (1:length(c_raw))[c_raw|>str_detect("Abrechnung Filmvorführung")]
          c_temp1 <- df_Abrechnung|>
            filter(Datum == (df_mapping__|>filter(index == ii)|>select(Datum)|>pull()))|>
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
          
          # Render
          rmarkdown::render(paste0("source/temp.Rmd"),
                            df_Render$Render,
                            output_dir = paste0(getwd(), "/output"))
          
          # Rename the file
          for (jj in 1:length(df_Render$Render)) {
            file.rename(from = paste0(getwd(),"/output/temp",df_Render$fileExt[jj]),
                        to   = paste0(getwd(),"/output/", "Abrechnung Filmvorführung ",df_mapping__|>filter(index == ii)|>select(user_Datum)|>pull(),df_Render$fileExt[jj])
            )
          }
          
          # user interaction
          print(clc)
          paste("Bericht: \nFilmabrechnung vom", df_mapping__|>filter(index == ii)|>select(user_Datum)|>pull(), "erstellt")|>
            writeLines()
          
          ############################################################################################
          # Muss eine Verleiherrechnung erstellt werden?
          ############################################################################################
          if(df_mapping__|>filter(index == ii)|>select(CreateReportVerleiherabrechnung)|>pull() ){
            
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
                              output_file = paste0("Verleiherabrechnung ", df_mapping__|>filter(index == ii)|>select(user_Datum)|>pull(), df_Render$fileExt[jj]),
                              output_format = df_Render$Render,
                              output_dir = paste0(getwd(), "/output"))

            # user interaction
            print(clc)
            paste("Bericht: \nVerleiherabrechnung vom", df_mapping__|>filter(index == ii)|>select(user_Datum)|>pull(), "erstellt")|>
              writeLines()
            
            # remove file
            file.remove("Verleiherabrechnung.Rmd")
          }
        }
        remove(c_raw, index,ii,jj)
        
      }, error = function(e) {
        ausgabe_text(paste("Fehler beim Bericht erstellen:", e$message))
      })
    } else {
      ausgabe_text("Das Enddatum darf nicht vor dem Startdatum liegen.")
    }

  })
  
  # Ausgabe aktualisieren
  output$ausgabe <- renderText({
    ausgabe_text()
  })
  
  # Create a reactive table with all the dates in the selected range
  output$dateTable <- renderTable({

    start_datum <- input$dateRange|>min()
    end_datum <- input$dateRange|>max()
    
    df_Abrechnung|>
      filter(between(Datum, start_datum, end_datum))|>
      mutate(Datum = format(Datum, "%d.%m.%Y"),
             Zeit  = format(Anfang, "%H%M"))|>
      select(Datum, Zeit, Filmtitel, `Suisa Nummer`)
  })
  
  # Warnings aus 'calculate.R' anzeigen
  output$warnings_output <- renderText({
    if (!is.null(calculate_warnings) && length(calculate_warnings) > 0) {
      paste("Warnings aus 'source/calculate.R':", paste(calculate_warnings, collapse = "\n"))
    } else {
      "Keine Warnings beim Laden von 'source/calculate.R'."
    }
  })
}

# Shiny-App starten
shinyApp(ui = ui, server = server)
