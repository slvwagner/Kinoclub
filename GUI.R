# Lade das Shiny-Paket
library(shiny)

rm(list = ls())

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



###########################################################################################################
# UI-Definition
###########################################################################################################
ui <- fluidPage(
  titlePanel("Start- und Enddatum auswählen und Script ausführen"),
  
  sidebarLayout(
    sidebarPanel(
      # Kalender zur Auswahl von Start- und Enddatum mit Schweizer Datumsformat
      dateInput("start_datum", "Wähle ein Startdatum:", value = min(datum_vektor),
                min = min(datum_vektor), max = max(datum_vektor), format = "dd.mm.yyyy"),
      dateInput("end_datum", "Wähle ein Enddatum:", value = max(datum_vektor),
                min = min(datum_vektor), max = max(datum_vektor), format = "dd.mm.yyyy"),
      
      # Button zum Ausführen des Codes
      actionButton("ausfuehren", "Berichte erstellen")
    ),
    
    mainPanel(
      # Rückmeldung
      verbatimTextOutput("ausgabe"),
      # Bereich, um Warnings darzustellen
      verbatimTextOutput("warnings_output")
    )
  )
)

###########################################################################################################
# Server-Logik
###########################################################################################################
server <- function(input, output, session, c_Date_GUI) {
  
  # Variable, um Status zu speichern
  ausgabe_text <- reactiveVal("Wähle ein Start- und Enddatum und klicke auf 'Code ausführen'.")
  
  # Überwachung des Buttons
  observeEvent(input$ausfuehren, {
    start_datum <- input$start_datum
    end_datum <- input$end_datum
        
    # Überprüfen, ob beide Daten gültig sind
    if (start_datum %in% datum_vektor && end_datum %in% datum_vektor) {
      if (start_datum <= end_datum) {
        # Aktion ausführen
        ausgabe_text(paste("Der Code wurde für den Zeitraum von", 
                           format(start_datum, "%d.%m.%Y"), "bis", 
                           format(end_datum, "%d.%m.%Y"), "ausgeführt."))
        
        tryCatch({
          c_Date <- c_Date[between(c_Date, start_datum, end_datum)]
          print(c_Date)
          source("Erstelle Abrechnung.R")
          
        }, error = function(e) {
          ausgabe_text(paste("Fehler beim Ausführen von 'Erstelle Abrechnung.R':", e$message))
        })
      } else {
        ausgabe_text("Das Enddatum darf nicht vor dem Startdatum liegen.")
      }
    } else {
      ausgabe_text("Das ausgewählte Datum liegt außerhalb des gültigen Bereichs.")
    }
  })
  
  # Ausgabe aktualisieren
  output$ausgabe <- renderText({
    ausgabe_text()
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
