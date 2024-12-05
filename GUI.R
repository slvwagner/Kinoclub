# Lade das Shiny-Paket
library(shiny)

# Daten berechnen und laden
tryCatch({
  source("source/calculate.R")
}, error = function(e) {
  stop("Fehler beim Laden von 'calculate.R': ", e$message)
})

# Sicherstellen, dass df_show$Datum existiert und korrekt formatiert ist
if (!exists("df_show") || !"Datum" %in% colnames(df_show)) {
  stop("Die DataFrame 'df_show' oder die Spalte 'Datum' existiert nicht.")
}

# Konvertiere Datum-Spalte (falls nötig)
df_show$Datum <- as.Date(df_show$Datum)

# Vektor mit Datumseinträgen
datum_vektor <- df_show$Datum

# UI-Definition
ui <- fluidPage(
  titlePanel("Start- und Enddatum auswählen und Code ausführen"),
  
  sidebarLayout(
    sidebarPanel(
      # Kalender zur Auswahl von Start- und Enddatum
      dateInput("start_datum", "Wähle ein Startdatum:", value = min(datum_vektor),
                min = min(datum_vektor), max = max(datum_vektor)),
      dateInput("end_datum", "Wähle ein Enddatum:", value = max(datum_vektor),
                min = min(datum_vektor), max = max(datum_vektor)),
      
      # Button zum Ausführen des Codes
      actionButton("ausfuehren", "Code ausführen")
    ),
    
    mainPanel(
      # Rückmeldung
      verbatimTextOutput("ausgabe")
    )
  )
)

# Server-Logik
server <- function(input, output, session) {
  
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
                           start_datum, "bis", end_datum, "ausgeführt."))
        tryCatch({
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
}

# Shiny-App starten
shinyApp(ui = ui, server = server)
