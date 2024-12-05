# Lade das Shiny-Paket
library(shiny)

# Beispiel-Daten: Ein Vektor mit Datumseinträgen
datum_vektor <- seq(as.Date("2024-12-01"), as.Date("2024-12-31"), by = "day")

source("calculate.R")

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
    
    # Prüfen, ob die ausgewählten Daten im Vektor und korrekt sind
    if (start_datum %in% datum_vektor && end_datum %in% datum_vektor && start_datum <= end_datum) {
      # Beispiel: Einen Code ausführen (z.B. auf Basis des ausgewählten Zeitraums)
      ausgabe_text(paste("Der Code wurde für den Zeitraum von", 
                         start_datum, "bis", end_datum, "ausgeführt."))
      
      # Beispiel: Hier könnte ein Skript ausgeführt werden
      # source("dein_script.R")
    } else {
      ausgabe_text("Bitte wähle gültige Start- und Enddaten aus.")
    }
  })
  
  # Ausgabe aktualisieren
  output$ausgabe <- renderText({
    ausgabe_text()
  })
}

# Shiny-App starten
shinyApp(ui = ui, server = server)
