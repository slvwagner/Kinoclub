library(shiny)


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


# Set locale to German
Sys.setlocale("LC_TIME", "de_DE.UTF-8") # Ensure your system supports this locale

# Define UI
ui <- fluidPage(
  titlePanel("Date Range Input Example (German Format)"),
  
  sidebarLayout(
    sidebarPanel(
      # Date range input widget with German formatting
      dateRangeInput(
        inputId = "dateRange", 
        label = "Wählen Sie einen Datumsbereich aus:",
        start = Sys.Date() - 7, # Default start date (one week ago)
        end = Sys.Date(),       # Default end date (today)
        min = "2020-01-01",     # Earliest selectable date
        max = Sys.Date(),       # Latest selectable date
        format = "dd.mm.yyyy",  # Set input format to German (DD.MM.YYYY)
        separator = " bis "     # Separator for the two dates in German
      )
    ),
    
    mainPanel(
      # Display selected date range
      textOutput("selectedDates"),
      # Example: display a table of the days in the selected range
      tableOutput("dateTable")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Display selected date range in German format
  output$selectedDates <- renderText({
    paste(
      "Sie haben ausgewählt:",
      format(as.Date(input$dateRange[1]), "%d.%m.%Y"),
      "bis",
      format(as.Date(input$dateRange[2]), "%d.%m.%Y")
    )
  })
  
  # Create a reactive table with all the dates in the selected range
  output$dateTable <- renderTable({
    if (is.null(input$dateRange)) return(NULL)
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    
    mapping(input$dateRange)
    
    # data.frame(
    #   Datum = format(seq(from = start_date, to = end_date, by = "days"), "%d.%m.%Y")
    # )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
