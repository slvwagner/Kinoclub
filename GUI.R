#############################################################################################################################################
# Graphical user interface für den Kinoklub
# Diese App kann mit Run App in Rstudio gestartet werden. 
#############################################################################################################################################

#############################################################################################################################################
# Vorbereiten / Installieren
#############################################################################################################################################
rm(list = ls())

# Define libraries to be installed
packages <- c("rmarkdown", "rebus", "openxlsx", "flextable", "tidyverse", "lubridate","DT", "shiny", "shinyBS", "magick", "webshot")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

source("source/functions.R")

#############################################################################################################################################
# Benutzereinstellungen 
#############################################################################################################################################

sommerpause = (65) # Tage

# Sollen Inhaltsverzeichnisse erstellt werden
toc <- reactiveVal(TRUE)

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
c_render_option <- reactiveVal("1") 

#############################################################################################################################################
# Functions
#############################################################################################################################################

###############################################
# Index pro Suisa-Nummer und Datum erstellen
###############################################
mapping <- function(c_Datum) {
  df_mapping <- tibble(Datum = c_Datum)|>
    mutate(user_Datum = paste0(day(Datum),".", month(Datum),".", year(Datum)),
           index = row_number())
  
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

###############################################
# Statistik-Bericht erstellen
###############################################
StatistikErstellen <- function() {
  print(clc)
  # Einlesen
  c_raw <- readLines("source/Statistik.Rmd")
  c_raw
  
  # Inhaltsverzeichnis
  if(toc()){# neues file schreiben mit toc
    c_raw|>
      r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
      writeLines(paste0("source/temp.Rmd"))
  }else {# neues file schreiben ohne toc
    c_raw|>
      writeLines(paste0("source/temp.Rmd"))
  }
  
  # Render
  rmarkdown::render(paste0("source/temp.Rmd"),
                    df_Render()$Render,
                    output_dir = paste0(getwd(), "/output"))
  
  # Rename the file
  for (jj in 1:length(df_Render()$Render)) {
    file.rename(from = paste0(getwd(),"/output/temp",df_Render()$fileExt[jj]),
                to   = paste0(getwd(),"/output/", "Statistik",df_Render()$fileExt[jj] )
    )
  }
}

###############################################
# Jahresrechnung-Bericht erstellen
###############################################
JahresrechnungErstellen <- function() {
  # Einlesen
  c_raw <- readLines("source/Jahresrechnung.Rmd")
  c_raw
  
  # Inhaltsverzeichnis
  if(toc()){# neues file schreiben mit toc
    c_raw|>
      r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
      writeLines(paste0("source/temp.Rmd"))
  }else {# neues file schreiben ohne toc
    c_raw|>
      writeLines(paste0("source/temp.Rmd"))
  }
  
  # Render
  rmarkdown::render(paste0("source/temp.Rmd"),
                    df_Render()$Render,
                    output_dir = paste0(getwd(), "/output"))
  
  # Rename the file
  for (jj in 1:length(df_Render()$Render)) {
    file.rename(from = paste0(getwd(),"/output/temp",df_Render()$fileExt[jj]),
                to   = paste0(getwd(),"/output/", "Jahresrechnung",df_Render()$fileExt[jj] )
    )
  }
}

#######################################################
# function to edit Site-Map: insert pictures
#######################################################
instert_picts <- function(raw_rmd, output_dir, index,fileNames, url) {
  # create link to pict and link to file 
  if(length(raw_rmd) == index){
    for (ii in 1:(length(fileNames))) { 
      if(ii == 1){ #letzte Zeile von Rmd
        raw_rmd <- c(raw_rmd[1:index],
                     paste0("[","![",fileNames[ii],"](",output_dir,fileNames[ii],".png)","](", url[ii],")")#,"  \\\n\\")," "
        )
      }else{ # normales einfügen
        raw_rmd <- c(raw_rmd[1:index],
                     paste0("[","![",fileNames[ii],"](",output_dir,fileNames[ii],".png)","](", url[ii],")",if((ii %% 2) == 0) {" \\"}),#,"  \\\n\\"),
                     if((ii %% 2) == 0) {"\\"}, # if index is even put additional spacing 
                     raw_rmd[(index+1):length(raw_rmd)]
        )
      }
    }
  }else{ # normales einfügen 
    for (ii in 1:(length(fileNames))) {
      raw_rmd <- c(raw_rmd[1:index],
                   paste0("[","![",fileNames[ii],"](",output_dir,fileNames[ii],".png)","](", url[ii],")", if((ii %% 2) == 0) {" \\"}),#,"  \\\n\\"),
                   if((ii %% 2) == 0) {"\\"}, # if index is even put additional spacing 
                   raw_rmd[(index+1):length(raw_rmd)]
      )
    }
  }
  return(raw_rmd)
}

#######################################################
# Create Site-Map and webserver data 
#######################################################
webserver <- function() {
  # Package names
  packages <- c("xml2")
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  #######################################################
  # Find reports 
  #######################################################
  c_path <- "output/"
  
  df_reports <- tibble(FileName = list.files(c_path, "html"))
  df_reports
  
  df_temp1 <-  df_reports|>
    filter(str_detect(FileName, "Abrechnung"))|>
    pull()|>
    lapply(function(x){
      doc <- read_html(paste0(c_path,x))
      # Find elements to edit 
      element <- xml_find_first(doc, "body")|>
        xml_find_first("div")
      element
      
      children <- xml_children(element)[3]
      # Extract data
      c_raw <- xml_text(children)|>
        str_split("\n")|>
        unlist()|>
        str_remove("\r")
      c_raw
      
      # Create data to return
      tibble(`Suisa-Nummer` = c_raw[17],
             Filmtitel = c_raw[14],
             Datum = c_raw[11],
             typ = "Abrechnung Filmvorführungen",
             FileName = x)
    })|>
    bind_rows()
  
  df_temp1
  
  df_temp2 <-  df_reports|>
    filter(str_detect(FileName, "Verleiher"))|>
    pull()|>
    lapply(function(x){
      doc <- read_html(paste0(c_path,x))
      # Find elements to edit 
      element <- xml_find_first(doc, "body")|>
        xml_find_first("div")
      
      # Find all children of the node
      children <- xml_children(element)
      children <- children[[5]]|>
        xml_children()
      
      # Extract data
      c_raw <- xml_text(children[[2]])[1]|>
        str_split("\n", simplify = T)
      
      # Create data to return
      tibble(`Suisa-Nummer` = c_raw[,7],
             Filmtitel = c_raw[,8],
             Datum = c_raw[,9],
             FileName = x)
    })|>
    bind_rows()|>
    mutate(`Suisa-Nummer` = str_remove(`Suisa-Nummer`, "\r"),
           Filmtitel = str_remove(Filmtitel, "\r"),
           Datum = str_remove(Datum, "\r"),
           typ = "Verleiherabrechnung",
    )
  df_temp1
  df_temp2
  
  m_Film <- bind_rows(df_temp2, 
                      df_temp1,
                      tibble(`Suisa-Nummer`= NA,
                             Filmtitel = NA,
                             Datum = NA,
                             typ = "Statistik",
                             FileName = "Statistik.html"),
                      tibble(`Suisa-Nummer`= NA,
                             Filmtitel = NA,
                             Datum = NA,
                             typ = "Jahresrechnung",
                             FileName = "Jahresrechnung.html")
  )
  m_Film <- m_Film|>
    mutate(Datum = dmy(Datum))|>
    arrange(Datum)|>
    mutate(Datum = paste0(day(Datum),".",month(Datum),".", year(Datum)))
  m_Film
  
  #######################################################
  # create site map
  #######################################################
  # Was für Berichte typen sind vorhanden
  c_typ_Berichte <- m_Film$FileName|>
    str_extract(START%R%one_or_more(WRD))|>
    factor()|>
    levels()
  c_typ_Berichte
  
  # Convert filenames to URL
  c_url <- paste0("file:///",URLencode(paste0(getwd(),"/output/", m_Film$FileName)), 
                  sep = "")
  c_url
  
  c_path <- paste0(getwd(),"/output/pict")
  c_path
  dir.create(c_path)|>suppressWarnings()
  
  # Vorschaubilder erzeugen wenn noch nicht vorhanden 
  ii <- 1
  if(!(length(list.files("output/", "html")) == length(list.files("output/pict/")))){
    library(magick)
    writeLines("Site-Map previews werden erstellt, einen Moment bitte: ")
    
    c_select <- !((m_Film$FileName|>str_remove(".html")) %in% (list.files("output/pict/")|>str_remove(".html.png"))
    )
    c_select
    
    ii <- 1
    for (ii in 1:length(m_Film$FileName[c_select])) {
      # Set the path to the input image
      input_path <- paste0(c_path, "/",m_Film$FileName[c_select][ii],".png")
      input_path
      
      # create a webshot, printed html
      webshot::webshot(url = c_url[c_select][ii], file = input_path)
      
      # Read the image crop and resize and save
      image_read(input_path)|>
        image_crop(geometry = "992x992+0+0")|>
        image_resize("400x400")|>
        image_write(input_path)
      
      writeLines(".", sep = "")
    }
  }
  
  # Einlesen template der Verleiherabrechnung
  c_raw <- readLines("source/Site_Map.Rmd")
  c_raw
  
  ii <- 1
  for (ii in 1:length(c_typ_Berichte)) { # Für jeden Bericht typ muss ein Bilde und Link eingefügt werden
    # Index where to insert  
    c_index <- (1:length(c_raw))[c_raw|>str_detect(c_typ_Berichte[ii])]
    c_index <- c_index[length(c_index)]
    c_index
    
    c_raw
    c_raw[c_index]
    
    if(c_typ_Berichte[ii] == "Jahresrechnung"){
      c_select <- str_detect(m_Film$FileName, START%R%c_typ_Berichte[ii]%R%DOT%R%"html")
      c_raw <- instert_picts(c_raw,"output/pict/",c_index,m_Film$FileName[c_select], c_url[c_select])
    }else{
      c_select <- str_detect(m_Film$FileName, START%R%c_typ_Berichte[ii])
      c_raw <- instert_picts(c_raw,"output/pict/",c_index,m_Film$FileName[c_select], c_url[c_select])
    }
    
    # Linkliste einfügen
    if(c_typ_Berichte[ii]=="Verleiherabrechnung"){
      for (jj in 1:length(m_Film$FileName[c_select])) {
        c_raw <- c(c_raw[1:(c_index)],
                   paste0("[",m_Film$FileName[c_select][jj],"](", c_url[c_select][jj],")  ",m_Film$Filmtitel[jj],"  \\"), 
                   c_raw[(c_index+1):length(c_raw)])
      }
      c_raw <- c(c_raw[1:(c_index + jj)],
                 paste0("  \\"), 
                 c_raw[(c_index + jj + 1):length(c_raw)])
    }
    c_raw
    
    # Linkliste einfügen
    if(c_typ_Berichte[ii]=="Abrechnung"){
      for (jj in 1:length(m_Film$FileName[c_select])) {
        c_raw <- c(c_raw[1:(c_index)],
                   paste0("[",m_Film$FileName[c_select][jj],"](", c_url[c_select][jj],")  ",m_Film$Filmtitel[jj],"  \\"),
                   c_raw[(c_index+1):length(c_raw)])
      }
      c_raw <- c(c_raw[1:(c_index + jj)],
                 paste0("  \\"),
                 c_raw[(c_index + jj + 1):length(c_raw)])
    }
  }
  c_raw
  
  # neues file schreiben
  c_raw|>
    r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
    writeLines("Site-Map.Rmd")
  
  # Render
  rmarkdown::render(input = "Site-Map.Rmd")
  # Remove file
  file.remove("Site-Map.Rmd")
  
  #######################################################
  # Data for Webserver
  #######################################################
  #copy data from .../output to .../output/webserver
  c_path <- "output/webserver"
  
  if(!dir.exists(c_path)){
    dir.create(c_path)
  }
  if(!dir.exists(paste0(c_path,"/pict"))){
    dir.create(paste0(c_path,"/pict"))
  }
  
  # copy png
  paste0(getwd(),"/output/pict/",list.files("output/pict/", pattern = "png", include.dirs = TRUE, recursive = FALSE))|>
    file.copy(paste0(c_path,"/pict"))
  
  
  m_Film$FileName <- m_Film$FileName
  
  # Was für Berichte typen sind vorhanden
  c_typ_Berichte <- m_Film$FileName|>
    str_extract(START%R%one_or_more(WRD))|>
    factor()|>
    levels()
  c_typ_Berichte
  
  # Convert filenames to URL
  c_url <- paste0("",URLencode(m_Film$FileName))
  c_url
  
  # Einlesen template der Verleiherabrechnung
  c_raw <- readLines("source/Site_Map.Rmd")
  c_raw
  
  ii <- 1
  for (ii in 1:length(c_typ_Berichte)) { # Für jeden Bericht typ muss ein Bilde und Link eingefügt werden
    # Index where to insert  
    c_index <- (1:length(c_raw))[c_raw|>str_detect(c_typ_Berichte[ii])]
    c_index <- c_index[length(c_index)]
    c_index
    
    c_raw
    c_raw[c_index]
    
    if(c_typ_Berichte[ii] == "Jahresrechnung"){
      c_select <- str_detect(m_Film$FileName, START%R%c_typ_Berichte[ii]%R%DOT%R%"html")
    }else{
      c_select <- str_detect(m_Film$FileName, START%R%c_typ_Berichte[ii])
    }
    
    c_raw
    m_Film$FileName[c_select]
    c_url[c_select]
    
    c_raw <- instert_picts(c_raw,"pict/",c_index,m_Film$FileName[c_select], c_url[c_select])
    c_raw
    
    c_raw[c_index]
    
    
    # Linkliste einfügen
    if(c_typ_Berichte[ii]=="Verleiherabrechnung"){
      for (jj in 1:length(m_Film$FileName[c_select])) {
        c_raw <- c(c_raw[1:(c_index)],
                   paste0("[",m_Film$FileName[c_select][jj],"](", c_url[c_select][jj],")  ",m_Film$Filmtitel[c_select][jj],"  \\"), 
                   c_raw[(c_index+1):length(c_raw)])
      }
      c_raw <- c(c_raw[1:(c_index + jj)],
                 paste0("  \\"), 
                 c_raw[(c_index + jj + 1):length(c_raw)])
    }
    
    # Linkliste einfügen
    if(c_typ_Berichte[ii]=="Abrechnung"){
      for (jj in 1:length(m_Film$FileName[c_select])) {
        c_raw <- c(c_raw[1:(c_index)],
                   paste0("[",m_Film$FileName[c_select][jj],"](", c_url[c_select][jj],")  ",m_Film$Filmtitel[c_select][jj],"  \\"), 
                   c_raw[(c_index+1):length(c_raw)])
      }
      c_raw <- c(c_raw[1:(c_index + jj)],
                 paste0("  \\"), 
                 c_raw[(c_index + jj + 1):length(c_raw)])
    }
    c_raw
  }
  
  c_typ_Berichte[ii]
  c_raw
  
  # neues file schreiben
  c_raw|>
    r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
    writeLines("output/webserver/index.Rmd")
  
  # Render
  rmarkdown::render(input = "output/webserver/index.Rmd")
  # Remove file
  file.remove("output/webserver/index.Rmd")
  # Remove directory
  unlink(paste0(c_path,"/pict"), recursive = TRUE)
  
  #######################################################
  # edit html
  #######################################################
  # Package names
  packages <- c("xml2")
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  
  add_SiteMapLink <- function(file_path) {
    # load html file
    doc <- read_html(file_path)
    
    # Find elements to edit 
    element <- xml_find_first(doc, "body")|>
      xml_find_first("div")
    
    # Find all children of the parent node
    children <- xml_children(element)
    
    # Insert Node
    xml_add_child(children[[1]], paste0("a href=\"",URLencode(paste0("index.html")),"\""), "Site-Map")
    write_xml(doc, file_path)
  }
  
  #copy data from .../output to .../output/webserver
  c_path <- "output/webserver"
  
  # copy html 
  paste0("output/",list.files("output/",pattern = "html",include.dirs = FALSE, recursive = FALSE))|>
    file.copy(paste0(c_path,""), overwrite = TRUE)
  
  c_files <- list.files("output/webserver/",pattern = "html"%R%END,include.dirs = FALSE, recursive = FALSE)
  c_files <- paste0("output/webserver/",c_files)
  
  # apply Site-Map link
  c_files|>
    lapply(add_SiteMapLink)
  
  # remove files
  file.remove("Site-Map.html")
}

#######################################################
# Erstellen der Abrechnung pro Filmvorführung
#######################################################
AbrechnungErstellen <- function(df_mapping__, df_Abrechnung) {
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
    if(toc()){# neues file schreiben mit toc
      c_raw|>
        r_toc_for_Rmd(toc_heading_string = "Inhaltsverzeichnis")|>
        writeLines(paste0("source/temp.Rmd"))
    }else {# neues file schreiben ohne toc
      c_raw|>
        writeLines(paste0("source/temp.Rmd"))
    }
    
    # Render
    rmarkdown::render(paste0("source/temp.Rmd"),
                      df_Render()$Render,
                      output_dir = paste0(getwd(), "/output"))
    
    # Rename the file
    for (jj in 1:length(df_Render()$Render)) {
      file.rename(from = paste0(getwd(),"/output/temp",df_Render()$fileExt[jj]),
                  to   = paste0(getwd(),"/output/", "Abrechnung Filmvorführung ",df_mapping__|>filter(index == ii)|>select(user_Datum)|>pull(),df_Render()$fileExt[jj])
      )
    }
    
    # user interaction
    print(clc)
    paste("Bericht: \nFilmabrechnung vom", df_mapping__|>filter(index == ii)|>select(user_Datum)|>pull(), "erstellt")|>
      writeLines()
    
    ####################
    # Muss eine Verleiherrechnung erstellt werden?
    ####################
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
                        output_file = paste0("Verleiherabrechnung ", df_mapping__|>filter(index == ii)|>select(user_Datum)|>pull(), df_Render()$fileExt[jj]),
                        output_format = df_Render()$Render,
                        output_dir = paste0(getwd(), "/output"))
      
      
      # user interaction
      print(clc)
      paste("Bericht: \nVerleiherabrechnung vom", df_mapping__|>filter(index == ii)|>select(user_Datum)|>pull(), "erstellt")|>
        writeLines()
      
      # remove file
      file.remove("Verleiherabrechnung.Rmd")
    }
    
  }
}

#############################################################################################################################################
# System Variablen und Vorlagen
#############################################################################################################################################

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

# Variable, um Warnings zu speichern
calculate_warnings <- reactiveVal(NULL)

# Daten berechnen und laden, Warnings auffangen
tryCatch({
  # Warnings abfangen
  calculate_warnings <- capture.output({
    source("source/calculate.R")
  }, type = "message")
}, error = function(e) {
  stop("Fehler beim Laden von 'source/calculate.R': ", e$message)
})

# Vektor mit Datumseinträgen
datum_vektor <- (df_show$Datum)

# Variable, um Status zu speichern
ausgabe_text <- paste0(calculate_warnings, 
                       collapse = "\n")|>
  reactiveVal()
df_Render <- reactiveVal(NULL)

#############################################################################################################################################
# UI-Definition
#############################################################################################################################################
ui <- fluidPage(
  titlePanel("Kinoklub GUI"),
  
  sidebarLayout(
    sidebarPanel(
      #############################
      # Inhaltsverzeichnis
      #############################
      selectInput(
        inputId = "Inhaltsverzeichnis",
        label = "Inhaltsverzeichnis erstellen?",
        choices = list(
          "Ja" = TRUE,
          "Nein" = FALSE
        ),
        selected = TRUE # Default value
      ),

      #############################
      # Ausgabeformat
      #############################
      selectInput(
        inputId = "render_option",
        label = "Ausgabeformat wählen:",
        choices = list(
          "HTML" = "1",
          "DOCX" = "2",
          "PDF" = "3",
          "HTML and DOCX" = "4",
          "HTML and PDF" = "5",
          "DOCX and PDF" = "6",
          "HTML, DOCX, and PDF" = "7"
        ),
        selected = "1" # Default value
      ),
      # Add tooltips using shinyBS
      bsTooltip(
        id = "render_option",
        title = "PDF options require LaTeX installation (e.g., MikTeX for Windows, MacTeX for Mac).",
        placement = "right",
        trigger = "hover"
      ),
      
      shiny::tags$h5("**********************"),
      #############################
      # Button Daten Einlesen
      #############################
      actionButton("DatenEinlesen", "Daten Einlesen"),
      # Add tooltips using shinyBS
      bsTooltip(
        id = "DatenEinlesen",
        title = "Es werden alle files in ordner .../Kinoklub/input eingelesen.",
        placement = "right",
        trigger = "hover"
      ),
      
      shiny::tags$h5("**********************"),
      
      #############################
      # Datumsbereich auswählen für die Abrechnung Filmvorführungen
      #############################
      dateRangeInput(
        inputId = "dateRange", 
        label = "Wählen Sie einen Datumsbereich aus:",
        start = Sys.Date() - 14,  # Default start date (one week ago)
        end = max(datum_vektor), # Default end date (last show)
        min = min(datum_vektor), # Earliest selectable date
        max = max(datum_vektor), # Latest selectable date
        format = "dd.mm.yyyy",   # Set input format to German (DD.MM.YYYY)
        separator = " bis "      # Separator for the two dates in German
      ),
      
      #############################
      # Button zum Ausführen von Code
      #############################
      actionButton("Abrechnung", "Filmabrechnunge(n) erstellen"),
      # Add tooltips using shinyBS
      bsTooltip(
        id = "Abrechnung",
        title = "Es werden die Filmabrechnungen im gewählten Datumsbereich erstellt.",
        placement = "right",
        trigger = "hover"
      ),
      
      shiny::tags$h5("**********************"),
      
      #############################
      # Button zum Ausführen von Code
      ############################# 
      actionButton("Statistik", "Statistik erstellen"),

      #############################
      # Button zum Ausführen von Code 
      #############################
      actionButton("Jahresrechnung", "Jahresrechnung erstellen"),
      
      shiny::tags$h5("**********************"),
      
      #############################
      # Button zum Ausführen von Code 
      #############################
      actionButton("webserver", "Update Site-Map"),
            
      shiny::tags$h5("**********************"),
      
      #############################
      # Button zum Ausführen von Code
      ############################# 
      actionButton("wordpress", "Filmumfrage Wordpress auswerten"),
      
      shiny::tags$h5("**********************"),

      #############################
      # Button zum Ausführen von Code 
      #############################
      actionButton("ErstelleAbrechnung", "Alles erstellen mit Webserver"),
      # Add tooltips using shinyBS
      bsTooltip(
        id = "ErstelleAbrechnung",
        title = "Achtung die Ausführung braucht Zeit!",
        placement = "right",
        trigger = "hover"
      ),
    ),
    
    #############################
    # Render the output
    #############################
    mainPanel(
      if(file.exists("output/webserver/index.html")) shiny::tags$h4("Webserver:"), 
      if(file.exists("output/webserver/index.html")) shiny::tags$a (href = "reports/index.html", "Site-map", target = "_blank", style = "font-size: 16px;"),
      shiny::tags$h4 ("Filme in der gewählten Periode"),
      tableOutput("dateTable"),
      # Rückmeldung
      shiny::tags$h4 ("System Rückmeldungen"),
      verbatimTextOutput("ausgabe"),
      # Bereich, um Warnings darzustellen
      verbatimTextOutput("warnings_output")
    )
  )
)


#############################################################################################################################################
# Server-Logik
#############################################################################################################################################
server <- function(input, output, session) {
  
  # Map the URL path "custom" to the local directory "output/webserver"
  shiny::addResourcePath("reports", "output/webserver")
  
  ######################################
  # Überwachung Inhaltsverzeichniss
  ######################################
  observeEvent(input$Inhaltsverzeichnis , {
    print(clc)
    toc(input$Inhaltsverzeichnis)
    print(toc())
  })
  
  ######################################
  # Überwachung Ausgabeformat
  ######################################
  observeEvent(input$render_option , {
    print(clc)
    
    df_Render(
      switch (
        input$render_option,
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
    )

    print(df_Render())
  })
  
  ######################################
  # Überwachung Button Dateneinlesen
  ######################################
  observeEvent(input$DatenEinlesen, {
    print(clc)
    
    # Laden und Berechnen der Input-Daten, Warnings auffangen
    tryCatch({
      # Warnings abfangen
      calculate_warnings <- capture.output({
        source("source/calculate.R")
      }, type = "message")
    }, error = function(e) {
      stop("Fehler beim Laden von 'source/calculate.R': ", e$message)
    })
    paste0("Bericht:\nDaten wurden eingelesen:\n",
           paste0(calculate_warnings, collapse = "\n"))|>
      ausgabe_text()
    
    datum_vektor <- df_show$Datum
    
  })
  
  ######################################
  # Überwachung Button Abrechnunge erstellen über Datum-Range
  ######################################
  observeEvent(input$Abrechnung, {
    start_datum <- input$dateRange|>min()
    end_datum <- input$dateRange|>max()
    
    # Überprüfen, ob beide Daten gültig sind
    if (start_datum <= end_datum) {
      # Aktion ausführen
      ausgabe_text(paste0("Die Filmabrechnungen für den Zeitraum \n", 
                         format(start_datum, "%d.%m.%Y"), " bis ", 
                         format(end_datum, "%d.%m.%Y"), " wurden erstellt",
                         paste0("\n",getwd(), "/output")
                         ))
      
      ##############################################
      # Filmabrechnungen erstellen mit dateRange user input
      tryCatch({
        print(clc)
        print("Erstelle Abrechnung")
        df_mapping__ <- mapping(c_Date)|>
          filter(between(Datum, start_datum, end_datum))
        AbrechnungErstellen(df_mapping__, df_Abrechnung)
      }, error = function(e) {
        ausgabe_text(paste("Fehler beim Bericht erstellen:", e$message))
      })
    } else {
      ausgabe_text("Das Enddatum darf nicht vor dem Startdatum liegen.")
    }
  })
  
  ######################################
  # Überwachung Button Statistik
  ######################################
  observeEvent(input$Statistik, {
    print(clc)    
    StatistikErstellen()
    # User feedback
    ausgabe_text(paste0("Bericht: \nStatistik erstellt",
                       paste0("\n",getwd(), "/output")
                       ))
  })
  
  ######################################
  # Überwachung Button Jahresrechnung
  ######################################
  observeEvent(input$Jahresrechnung, {
    print(clc)
    JahresrechnungErstellen()
    # User feedback
    paste0("Bericht: \nJahresrechnung erstellt",
          paste0("\n",getwd(), "/output")
          )|>
      ausgabe_text()
  })

  ######################################
  # Überwachung Button Wordpress
  ######################################
  observeEvent(input$wordpress, {
    print(clc)
    source("source/read_and_convert_wordPress.R")
    paste0("Bericht:\nFilmumfrage auswertung ausgeführt",
           "\nFinde die Berechneten daten im Verzeichnis:",
           "\n", getwd(), "output/data/Filmvorschläge.xlsx"
           )|>
      ausgabe_text()
  })
  
  ######################################
  # Überwachung Button webserver
  ######################################
  observeEvent(input$webserver , {
    print(clc)
    webserver() 
    paste0("Bericht:",
           "\nWebserver und Site-Map wurden aktualisiert.",
           "\n", getwd(), "output/webserver"
    )|>
      ausgabe_text()
  })
  
  
  ######################################
  # Überwachung Button Erstelle Abrechnung (source("Erstelle Abrechnung.R"))
  ######################################
  observeEvent(input$ErstelleAbrechnung, {
    print(clc)

    #########
    # erstellen von Verzeichnissen
    #########
    dir.create("output/") |> suppressWarnings()
    dir.create("output/data/") |> suppressWarnings()
    
    ####################
    # Daten einlesen und konvertieren
    ####################
    source("source/calculate.R")
    
    ####################
    # Statistik-Bericht erstellen
    ####################
    StatistikErstellen()
    paste("Bericht: \nStatistik erstellt")|>
      writeLines()
    
    
    ####################
    # Jahresrechnung-Bericht erstellen
    ####################
    print(clc)
    JahresrechnungErstellen()
    
    paste("Bericht: \nJahresrechnung erstellt")|>
      writeLines()
    

    ####################
    # Bericht(e) Abrechnung pro Filmforführung erstellen
    ####################
    
    df_mapping__ <- mapping(c_Date)
    AbrechnungErstellen(df_mapping__, df_Abrechnung)
    
    print(clc)
    
    paste("Bericht: \nAlle Abrechnungen für Filmvorführungen wurden erstellt.")|>
      writeLines()
    
    ####################
    # Create webserver data 
    ####################
    webserver() 
    
    ####################
    # User Interaktion
    ####################
    print(clc)
    paste0("****************************************\n",
           "\n\nAlles wurde korrekt ausgeführt.", if(warnings()|>length()>0) {"\nEs Fehlen noch Datensätze. \nBitte beachte die Fehlermeldungen unten in orange."},"\n\n",
           paste0("Dateinen wurden im folgenden Verzeichniss erstellt:\n", getwd(), "/output/"),
           "\n****************************************\n")|>
      writeLines()
    
    paste0("Script wurde korrekt ausgeführt.",
           "\nWebserver erstellt.",
           paste0("\n",(paste0(getwd(),"/output/webserver/", "index.html")), 
                  sep = "")
           )|>
      ausgabe_text()
    renderText(calculate_warnings)
  })
  
  ######################################
  # Ausgabe aktualisieren
  ######################################
  output$ausgabe <- renderText({
    ausgabe_text()
  })
  
  ######################################
  # Update table with all the dates in the selected range
  ######################################
  output$dateTable <- renderTable({
    start_datum <- input$dateRange|>min()
    end_datum <- input$dateRange|>max()
    
    df_Abrechnung|>
      filter(between(Datum, start_datum, end_datum))|>
      mutate(Datum = format(Datum, "%d.%m.%Y"),
             Zeit  = format(Anfang, "%H%M"))|>
      select(Datum, Zeit, Filmtitel, `Suisa Nummer`)
  })
}

# Shiny-App starten
shinyApp(ui = ui, server = server)
