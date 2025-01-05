## read in all kiosk files

library(rebus)
library(tidyverse)
library(lubridate)
library(readxl)
source("source/functions.R")


######################################################################## 
# Read in all abos
######################################################################## 
library(readr)
all_abos <- read_delim("Input/advance tickets/all_abos.txt",
                       delim = "\t", escape_double = FALSE,
                       col_types = cols(Abo = col_factor(),
                                        id = col_double(),
                                        creation = col_date(format = "%Y-%m-%d"),
                                        first_use = col_date(format = "%Y-%m-%d"),
                                        last_use = col_date(format = "%Y-%m-%d"),
                                        expiration = col_date(format = "%Y-%m-%d"),
                                        amount = col_double(),
                                        count_use = col_integer()),
                       trim_ws = TRUE)

all_abos$Abo|>
  levels()

all_abos|>
  filter(Abo == "foerderer", creation == "2024-09-09")

all_abos|>
  filter(Abo == "foerderer", creation != "2024-09-09")

all_abos|>
  filter(Abo == "foerderer", count_use == 2)
all_abos|>
  filter(Abo == "foerderer", count_use == 1)
all_abos|>
  filter(Abo == "foerderer", count_use == 0)


all_abos|>
  filter(Abo == "abo")

all_abos|>
  filter(Abo == "abo", expiration > as.Date(paste0(Abrechungsjahr,"-12-31"))
         )


all_abos|>
  filter(Abo == "foerderer")

all_abos|>
  filter(Abo == "foerderer", expiration < as.Date(paste0(Abrechungsjahr,"-12-31"))
  )

all_abos|>
  filter(Abo == "foerderer", expiration > as.Date(paste0(Abrechungsjahr,"-12-31"))
  )

library(readr)
atelierkino_abo <- read_delim("Input/advance tickets/atelierkino_abo.txt", 
                              delim = "\t", escape_double = FALSE, 
                              col_types = cols(creation = col_date(format = "%Y-%m-%d"), 
                                               first_use = col_date(format = "%Y-%m-%d"), 
                                               last_use = col_date(format = "%Y-%m-%d"), 
                                               expiration = col_date(format = "%Y-%m-%d"), 
                                               count_use = col_integer()), trim_ws = TRUE)

atelierkino_abo|>
  filter(expiration > as.Date(paste0(Abrechungsjahr,"-12-31")))

atelierkino_foerderer <- read_delim("Input/advance tickets/atelierkino_foerderer.txt", 
                                    delim = "\t", escape_double = FALSE, 
                                    col_types = cols(creation = col_date(format = "%Y-%m-%d"), 
                                                     first_use = col_date(format = "%Y-%m-%d"), 
                                                     last_use = col_date(format = "%Y-%m-%d"), 
                                                     expiration = col_date(format = "%Y-%m-%d"), 
                                                     count_use = col_integer()), trim_ws = TRUE)

atelierkino_foerderer

atelierkino_foerderer|>
  filter(expiration > as.Date(paste0(Abrechungsjahr,"-12-31")))


atelierkino_gutschein <- read_delim("Input/advance tickets/atelierkino_gutschein.txt", 
                                    delim = "\t", escape_double = FALSE, 
                                    col_types = cols(creation = col_date(format = "%Y-%m-%d"), 
                                                     first_use = col_date(format = "%Y-%m-%d"), 
                                                     last_use = col_date(format = "%Y-%m-%d"), 
                                                     expiration = col_date(format = "%Y-%m-%d"), 
                                                     amount = col_double(), count_use = col_integer()), 
                                    trim_ws = TRUE)
atelierkino_gutschein
atelierkino_gutschein|>
  filter(expiration > as.Date(paste0(Abrechungsjahr,"-12-31")))

######################################################################## 
# files to read in 
######################################################################## 
c_path <- "input/advance tickets/"
c_files <- c(list.files(path = c_path,pattern = "Abo Gutscheine", recursive = T), list.files(path = c_path, pattern = "Kiosk", recursive = F) )
c_files


l_raw <- lapply(c_files, function (x) suppressWarnings(readLines(paste0(c_path,x))))
l_raw

## extract file date 
p <- capture(one_or_more(DGT)%R%DOT%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT))%R%DOT%R%"txt"

c_fileDate <- str_match(c_files, p)[,2]
c_fileDate

p <- rebus::or(rebus::optional("-")%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT),
               rebus::optional("-")%R%one_or_more(DGT)
               )

######################################################################## 
# Verkauf: Abo 
########################################################################

l_Abos <- l_raw|>
  lapply(function(x){
    y <- x[str_detect(x,START%R%"Verkauf Abo")]|>
      str_split(pattern = "\t", simplify = T)
    y
    c_lenght <- ncol(y)
    c_lenght
    
    if(c_lenght == 7){ # mit Korrekturbuchungen
      if(nrow(y) == 1){ #matrix convertierung in vector verhindern ncol = 1
        y <- y[,c(1:2,4:5,7)]|>
          matrix(ncol = 5)
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          lapply(as.numeric)|>
          unlist()|>
          matrix(ncol = 4)
      }else{
        y <- y[,c(1:2,4:5,7)]
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          apply(2, as.numeric)
      }
      y
      
      colnames(y) <- c("Einzelpreis", "Anzahl", "Korrektur", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)|>
        mutate(Anzahl = if_else(is.na(Korrektur), Anzahl,Anzahl + Korrektur))|>
        select(-Korrektur)
      
    }else if(c_lenght == 5){ # keine Korrekturbuchungen
      y <- y[,c(1:3,5)]
      c_Verkaufsartikel <- y[,1]
      y <- y[,2:ncol(y)]|>
        apply(2, as.numeric)
      colnames(y) <- c("Einzelpreis", "Anzahl", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)
    }
  })

names(l_Abos) <- dmy(c_fileDate)
l_Abos

df_Abo_Verkauf <- bind_rows(l_Abos,.id = "Datum")|>
  mutate(Datum = as.Date(Datum))

df_Abo_Verkauf

######################################################################## 
# Verkauf: Kinogutschein
######################################################################## 
p <- rebus::or(rebus::optional("-")%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT),
               rebus::optional("-")%R%one_or_more(DGT)
)

l_Abos <- l_raw|>
  lapply(function(x){
    y <- x[str_detect(x,START%R%"Verkauf Kinogutschein")]|>
      str_split(pattern = "\t", simplify = T)
    y
    c_lenght <- ncol(y)
    c_lenght
    
    if(c_lenght == 7){ # mit Korrekturbuchungen
      if(nrow(y) == 1){ #matrix convertierung in vector verhindern ncol = 1
        y <- y[,c(1:2,4:5,7)]|>
          matrix(ncol = 5)
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          lapply(as.numeric)|>
          unlist()|>
          matrix(ncol = 4)
      }else{
        y <- y[,c(1:2,4:5,7)]
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          apply(2, as.numeric)
      }
      y
      
      colnames(y) <- c("Einzelpreis", "Anzahl", "Korrektur", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)|>
        mutate(Anzahl = if_else(is.na(Korrektur), Anzahl,Anzahl + Korrektur))|>
        select(-Korrektur)
      
    }else if(c_lenght == 5){ # keine Korrekturbuchungen
      y <- y[,c(1:3,5)]
      c_Verkaufsartikel <- y[,1]
      y <- y[,2:ncol(y)]|>
        apply(2, as.numeric)
      colnames(y) <- c("Einzelpreis", "Anzahl", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)
    }
  })

names(l_Abos) <- dmy(c_fileDate)

df_Kinogutschein_Verkauf <- bind_rows(l_Abos,.id = "Datum")|>
  mutate(Datum = as.Date(Datum))
  
df_Kinogutschein_Verkauf


######################################################################## 
# Eingelöst: Kinogutschein
######################################################################## 
p <- rebus::or(rebus::optional("-")%R%one_or_more(DGT)%R%DOT%R%one_or_more(DGT),
               rebus::optional("-")%R%one_or_more(DGT)
)

l_Abos <- l_raw|>
  lapply(function(x){
    y <- x[str_detect(x,START%R%"Kinogutschein")]|>
      str_split(pattern = "\t", simplify = T)
    y
    c_lenght <- ncol(y)
    c_lenght
    
    if(c_lenght == 7){ # mit Korrekturbuchungen
      if(nrow(y) == 1){ #matrix convertierung in vector verhindern ncol = 1
        y <- y[,c(1:2,4:5,7)]|>
          matrix(ncol = 5)
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          lapply(as.numeric)|>
          unlist()|>
          matrix(ncol = 4)
      }else{
        y <- y[,c(1:2,4:5,7)]
        c_Verkaufsartikel <- y[,1]
        y <- y[,2:ncol(y)]|>
          apply(2, as.numeric)
      }
      y
      
      colnames(y) <- c("Einzelpreis", "Anzahl", "Korrektur", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)|>
        mutate(Anzahl = if_else(is.na(Korrektur), Anzahl,Anzahl + Korrektur))|>
        select(-Korrektur)
      
    }else if(c_lenght == 5){ # keine Korrekturbuchungen
      y <- y[,c(1:3,5)]
      c_Verkaufsartikel <- y[,1]
      y <- y[,2:ncol(y)]|>
        apply(2, as.numeric)
      colnames(y) <- c("Einzelpreis", "Anzahl", "Betrag")
      bind_cols(Verkaufsartikel = c_Verkaufsartikel, y)
    }
  })

names(l_Abos) <- dmy(c_fileDate)
l_Abos

df_Kinogutschein_Eingeloest <- bind_rows(l_Abos,.id = "Datum")|>
  mutate(Datum = as.Date(Datum))|>
  left_join(df_show, by = join_by(Datum))

