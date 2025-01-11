library(rebus)
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)

source("source/functions.R")

######################################################################## 
# Read in all abos
######################################################################## 
if(!file.exists("Input/advance tickets/atelierkino_abo.txt")) stop("Die Datei: .../Input/advance tickets/atelierkino_abo.txt wurde nicht gefunden.")
atelierkino_abo <- read_delim("Input/advance tickets/atelierkino_abo.txt", 
                              delim = "\t", escape_double = FALSE, 
                              col_types = cols(creation = col_date(format = "%Y-%m-%d"), 
                                               first_use = col_date(format = "%Y-%m-%d"), 
                                               last_use = col_date(format = "%Y-%m-%d"), 
                                               expiration = col_date(format = "%Y-%m-%d"), 
                                               count_use = col_integer()), trim_ws = TRUE)

if(!file.exists("Input/advance tickets/atelierkino_foerderer.txt")) stop("Die Datei: .../Input/advance tickets/atelierkino_foerderer.txt wurde nicht gefunden.")
atelierkino_foerderer <- read_delim("Input/advance tickets/atelierkino_foerderer.txt", 
                                    delim = "\t", escape_double = FALSE, 
                                    col_types = cols(creation = col_date(format = "%Y-%m-%d"), 
                                                     first_use = col_date(format = "%Y-%m-%d"), 
                                                     last_use = col_date(format = "%Y-%m-%d"), 
                                                     expiration = col_date(format = "%Y-%m-%d"), 
                                                     count_use = col_integer()), trim_ws = TRUE)

if(!file.exists("Input/advance tickets/atelierkino_gutschein.txt")) stop("Die Datei: .../Input/advance tickets/atelierkino_gutschein.txt wurde nicht gefunden.")
atelierkino_gutschein <- read_delim("Input/advance tickets/atelierkino_gutschein.txt", 
                                    delim = "\t", escape_double = FALSE, 
                                    col_types = cols(creation = col_date(format = "%Y-%m-%d"), 
                                                     first_use = col_date(format = "%Y-%m-%d"), 
                                                     last_use = col_date(format = "%Y-%m-%d"), 
                                                     expiration = col_date(format = "%Y-%m-%d"), 
                                                     amount = col_double(), count_use = col_integer()), 
                                    trim_ws = TRUE)

