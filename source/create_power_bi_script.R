library(tidyverse)
library(rebus)
rm(list = ls())

find_connected_source <- function(c_file){
  c_raw <- readLines(c_file)
  # Ändern des Templates mit user eingaben (ii <- ??) verwendet für Datum
  index <- (1:length(c_raw))[c_raw|>str_detect("source")]
  index
  
  c_temp <- str_split(c_raw[index],"/", simplify = T)
  c_temp
  
  c_file <- c_temp[,2]|>
    str_remove_all(literal(")"))|>
    str_remove_all(literal("\""))
  c_file
  
  c_path <- str_split(c_temp[,1], literal("("), simplify = T)[,1]|>
    paste0("/")
  c_path
  
  tibble(path = c_path,
         file = c_file, 
         filePath = paste0(c_path, c_file),
         index, 
         checked = F
         )|>
    filter(!str_detect(file, DOT%R%"Rmd"),
           !str_detect(path, "#"))|>
    mutate(path = str_trim(path),
           file = str_trim(file),
           filePath = str_trim(filePath))
}

c_file <- "source/calculate.R"
find_connected_source(c_file)|>
  print()


c_file <- "Erstelle Abrechnung.R"
find_connected_source(c_file)|>
  print()


##############################################################################################################
