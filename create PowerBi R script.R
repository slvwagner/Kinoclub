library(tidyverse)
library(rebus)
rm(list = ls())

###########################################################################################
# Find source link in file source("xx.R") and return the index where to insert the additional
# source file
# Input raw file text
# Return file and index by data frame
###########################################################################################
find_source_files <- function(c_raw) {
  # Finde alle source Eintäge
  df_index <- tibble(index = (1:length(c_raw))[c_raw|>str_detect("source/")],
                     c_raw = c_raw[index])
  # remove anything not needed
  df_index <- df_index|>
    mutate(c_raw = str_remove_all(c_raw,"\""))|>
    filter(!str_detect(c_raw,"temp.Rmd"),
           !str_detect(c_raw,"rmarkdown::"),
           !str_detect(c_raw,"<-"),
           !str_detect(c_raw,"#")
           )
  
  df_index <- df_index|>
    separate(c_raw, into = c("a", "file"), sep = "/", remove = F)|>
    mutate(a = NULL,
           path = str_extract(c_raw, one_or_more(WRD)%R%literal("/")),
           file = str_remove(file, literal(")")),
           filePath = paste0(path,file),
           c_raw = NULL,
           checked = F
           )
  
  if(nrow(df_index)>0) return(df_index)
  else return(NULL)
}

###########################################################################################
# check source for othe linked source 
###########################################################################################
c_file <- "Erstelle Abrechnung.R"
c_raw <- readLines(c_file)

df_source <- find_source_files(c_raw)|>
  mutate(link = c_file)
df_source

df_source <- df_source|>
  bind_rows(tibble(
    index = 0,
    file = c_file, 
    path = "", 
    filePath = c_file,
    checked = T,
    link = ""
  ))
df_source

l_raw <- list(c_raw)
cnt <- 1
while (TRUE) {
  df_temp <- df_source|>
    filter(checked == F)
  df_temp
  if(nrow(df_temp) > 0){
    c_raw <- readLines(df_temp$filePath[1])
    df_temp$checked[1] <- TRUE
    df_source <-  bind_rows(df_source|>
                              filter(checked == T),
                            df_temp
                            )
    df_source
    if(!is.null(find_source_files(c_raw))){
      df_source <- 
        bind_rows(
          df_source,
          find_source_files(c_raw)|>
            mutate(link = df_temp$filePath[1])
        )
    }
  }else{
    break
  }
}
l_raw <- lapply(df_source$filePath, readLines)
names(l_raw) <- df_source$filePath

df_source|>
  print()

df_source <- df_source|>
  distinct(file, .keep_all = T)

df_source|>
  print()




