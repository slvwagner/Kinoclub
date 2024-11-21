library(tidyverse)
library(rebus)
source("source/functions.R")
print(clc)
rm(list = ls())

extract_source <- function(input_string) {
  # Use a regular expression to extract the path within quotes
  match <- regmatches(input_string, regexpr('(?<=").*?(?=")', input_string, perl = TRUE))
  return(match)
}

read_source <- function(c_path, c_file){
  if(c_path == ""){
    c_raw <- paste0(c_path, c_file)|>
      readLines()  
  }else{
    c_raw <- paste0(c_path, "/",c_file)|>
      readLines()
  }
  return(c_raw)
}

check_source <- function(c_raw){
  df_index <- tibble(index = (1:length(c_raw))[c_raw|>str_detect('source\\(["\'](.+?)["\']\\)')])
  df_index
  if(is.null(df_index)) {
    return(NULL)
  }else{
    df_index <- df_index|>
      mutate(link = c_raw[index]|>
               extract_source()
             )|>
      filter(!str_detect(link, "#"), !str_detect(link, "clc.R"))
    df_index
    if(nrow(df_index) == 0) return(NULL)
    else return(df_index)
  }
}

c_raw <- readLines("source/calculate.R")
readLines("source/calculate.R")|>
  check_source()
readLines("source/functions.R")|>
  check_source()
readLines("Erstelle Abrechnung.R")|>
  check_source()


find_connected_source_ <- function(c_path, c_file){
  c_raw <- read_source(c_path, c_file)
  df_index <- check_source(c_raw)

  if(!is.null(df_index)){
    df_index <- df_index|>
      mutate(input_file = if_else(c_path != "",
                            c(c_path, c_file)|>
                              paste0(collapse = "/"),
                            c_file
                            ),
             checked = F)
    df_index
    
    return(df_index)
  }else{
    return(NULL)
  }
}

find_connected_source <- function(c_filePath){
  if(str_detect(c_filePath, "/")){
    c_filePath <- str_split(c_filePath, "/", simplify = T)|>
      as.vector()
    return(find_connected_source_(c_filePath[1], c_filePath[2]))
  }else{
    return(find_connected_source_("",c_filePath))
  }
}

find_connected_source_("source", "functions.R")
find_connected_source_("source", "calculate.R")

find_connected_source("source/functions.R")
find_connected_source("source/calculate.R")
find_connected_source("source/read and convert.R")
find_connected_source("Erstelle Abrechnung.R")


c_path <- "source"
c_file <- "calculate.R"
c_file <- "functions.R"

c_path <- ""
c_file <- "read and convert.R"
c_file <- "Erstelle Abrechnung.R"

##############################################################################################################
# find all connected source
##############################################################################################################
df_source <- tibble(index = NA, 
                    link = "Erstelle Abrechnung.R",
                    input_file = "",
                    checked = F,
                    level = 1L
                    )
df_source

cnt <- 2L
l_source <- list()
while (TRUE) {
  df_source
  df_temp <- df_source|>
    filter(!checked)
  df_temp
  
  if(nrow(df_temp) > 0){
    df_index <- find_connected_source(df_temp$link[1])
    df_temp$checked[1] <- TRUE
    
    if(!is.null(df_index)){
        df_index <- df_index|>
          mutate(level = cnt)
        df_index
        
        df_source
        df_source <- 
          bind_rows(df_source|>
                       filter(checked),
                    df_temp|>slice(1),
                    df_index
          )
    }else{
      df_source <- 
        bind_rows(df_source|>
                    filter(checked),
                  df_temp
        )
    }
  }else{
    break
  }
  writeLines(paste("\n*******************\ncnt:", cnt))
  print(df_source)

  cnt <- cnt + 1
}

  
# # ##############################################################################################################
# # # stitch together all connected sources
# # ##############################################################################################################
# # read all sources
# 
# ii <- 16
# for (ii in nrow(df_source):1) {
#   df_temp <- df_source|>
#     filter(row_number == ii)
#   df_temp
# 
#   if(df_temp$link == "") next
# 
#   jj <- 1
#   for (jj in 1:nrow(df_temp)) {
#     c_raw <- readLines(df_temp$link[jj])
#     c_insert <- readLines(df_temp$filePath[jj])
#     c_raw[df_temp$index[jj]]
# 
#     c_raw <- c(c_raw[1:(df_temp$index[jj] - 1)],
#                c_insert,
#                c_raw[(df_temp$index[jj] + 1):length(c_raw)]
#                )
#   }
#   c_raw
#   check_source(c_raw)
#   write(c_raw,"powerBi Script.R")
# }
# 
# 
# c_filePath <- str_split(c_filePath, "/", simplify = T)|>
#   as.vector()
# 
# c_raw|>
#   find_connected_source_()
# 
# writeLines("\nPowerBi script generated")
# 
# 
