library(tidyverse)
library(rebus)
source("source/functions.R")
print(clc)
rm(list = ls())

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
  tibble(index = (1:length(c_raw))[c_raw|>str_detect("source")])
}

find_connected_source <- function(c_path, c_file){
  c_raw <- read_source(c_path, c_file)
  df_index <- check_source(c_raw)
  
  if(nrow(df_index)>0){
    df_index <- df_index|>
      mutate(raw = c_raw[index])|>
      separate(raw, into = c("path", "file_"), sep = "/", remove = T )|>
      filter(path != "",
             path != c_path,
             !str_detect(path ,"#"),
             !str_detect(file_, DOT%R%"Rmd"))|>
      mutate(path = str_remove(path,"source")|>
               str_remove(literal("("))|>
               str_remove(literal("\"")),
             file_ = file_|>
               str_remove(literal(")"))|>
               str_remove(literal("\""))
             )|>
      suppressWarnings()
    df_index
    
    df_index <- df_index|>
      mutate(link = if_else(c_path != "",
                            c(c_path, c_file)|>
                              paste0(collapse = "/"),
                            c_file
                            ),
             checked = F)
    df_index <- df_index|>
      mutate(filePath = if_else(path == "",
                                paste0(path, file_),
                                paste0(path, "/",file_)
                                )|>
               str_trim(),
             path = str_trim(path)
      )
    return(df_index)
  }else{
    return(NULL)
  }
}
c_path <- "source"
c_file <- "calculate.R"
c_path <- ""
c_file <- "read and convert.R"
c_file <- "Erstelle Abrechnung.R"
find_connected_source(c_path, c_file)

##############################################################################################################
# find all connected source
##############################################################################################################
df_source <- tibble(path = c_path,
                    file_ = c_file,
                    filePath = paste0(c_path,"/", c_file),
                    index = 1, 
                    checked = F,
                    link = "",
                    level = 1L
                    )
df_source

cnt <- 1L
l_source <- list()
while (TRUE) {
  # paste0("\n*****************************  ",cnt,"  *****************************\n")|>writeLines()
  df_source
  df_temp <- df_source|>
    filter(!checked)|>
    mutate(level = cnt)
  df_temp

  if(nrow(df_temp) > 0){
    l_temp <- list()
    df_temp$checked[1] <- TRUE
    for (ii in 1:nrow(df_temp)) {
      # paste0("\nfor loop: ", ii )|>
      #   writeLines()
      if(!is.null(find_connected_source(df_temp$path[ii], df_temp$file_[ii]))){
        l_temp[[ii]] <- 
          find_connected_source(df_temp$path[ii], df_temp$file_[ii])|>
          mutate(level = cnt)
        # paste0("\nFound new source loop ii: ", ii )|>
        #   writeLines()
      }
    }
  }else{
    break
  }
  df_source <- 
    bind_rows(df_temp|>slice(1),
              l_temp|>bind_rows())
  
  l_source[[cnt]] <- df_source|>
    mutate(checked = T)
  # print(df_source)  
  cnt <- cnt + 1
}

df_source <- l_source|>
  bind_rows()|>
  mutate(filePath = str_remove(filePath, START%R%literal("/")),
         index = row_number())
# df_source|>
#   print()


##############################################################################################################
# stitch together all connected sources
##############################################################################################################

# read all sources
l_raw <- df_source$filePath|>
  lapply(readLines)
ii <- 16
for (ii in sort(df_source$index, decreasing = T)) {
  df_temp <- df_source|>
    filter(index == ii)
  df_temp

  if(df_temp$link == "") next
  
  l_raw <- df_temp$link|>
    lapply(readLines)
  
  for (jj in 1:length(l_raw)) {
    df_index <- l_raw[[jj]]|>
      check_source()
    df_index|>
      mutate(c_source = "df")
    
  }
  
}

writeLines("\nPowerBi script generated")


