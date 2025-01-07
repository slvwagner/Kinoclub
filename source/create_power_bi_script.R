library(tidyverse)
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



find_all_source <- function(c_filePath) {
  ##############################################################################################################
  # find all connected source
  ##############################################################################################################
  df_source <- tibble(index = NA, 
                      link = c_filePath,
                      input_file = "",
                      checked = F,
                      level = 1L
                      )
  cnt <- 1L
  l_source <- list()
  while (TRUE) {
    df_temp <- df_source|>
      filter(!checked)
    
    if(nrow(df_temp) > 0){
      df_index <- find_connected_source(df_temp$link[1])
      df_temp$checked[1] <- TRUE
      if(!is.null(df_index)){
        cnt <- cnt + 1
        df_index <- df_index|>
          mutate(level = cnt)
        df_index
        
        df_source
        df_source <- 
          bind_rows(df_source|>
                       filter(checked),
                    df_temp,
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
  }
  return(df_source)
}

c_filePath <- "source/calculate.R"
find_all_source(c_filePath)|>
  print()

extract_source <- function(input_string) {
  # Use a regular expression to extract the path within quotes
  match <- regmatches(input_string, regexpr('(?<=").*?(?=")', input_string, perl = TRUE))
  return(match)
}

read_source <- function(c_path, c_file) {
  file_path <- if (c_path == "") c_file else file.path(c_path, c_file)
  c_raw <- readLines(file_path, warn = FALSE)
  return(c_raw)
}

check_source <- function(c_raw) {
  indices <- which(grepl('source\\(["\'](.+?)["\']\\)', c_raw, perl = TRUE))
  if (length(indices) == 0) {
    return(NULL)
  }
  
  links <- sapply(c_raw[indices], extract_source, USE.NAMES = FALSE)
  links <- links[!grepl("#", links) & !grepl("clc.R", links)]
  
  if (length(links) == 0) {
    return(NULL)
  }
  
  df_index <- data.frame(
    index = indices,
    link = links,
    stringsAsFactors = FALSE
  )
  
  return(df_index)
}

find_connected_source_ <- function(c_path, c_file) {
  c_raw <- read_source(c_path, c_file)
  df_index <- check_source(c_raw)
  
  if (!is.null(df_index)) {
    input_file <- if (c_path != "") file.path(c_path, c_file) else c_file
    df_index$input_file <- input_file
    df_index$checked <- FALSE
    return(df_index)
  } else {
    return(NULL)
  }
}

find_connected_source <- function(c_filePath) {
  if (grepl("/", c_filePath)) {
    components <- strsplit(c_filePath, "/", fixed = TRUE)[[1]]
    c_path <- paste(head(components, -1), collapse = "/")
    c_file <- tail(components, 1)
    return(find_connected_source_(c_path, c_file))
  } else {
    return(find_connected_source_("", c_filePath))
  }
}

find_all_source <- function(c_filePath) {
  # Initialize a data frame to track sources
  df_source <- data.frame(
    index = NA,
    link = c_filePath,
    input_file = "",
    checked = FALSE,
    level = 1L,
    stringsAsFactors = FALSE
  )
  
  cnt <- 1L
  while (TRUE) {
    df_temp <- df_source[!df_source$checked, ]
    
    if (nrow(df_temp) > 0) {
      df_index <- find_connected_source(df_temp$link[1])
      df_source$checked[df_source$link == df_temp$link[1]] <- TRUE
      
      if (!is.null(df_index)) {
        cnt <- cnt + 1
        df_index$level <- cnt
        df_source <- rbind(df_source, df_index)
      }
    } else {
      break
    }
  }
  
  return(df_source)
}

c_filePath <- "source/calculate.R"
find_all_source(c_filePath)|>
  print()