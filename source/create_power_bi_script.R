library(tidyverse)
library(rebus)
source("source/functions.R")
print(clc)
rm(list = ls())
# 
# merge_sources <- function(main_file, output_file) {
#   # Helper function to resolve dependencies recursively
#   resolve_dependencies <- function(file_path, resolved, visited) {
#     # Avoid circular dependencies
#     if (file_path %in% visited) return(resolved)
#     
#     # Mark file as visited
#     visited <- c(visited, file_path)
#     
#     # Read the content of the current file
#     if (!file.exists(file_path)) {
#       warning(paste("File not found:", file_path))
#       return(resolved)
#     }
#     content <- readLines(file_path, warn = FALSE)
#     
#     # Find all source("file.R") calls
#     matches <- regmatches(content, gregexpr('source\\(["\'](.+?)["\']\\)', content))
#     sources <- unique(unlist(lapply(matches, function(x) gsub('source\\(["\'](.+?)["\']\\)', '\\1', x))))
#     
#     # Resolve relative paths and process dependencies
#     base_dir <- dirname(file_path)
#     for (src in sources) {
#       dependency_path <- normalizePath(file.path(base_dir, src), mustWork = FALSE)
#       resolved <- resolve_dependencies(dependency_path, resolved, visited)
#     }
#     
#     # Add current file to the resolved list
#     resolved <- c(resolved, file_path)
#     return(resolved)
#   }
#   
#   # Start resolving from the main file
#   resolved_files <- resolve_dependencies(main_file, character(0), character(0))
#   resolved_files <- unique(resolved_files) # Ensure no duplicates
#   
#   # Write combined script to the output file
#   cat("", file = output_file) # Clear output file
#   for (file in resolved_files) {
#     if (file.exists(file)) {
#       cat(paste0("# --- Begin ", file, " ---\n"), file = output_file, append = TRUE)
#       cat(readLines(file, warn = FALSE), file = output_file, append = TRUE, sep = "\n")
#       cat(paste0("\n# --- End ", file, " ---\n\n"), file = output_file, append = TRUE)
#     } else {
#       warning(paste("File not found during write:", file))
#     }
#   }
#   
#   message("Combined script saved to ", output_file)
# }
# 
# merge_sources("source/calculate.R", "test.R")

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

find_connected_source_ <- function(c_filePath){
  c_filePath <- str_split(c_filePath, "/", simplify = T)|>
    as.vector()
  find_connected_source(c_filePath[1], c_filePath[2])
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
    
    if(nrow(df_index) == 0) return(NULL)
    
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
             )|>
      separate(link, into = c("link_path", "link_file"), sep = "/", remove = F)
    return(df_index)
  }else{
    return(NULL)
  }
}


find_connected_source("source", "functions.R")
find_connected_source("source", "calculate.R")

find_connected_source_("source/functions.R")
find_connected_source_("source/calculate.R")
find_connected_source_("source/read and convert.R")





c_path <- "source"
c_file <- "calculate.R"
c_file <- "functions.R"
find_connected_source(c_path, c_file)


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
         row_number = row_number())
df_source
 
# ##############################################################################################################
# # stitch together all connected sources
# ##############################################################################################################
# read all sources

ii <- 16
for (ii in nrow(df_source):1) {
  df_temp <- df_source|>
    filter(row_number == ii)
  df_temp

  if(df_temp$link == "") next

  jj <- 1
  for (jj in 1:nrow(df_temp)) {
    c_raw <- readLines(df_temp$link[jj])
    c_insert <- readLines(df_temp$filePath[jj])
    c_raw[df_temp$index[jj]]

    c_raw <- c(c_raw[1:(df_temp$index[jj] - 1)],
               c_insert,
               c_raw[(df_temp$index[jj] + 1):length(c_raw)]
               )
  }
  c_raw
  find_connected_source_(c_raw)
  write(c_raw,"powerBi Script.R")
}


c_filePath <- str_split(c_filePath, "/", simplify = T)|>
  as.vector()

c_raw|>
  find_connected_source_()

writeLines("\nPowerBi script generated")


