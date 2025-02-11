library(rebus)
library(tidyverse)

# Error handling
c_path <- "Input/WordPress/"
c_filePath <- list.files(c_path,full.names = T)

if(length(c_filePath) > 1)   {
  warning(paste0("\nIm ordner: \n\".../", c_path, "\" darf nur eine Datei abgespeichert werden! Es sind aber diese: ", c_filePath))
  } else if (length(c_filePath) < 1)  {
    stop(paste0("\nIm ordner: \".../", c_path, "\" ist keine Datei vorhanden!",
                "\nBitte auf dem Wordpress backend für die Kinoklub Homepage",
                "\nherunterladen und abspeicher oder über GUI hochladen.", c_filePath)
    )
  }

# read in csv export from wordpress homepage 
df_Filmvorschlag <- df_Filmvorschlag <-
  read_csv(
    c_filePath,
    col_types = cols(
      ID = col_integer(),
      Excerpt = col_skip(),
      Date = col_datetime(format = "%Y-%m-%d"),
      `Post Type` = col_skip(),
      `Image Title` = col_skip(),
      `Image Caption` = col_skip(),
      `Image Description` = col_skip(),
      `Image Alt Text` = col_skip(),
      `Attachment URL` = col_skip(),
      Kategorien = col_skip(),
      altersfreigabe = col_integer(),
      bildupload = col_skip(),
      review = col_skip(),
      playdate = col_skip(),
      `_fpsm_form_alias` = col_skip(),
      rmp_vote_count = col_integer(),
      rmp_rating_val_sum = col_integer(),
      rmp_avg_rating = col_integer(),
      `_wp_trash_meta_status` = col_skip(),
      `_wp_trash_meta_time` = col_skip(),
      `_wp_desired_post_slug` = col_skip(),
      Status = col_skip(),
      `Author ID` = col_integer(),
      Slug = col_skip(),
      Format = col_skip(),
      Template = col_skip(),
      Parent = col_skip(),
      `Parent Slug` = col_skip(),
      Order = col_skip(),
      `Post Modified Date` = col_datetime(format = "%Y-%m-%d")
    )
  )

df_Filmvorschlag <- df_Filmvorschlag|>
  select(-starts_with("Author"),
         -ends_with("Status"))

# clean up Content column
p <- or("<"%R%one_or_more(WRD)%R%">",
        "</"%R%one_or_more(WRD)%R%">",
        "<"%R%one_or_more(WRD)%R%one_or_more(DGT)%R%">",
        "</"%R%one_or_more(WRD)%R%one_or_more(DGT)%R%">",
        "\\r",
        "\\n"
        )

df_Filmvorschlag$Content <- df_Filmvorschlag$Content|>
  lapply(function(x){
    str_trim(x)|>
      str_remove_all(p)
  })|>
  unlist()

df_Filmvorschlag$Content[6]

p <- or("\"",
        "\\\"")

for (ii in 1:nrow(df_Filmvorschlag)) {
  x <- df_Filmvorschlag$Content[ii]|>
    str_remove_all(p)
  run <- TRUE
  while (run) {
    m_pos <- x|>
      str_locate_all(or("<",
                        ">"
                        )
      )
    m_pos <- m_pos[[1]]
    
    if(nrow(m_pos)>1){
      x <- x|>
        substring(m_pos[,"start"][2]+1,nchar(x))
    }else{
      df_Filmvorschlag$Content[ii] <- x
      break
    }
  }
}

# Matching column Filmtitle
df_Filmvorschlag <- df_Filmvorschlag|>
  mutate(Filmtitel = tolower(Title)|>str_squish(),
         Filmtitel = str_remove(Filmtitel, OPEN_PAREN%R%one_or_more(WRD)%R%SPC%R%one_or_more(WRD)%R%SPC%R%"erhältlich"%R%CLOSE_PAREN),
         Filmtitel = str_extract_all(Filmtitel,one_or_more(WRD))|>lapply(paste, collapse = " ")|>unlist()|>str_squish()
  )|>
  arrange(Filmtitel)

df_Filmvorschlag

# find the removed by distinct Films
df_removed <- df_Filmvorschlag|>
  select(ID, Title, Filmtitel)|>
  anti_join(
    df_Filmvorschlag|>
      distinct(Filmtitel, .keep_all = T)|>
      select(ID, Title, Filmtitel),
    by = c("ID")
    )
df_removed

# consolidate the multiple Films to single row
df_temp <- df_Filmvorschlag|>
  filter(Filmtitel %in% df_removed$Filmtitel)|>
  distinct(Filmtitel)|>
  pull()|>
  lapply(function(ii){
    df_Filmvorschlag|>
      filter(Filmtitel == ii)|>
      as.matrix()|>
      apply(2, paste, collapse = ";")|>
      t()|>
      as_tibble()
  })|>
  bind_rows()|>
  mutate(Filmtitel = df_removed$Filmtitel)
df_temp

# find affected rows (IDs)
c_IDs <- df_temp$ID|>str_split(";")|>unlist()|>as.integer()

# Filmvorschläge reduziert 
df_Filmvorschläge_reduced <- bind_rows(
  df_temp,
  df_Filmvorschlag|>
    filter(!(ID %in% c_IDs))|>
    as.matrix()|>
    as_tibble()
)|>
  apply(1, str_remove, pattern = "NA;NA")|>
  apply(2, str_remove, pattern = "NA;")|>
  apply(1, str_remove, pattern = ";NA")|>
  apply(2, function(x){
    ifelse(x == "", NA, x)
  })|>
  as_tibble()
names(df_Filmvorschläge_reduced) <- names(df_Filmvorschlag)
df_Filmvorschläge_reduced


# Filmtitle anpassung
s_df_Procinema <- s_df_Procinema|>
  mutate(Filmtitel = tolower(Filmtitel)|>str_squish(),
         Filmtitel = str_extract_all(Filmtitel,one_or_more(WRD))|>lapply(paste, collapse = " ")|>unlist()|>str_squish()
         )
s_df_Procinema

df_temp <- df_Filmvorschläge_reduced|>
  rename(Suisanummer = suisa)|>
  select(ID, Title, Filmtitel, Suisanummer)|>
  separate(Suisanummer, into = c("suisa1","suisa2"), sep = ";")|>
  mutate(Suisanummer = if_else(is.na(suisa1), suisa2, suisa1))|>
  select(-suisa1, -suisa2)|>
  suppressWarnings()
df_temp

# join by Filmtitel
df_temp <- df_temp|>
  left_join(
    s_df_Procinema,
    by = join_by(Filmtitel)
    )|>
  rename(Suisanummer = Suisanummer.x)|>
  select(-Suisanummer.y,-ration_from_total_box, -Besucherzahlen_ch)
df_temp

# join by Suisanummer
df_temp <- df_temp|>
  select(ID, Title, Filmtitel, Suisanummer)|>
  left_join(
    s_df_Procinema,
    by = join_by(Suisanummer)
  )|>
  rename(Filmtitel = Filmtitel.x)|>
  select(-Filmtitel.y,-ration_from_total_box, -Besucherzahlen_ch, -Title)|>
  separate(ID, into = c("ID1","ID2"), sep = ";")|>
  suppressWarnings()|>
  rename(ID = ID1)|>
  select(-ID2)|>
  mutate(ID = as.integer(ID))
df_temp

# rename 
df_Filmvorschläge_reduced <- df_Filmvorschläge_reduced|>
  rename(Suisanummer = suisa)|>
  separate(ID, into = c("ID1","ID2"), sep = ";")|>
  suppressWarnings()|>
  rename(ID = ID1)|>
  select(-ID2)|>
  mutate(ID = as.integer(ID))
df_Filmvorschläge_reduced

# jkllsjdf
df_Filmvorschlag <- df_Filmvorschläge_reduced|>
  filter(ID %in% df_temp$ID)|>
  mutate(ID = as.integer(ID))|>
  left_join(df_temp,
            by = join_by(ID))|>
  rename(
    Filmtitel = Filmtitel.x,
    Suisanummer = Suisanummer.x
    )|>
  select(-Filmtitel.y, -Suisanummer.y)|>
  select("Suisanummer", "Filmtitel", "Content","Spielwochen", "Besucherzahl", 
         "Schlagwörter", 
         "trailerlink", 
         "Schauspieler",   
         "rmp_avg_rating", "rmp_vote_count",
         "youtubeembed", "autorname",  "releasedate", "verleih", 
         "nameverleiher","filmserie", "Post Modified Date", `Image Featured` 
         )|>
  arrange(desc(Besucherzahl))|>
  mutate(Verleiher = if_else(is.na(verleih), nameverleiher,verleih),
         trailerlink = if_else(is.na(trailerlink), trailerlink, youtubeembed)
  )|>
  select(-verleih, -nameverleiher)|>
  rename(Kinoklubmitglied = autorname,
         Rating = rmp_avg_rating,
         Trailerlink = trailerlink
  )

df_Filmvorschlag

remove(df_removed, df_temp, df_Filmvorschläge_reduced)

################################################################################################################################
# Auswertung Plot 
df_Filmvorschlag|>
  mutate(`Durchschnittliche-\nbewertung` = factor(Rating))|>
  ggplot(aes(Filmtitel, (rmp_vote_count), fill = `Durchschnittliche-\nbewertung`))+ 
  # scale_y_continuous(breaks = 0:max(df_Filmvorschlag$rmp_vote_count, na.rm = T))+
  geom_col(na.rm = TRUE)+
  labs(y = "Anzahl Bewertungen",
       x = "Filmtitel",
       color = "Durchschnittlichebewertung")+
  coord_flip()|>
  suppressMessages()

c_fileName <- "output/data/Filmvorschläge.jpg"

ggplot2::ggsave(c_fileName, width = 25, height = 3 + nrow(df_Filmvorschlag) * 0.35, units = "cm")


library(lubridate)
library(stringr)

# function to extract date from string in different formats 
parse_dates <- function(date_vector) {
  parsed_dates <- sapply(date_vector, function(date) {
    if (is.na(date) || date == "") {
      return(NA)
    }
    
    # Handle multiple dates separated by ";"
    date_parts <- unlist(strsplit(date, ";"))
    parsed_part <- sapply(date_parts, function(part) {
      part <- str_trim(part)
      
      # Check and parse different formats
      if (str_detect(part, "^\\d{4}-\\d{2}-\\d{2}$")) {
        return(as.Date(part))  # YYYY-MM-DD
      } else if (str_detect(part, "^\\d{4}$")) {
        return(as.Date(paste0(part, "-01-01")))  # YYYY assumed to be start of year
      } else if (str_detect(part, "^\\d{2}\\.\\d{2}\\.\\d{4}$")) {
        return(dmy(part))  # DD.MM.YYYY
      } else if (str_detect(part, "^[A-Za-z]+, \\d{1,2} [A-Za-z]+, \\d{4}$")) {
        return(dmy(str_remove(part, "^[A-Za-z]+, ")))  # "Day, DD Month, YYYY"
      }
      return(NA)  # Return NA if format is unknown
    })
    
    # Return the first valid parsed date (if multiple found)
    return(parsed_part[!is.na(parsed_part)][1])
  })
  
  return(as.Date(parsed_dates, origin = "1970-01-01"))
}

df_Filmvorschlag <- df_Filmvorschlag|>
  mutate(Trailerlink = if_else(is.na(Besucherzahl), Trailerlink, youtubeembed),
         Besucherzahl = if_else(is.na(Besucherzahl), FALSE, TRUE)
         )|>
  select(-rmp_vote_count, -youtubeembed, -Spielwochen, -Schauspieler)|>
  mutate(filmserie = str_remove(filmserie, "keine"),
         filmserie = str_remove(filmserie, ";keine"),
         filmserie = if_else(filmserie == "",NA,filmserie),
         filmserie = str_remove(filmserie, ";"%R%END)
         )|>
  rename("Link zum Trailer" = Trailerlink,
         "Film bereits gezeigt?" = Besucherzahl,
         Filmserie = filmserie
         )|>
  mutate(Rating = as.integer(Rating))

df_Filmvorschlag <- df_Filmvorschlag|>
  separate(Suisanummer, c("Suisanummer", "suisa"), sep = ";")|>
  mutate(Suisanummer = if_else(is.na(Suisanummer), suisa, Suisanummer))|>
  select(-suisa)|>
  suppressWarnings()


df_Filmvorschlag <- df_Filmvorschlag|>
  mutate(releasedate = parse_dates(releasedate))|>
  separate(`Post Modified Date`,into = c("Post Modified Date","Post"), sep = ";")|>
  mutate(`Post Modified Date` = parse_date(`Post Modified Date`),
         Post = parse_date(Post),
         `Post Modified Date` = if_else(`Post Modified Date` > Post, `Post Modified Date`, Post)
         )|>
  select(-Post)|>
  suppressWarnings()

df_Filmvorschlag <- df_Filmvorschlag|>
  separate(`Image Featured`, into = c("Image Featured","Image"), sep = ";")|>
  suppressWarnings()|>
  mutate(`Image Featured` = if_else(is.na(`Image Featured`), Image, `Image Featured`)
         )|>
  select(-Image)

# ################################################################################################################################
# # download pictures and insert it to excel output
# download_picts <- function(image_urls, download_dir) {
#   # Define download directory
#   download_dir <- "output/data/pict"  # Change this to your desired folder
#   dir.create(download_dir, showWarnings = FALSE)
# 
#   # Download each image
#   for (url in image_urls) {
#     if(!is.na(url)){
#       file_name <- paste0(download_dir, "/", basename(url))  # Extract file name
#       tryCatch({
#         download.file(url, destfile = file_name, mode = "wb")
#         message("Downloaded: ", file_name)
#       }, error = function(e) {
#         message("Failed to download: ", url)
#       })
#     }
#   }
# }
# 
# # get pictues 
# c_path <- "output/data/pict"
# if(dir.exists("output/data/pict")){
#   # find or download picts
#   c_picts <- df_Filmvorschlag$`Image Featured`|>
#     str_split("/")|>
#     lapply(function(x){
#       x[length(x)]
#     })|>
#     unlist()
# }else{
#   # download picturs
#   df_Filmvorschlag$`Image Featured`|>
#     download_picts(c_path)
#   
#   # find or download picts
#   c_picts <- df_Filmvorschlag$`Image Featured`|>
#     str_split("/")|>
#     lapply(function(x){
#       x[length(x)]
#     })|>
#     unlist()
# }
# c_picts


################################################################################################################################
df_Filmvorschlag <- df_Filmvorschlag|>
  rename(Filmbeschrieb = Content,
         Erscheinungsdatum = releasedate
         )|>
  select(Suisanummer, Filmtitel, Filmbeschrieb, Erscheinungsdatum, Verleiher,`Film bereits gezeigt?`, 
         Kinoklubmitglied, `Post Modified Date`, Schlagwörter, `Link zum Trailer`)

df_Filmvorschlag <- df_Filmvorschlag|>
  mutate(Erscheinungsdatum = as.Date(Erscheinungsdatum),
         `Post Modified Date` = as.Date(`Post Modified Date`)
         )

# used to insert pictures
# df_Filmvorschlag$pict <- c_picts


################################################################################################################################
library(openxlsx)

# Define the style for text wrapping
wrap_text <- createStyle(wrapText = TRUE)

# Define the style for date formatting
date_style <- createStyle(numFmt = "dd.mm.yyyy")

# Create the workbook and build it with the data
wb_xlsx <- buildWorkbook(df_Filmvorschlag, asTable = TRUE, tableStyle = "TableStyleMedium2")
c_sheet_name <- "Filmvorschläge"
names(wb_xlsx) <- c_sheet_name

# Add style to the desired columnsg
for (col in 3:10) {
  addStyle(wb_xlsx, sheet = c_sheet_name, style = wrap_text, 
           rows = 1:(nrow(df_Filmvorschlag) + 1), cols = col, gridExpand = TRUE)
}

# Apply date format to the desired columns
for (ii in c(4,8)) {
  addStyle(wb_xlsx, sheet = c_sheet_name, style = date_style, rows = 2:(nrow(df_Filmvorschlag) + 1), cols = ii, gridExpand = TRUE)
}

# Format column 10 as hyperlinks
x <- df_Filmvorschlag$`Link zum Trailer`
class(x) <- "hyperlink"
writeData(wb_xlsx, c_sheet_name, x = x, startRow = 2, startCol = 10)

# # Insert images in column 11, starting at row 2
# c_path <- "output/data/pict"
# image_files <- paste0(getwd(),"/",c_path, "/", c_picts)  # Assuming you have a column with image file paths
# 
# # Loop through each row and insert the corresponding image in column 11
# for (ii in seq_along(image_files)) {
#   if (!is.na(image_files[ii]) && file.exists(image_files[ii])) {
#     insertImage(
#       wb_xlsx,
#       sheet = c_sheet_name,
#       file = image_files[ii],
#       startRow = ii + 1,  # Start from row 2 (since row 1 is the header)
#       startCol = 11,      # Insert in column 11
#       width = 8,          # Adjust width to fit inside a cell
#       height = 6,         # Adjust height to fit inside a cell
#       units = "cm"
#     )
#   }
# }

# Set column widths
setColWidths(wb_xlsx, sheet = c_sheet_name, 
             cols =   c( 1,  2,  3, 11), 
             widths = c(15, 15, 80, 50)
)

# Insert plots to the workbook
addWorksheet(wb_xlsx, "Plot")

insertImage(
  wb_xlsx,  sheet = "Plot",  file = c_fileName,
  startRow = 1,  startCol = 1,
  width = 25,  height = 3 + nrow(df_Filmvorschlag) * 0.35,
  units = "cm"
)

# Save the workbook to a file
saveWorkbook(wb_xlsx, file = "output/data/Filmvorschläge.xlsx", overwrite = TRUE)

writeLines("Wordpress daten ausgewertet")
