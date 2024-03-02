################################################################################
# SCOTUSWatch - Oral Argument Transcript Parser (OT2023)
# Author: Jake S. Truscott, Ph.D
# Updated March 2024
################################################################################

###############################################################################
#Load Packages & Libraries
###############################################################################
library(dplyr); library(httr); library(tidyr); library(stringr); library(stringi); library(rvest); library(jsonlite)


################################################################################
#Load URL Dataframe
################################################################################
links <- read.csv("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/ot23_arguments/.venv/Oyez Scrapper/oyez_argument_transcripts_links.csv")

################################################################################
#Retrieve Transcript - Save as JSON
#Errors Might Occur w/ Collection - If so, Get Missing Links from setdiff()
################################################################################

for (i in unique(links$year)){
  term_data <- links %>%
    filter(year == i)

  output_path = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/ot23_arguments/argument_transcripts_json/")

  sittings <- unique(term_data$sitting)

  for (s in sittings){
    if (!dir.exists(paste0(output_path, "/", s))) {
      dir.create(paste0(output_path, "/", s), recursive = TRUE)  # Use recursive = TRUE to create parent directories if needed
    }
  }


  for (j in 1:nrow(term_data)) {
    active_link <- term_data[j,]
    year <- active_link$year
    docket <- active_link$docket
    link <- active_link$audio_link
    sitting <- active_link$sitting
    unique_transcript <- gsub(".*\\/", '', link)
    output_file <- paste0(output_path, sitting, "/",  docket, "_", unique_transcript, ".json")

    tryCatch({
      url <- link
      response <- GET(url)

      if (status_code(response) == 200) {
        json_content <- content(response, as = "text", encoding = "UTF-8")  # Specify the encoding
        json_data <- fromJSON(json_content)
        write_json(json_data, output_file)

        if (j %% 20 == 0) {
          message('Completed ', j, ' of ', nrow(term_data), ' for (', i, ')')
        }

      } else {
        stop(paste("Failed to retrieve JSON data from the URL with status code:", status_code(response)))
      }
    }, error = function(e) {
      cat("Error downloading file: ", output_file, "\n")
    })
  }

} #Convert to JSON

{

  list_json_files <- function(folder_path) {
    files <- list.files(path = folder_path, full.names = TRUE)
    json_files <- files[grepl(".json$", files, ignore.case = TRUE)]
    return(json_files)
  } #Collect Single Folder

  list_json_files_recursive <- function(folder_path, min_file_size = 2) {
    all_json_files <- character(0)
    items <- list.files(path = folder_path, full.names = TRUE)
    json_files <- items[grepl(".json$", items, ignore.case = TRUE)]

    # Filter JSON files by file size
    for (json_file in json_files) {
      file_info <- file.info(json_file)
      if (file_info$size >= min_file_size * 1024) {  # Convert min_file_size to bytes
        all_json_files <- c(all_json_files, json_file)
      }
    }

    subdirs <- list.dirs(path = folder_path, full.names = TRUE, recursive = FALSE)

    for (subdir in subdirs) {
      subdir_json_files <- list_json_files_recursive(subdir, min_file_size)
      all_json_files <- c(all_json_files, subdir_json_files)
    }

    return(all_json_files)
  } #Recursively Search Folder to Folder

  directory_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/ot23_arguments/argument_transcripts_json"
  min_file_size_kb <- 2 #Only Keep Files Greater than 2kb (Indicating They Aren't Empty)
  transcript_json_list <- list_json_files_recursive(directory_path)


} #Get List of JSON Files After Collection

{

  output_directory = "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/ot23_arguments/argument_transcripts_processed/2023"

  files <- data.frame(files = transcript_json_list)
  files <- data.frame(files = files)

  json_files <- files %>%
    mutate(sitting = gsub("C\\:\\/Users\\/Jake Truscott\\/Documents\\/GitHub\\/jaketruscott.github.io\\/_scotuswatch\\/ot23_arguments\\/argument_transcripts_json\\/", "", files)) %>%
    mutate(sitting = gsub("\\/.*", "", sitting))

  for(sitting in unique(json_files$sitting)){
    files <- json_files$files[json_files$sitting == sitting] #Subset Files to sitting
    transcripts <- data.frame() #Create Empty Frame

    message('Beginning ', sitting, "\n")

    for (i in 1:length(files)) {
      tryCatch({
        file_path <- files[i]
        json_text <- paste0(readLines(file_path), collapse = "\n")
        parsed_json <- fromJSON(json_text)

        docket <- gsub(".*\\/", "", files[i])
        docket <- gsub("\\_.*", "", docket)

        temp <- data.frame()

        for (j in 1:length(parsed_json$transcript$sections$turns)) {
          temp_text <- parsed_json$transcript$sections$turns[j]
          temp <- bind_rows(temp, temp_text)
        }

        temp_meta <- data.frame(
          case_name = parsed_json$transcript$title[1]
        )

        temp_complete <- data.frame()

        for (k in 1:nrow(temp)) {
          text_blocks <- temp$text_blocks[k]
          flattened_text <- paste(sapply(text_blocks, function (block) block$text), collapse = " ")

          temp_full <- data.frame(
            case_name = temp_meta$case_name,
            text_start = temp$start[k],
            text_stop = temp$stop[k],
            speaker = temp$speaker$name[k],
            role = ifelse(is.null(temp$speaker$roles[k][[1]]$role_title) && is.null(temp$speaker$name[k]), "NA",
                          ifelse(is.null(temp$speaker$roles[k][[1]]$role_title), "Attorney", temp$speaker$roles[k][[1]]$role_title)),
            text = flattened_text,
            word_count = str_count(flattened_text, "\\w+"),
            row_id = k
          )

          temp_complete <- bind_rows(temp_complete, temp_full)
        }


        temp_complete <- temp_complete %>%
          mutate(
            argument_duration = temp_meta$argument_duration,
            sitting = sitting,
            docket = docket
          ) %>%
          relocate(sitting, case_name, docket) %>%
          mutate(role = ifelse(is.na(speaker), NA, role)) %>%
          mutate(role = ifelse(grepl('Justice', role, ignore.case = T), 'Justice', role))

        transcripts <- bind_rows(transcripts, temp_complete)

        if (i %% 20 == 0) {
          message("          Completed ", temp_meta$case_name, "   -- ", i, " of ", length(files))
        }



      }, error = function(e) {
        message("Error processing file ", i, ": ", e$message, "...Moving On")
      })
    }

    active_data_term <- paste0('OT_23_', sitting)
    assign(active_data_term, transcripts)

    save(transcripts, file = file.path(output_directory, paste0(active_data_term, ".RData")))

    message('Completed ', sitting, ' Sitting...Moving on\n')

    objects_to_remove <- ls(pattern = '^transcripts|^temp')
    rm(list = objects_to_remove) #Remove from Global Before Moving on - Saves Spac


  } #Run Loop Across JSON


} #Full Run


transcript_list <- list.files("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/ot23_arguments/argument_transcripts_processed/2023", full.names = T)


################################################################################
#Put Into OT2023 Dataframe
################################################################################
scotus_OT23 <- data.frame()

for (file in transcript_list) {
  loaded_data <- get(load(file))
  scotus_OT23 <- bind_rows(scotus_OT23, loaded_data)
} # Load each RData file and store the objects in the list

{
  scotus_OT23 <- scotus_OT23 %>%
    mutate(speaker = case_when(
      .default = speaker,
      speaker == "John G. Roberts, Jr." & role == "Justice" ~ "ROBERTS",
      speaker == "Clarence Thomas" & role == "Justice" ~ 'THOMAS',
      speaker == 'Elena Kagan' & role == "Justice" ~ 'KAGAN',
      speaker == "Ketanji Brown Jackson" & role == "Justice" ~ 'JACKSON',
      speaker == "Amy Coney Barrett" & role == "Justice" ~ 'BARRETT',
      speaker == 'Samuel A. Alito, Jr.' & role == "Justice" ~ "ALITO",
      speaker == "Brett M. Kavanaugh" & role == "Justice" ~ "KAVANAUGH",
      speaker == "Neil M. Gorsuch" & role == "Justice" ~ 'GORSUCH',
      speaker == "Neil Gorsuch" & role == "Justice" ~ 'GORSUCH',
      speaker == "Sonia Sotomayor" & role == "Justice" ~ 'SOTOMAYOR'
    )) %>%
    rename(speaker_type = role) %>%
    mutate(case_name = gsub("(\\, Petitioner\\,|\\, Petitioners\\,|\\, Appellants\\,|\\, Appellant\\,| Petitioner\\,)", "", case_name)) %>%
    mutate(case_name = gsub("(\\, Respondent|\\, Respondents|\\, Appellees|\\, Appellee| Respondent\\,)", "", case_name)) %>%
    mutate(case_name = str_to_title(case_name)) %>%
    mutate(case_name = gsub("Llc", "LLC", case_name)) %>%
    mutate(case_name = gsub( " V.\\ ", " v. ", case_name))
} #Specialized Fixes for OT2023



################################################################################
#Save as Excel & rdata
################################################################################

save(scotus_OT23, file = "C:/Users/Jake Truscott/Documents/GitHub/scotustext/Data/scotus_transcripts_23.rdata")
writexl::write_xlsx(scotus_OT23, path = "C:/Users/Jake Truscott/Documents/GitHub/scotustext/Data/scotus_transcripts_23.xlsx")
