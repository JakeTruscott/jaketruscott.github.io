################################################################################
# Docket Parser - SCOTUSWatch
# Developed by Jake S. Truscott
# Updated January 2024
################################################################################


################################################################################
#Load Packages
################################################################################
library(dplyr); library(stringr); library(stringi); library(rvest); library(httr); library(httr2); library(httr); library(xml2)



################################################################################
# Core Function
# Only Works for Cases Filed Post-2016...

# As of 6/16 -
# Regular: Up Through 23-1312
# IFP: Through 23-7730
# App: Through 23-1114
# M: Through 23M102
################################################################################

dockets_through_6_16 <- c(paste0('23-', 1:1312), paste0('23-', 5001:7730), paste0('23M', 1:102), paste0('23A', 1:1114))


docket_parser <- function(dockets){

  dockets_final <- data.frame()
  failed_dockets <- c()

  initial_dockets_count <- length(dockets)

  dockets_already_completed = list.files("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/docket_parser/OT23_docket_sheets")
  dockets_already_completed <- gsub('\\.rdata', '', dockets_already_completed)
  already_completed <- dockets_already_completed[dockets_already_completed %in% dockets]

  dockets_remaining <- length(dockets) - length(dockets_already_completed)

  dockets = dockets[!dockets %in% dockets_already_completed]

  message('\033[32m Beginning Docket Parsing \033[0m')
  message('\033[32m Dockets Already Collected: ', length(already_completed), ' \033[0m')
  message('\033[32m Remaining Dockets to Collect: ', length(dockets), ' \033[0m')

  for (d in 1:length(dockets)){

    tryCatch({

      Sys.sleep(5)

      temp_term <- as.numeric(gsub('(\\-.*|M.*|A,*)', '', dockets[d]))

      if (temp_term <= 16){

        base_url <- 'https://www.supremecourt.gov/search.aspx?filename=/docketfiles/'
        url <- paste0(base_url, dockets[d], '.htm')

      } else {

        base_url <- 'https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/'
        url <- paste0(base_url, dockets[d], ".html")

      }

      response <- GET(url)

      #html_content <- read_html(content(response, "text"))

      html_content <- read_html(httr::content(response, as = "text"))


      docketinfo_table <- html_nodes(html_content, xpath = '//*[@id="docketinfo"]') %>% html_table(fill = TRUE)
      proceedings_table <- html_nodes(html_content, xpath = '//*[@id="proceedings"]') %>% html_table(fill = TRUE)
      contacts_table <- html_nodes(html_content, xpath = '//*[@id="Contacts"]') %>% html_table(fill = TRUE)

      {
        docket_info <- data.frame(docketinfo_table)
        names(docket_info) <- c('input', 'info')
        docket_info <- docket_info %>%
          filter(!input == "") %>%
          mutate(input = gsub("\\:", '', input),
                 input = tolower(gsub(' ', '_', input)),
                 info = ifelse(grepl('linked_with', input), gsub('LINKED\\_WITH\\_', '', toupper(input)), info),
                 input = ifelse(grepl('linked_with', input), 'linked_with', input),
                 input = ifelse(grepl('case_number', input), 'lc_case_number', input))

        docket_info <- tidyr::spread(docket_info, key = input, value = info)

        docket_info$linked_with = ifelse(any(names(docket_info) %in% 'linked_with'), docket_info$linked_with, NA)
        docket_info$decision_date = ifelse(any(names(docket_info) %in% 'decision_date'), docket_info$decision_date, NA)

        docket_info_temp <- data.frame(
          case_title = docket_info$title[1],
          docket_number = dockets[d],
          linked_with = docket_info$linked_with[1],
          docketed = docket_info$docketed[1],
          plaintiff = gsub("v\\..*", '', docket_info$title[1]),
          respondent = gsub(".* v\\.", '', docket_info$title[1]),
          lower_court = docket_info$lower_ct[1],
          lower_court_case_number = docket_info$lc_case_number[1],
          lower_court_decision_date = docket_info$decision_date[1],
          scotus_docket_url = url
        )

        docket_info_temp <- docket_info_temp %>%
          mutate(plaintiff = trimws(plaintiff),
                 respondent = trimws(respondent))

      } #Baseline Docket Info ('docket_info_temp' returned)

      {
        docket_proceedings <- data.frame(proceedings_table)
        docket_proceedings <- docket_proceedings %>%
          rename(date = X1,
                 entry = X2) %>%
          filter(!date == '') %>%
          filter(!grepl('date|Date', date)) %>%
          mutate(date = anytime::anydate(date))

        docket_entries <- list()
        docket_entries[['full_docket']] <- docket_proceedings
      } #Docket Entries ('docket_entries[['full_docket']]' returned)

      {
        contacts_table <- html_nodes(html_content, xpath = '//*[@id="Contacts"]') %>% html_table(fill = TRUE)
        docket_contacts <- data.frame(contacts_table)
        docket_contacts <- docket_contacts[c(2:nrow(docket_contacts)),] %>%
          mutate(type = case_when(
            grepl('(Attorney|Attorneys) for (Petitioner|Petitioners|Appellant|Appellants)', X1, ignore.case = T) ~ 'Petitioner',
            grepl('(Attorney|Attorneys) for (Respondent|Respondents|Appellee|Appellees)', X1, ignore.case = T) ~ 'Respondent',
            grepl('Other$|other$', X1, ignore.case = T) & grepl('Other$|other$', X2, ignore.case = T) & grepl('Other$|other$', X3, ignore.case = T) ~ 'Other'
          ))

        docket_contacts <- tidyr::fill(docket_contacts, type)

        {
          petitioner_contacts <- docket_contacts %>%
            filter(type == 'petitioner') %>%
            filter(!X1 == '') %>%
            filter(!grepl('(Attorney|Attorneys) for (petitioner|petitioners|Appellee|Appellees)', X1, ignore.case = T))

          if (nrow(petitioner_contacts >= 1)){
            petitioner_contacts <- petitioner_contacts %>%
              mutate(id = rep(1:(n() / 2), each = 2))
            petitioner_counsel <- data.frame()

            for (i in unique(petitioner_contacts$id)){
              petitioner_contacts_temp <- petitioner_contacts %>%
                filter(id == i) %>%
                select(-c(id, type)) %>%
                mutate(
                  name = X1[1],
                  location = X2[1],
                  contact = X3[1],
                  party = X1[2]
                ) %>%
                mutate(party = gsub('Party name: ', '', party, ignore.case = T),
                       counsel_type = ifelse(grepl('Counsel of Record', name), 'Counsel of Record', 'Additional')) %>%
                select(name, location, contact, party, counsel_type) %>%
                mutate(name = ifelse(grepl('Counsel of Record', name, ignore.case = T), gsub('\\s*Counsel of Record.*', '', name), name)) %>%
                slice(1) %>%
                mutate(name = str_trim(name, side = "right")) %>%
                mutate(name = trimws(name)) %>%
                mutate(party_type = 'petitioner')

              petitioner_counsel <- bind_rows(petitioner_counsel, petitioner_contacts_temp)

            }

          } else {
            petitioner_counsel <- data.frame()
          }

        } #Petitioner

        {

          respondent_contacts <- docket_contacts %>%
            filter(type == 'Respondent') %>%
            filter(!X1 == '') %>%
            filter(!grepl('(Attorney|Attorneys) for (Respondent|Respondents|Appellee|Appellees)', X1, ignore.case = T))

          if (nrow(respondent_contacts >= 1)){
            respondent_contacts <- respondent_contacts %>%
              mutate(id = rep(1:(n() / 2), each = 2))
            respondent_counsel <- data.frame()

            for (i in unique(respondent_contacts$id)){
              respondent_contacts_temp <- respondent_contacts %>%
                filter(id == i) %>%
                select(-c(id, type)) %>%
                mutate(
                  name = X1[1],
                  location = X2[1],
                  contact = X3[1],
                  party = X1[2]
                ) %>%
                mutate(party = gsub('Party name: ', '', party, ignore.case = T),
                       counsel_type = ifelse(grepl('Counsel of Record', name), 'Counsel of Record', 'Additional')) %>%
                select(name, location, contact, party, counsel_type) %>%
                mutate(name = ifelse(grepl('Counsel of Record', name, ignore.case = T), gsub('\\s*Counsel of Record.*', '', name), name)) %>%
                slice(1) %>%
                mutate(name = str_trim(name, side = "right")) %>%
                mutate(name = trimws(name)) %>%
                mutate(party_type = 'Respondent')

              respondent_counsel <- bind_rows(respondent_counsel, respondent_contacts_temp)

            }

          } else {
            respondent_counsel <- data.frame()
          }

        } # Respondent

        {

          other_contacts <- docket_contacts %>%
            filter(type == 'Other') %>%
            filter(!X1 == '') %>%
            filter(!X1 == 'Other') %>%
            filter(!X1 == 'other')

          if (nrow(other_contacts) == 0){
            other_counsel = data.frame()
          }  else {

            if (grepl("Party name", other_contacts$X1[1], ignore.case = T)){
              other_contacts <- other_contacts[-1,]
            }

            other_contacts <- other_contacts %>%
             mutate(id = rep(1:(n() / 2), each = 2))

            other_counsel <- data.frame()

            for (i in unique(other_contacts$id)){
              other_contacts_temp <- other_contacts %>%
                filter(id == i) %>%
                select(-c(id, type)) %>%
                mutate(
                  name = X1[1],
                  location = X2[1],
                  contact = X3[1],
                  party = X1[2]
                ) %>%
                mutate(party = gsub('Party name: ', '', party, ignore.case = T),
                       counsel_type = ifelse(grepl('Counsel of Record', name), 'Counsel of Record', 'Additional')) %>%
                select(name, location, contact, party, counsel_type) %>%
                mutate(name = ifelse(grepl('Counsel of Record', name, ignore.case = T), gsub('\\s*Counsel of Record.*', '', name), name)) %>%
                slice(1) %>%
                mutate(name = str_trim(name, side = "right")) %>%
                mutate(name = trimws(name)) %>%
                mutate(party_type = 'Other/Amicus')

              other_counsel <- bind_rows(other_counsel, other_contacts_temp)
            }

          }

        } #Other


        if (nrow(petitioner_counsel) >= 1){
          petitioner_counsel$name <- trimws(petitioner_counsel$name, whitespace = "\\s+")
        }

        if (nrow(respondent_counsel) >= 1){
          respondent_counsel$name <- trimws(respondent_counsel$name, whitespace = "\\s+")
        }

        if (nrow(other_counsel) >= 1){
          other_counsel$name <- trimws(other_counsel$name, whitespace = "\\s+")
        }


        counsel <- list()

        counsel[['Petitioner']] <- petitioner_counsel
        counsel[['Respondent']] <- respondent_counsel
        counsel[['Other_Amicus']] <- other_counsel
      } #Attorneys ('counsel'[[Petitioner/Respondent/Other_Amicus]] returned)


      docket_combined <- cbind(docket_info_temp,
                               docket = I(list(docket_proceedings)),
                               counsel = I(list(counsel)))

      output_dir_temp <- paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/docket_parser/OT23_docket_sheets/", dockets[d], '.rdata')

      save(docket_combined, file = output_dir_temp)

      dockets_final <- bind_rows(dockets_final, docket_combined)

      if (d %% 5 == 0){
        message('Completed Docket ', d, ' of ', length(dockets))
      }



    }, #Core Function

    error = function(e){
      failed_dockets <- c(failed_dockets, dockets[d])
      cat('\nError: Docket ', dockets[d], 'Failed...Moving On\n') })

  } #For d in 1:length(dockets)


  message('------------------------------')
  message('------Completion Summary------')
  message('------------------------------')
  message('Unique Dockets: \033[32m', nrow(dockets_final), '\033[0m')
  message('------------------------------')


  return(dockets_final)



} #Core Function


docket_OT23_5_23_update <- docket_parser(dockets = dockets_through_6_16)


################################################################################
# Get 2018 to 2023 Dockets
################################################################################
old_dockets <- get(load("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/docket_parser/dockets_archive-062823_Update.rdata"))

docket_numbers <- old_dockets %>%
  select(docket_number) %>%
  mutate(docket_year = gsub('(\\-.*|m.*|a.*)', '', docket_number),
         docket_year = as.numeric(docket_year)) %>%
  filter(docket_year >= 18)

docket_OT_18_22 <- unique(docket_numbers$docket_number)

docket_parser_old_dockets <- function(dockets){

  dockets_final <- data.frame()
  failed_dockets <- c()

  initial_dockets_count <- length(dockets)

  dockets_already_completed = list.files("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/docket_parser/OT18_OT22_docket_sheets")
  dockets_already_completed <- gsub('\\.rdata', '', dockets_already_completed)
  already_completed <- dockets_already_completed[dockets_already_completed %in% dockets]

  dockets_remaining <- length(dockets) - length(dockets_already_completed)

  dockets = dockets[!dockets %in% dockets_already_completed]

  message('\033[32m Beginning Docket Parsing \033[0m')
  message('\033[32m Dockets Already Collected: ', length(already_completed), ' \033[0m')
  message('\033[32m Remaining Dockets to Collect: ', length(dockets), ' \033[0m')

  for (d in 1:length(dockets)){

    tryCatch({

      base_url <- 'https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/'
      url <- paste0(base_url, dockets[d], ".html")

      response <- GET(url)

      #html_content <- read_html(content(response, "text"))

      html_content <- read_html(httr::content(response, as = "text"))


      docketinfo_table <- html_nodes(html_content, xpath = '//*[@id="docketinfo"]') %>% html_table(fill = TRUE)
      proceedings_table <- html_nodes(html_content, xpath = '//*[@id="proceedings"]') %>% html_table(fill = TRUE)
      contacts_table <- html_nodes(html_content, xpath = '//*[@id="Contacts"]') %>% html_table(fill = TRUE)

      {
        docket_info <- data.frame(docketinfo_table)
        names(docket_info) <- c('input', 'info')
        docket_info <- docket_info %>%
          filter(!input == "") %>%
          mutate(input = gsub("\\:", '', input),
                 input = tolower(gsub(' ', '_', input)),
                 info = ifelse(grepl('linked_with', input), gsub('LINKED\\_WITH\\_', '', toupper(input)), info),
                 input = ifelse(grepl('linked_with', input), 'linked_with', input),
                 input = ifelse(grepl('case_number', input), 'lc_case_number', input))

        docket_info <- tidyr::spread(docket_info, key = input, value = info)

        docket_info$linked_with = ifelse(any(names(docket_info) %in% 'linked_with'), docket_info$linked_with, NA)
        docket_info$decision_date = ifelse(any(names(docket_info) %in% 'decision_date'), docket_info$decision_date, NA)

        docket_info_temp <- data.frame(
          case_title = docket_info$title[1],
          docket_number = dockets[d],
          linked_with = docket_info$linked_with[1],
          docketed = docket_info$docketed[1],
          plaintiff = gsub("v\\..*", '', docket_info$title[1]),
          respondent = gsub(".* v\\.", '', docket_info$title[1]),
          lower_court = docket_info$lower_ct[1],
          lower_court_case_number = docket_info$lc_case_number[1],
          lower_court_decision_date = docket_info$decision_date[1],
          scotus_docket_url = url
        )

        docket_info_temp <- docket_info_temp %>%
          mutate(plaintiff = trimws(plaintiff),
                 respondent = trimws(respondent))

      } #Baseline Docket Info ('docket_info_temp' returned)

      {
        docket_proceedings <- data.frame(proceedings_table)
        docket_proceedings <- docket_proceedings %>%
          rename(date = X1,
                 entry = X2) %>%
          filter(!date == '') %>%
          filter(!grepl('date|Date', date)) %>%
          mutate(date = anytime::anydate(date))

        docket_entries <- list()
        docket_entries[['full_docket']] <- docket_proceedings
      } #Docket Entries ('docket_entries[['full_docket']]' returned)

      {
        contacts_table <- html_nodes(html_content, xpath = '//*[@id="Contacts"]') %>% html_table(fill = TRUE)
        docket_contacts <- data.frame(contacts_table)
        docket_contacts <- docket_contacts[c(2:nrow(docket_contacts)),] %>%
          mutate(type = case_when(
            grepl('(Attorney|Attorneys) for (Petitioner|Petitioners|Appellant|Appellants)', X1, ignore.case = T) ~ 'Petitioner',
            grepl('(Attorney|Attorneys) for (Respondent|Respondents|Appellee|Appellees)', X1, ignore.case = T) ~ 'Respondent',
            grepl('Other$|other$', X1, ignore.case = T) & grepl('Other$|other$', X2, ignore.case = T) & grepl('Other$|other$', X3, ignore.case = T) ~ 'Other'
          ))

        docket_contacts <- tidyr::fill(docket_contacts, type)

        petitioner_counsel <- data.frame()
        respondent_counsel <- data.frame()
        other_counsel <- data.frame()

        {

          petitioner_contacts <- docket_contacts %>%
            filter(type == 'Petitioner') %>%
            filter(!X1 == '') %>%
            filter(!grepl('(Attorney|Attorneys) for (Petitioner|Petitioners|Appellee|Appellees)', X1, ignore.case = T)) %>%
            filter(!grepl('Party name:', X1))

          if (nrow(petitioner_contacts >= 1)){
            petitioner_contacts <- petitioner_contacts %>%
              mutate(id = row_number())
            petitioner_counsel <- data.frame()

            for (i in unique(petitioner_contacts$id)){
              petitioner_contacts_temp <- petitioner_contacts %>%
                filter(id == i) %>%
                select(-c(id, type)) %>%
                mutate(
                  name = X1[1],
                  location = X2[1],
                  contact = X3[1]
                ) %>%
                mutate(counsel_type = ifelse(grepl('Counsel of Record', name), 'Counsel of Record', 'Additional')) %>%
                select(name, location, contact, counsel_type) %>%
                mutate(name = ifelse(grepl('Counsel of Record', name, ignore.case = T), gsub('\\s*Counsel of Record.*', '', name), name)) %>%
                slice(1) %>%
                mutate(name = str_trim(name, side = "right")) %>%
                mutate(name = trimws(name)) %>%
                mutate(party_type = 'Petitioner')

              petitioner_counsel <- bind_rows(petitioner_counsel, petitioner_contacts_temp)

            }

          } else {
            petitioner_counsel <- data.frame()
          }

        } # petitioner

        {

          respondent_contacts <- docket_contacts %>%
            filter(type == 'Respondent') %>%
            filter(!X1 == '') %>%
            filter(!grepl('(Attorney|Attorneys) for (Respondent|Respondents|Appellee|Appellees)', X1, ignore.case = T)) %>%
            filter(!grepl('Party name:', X1))

          if (nrow(respondent_contacts >= 1)){
            respondent_contacts <- respondent_contacts %>%
              mutate(id = row_number())
            respondent_counsel <- data.frame()

            for (i in unique(respondent_contacts$id)){
              respondent_contacts_temp <- respondent_contacts %>%
                filter(id == i) %>%
                select(-c(id, type)) %>%
                mutate(
                  name = X1[1],
                  location = X2[1],
                  contact = X3[1]
                ) %>%
                mutate(counsel_type = ifelse(grepl('Counsel of Record', name), 'Counsel of Record', 'Additional')) %>%
                select(name, location, contact, counsel_type) %>%
                mutate(name = ifelse(grepl('Counsel of Record', name, ignore.case = T), gsub('\\s*Counsel of Record.*', '', name), name)) %>%
                slice(1) %>%
                mutate(name = str_trim(name, side = "right")) %>%
                mutate(name = trimws(name)) %>%
                mutate(party_type = 'Respondent')

              respondent_counsel <- bind_rows(respondent_counsel, respondent_contacts_temp)

            }

          } else {
            respondent_counsel <- data.frame()
          }

        } # Respondent

        {

          other_contacts <- docket_contacts %>%
            filter(type == 'Other') %>%
            filter(!X1 == '') %>%
            filter(!grepl('(Attorney|Attorneys) for (Other|Others|Appellee|Appellees)', X1, ignore.case = T)) %>%
            filter(!grepl('Party name:', X1))

          if (nrow(other_contacts >= 1)){
            other_contacts <- other_contacts %>%
              mutate(id = row_number())
            other_counsel <- data.frame()

            for (i in unique(other_contacts$id)){
              other_contacts_temp <- other_contacts %>%
                filter(id == i) %>%
                select(-c(id, type)) %>%
                mutate(
                  name = X1[1],
                  location = X2[1],
                  contact = X3[1]
                ) %>%
                mutate(counsel_type = ifelse(grepl('Counsel of Record', name), 'Counsel of Record', 'Additional')) %>%
                select(name, location, contact, counsel_type) %>%
                mutate(name = ifelse(grepl('Counsel of Record', name, ignore.case = T), gsub('\\s*Counsel of Record.*', '', name), name)) %>%
                slice(1) %>%
                mutate(name = str_trim(name, side = "right")) %>%
                mutate(name = trimws(name)) %>%
                mutate(party_type = 'Other')

              other_counsel <- bind_rows(other_counsel, other_contacts_temp)

            }

          } else {
            Other_counsel <- data.frame()
          }

        } # Other

        if (nrow(petitioner_counsel) >= 1){
          petitioner_counsel$name <- trimws(petitioner_counsel$name, whitespace = "\\s+")
        }

        if (nrow(respondent_counsel) >= 1){
          respondent_counsel$name <- trimws(respondent_counsel$name, whitespace = "\\s+")
        }

        if (nrow(other_counsel) >= 1){
          other_counsel$name <- trimws(other_counsel$name, whitespace = "\\s+")
        }


        counsel <- list()

        counsel[['Petitioner']] <- petitioner_counsel
        counsel[['Respondent']] <- respondent_counsel
        counsel[['Other_Amicus']] <- other_counsel
      } #Attorneys ('counsel'[[Petitioner/Respondent/Other_Amicus]] returned)


      docket_combined <- cbind(docket_info_temp,
                               docket = I(list(docket_proceedings)),
                               counsel = I(list(counsel)))

      output_dir_temp <- paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/docket_parser/OT18_OT22_docket_sheets/", dockets[d], '.rdata')

      save(docket_combined, file = output_dir_temp)

      dockets_final <- bind_rows(dockets_final, docket_combined)

      if (d %% 5 == 0){
        message('Completed Docket ', d, ' of ', length(dockets))
      }

      Sys.sleep(5)


    }, #Core Function

    error = function(e){
      failed_dockets <- c(failed_dockets, dockets[d])
      cat('\nError: Docket ', dockets[d], 'Failed...Moving On\n')
      Sys.sleep(10)
      })

  } #For d in 1:length(dockets)


  message('------------------------------')
  message('------Completion Summary------')
  message('------------------------------')
  message('Unique Dockets: \033[32m', nrow(dockets_final), '\033[0m')
  message('------------------------------')


  return(dockets_final)



} #Core Function

OT18_OT22_dockets_combined <- docket_parser_old_dockets(dockets = docket_OT_18_22)


################################################################################
# Rebuild 2001-2022 Dockets
################################################################################

cleaned_docket_archive <- data.frame()
load("C:/Users/Jake Truscott/Desktop/docket search with data retrieval/dockets_archive.rdata")

for (i in 53819:nrow(dockets_archive)){

  temp_df <- dockets_archive[i,] %>%
    select(docket_number, case_title, petitioner, respondent, docketed, linked_with, all_docket_entries, docket_url, all_petitioner_counsel, all_respondent_counsel, all_other_counsel) %>%
    rename(scotus_docket_url = docket_url)

  temp_archive_rebuilt <- list()

  temp_archive_rebuilt <- temp_df %>%
    mutate(parsed_docket = strsplit(all_docket_entries, "; ")) %>%
    tidyr::unnest(cols = parsed_docket) %>%
    tidyr::separate(parsed_docket, into = c("date", "entry"), sep = "(?<=20\\d{2})\\s+", remove = FALSE) %>%
    select(date, entry) %>%
    mutate(date = as.Date(date, format = "%b %d %Y"))

  counsel <- list()

  {

    if (is.na(temp_df$all_petitioner_counsel)){

      petitioner <- data.frame()

    } else {

      petitioner <- temp_df %>%
        select(all_petitioner_counsel) %>%
        mutate(parsed_counsel = strsplit(all_petitioner_counsel, '; ')) %>%
        tidyr::unnest(cols = parsed_counsel) %>%
        tidyr::separate(parsed_counsel, into = c('Attorney', 'Organization', 'Party'), sep = '(\\nOrganization\\:|\\nParty name\\:)', remove = F) %>%
        select(-c(parsed_counsel, all_petitioner_counsel)) %>%
        mutate(Attorney = gsub('Attorney\\: ', '', trimws(Attorney)),
               Organization = trimws(Organization),
               Party = trimws(Party))

    }

    if (is.na(temp_df$all_respondent_counsel)){

      respondent <- data.frame()

    } else {

      respondent <- temp_df %>%
        select(all_respondent_counsel)  %>%
        mutate(parsed_counsel = strsplit(all_respondent_counsel, '; ')) %>%
        tidyr::unnest(cols = parsed_counsel) %>%
        tidyr::separate(parsed_counsel, into = c('Attorney', 'Organization', 'Party'), sep = '(\\nOrganization\\:|\\nParty name\\:)', remove = F) %>%
        select(-c(parsed_counsel, all_respondent_counsel)) %>%
        mutate(Attorney = gsub('Attorney\\: ', '', trimws(Attorney)),
               Organization = trimws(Organization),
               Party = trimws(Party))
    }


    if (is.na(temp_df$all_other_counsel)){

      other <- data.frame()

    } else {

      other <- temp_df %>%
        select(all_other_counsel) %>%
        mutate(parsed_counsel = strsplit(all_other_counsel, '; ')) %>%
        tidyr::unnest(cols = parsed_counsel) %>%
        tidyr::separate(parsed_counsel, into = c('Attorney', 'Organization', 'Party'), sep = '(\\nOrganization\\:|\\nParty name\\:)', remove = F) %>%
        select(-c(parsed_counsel, all_other_counsel)) %>%
        mutate(Attorney = gsub('Attorney\\: ', '', trimws(Attorney)),
               Organization = trimws(Organization),
               Party = trimws(Party))

    }



  } #Counsel Reformatting

  counsel[['Petitioner']] <- petitioner
  counsel[['Respondent']] <- respondent
  counsel[['Other_Amicus']] <- other

  rebuilt_docket <- cbind(temp_df %>%
                            mutate(linked_with = ifelse(linked_with == 'NA', NA, linked_with)) %>%
                            select(-c(all_docket_entries, all_petitioner_counsel, all_respondent_counsel, all_other_counsel)),
                          docket = I(list(temp_archive_rebuilt)),
                          counsel = I(list(counsel)))


  cleaned_docket_archive <- bind_rows(cleaned_docket_archive, rebuilt_docket)

  if (i %% 500 == 0){
    message('Completed ', i, ' of ', nrow(dockets_archive))
  }


}

save(cleaned_docket_archive, file = 'docket_parser/OT01_OT22_dockets_updated.rdata')


################################################################################
# Grab Lower Court - Save
################################################################################

load('docket_parser/OT01_OT22_dockets_updated.rdata')

update_dockets <- function(output_path, cleaned_dockets){

  completed_dockets <- list.files(output_directory)
  completed_dockets <- gsub('\\.rdata', '', completed_dockets)

  message('Dockets Already Completed: ', length(completed_dockets))

  dockets_to_complete <- cleaned_dockets %>%
    filter(!docket_number %in% completed_dockets) %>%
    filter(grepl('\\-', docket_number))

  message('Dockets to Complete: ', nrow(dockets_to_complete))

  estimated_time <- round(((nrow(dockets_to_complete)*5)/3600), 2)
  message('Estimated Time to Complete: ', estimated_time, ' Hours')

  for (i in 1:nrow(dockets_to_complete)){

    temp_row <- dockets_to_complete[i,]
    temp_docket_number <- temp_row$docket_number
    temp_url <- temp_row$scotus_docket_url
    temp_response <- GET(temp_url)
    temp_text <- rvest::read_html(temp_response)
    temp_text <- html_text(temp_text, trim = T)
    lower_court <- gsub('.*Lower Ct\\:', '', temp_text)
    lower_court <- gsub('Case N.*', '', lower_court)
    lower_court <- gsub("[^A-Za-z]+$", '', as.character(lower_court))
    lower_court = trimws(lower_court)

    if (lower_court == ""){
      lower_court <- NA
    }

    temp_row$lower_court <- lower_court
    save(temp_row, file = file.path(output_directory, paste0(temp_docket_number, '.rdata')))

    Sys.sleep(5)

    if (i %% 50 == 0){
      message('Completed ', i, ' of ', nrow(dockets_to_complete))
    }

  }

}

output_directory <- 'docket_parser/OT01_OT22_docket_sheets'


update_dockets(output_path = output_directory,
               cleaned_docket = cleaned_docket_archive)



# TO do - Go to URL & Get Lower Court

################################################################################
# Recompile 2001-2022 Dockets
################################################################################

dockets_OT01_OT22 <- data.frame()
files <- list.files('docket_parser/OT01_OT22_docket_sheets', full.names = T)

for (i in 1:length(files)){
  temp <- get(load(files[i]))
  dockets_OT01_OT22 <- bind_rows(dockets_OT01_OT22, temp)

  if (i %% 500 == 0){
    message('Completed ', i, ' of ', length(files))
  }

}

save(dockets_OT01_OT22, file = 'docket_parser/dockets_OT01_OT22_2024Update_original.rdata')

load('docket_parser/dockets_OT01_OT22_2024Update_original.rdata')

dockets_OT01_OT22 <- dockets_OT01_OT22 %>%
  mutate(okay = ifelse(grepl('Lower Ct\\:', docketed), gsub('Lower Ct\\:', '', docketed), lower_court),
         okay = ifelse(grepl('(In Re |In re)', case_title), 'Habeas', okay)) %>%
  mutate(okay = trimws(okay)) %>%
  mutate(docketed = ifelse(grepl('Lower Ct\\:', docketed), NA, docketed))

for (i in 1:nrow(dockets_OT01_OT22)){

  docketed_date <- dockets_OT01_OT22$docket[[i]]$date[1]
  if (is.na(docketed_date)){
    docketed_date <- dockets_OT01_OT22$docket[[i]]$date[2]

    dockets_OT01_OT22$docket[[i]] <- dockets_OT01_OT22$docket[[i]] %>%
      filter(!is.na(date))

  }

  dockets_OT01_OT22$docketed[i] <- as.character(docketed_date)

  if (i %% 1000 == 0){
    message('Completed ', i, ' of ', nrow(dockets_OT01_OT22))
  }

}

dockets_OT01_OT22 <- dockets_OT01_OT22 %>%
  mutate(lower_court = okay) %>%
  select(-c(okay)) %>%
  relocate(lower_court, .after = docketed) %>%
  mutate(lower_court = gsub('Decision Date.*', '', lower_court),
         lower_court = trimws(lower_court)) %>%
  mutate(lower_court = ifelse(docket_number == '03-8786', 'United States Court of Appeals for the Third Circuit', lower_court),
         lower_court = ifelse(docket_number == '12-6571', 'United States Court of Appeals for the Ninth Circuit', lower_court)) %>%
  mutate(lower_court = ifelse(grepl('function', lower_court), 'Unknown', lower_court)) %>%
  mutate(IFP_status = gsub('.*\\-', '', docket_number),
         filing_term = gsub('\\-.*', '', docket_number),
         IFP_status = ifelse(as.numeric(IFP_status) >= 5000, 'IFP', 'Paid'),
         filing_term = as.numeric(paste0('20', filing_term)))  %>%
  mutate(court_origin_type = ifelse(grepl('United States', lower_court), 'Federal', 'State/Other'),
         court_origin_type = ifelse(lower_court == 'Habeas', 'Habeas', court_origin_type)) %>%
  select(docket_number, filing_term, case_title, petitioner, respondent, counsel, docketed, linked_with, lower_court, court_origin_type, IFP_status, docket, scotus_docket_url)

save(dockets_OT01_OT22, file = 'docket_parser/dockets_OT01_OT22_Update_cleaned.rdata')




