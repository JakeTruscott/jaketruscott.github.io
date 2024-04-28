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

# As of 04/27 -
# Regular: Up Through 23-1162
# IFP: Through 23-7323
# App: Through 23-965
# M: Through 23M95
################################################################################

dockets_through_4_9 <- c(paste0('23-', 1:1162), paste0('23-', 5001:7323), paste0('23M', 1:95), paste0('23A', 1:965))


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

      Sys.sleep(10)

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


docket_OT23_4_9_update <- docket_parser(dockets = dockets_through_4_9)


################################################################################
# Get 2010 to 2023 Dockets
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



