################################################################################
# Docket Parser - SCOTUSWatch
# Developed by Jake S. Truscott
# Updated January 2024
################################################################################


################################################################################
#Load Packages
################################################################################
library(dplyr); library(stringr); library(stringi); library(rvest); library(httr); library(httr2)


################################################################################
# Get Docket Numbers (2001 to 2022 Terms - SCDB)
################################################################################

load("~/GitHub/jaketruscott.github.io/_scotuswatch/docket_parser/SCDB_2023_01_caseCentered_Citation.Rdata") #Load SCDB
scdb <- SCDB_2023_01_caseCentered_Citation

scdb <- scdb %>%
  select(term, docket) %>%
  filter(term >= 2017)

docket_number = '15-1498'
url <- 'https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/15-1498.html'


docket_parser <- function(dockets){

  dockets_final <- data.frame()

  tryCatch(
    for (d in 1:length(dockets)){

    base_url <- 'https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/'
    url <- paste0(base_url, dockets[d], ".html")


    html_content <- read_html(url)

    docketinfo_table <- html_nodes(html_content, xpath = '//*[@id="docketinfo"]') %>% html_table(fill = TRUE)
    proceedings_table <- html_nodes(html_content, xpath = '//*[@id="proceedings"]') %>% html_table(fill = TRUE)
    contacts_table <- html_nodes(html_content, xpath = '//*[@id="Contacts"]') %>% html_table(fill = TRUE)

    {
      docket_info <- data.frame(docketinfo_table)
      names(docket_info) <- c('input', 'info')
      docket_info <- docket_info %>%
        filter(!input == "") %>%
        mutate(input = gsub("\\:", '', input)) %>%
        mutate(input = tolower(gsub(' ', '_', input))) %>%
        mutate(info = ifelse(grepl('linked_with', input), gsub('LINKED\\_WITH\\_', '', toupper(input)), info)) %>%
        mutate(input = ifelse(grepl('linked_with', input), 'linked_with', input)) %>%
        mutate(input = ifelse(grepl('case_number', input), 'lc_case_number', input))

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
          grepl('Other|other', X1, ignore.case = T) ~ 'Other'
        ))

      docket_contacts <- tidyr::fill(docket_contacts, type)

      petitioner_contacts <- docket_contacts %>%
        filter(type == 'Petitioner') %>%
        filter(!X1 == '') %>%
        filter(!grepl('(Attorney|Attorneys)s for (Petitioner|Petitioners|Appellant|Appellants)', X1, ignore.case = T)) %>%
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
          mutate(name = trimws(name)) %>%
          mutate(party_type = 'Petitioner')

        petitioner_counsel <- bind_rows(petitioner_counsel, petitioner_contacts_temp)
      }


      respondent_contacts <- docket_contacts %>%
        filter(type == 'Respondent') %>%
        filter(!X1 == '') %>%
        filter(!grepl('(Attorney|Attorneys) for (Respondent|Respondents|Appellee|Appellees)', X1, ignore.case = T)) %>%
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
          mutate(name = trimws(name)) %>%
          mutate(party_type = 'Respondent')

        respondent_counsel <- bind_rows(respondent_counsel, respondent_contacts_temp)
      }


      other_contacts <- docket_contacts %>%
        filter(type == 'Other') %>%
        filter(!X1 == '') %>%
        filter(!X1 == 'Other') %>%
        filter(!X1 == 'other')

      if (nrow(other_contacts) == 0){
        other_counsel = other_contacts
      }  else {
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
            mutate(name = trimws(name)) %>%
            mutate(party_type = 'Other/Amicus')

          other_counsel <- bind_rows(other_counsel, other_contacts_temp)
        }

      }




      counsel <- list()

      counsel[['Petitioner']] <- petitioner_counsel
      counsel[['Respondent']] <- respondent_counsel
      counsel[['Other_Amicus']] <- other_counsel
    } #Attorneys ('counsel'[[Petitioner/Respondent/Other_Amicus]] returned)


    docket_combined <- cbind(docket_info_temp,
                             docket = I(list(docket_proceedings)),
                             counsel = I(list(counsel)))

    dockets_final <- bind_rows(dockets_final, docket_combined)

    if (d %% 5 == 0){
      message('Completed Docket ', d, ' of ', length(dockets))
    }

  }
  )



  message('Completion Summary: ')
  message('   Dockets Completed: ', nrow(dockets_final))

  return(dockets_final)


}

test <- docket_parser(dockets = c('22-324', '19-1392'))
