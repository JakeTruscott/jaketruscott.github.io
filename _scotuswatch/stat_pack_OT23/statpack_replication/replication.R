################################################################################
# Empirical SCOTUS StatPack OT2023 Replication
# Code Developed by Jake S. Truscott, Ph.D (CCSE, Purdue)
# Updated April 2024
################################################################################

################################################################################
# Data Disclosure & Notes
################################################################################
' We are grateful to the organized efforts of those maintaining Oyez (Chicago-Kent) and the Supreme Court Database (WashU St. Louis). We identify sources of data not explicitly collected or processed by us in the code below.

----------------------
Recommended Citations:
----------------------
Oyez: Oyez. (n.d.). Retrieved from https://www.oyez.org/

Supreme Court Database: Spaeth, H. J., Epstein, L., Martin, A. D., Segal, J. A., Ruger, T. J., & Benesh, S. C. (2023). Supreme Court Database, Version 2023 Release 01. Retrieved from http://supremecourtdatabase.org

Statpack: Feldman, A. & Truscott, J. S. (2024, June 30). Supreme Court 2023-2024 Term Stat Pack (Version 0.1). EmpiricalSCOTUS. Available at: https://empiricalscotus.com/ '

################################################################################
# Load Packages
################################################################################
library(kableExtra); library(dplyr);  library(tidyr); library(scotustext); library(htmltools); library(ggplot2); library(png); library(dplyr); library(stringi); library(stringr); library(ggplot2); library(ggthemes); library(anytime); library(tm); library(scotustext); library(readxl); library(ggpattern); library(png); library(ggtext); library(grid); library(tidyr); library(readxl); library(anytime)


################################################################################
# Load Data
################################################################################


################################################################################
# Decisions
# Word Counts
# Completed Using scotustext (R)
################################################################################

decisions_23 <- scotustext::decision_processor(dir_path = "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/ot23_decisions/decision_pdfs_OT23") #OT23

earlier_decisions <- list()
earlier_decisions_path <- list.files('C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/ot23_decisions/earlier_term_decisions', full.names = T)


for (i in 1:length(earlier_decisions_path)){

  temp_term_path <- earlier_decisions_path[i]
  temp_term <- gsub('.*\\/', '', temp_term_path)

  message('Beginning ', temp_term)

  temp_processed_decisions <- suppressMessages(scotustext::decision_processor(dir_path = temp_term_path))

  earlier_decisions[[as.character(temp_term)]] <- temp_processed_decisions

  message('\nCompleted ', temp_term, '... Moving On')


}


################################################################################
#Oral Arguments
# Calendar
# Participation - Justices & Attorneys
# Time & Word Counts - By Argument
################################################################################

{

  base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
  rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
  oa23 <- get(load(url(rdata_url)))

} #Load OT2023 OA Data

{

  arguments <- oa23 %>%
    filter(speaker_type == 'Attorney') %>%
    select(sitting, case_name, docket, speaker, speaker_type) %>%
    unique() %>%
    group_by(case_name, docket, sitting) %>%
    mutate(attorney_count = max(row_number())) %>%
    reframe(speaker = paste(speaker, collapse = " \\\\ & &  "),
            attorney_count = attorney_count) %>%
    unique() %>%
    mutate(case_name = paste0('multirow{', attorney_count, '}{=}{', case_name, '}'),
           docket = paste0('multirow{', attorney_count, '}{*}{', docket, '}')) %>%
    rowwise() %>%
    mutate(latex = paste0(case_name, ' & ', docket, ' & ', speaker, ' \\\\ \\addlinespace')) %>%
    filter(sitting == 'February') %>%
    unique() %>%
    select(latex)

  cat(arguments$latex)





} #Clean & Compile Table







