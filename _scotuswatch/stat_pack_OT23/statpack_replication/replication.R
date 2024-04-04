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








