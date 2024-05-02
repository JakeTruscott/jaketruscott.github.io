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
library(kableExtra); library(dplyr);  library(tidyr); library(scotustext); library(htmltools); library(ggplot2); library(png); library(dplyr); library(stringi); library(stringr); library(ggplot2); library(ggthemes); library(anytime); library(tm); library(scotustext); library(readxl); library(ggpattern); library(png); library(ggtext); library(grid); library(tidyr); library(readxl); library(anytime); library(sf); library(purrr); library(readxl)


################################################################################
# Load Data
################################################################################
{
  ROBERTS <- readPNG("stat_pack_OT23/Figures/justice_images/Roberts.png")
  ALITO <- readPNG("stat_pack_OT23/Figures/justice_images/Alito.png")
  THOMAS <- readPNG("stat_pack_OT23/Figures/justice_images/Thomas.png")
  SOTOMAYOR <- readPNG("stat_pack_OT23/Figures/justice_images/Sotomayor.png")
  KAGAN <- readPNG("stat_pack_OT23/Figures/justice_images/Kagan.png")
  GORSUCH <- readPNG("stat_pack_OT23/Figures/justice_images/Gorsuch.png")
  KAVANAUGH <- readPNG("stat_pack_OT23/Figures/justice_images/Kavanaugh.png")
  BARRETT <- readPNG("stat_pack_OT23/Figures/justice_images/Barrett.png")
  JACKSON <- readPNG("stat_pack_OT23/Figures/justice_images/Jackson.png")
  BREYER <- readPNG("stat_pack_OT23/Figures/justice_images/Breyer.png")
  GINSBURG <- readPNG("stat_pack_OT23/Figures/justice_images/Ginsburg.png")
  KENNEDY <- readPNG("stat_pack_OT23/Figures/justice_images/Kennedy.png")

  justice_image_labels <- c(
    ALITO = "<img src='stat_pack_OT23/Figures/justice_images/Alito.png' width='75' /><br>",
    ROBERTS = "<img src='stat_pack_OT23/Figures/justice_images/Roberts.png' width='75' /><br>",
    THOMAS = "<img src='stat_pack_OT23/Figures/justice_images/Thomas.png' width='75' /><br>",
    SOTOMAYOR = "<img src='stat_pack_OT23/Figures/justice_images/Sotomayor.png' width='75' /><br>",
    KAGAN = "<img src='stat_pack_OT23/Figures/justice_images/Kagan.png' width='75' /><br>",
    GORSUCH = "<img src='stat_pack_OT23/Figures/justice_images/Gorsuch.png' width='75' /><br>",
    KAVANAUGH = "<img src='stat_pack_OT23/Figures/justice_images/Kavanaugh.png' width='75' /><br>",
    BARRETT = "<img src='stat_pack_OT23/Figures/justice_images/Barrett.png' width='75' /><br>",
    JACKSON = "<img src='stat_pack_OT23/Figures/justice_images/Jackson.png' width='75' /><br>",
    BREYER = "<img src='stat_pack_OT23/Figures/justice_images/Breyer.png' width='75' /><br>",
    GINSBURG = "<img src='stat_pack_OT23/Figures/justice_images/Ginsburg.png' width='75' /><br>",
    KENNEDY = "<img src='stat_pack_OT23/Figures/justice_images/Kennedy.png' width='75' /><br>"
  )
} #Justice Images (Oyez)
{

  shorthand_case_names <- read.csv('ot23_decisions/shorthand_case_names.csv', as.is = T)


} #Shorthand Case Names...
{

  scdb_justice_names <- data.frame(
    justice = c(106, 108, 109:118),
    justice_name = c('KENNEDY', 'THOMAS', 'GINSBURG', 'BREYER', 'ROBERTS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON'))

} #SCDB Justice ID Conversion
{
  load("stat_pack_OT23/statpack_replication/Misc Data/scdb_cases_2023.rdata")
  load("stat_pack_OT23/statpack_replication/Misc Data/scdb_justices_2023.rdata")

} #SCDB Data (Updated September 2023)

################################################################################
# Decisions
################################################################################


{

  decisions_ot_23 <- read.csv(file = "ot23_decisions/OT_23_Decisions.csv")
  decisions_ot_23 <- decisions_ot_23 %>%
    mutate(`Case` = gsub('\\,', '', `Case`),
           Decision = gsub('\\,', '', Decision))


} # Load OT23 Decisions Table

decisions_23 <- scotustext::decision_processor(dir_path = "ot23_decisions/decision_pdfs_OT23") #OT23

earlier_decisions <- get(load('ot23_decisions/earlier_decisions_processed.rdata'))

combined_decisions


{

  decisions_matrix_data <- decisions_ot_23 %>%
    select(ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON) %>%
    mutate(across(everything(), ~ ifelse(. > 0, 1, 0))) #Get Justices and Replace Vote w/ Majority or Dissent
  judge_matrix <- as.matrix(decisions_matrix_data) #Convert to Matrix

  n_judges <- ncol(decisions_matrix_data) #Number of Justices
  agreement_matrix <- matrix(NA, nrow = n_judges, ncol = n_judges) #Create Empty Matrix

  # Calculate the percentage agreement between each pair of judges
  for (i in 1:n_judges) {
    for (j in 1:n_judges) {
      agreement_matrix[i, j] <- round(sum(decisions_matrix_data[, i] == decisions_matrix_data[, j]) / nrow(decisions_matrix_data) * 100, 2)
    }
  } #Calculate Agreement Percentage

  for (i in 1:ncol(agreement_matrix)) {
    for (j in 1:ncol(agreement_matrix)) {
      if (i == j) {
        agreement_matrix[i, j] <- ""
      } else if (i < j) {
        agreement_matrix[i, j] <- ""
      }
    }
  } #Replace Diagonal & Above w/ ""

  colnames(agreement_matrix) <- colnames(decisions_matrix_data) #Add Column Names
  rownames(agreement_matrix) <- colnames(decisions_matrix_data) #Add Row Names

  row_names_column <- paste0(stringr::str_to_title(colnames(decisions_matrix_data)), ".png") #Add Row Name for Image Path

  agreement_matrix <- data.frame(cbind(row_names_column, agreement_matrix)) #Combine Row Names Column to Matrix
  names(agreement_matrix)[1] <- ""


  write.table(agreement_matrix, file = 'stat_pack_OT23/Tables/decision_tables/agreement_matrix.csv', sep = ',', quote = FALSE, row.names = F) #Save

  agreement_matrix[,1] = gsub('\\.png', '', toupper(rownames(agreement_matrix)))

  write.table(agreement_matrix, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Agreement Matrix/OT23_justice_agreement_matrix.csv', sep = ',', quote = FALSE, row.names = F) #Save



} # Justice Agreement Vote Matrix (OT23)

{

  for (i in unique(shorthand_case_names$sitting)){

    decision_descriptions_test <- decisions_ot_23 %>%
      rowwise() %>%
      mutate(Coalition = ifelse(any(c_across(ROBERTS:JACKSON) %in% c(2,4,7)), paste0(Coalition, '*'), Coalition)) %>%
      ungroup() %>%
      select(Docket, Date_Argued, Date_Decided, Lower_Court, Decision, Author, Coalition) %>%
      rename(docket = Docket) %>%
      left_join(shorthand_case_names, by = 'docket') %>%
      mutate(docket = paste0('(', docket, ')')) %>%
      mutate(description = 'This is a test. Your station is conducting a test of the Emergency Broadcast System. This is only a test.') %>%
      mutate(Decision = gsub('\\&', '\\\\&', Decision)) %>%
      select(short_hand, description, docket, Lower_Court, Decision, Author, Coalition, sitting) %>%
      filter(sitting == i) %>%
      select(-c(sitting))

    write.table(decision_descriptions_test, file = paste0('stat_pack_OT23/Tables/decision_tables/decision_description_', i, '.csv'), sep = ',', quote = FALSE, row.names = F)

    write.table(decision_descriptions_test, file = paste0('stat_pack_OT23/Statpack Replication Data/Decisions/Decision Descriptions/', i, '_Decision_Descriptions_OT23.csv'), sep = ',', quote = F, row.names = F)


  }


} # Decision-Level Breakdowns

{

  opinion_type_by_justice_ot23 <- decisions_ot_23 %>%
    select(ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON) %>%
    pivot_longer(cols = c(ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON), names_to = "justice", values_to = "decision") %>%
    mutate(decision_type = case_when(
      .default = NA,
      decision == 100 ~ "Majority",
      decision == 2 ~ "Concurrence",
      decision == 4 ~ "Concurrence",
      decision == 5 ~ "Other Concurrence",
      decision == 7 ~ "Other Concurrence",
      decision == -1 ~ "Dissent",
      decision == -3 ~ "Dissent")) %>%
    select(-c(decision)) %>%
    filter(!is.na(decision_type)) %>%
    group_by(justice, decision_type) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = decision_type, values_from = count, values_fill = 0) %>%
    select(justice, Majority, Concurrence, `Other Concurrence`, Dissent)

  write.table(opinion_type_by_justice_ot23, file = 'stat_pack_OT23/Tables/decision_tables/opinion_type_by_justice_ot23.csv', sep = ',', quote = FALSE, row.names = F) #Save



} #Table of Opinion Types by Justice (OT23)

{


  decisions_by_justice_1 <- decisions_ot_23 %>%
    select(Case, Docket, Coalition, ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN) %>%
    pivot_longer(cols = c(ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN), names_to = "justice", values_to = "decision") %>%
    mutate(justice = factor(justice, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN'))) %>%
    filter(decision == 100) %>%
    select(-c(decision)) %>%
    arrange(justice) %>%
    rename(docket = Docket) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, Coalition, justice) %>%
    mutate(Case = paste0(short_hand, ' ', Coalition)) %>%
    select(Case, justice) %>%
    group_by(justice) %>%
    mutate(justice_count = row_number()) %>%
    mutate(justice = ifelse(justice_count == 1, as.character(justice), "")) %>%
    select(-c(justice_count))

  write.table(decisions_by_justice_1, file = 'stat_pack_OT23/Tables/decision_tables/decisions_by_justice_1.csv', sep = ',', quote = FALSE, row.names = F) #Save

  decisions_by_justice_2 <- decisions_ot_23 %>%
    select(Case, Docket, Coalition, GORSUCH, KAVANAUGH, BARRETT, JACKSON) %>%
    pivot_longer(cols = c(GORSUCH, KAVANAUGH, BARRETT, JACKSON), names_to = "justice", values_to = "decision") %>%
    mutate(justice = factor(justice, levels = c('GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON'))) %>%
    filter(decision == 100) %>%
    select(-c(decision)) %>%
    arrange(justice) %>%
    rename(docket = Docket) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, Coalition, justice) %>%
    mutate(Case = paste0(short_hand, ' ', Coalition)) %>%
    select(Case, justice) %>%
    group_by(justice) %>%
    mutate(justice_count = row_number()) %>%
    mutate(justice = ifelse(justice_count == 1, as.character(justice), "")) %>%
    select(-c(justice_count))

  write.table(decisions_by_justice_2, file = 'stat_pack_OT23/Tables/decision_tables/decisions_by_justice_2.csv', sep = ',', quote = FALSE, row.names = F) #Save


} # Majority Opinions by Justice (OT23)

{

  decisions_by_coalition <- decisions_ot_23 %>%
    rename(docket = Docket) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, Coalition) %>%
    rename(case = short_hand,
           coalition = Coalition) %>%
    mutate(case = ifelse(grepl('U\\.\\S\\.', case), case, gsub(' v\\..*', '', case)))

  max_decision_rows <- decisions_by_coalition %>%
    group_by(coalition) %>%
    summarise(count = n()) %>%
    select(count) %>%
    filter(count == max(count))

  coalition_types <- c('(9-0)', '(8-1)', '(7-2)', '(6-3)', '(5-4)')

  decisions_by_coalition_combined <- data.frame(matrix(nrow = max_decision_rows$count, ncol = 5))
  colnames(decisions_by_coalition_combined) <- factor(coalition_types)

  for (i in unique(decisions_by_coalition$coalition)){

    temp_coalition <- i
    temp_decisions <- decisions_by_coalition %>%
      filter(coalition == i)
    temp_cases <- temp_decisions$case

    for (case in 1:length(temp_cases)){
      decisions_by_coalition_combined[case, temp_coalition] <- temp_cases[case]
    }


  } #Populate Coalition-Level Cases

  decisions_by_coalition_combined <- decisions_by_coalition_combined %>%
    mutate_all(~ifelse(is.na(.), "       ", .))


  write.table(decisions_by_coalition_combined, file = 'stat_pack_OT23/Tables/decision_tables/decisions_by_coalition.csv', row.names = F, quote = F, sep = ',')

  write.table(decisions_by_coalition_combined, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Decisions by Coalition/decisions_by_coalition_OT23.csv', row.names = F, quote = F, sep = ',')


} #Cases by Coalition Type

{

  decisions_by_coalition_longitudinal <- decisions_ot_23 %>%
    rename(docket = Docket) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, Coalition) %>%
    rename(case = short_hand,
           coalition = Coalition) %>%
    mutate(coalition = case_when(
      .default = '(9-0)',
      coalition %in% c('(8-1', '(8-0)') ~ '(8-1) or (8-0)',
      coalition %in% c('(7-2)', '(7-1)') ~  '(7-2) or (7-1)',
      coalition %in% c('(6-3)', '(6-2)') ~ '(6-3) or (6-2)',
      coalition %in% c('(5-4)', '(5-3)') ~ '(5-4) or (5-3)',
      coalition == '(4-4)' ~ '(4-4)')) %>%
    mutate(case = ifelse(grepl('U\\.\\S\\.', case), case, gsub(' v\\..*', '', case)))

  decisions_by_coalition_2018_2022 <- scdb_cases_2023 %>%
    filter(term >= 2018) %>%
    mutate(coalition = case_when(
      majVotes == 9 ~ '(9-0)',
      majVotes == 8 ~ '(8-1) or (8-0)',
      majVotes == 7 ~ '(7-2) or (7-1)',
      majVotes == 6 ~ '(6-3) or (6-2)',
      majVotes == 5 ~ '(5-4) or (5-3)',
      majVotes == 4 ~ '(4-4)')) %>%
    filter(!majVotes == 4) %>%
    select(term, coalition, docket) %>%
    bind_rows(decisions_by_coalition_longitudinal %>%
                mutate(term = 2023)) %>%
    group_by(term, coalition) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = factor(term), y = count, group = coalition)) +
    geom_bar(stat = 'identity', fill = 'gray50', position = position_dodge2(0.9), colour = 'gray5') +
    scale_y_continuous(lim = c(0, 40), breaks = seq(10, 40, 10)) +
    facet_wrap(~coalition, nrow = 6) +
    geom_text(aes(label = count), vjust = -1) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    labs(
      x = '\nTerm\n',
      y = '\nCount\n') +
    theme(legend.position = 'none',
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'),
          axis.text = element_text(size = 12, colour = 'black'),
          axis.title = element_text(size = 12, colour = 'black'))


    ggsave("stat_pack_OT23/Figures/statpack_figures/decisions_by_coalition_2018_2023.png", decisions_by_coalition_2018_2022, dpi = 300)


    decisions_by_coalition_2018_2022 <- scdb_cases_2023 %>%
      filter(term >= 2018) %>%
      mutate(coalition = case_when(
        majVotes == 9 ~ '(9-0)',
        majVotes == 8 ~ '(8-1) or (8-0)',
        majVotes == 7 ~ '(7-2) or (7-1)',
        majVotes == 6 ~ '(6-3) or (6-2)',
        majVotes == 5 ~ '(5-4) or (5-3)',
        majVotes == 4 ~ '(4-4)')) %>%
      filter(!majVotes == 4) %>%
      select(term, coalition, docket) %>%
      bind_rows(decisions_by_coalition_longitudinal %>%
                  mutate(term = 2023)) %>%
      group_by(term, coalition) %>%
      summarise(count = n())

    write.table(decisions_by_coalition_2018_2022, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Decisions by Coalition/decisions_by_coalition_OT18_OT23.csv', sep = ',', quote = F, row.names = F)

    } #Distribution of Coalitions (Current v. Past)

{

  decisions_by_coalition_longitudinal <- decisions_ot_23 %>%
    rename(docket = Docket) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    select(short_hand, Coalition) %>%
    rename(case = short_hand,
           coalition = Coalition) %>%
    mutate(coalition = case_when(
      .default = '(9-0)',
      coalition %in% c('(8-1', '(8-0)') ~ '(8-1) or (8-0)',
      coalition %in% c('(7-2)', '(7-1)') ~  '(7-2) or (7-1)',
      coalition %in% c('(6-3)', '(6-2)') ~ '(6-3) or (6-2)',
      coalition %in% c('(5-4)', '(5-3)') ~ '(5-4) or (5-3)',
      coalition == '(4-4)' ~ '(4-4)')) %>%
    mutate(case = ifelse(grepl('U\\.\\S\\.', case), case, gsub(' v\\..*', '', case)))

  share_of_unanimity <- scdb_cases_2023 %>%
    filter(term >= 2018) %>%
    mutate(coalition = case_when(
      majVotes == 9 ~ '(9-0)',
      majVotes == 8 ~ '(8-1) or (8-0)',
      majVotes == 7 ~ '(7-2) or (7-1)',
      majVotes == 6 ~ '(6-3) or (6-2)',
      majVotes == 5 ~ '(5-4) or (5-3)',
      majVotes == 4 ~ '(4-4)')) %>%
    select(term, coalition, docket) %>%
    bind_rows(decisions_by_coalition_longitudinal %>%
                mutate(term = 2023)) %>%
    mutate(coalition = ifelse(coalition == "(9-0)", "(9-0)", "Other")) %>%
    group_by(term, coalition) %>%
    summarise(count = n()) %>%
    group_by(term) %>%
    reframe(total_cases = sum(count),
            count = count,
            coalition = coalition) %>%
    mutate(percent = round((count/total_cases)*100, 2),
           term = paste0(term, ' Term (', total_cases, ')')) %>%
    ggplot(aes(x = "", y = percent, fill = coalition)) +
    geom_bar(stat = 'identity', colour = 'gray5') +
    coord_polar(theta = "y", start = 0) +
    facet_wrap(~term) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('deepskyblue3', 'coral')) +
    geom_label(aes(label = paste0(percent, ' %')), position = position_stack(vjust = 0.5), color = "gray5", size=5, show.legend = F) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))


  ggsave("stat_pack_OT23/Figures/statpack_figures/share_of_unanimity_2018_2023.png", share_of_unanimity, dpi = 300)


  share_of_unanimity <- scdb_cases_2023 %>%
    filter(term >= 2018) %>%
    mutate(coalition = case_when(
      majVotes == 9 ~ '(9-0)',
      majVotes == 8 ~ '(8-1) or (8-0)',
      majVotes == 7 ~ '(7-2) or (7-1)',
      majVotes == 6 ~ '(6-3) or (6-2)',
      majVotes == 5 ~ '(5-4) or (5-3)',
      majVotes == 4 ~ '(4-4)')) %>%
    select(term, coalition, docket) %>%
    bind_rows(decisions_by_coalition_longitudinal %>%
                mutate(term = 2023)) %>%
    mutate(coalition = ifelse(coalition == "(9-0)", "(9-0)", "Other")) %>%
    group_by(term, coalition) %>%
    summarise(count = n()) %>%
    group_by(term) %>%
    reframe(total_cases = sum(count),
            count = count,
            coalition = coalition) %>%
    mutate(percent = round((count/total_cases)*100, 2),
           term = paste0(term, ' Term (', total_cases, ')'))

  write.table(share_of_unanimity, file = 'stat_pack_OT23/Statpack replication Data/Decisions/Share of Unanimity/share_of_unanimity_OT18_OT23.csv', sep = ",", quote = F, row.names = F)

} #Share of Unanimity Over Time

{


  opinion_type_share_18_23 <- scdb_justices_2023 %>%
    filter(term >= 2018) %>%
    select(vote, docket, term) %>%
    mutate(justice_vote = case_when(
      .default = 'Joined Majority',
      vote %in% c(2, 6, 7) ~ 'Dissent',
      vote %in% c(3:5) ~ 'Concurrence',
      vote == 8 ~ 'Equally Divided')) %>%
    filter(!justice_vote == 'Equally Divided') %>%
    mutate(justice_vote = ifelse(justice_vote == 'Joined Majority', 'Majority', 'Other')) %>%
    group_by(docket, justice_vote) %>%
    reframe(count = n(),
              term) %>%
    unique() %>%
    group_by(docket) %>%
    reframe(total_voting = sum(count),
              justice_vote,
              term,
              docket,
            count)  %>%
    group_by(term) %>%
    pivot_wider(values_from = count, names_from = justice_vote) %>%
    mutate(other_votes = ifelse(Other == 0 | is.na(Other), 0, 1)) %>%
    group_by(term) %>%
    reframe(total_cases = length(unique(docket)),
            term,
            concurrence_dissent = sum(other_votes)) %>%
    unique() %>%
    mutate(majority_only = total_cases - concurrence_dissent) %>%
    pivot_longer(cols = c(concurrence_dissent, majority_only), names_to = 'votes') %>%
    mutate(percent = round((value/total_cases)*100, 2)) %>%
    bind_rows( decisions_ot_23 %>%
                 rowwise() %>%
                 mutate(other_votes = ifelse(any(!c_across(ROBERTS:JACKSON) %in% c(1, 100)), "concurrence_dissent", "majority_only")) %>%
                 select(other_votes) %>%
                 group_by(other_votes) %>%
                 reframe(value = n(),
                         votes = other_votes,
                         total_cases = nrow(decisions_ot_23)) %>%
                 unique() %>%
                 mutate(term = 2023,
                        percent = round((value/total_cases)*100, 2)) %>%
                 select(term, total_cases, votes, value, percent)) %>%
    mutate(votes = ifelse(votes == 'majority_only', 'Majority Only', 'Concurrences and (or) Dissents'),
           votes = factor(votes, levels = c('Majority Only', 'Concurrences and (or) Dissents')),
           term = paste0(term, ' Term (', total_cases, ')')) %>%
    ggplot(aes(x = "", y = percent, fill = votes)) +
    geom_bar(stat = 'identity', colour = 'gray5') +
    coord_polar(theta = "y", start = 0) +
    facet_wrap(~term) +
    theme_void() +
    labs(
      x = ' ',
      y = ' ',
      fill = ' ') +
    scale_fill_manual(values = c('deepskyblue3', 'coral')) +
    geom_label(aes(label = paste0(percent, ' %')), position = position_stack(vjust = 0.5), color = "gray5", size=5, show.legend = F) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 15, colour = 'gray5'),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = 12),
          axis.line = element_line(colour = 'black'),
          strip.text = element_text(size = 12, colour = 'black', face = 'bold',
                                    margin = margin(b = 10), vjust = -1, hjust = 0.5),
          strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
          panel.background = element_rect(size = 1, fill = 'white', colour = 'black'))


  ggsave("stat_pack_OT23/Figures/statpack_figures/opinion_type_share_18_23.png", opinion_type_share_18_23, dpi = 300)


  opinion_type_share_18_23 <- scdb_justices_2023 %>%
    filter(term >= 2018) %>%
    select(vote, docket, term) %>%
    mutate(justice_vote = case_when(
      .default = 'Joined Majority',
      vote %in% c(2, 6, 7) ~ 'Dissent',
      vote %in% c(3:5) ~ 'Concurrence',
      vote == 8 ~ 'Equally Divided')) %>%
    filter(!justice_vote == 'Equally Divided') %>%
    mutate(justice_vote = ifelse(justice_vote == 'Joined Majority', 'Majority', 'Other')) %>%
    group_by(docket, justice_vote) %>%
    reframe(count = n(),
            term) %>%
    unique() %>%
    group_by(docket) %>%
    reframe(total_voting = sum(count),
            justice_vote,
            term,
            docket,
            count)  %>%
    group_by(term) %>%
    pivot_wider(values_from = count, names_from = justice_vote) %>%
    mutate(other_votes = ifelse(Other == 0 | is.na(Other), 0, 1)) %>%
    group_by(term) %>%
    reframe(total_cases = length(unique(docket)),
            term,
            concurrence_dissent = sum(other_votes)) %>%
    unique() %>%
    mutate(majority_only = total_cases - concurrence_dissent) %>%
    pivot_longer(cols = c(concurrence_dissent, majority_only), names_to = 'votes') %>%
    mutate(percent = round((value/total_cases)*100, 2)) %>%
    bind_rows( decisions_ot_23 %>%
                 rowwise() %>%
                 mutate(other_votes = ifelse(any(!c_across(ROBERTS:JACKSON) %in% c(1, 100)), "concurrence_dissent", "majority_only")) %>%
                 select(other_votes) %>%
                 group_by(other_votes) %>%
                 reframe(value = n(),
                         votes = other_votes,
                         total_cases = nrow(decisions_ot_23)) %>%
                 unique() %>%
                 mutate(term = 2023,
                        percent = round((value/total_cases)*100, 2)) %>%
                 select(term, total_cases, votes, value, percent)) %>%
    mutate(votes = ifelse(votes == 'majority_only', 'Majority Only', 'Concurrences and (or) Dissents'),
           votes = factor(votes, levels = c('Majority Only', 'Concurrences and (or) Dissents')),
           term = paste0(term, ' Term (', total_cases, ')'))


  write.table(opinion_type_share_18_23, file = 'stat_pack_OT23/Statpack Replication Data/Decisions/Share of Unanimity/opinion_type_share_OT18_OT23.csv', quote = F, row.names = F, sep = ',')

} #Cases with Dissents/Concurrence Over Time

{


} # Word Counts

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

  for (i in unique(scotus_OT23$sitting)){

    attorneys <- scotus_OT23 %>%
      filter(sitting == i) %>%
      mutate(response_to = ifelse(lag(speaker_type) == 'Justice', lag(speaker), NA)) %>%
      filter(speaker_type == "Attorney") %>%
      mutate(speaker = ifelse(speaker == 'MR. SYNDER', 'MR. SNYDER', speaker)) %>%
      filter(!is.na(response_to)) %>%
      group_by(docket, speaker, response_to) %>%
      summarise(total_words = sum(word_count, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(response_to = gsub("(CHIEF JUSTICE |JUSTICE )", "", response_to)) %>%
      pivot_wider(names_from = response_to, values_from = total_words) %>%
      mutate(total_words = rowSums(select(., -c(docket, speaker)), na.rm = TRUE)) %>%
      select(docket, speaker, total_words) %>%
      mutate(speaker = gsub('(\\, Jr\\.| II$| III$)', '', speaker),
             speaker = str_extract(speaker, "\\b\\w+$")) %>%
      group_by(docket) %>%
      reframe(combined = paste0('\\\\ ', speaker, ' (', total_words, ')')) %>%
      group_by(docket) %>%
      summarize(combined = paste(combined, collapse = " ")) %>%
      left_join(shorthand_case_names, by = 'docket') %>%
      select(short_hand, combined, order) %>%
      arrange(order) %>%
      select(-c(order)) %>%
      rename(case_name = short_hand) %>%
      mutate(case_name = gsub('\\,', '', case_name))

    output_path = paste0("stat_pack_OT23/Tables/oral_argument_speaking/attorney_participation_", i, ".csv")
    write.table(attorneys, file = output_path,  row.names = F, quote = F, sep = ',')

    attorneys <- scotus_OT23 %>%
      filter(sitting == i) %>%
      mutate(response_to = ifelse(lag(speaker_type) == 'Justice', lag(speaker), NA)) %>%
      filter(speaker_type == "Attorney") %>%
      mutate(speaker = ifelse(speaker == 'MR. SYNDER', 'MR. SNYDER', speaker)) %>%
      filter(!is.na(response_to)) %>%
      group_by(case_name, speaker, response_to) %>%
      summarise(total_words = sum(word_count, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(response_to = gsub("(CHIEF JUSTICE |JUSTICE )", "", response_to)) %>%
      pivot_wider(names_from = response_to, values_from = total_words) %>%
      mutate(total_words = rowSums(select(., -c(case_name, speaker)), na.rm = TRUE)) %>%
      select(case_name, speaker, total_words) %>%
      mutate(speaker = gsub('(\\, Jr\\.| II$| III$)', '', speaker),
             speaker = str_extract(speaker, "\\b\\w+$"),
             case_name = gsub('\\,', '', case_name))

    output_path = paste0("stat_pack_OT23/Statpack Replication Data/Oral Arguments/Attorney Participation/", i, "_Sitting_Calendar_OT23.csv")
    write.csv(attorneys, file = output_path,  row.names = F, quote = F)

  }


} #Attorney Participation Table - Shortened for StatPack

{


  for (i in unique(scotus_OT23$sitting)){

     calendar_temp <- scotus_OT23 %>%
      filter(sitting == i) %>%
      filter(speaker_type == 'Attorney') %>%
      select(docket, speaker) %>%
      unique() %>%
      left_join(shorthand_case_names, by = 'docket') %>%
      unique() %>%
      rename(case_name = short_hand) %>%
      group_by(case_name) %>%
      reframe(attorney_count = max(row_number()),
              speaker = speaker,
              docket = docket,
              order = order) %>%
      mutate(speaker = gsub('(\\, Jr\\.| II$| III$)', '', speaker),
             speaker = str_extract(speaker, "\\b\\w+$"),
             case_name = gsub('\\,', '', case_name)) %>%
      group_by(case_name, docket) %>%
      reframe(combined = paste0('\\\\ ', speaker),
              attorney_count = attorney_count,
              order = order) %>%
      group_by(case_name) %>%
      mutate(attorney_number = row_number(),
             combined = ifelse(attorney_count == 2 & attorney_number == 2,  paste0(combined, ' \\\\'), combined)) %>%
      group_by(case_name, docket) %>%
      reframe(combined = paste(combined, collapse = ' '),
                order = order) %>%
       unique() %>%
       arrange(order)



    output_path = paste0("stat_pack_OT23/Tables/oral_argument_speaking/argument_calendar_", i, ".csv")

    write.table(calendar_temp, file = output_path,  row.names = F, quote = F, sep = ',')

  }



} #Argument Calendar

{


  {
    custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

    scotus_term <- scotus_OT23 %>%
      filter(speaker_type == 'Justice') %>%
      group_by(speaker, case_name) %>%
      summarise(total_word_count = sum(word_count)) %>%
      group_by(speaker) %>%
      summarise(total_word_count = sum(total_word_count)) %>%
      mutate(speaker = gsub("(CHIEF JUSTICE |JUSTICE )", "", speaker)) %>%
      mutate(speaker = factor(speaker, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON')))

    word_count_plot <- ggplot(data = scotus_term, aes(y = total_word_count, x = speaker, fill = total_word_count)) +
      #scale_y_continuous(labels = scales::comma, breaks = seq(10000, 80000, 10000)) +
      geom_hline(yintercept = 0, colour = 'gray5') +
      geom_col(colour = 'gray5') +
      geom_text(aes(label = total_word_count), vjust = -0.5, size = 5) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) + # Add a vertical line for the y-axis
      scale_fill_gradientn(
        colors = custom_colors,
        breaks = c(min(scotus_term$total_word_count), mean(scotus_term$total_word_count), max(scotus_term$total_word_count)), # Custom breaks
        labels = scales::comma_format(),
        guide = guide_colorbar(
          title = "Total Word Count",
          title.position = "top")) +
      labs(y  = " ",
           x = " ",
           title = " ") +
      scale_x_discrete(labels = justice_image_labels) +
      theme_classic() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = ggtext::element_markdown(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.background = element_rect(linewidth = 1, fill = "NA", colour = "black"),
        legend.box.background = element_rect(fill = NA, colour = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        plot.caption = element_text(hjust = 0.5, size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5))


  } #Totals Graph (Active)
  ggsave("stat_pack_OT23/Figures/statpack_figures/word_count_plot_OT23.png", word_count_plot, dpi = 300)

  oa <- scotus_OT23 %>%
    filter(speaker_type == 'Justice') %>%
    group_by(speaker, docket) %>%
    summarise(total_word_count = sum(word_count)) %>%
    pivot_wider(names_from = speaker, values_from = total_word_count, names_prefix = "word_count_") %>%
    left_join(shorthand_case_names, by = 'docket')

  names(oa) <- gsub('(CHIEF JUSTICE |JUSTICE )', '', names(oa))
  names(oa) <- gsub('word_count_', '', names(oa))

  oa <- oa %>%
    rename(case_name = short_hand) %>%
    relocate(case_name) %>%
    select(-c(docket))

  oa_data <- oa


  for (i in unique(oa_data$sitting)){

    oa_data_temp <- oa_data %>%
      filter(sitting == i) %>%
      arrange(order) %>%
      select(-c(sitting, order)) %>%
      mutate(case_name = gsub('\\,', '', case_name)) %>%
      select(case_name, ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON)

    write.table(oa_data_temp, file = paste0("stat_pack_OT23/Tables/oral_argument_speaking/oral_argument_participation_", i, ".csv"), row.names = F, quote = F, sep = ',')

    write.table(oa_data_temp, file = paste0('stat_pack_OT23/Statpack Replication Data/Oral Arguments/Justice Word Counts/', i, '_Sitting_OT23.csv'), row.names = F, quote = F, sep = ',')

  }


} #Word Count (By Sitting)

{
  custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

  word_counts <- scotus_OT23 %>%
    filter(speaker_type == 'Justice') %>%
    group_by(speaker) %>%
    summarise(word_count = sum(word_count)) %>%
    mutate(speaker = factor(speaker, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON')))

  word_count_plot <- ggplot(data = word_counts, aes(y = word_count, x = speaker, fill = word_count)) +
    geom_hline(yintercept = 0, colour = 'gray5') +
    geom_col(colour = 'gray5') +
    geom_text(aes(label = scales::comma(word_count)), vjust = -0.5, size = 5) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) + # Add a vertical line for the y-axis
    scale_fill_gradientn(
      colors = custom_colors,
      breaks = c(min(word_counts$word_count), mean(word_counts$word_count), max(word_counts$word_count)), # Custom breaks
      labels = scales::comma_format(),
      guide = guide_colorbar(
        title = "Total Word Count",
        title.position = "top")) +
    labs(y  = " ",
         x = " ",
         title = " ") +
    scale_x_discrete(labels = justice_image_labels) +
    theme_classic() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = ggtext::element_markdown(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.background = element_rect(linewidth = 1, fill = "NA", colour = "black"),
      legend.box.background = element_rect(fill = NA, colour = "black"),
      legend.position = "none",
      legend.title = element_blank(),
      legend.title.align = 0.5,
      legend.text = element_text(size = 12),
      plot.caption = element_text(hjust = 0.5, size = 12),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5))

  ggsave(word_count_plot, file = 'stat_pack_OT23/figures/word_count_plot_OT23.png')

} # Word Count (Total Figure)

{

  base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
  rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
  load(url(rdata_url))

  {
    custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

    scotus_term <- scotus_OT23 %>%
      filter(speaker_type == 'Justice') %>%
      mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa', case_name)) %>%
      mutate(time_spoken = text_stop - text_start) %>%
      group_by(speaker) %>%
      summarise(total_time_spoken = sum(time_spoken)) %>%
      mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
      select(speaker, total_time_spoken_minutes) %>%
      mutate(speaker = factor(speaker, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON')))

    total_time_spoken_plot <- ggplot(data = scotus_term, aes(y = total_time_spoken_minutes, x = speaker, fill = total_time_spoken_minutes)) +
      geom_hline(yintercept = 0, colour = 'gray5') +
      geom_col(colour = 'gray5') +
      geom_text(aes(label = total_time_spoken_minutes), vjust = -0.5, size = 5) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) + # Add a vertical line for the y-axis
      scale_fill_gradientn(
        colors = custom_colors,
        breaks = c(min(scotus_term$total_time_spoken_minutes), mean(scotus_term$total_time_spoken_minutes), max(scotus_term$total_time_spoken_minutes)), # Custom breaks
        labels = scales::comma_format(),
        guide = guide_colorbar(
          title = "Total Word Count",
          title.position = "top")) +
      labs(y  = " ",
           x = " ",
           title = " ") +
      scale_x_discrete(labels = justice_image_labels) +
      theme_classic() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = ggtext::element_markdown(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.background = element_rect(linewidth = 1, fill = "NA", colour = "black"),
        legend.box.background = element_rect(fill = NA, colour = "black"),
        legend.position = "none",
        legend.title = element_blank(),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        plot.caption = element_text(hjust = 0.5, size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5))



  } #Totals Graph (Active)
  ggsave("stat_pack_OT23/Figures/statpack_figures/total_time_spoken_plot_OT23.png", total_time_spoken_plot, dpi = 300)

  for (i in unique(scotus_OT23$sitting)){

    time_spoken_total <- scotus_OT23 %>%
      filter(speaker_type == 'Justice') %>%
      filter(sitting == i) %>%
      mutate(time_spoken = text_stop - text_start) %>%
      group_by(speaker, docket) %>%
      summarise(total_time_spoken = sum(time_spoken)) %>%
      mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
      group_by(docket) %>%
      pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
      summarise_all(.funs = sum, na.rm = T) %>%
      mutate(total_time_spoken = rowSums(across(-c(total_time_spoken, docket)))) %>%
      rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
      left_join(shorthand_case_names, by = 'docket') %>%
      arrange(order) %>%
      select(short_hand, total_time_spoken, ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON) %>%
      mutate(short_hand = gsub('\\,', '', short_hand))



    output_path = paste0("stat_pack_OT23/Tables/oral_argument_speaking/oral_argument_speaking_times_", i, '.csv')
    write.table(time_spoken_total, file = output_path, row.names = F, quote = F, sep = ',')


    output_path = paste0("stat_pack_OT23/Statpack Replication Data/Oral Arguments/Justice Speaking Times/", i, '_Sitting_OT23.csv')
    write.table(time_spoken_total, file = output_path, row.names = F, quote = F, sep = ",")


  }




} #Speaking Times (By Sitting)

{

  custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

  time_spoken_total <- scotus_OT23 %>%
    filter(speaker_type == 'Justice') %>%
    mutate(time_spoken = text_stop - text_start) %>%
    group_by(speaker, docket) %>%
    summarise(total_time_spoken = sum(time_spoken)) %>%
    mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
    group_by(docket) %>%
    pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
    summarise_all(.funs = sum, na.rm = T) %>%
    mutate(total_time_spoken = rowSums(across(-c(total_time_spoken, docket)))) %>%
    rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
    left_join(shorthand_case_names, by = 'docket') %>%
    arrange(order) %>%
    select(short_hand, total_time_spoken, ROBERTS, THOMAS, ALITO, SOTOMAYOR, KAGAN, GORSUCH, KAVANAUGH, BARRETT, JACKSON) %>%
    mutate(short_hand = gsub('\\,', '', short_hand)) %>%
    select(-c(short_hand, total_time_spoken)) %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(everything(),
                 names_to = "speaker",
                 values_to = "total_word_count") %>%
    mutate(speaker = factor(speaker, levels = c('ROBERTS', 'THOMAS', 'ALITO', 'SOTOMAYOR', 'KAGAN', 'GORSUCH', 'KAVANAUGH', 'BARRETT', 'JACKSON')))

  custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

  speaking_time_plot <- ggplot(data = time_spoken_total, aes(y = total_word_count, x = speaker, fill = total_word_count)) +
    #scale_y_continuous(labels = scales::comma, breaks = seq(10000, 80000, 10000)) +
    geom_hline(yintercept = 0, colour = 'gray5') +
    geom_col(colour = 'gray5') +
    geom_text(aes(label = scales::comma(total_word_count)), vjust = -0.5, size = 5) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) + # Add a vertical line for the y-axis
    scale_fill_gradientn(
      colors = custom_colors,
      breaks = c(min(time_spoken_total$total_word_count), mean(time_spoken_total$total_word_count), max(time_spoken_total$total_word_count)), # Custom breaks
      labels = scales::comma_format(),
      guide = guide_colorbar(
        title = "Total Word Count",
        title.position = "top")) +
    labs(y  = " ",
         x = " ",
         title = " ") +
    scale_x_discrete(labels = justice_image_labels) +
    theme_classic() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = ggtext::element_markdown(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.background = element_rect(linewidth = 1, fill = "NA", colour = "black"),
      legend.box.background = element_rect(fill = NA, colour = "black"),
      legend.position = "none",
      legend.title = element_blank(),
      legend.title.align = 0.5,
      legend.text = element_text(size = 12),
      plot.caption = element_text(hjust = 0.5, size = 12),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5))

    ggsave(speaking_time_plot, file = 'stat_pack_OT23/figures/total_time_spoken_plot_OT23.png')

} #Speaking Times (Total Figure)


################################################################################
# Docket
################################################################################

docket_path <- list.files("docket_parser/OT23_docket_sheets", full.names = T)

dockets <- data.frame()

for (i in 1:length(docket_path)){

  temp_docket <- get(load(docket_path[i]))
  dockets <- bind_rows(dockets, temp_docket)

  if (i %% 100 == 0){
    message('Completed ', i, ' of ', length(docket_path))
  }

  rm(docket_combined) #Remove Temp Docket
  rm(temp_docket)

} #Combine Docket Sheets Into Single DF

for (i in 1:nrow(dockets)){

  if (dockets$docketed[i] == '') {
    dockets$docketed[i] <- format(as.Date(dockets$docket[i][[1]][[1]][1], format = "%Y-%m-%d"), "%B %d, %Y")
  }

} #Fix Dockets w/out Docketed Date (Applications & Motions)

save(dockets, file = 'docket_parser/OT23_docket_sheets/docket_filings_ot_23.rdata')

load('docket_parser/OT23_docket_sheets/docket_filings_ot_23.rdata')

dockets <- dockets %>%
  unique() #Make Sure to Only Get Unique


{

  dockets_ot_23 <- dockets %>%
    mutate(docket_type = case_when(
      .default = 'Petitions',
      grepl('A', docket_number) ~ 'Applications',
      grepl('M', docket_number) ~ 'Motions'),
      docketed = lubridate::mdy(docketed),
      docketed = format(docketed, "%Y-%m")) %>%
    group_by(docketed, docket_type) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = docketed, y = count)) +
    geom_bar(stat = 'identity', color = 'gray5', aes(fill = docket_type), position = position_dodge2()) +
    scale_fill_manual(values = c('coral3', 'deepskyblue4', 'gray50')) +
    theme_minimal() +
    facet_wrap(~ docket_type, scales = "free_y", nrow = 3) +
    geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.2)))  +
    scale_x_discrete(labels = c('Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')) +
    geom_hline(yintercept = 0) +
    labs(
      x = '\n',
      y = '\n') +
    theme(
      panel.border = element_rect(size = 1, colour = 'gray5', fill = NA),
      axis.text = element_text(size = 12, colour = 'black'),
      axis.title = element_text(size = 14, colour = 'black'),
      plot.title = element_text(size = 16, colour = 'black', face = 'bold'),
      plot.subtitle = element_text(size = 14, colour = 'black'),
      legend.position = 'none',
      legend.title = element_blank(),
      legend.text = element_text(size = 10, colour = 'black'),
      strip.text = element_text(size = 12, colour = 'black', face = 'bold'),
      strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
      panel.background = element_rect(size = 1, fill = 'white', colour = 'white'),
      plot.background = element_rect(size = 1, fill = 'white', colour = 'white'))

  ggsave("stat_pack_OT23/Figures/statpack_figures/dockets_ot_23.png", dockets_ot_23 , dpi = 300)

  dockets_ot_23 <- dockets %>%
    mutate(docket_type = case_when(
      .default = 'Petitions',
      grepl('A', docket_number) ~ 'Applications',
      grepl('M', docket_number) ~ 'Motions'),
      docketed = lubridate::mdy(docketed),
      docketed = format(docketed, "%Y-%m")) %>%
    group_by(docketed, docket_type) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = 'docket_type', values_from = 'count') %>%
    mutate(month = gsub('.*\\-', '', docketed),
           month = case_when(
             month == '06' ~ 'June',
             month == '07' ~ 'July',
             month == '08' ~ 'August',
             month == '09' ~ 'September',
             month == '10' ~ 'October',
             month == '11' ~ 'November',
             month == '12' ~ 'December',
             month == '01' ~ 'January',
             month == '02' ~ 'February',
             month == '03' ~ 'March',
             month == '04' ~ 'April',
             month == '05' ~ 'May'),
           year = gsub('\\-.*', '', docketed),
           docketed = paste0(month, ' ', year)) %>%
    select(-c(month, year))


  write.csv(dockets_ot_23, file = 'stat_pack_OT23/Statpack Replication Data/Docket/filing_trends_by_month_OT23.csv', row.names = F)


} #Main Filing Trends (OT23) by Month

{

  dockets_ot_23_origin <- dockets %>%
    mutate(docket_type = case_when(
      .default = 'Petitions',
      grepl('A', docket_number) ~ 'Applications',
      grepl('M', docket_number) ~ 'Motions'),
      docketed = lubridate::mdy(docketed),
      docketed = format(docketed, "%Y-%m")) %>%
    filter(docket_type == 'Petitions') %>%
    filter(!lower_court == '') %>%
    group_by(lower_court) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(origin_type = ifelse(grepl('United States Court of Appeals for the', lower_court) & grepl('Circuit', lower_court), 'Circuit', 'State/Other')) %>%
    mutate(lower_court = ifelse(origin_type == 'Circuit', gsub('United States Court of Appeals for the ', '', lower_court), lower_court),
           lower_court = gsub('United States ', '', lower_court)) %>%
    mutate(lower_court = gsub('\\,', ' -', lower_court)) %>%
    mutate(lower_court = gsub('Division', 'Div.', lower_court),
           lower_court = gsub('District', 'Dist.', lower_court),
           lower_court = gsub('Appellate', 'App.', lower_court),
           lower_court = gsub('Department', 'Dept.', lower_court)) %>%
    select(lower_court, origin_type, count) %>%
    rename(`Origin` = lower_court,
           `Petitions` = count,
           `Type` = origin_type)

  write.csv(dockets_ot_23_origin, file = 'stat_pack_OT23/Statpack Replication Data/Docket/filings_court_of_origin_OT23.csv', row.names = F)

  fifth_circuit_petitions <- sum(dockets_ot_23_origin$Petitions[dockets_ot_23_origin$Origin == 'Fifth Circuit'])
  total_petitions <- sum(dockets_ot_23_origin$Petitions)
  (fifth_circuit_petitions / total_petitions) * 100

  max <- seq(20, nrow(dockets_ot_23_origin), by = 20)
  min = c()
  for (i in 1:length(max)){
    temp_min = max[i] - 19
    min = c(min, temp_min)
  }

  for (i in 1:length(min)){

    start = min[i]
    end = max[i]

    temp_data <- dockets_ot_23_origin[c(start:end),]
    temp_path = paste0('C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/stat_pack_OT23/Tables/dockets_ot_23_origin_', i, '.csv')

    write.table(temp_data, file = temp_path, row.names = F, quote = F, sep = ',')

  }







} #Filing Trends by Court of Origin

{

  shapefile_circuit<- st_read("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/stat_pack_OT23/statpack_replication/circuit_court_maps/US_CourtOfAppealsCircuits.shp") #Circuit Maps

  circuit_origin <- dockets_ot_23_origin %>%
    filter(Type == 'Circuit') %>%
    mutate(NAME = toupper(Origin)) %>%
    left_join(shapefile_circuit, by = 'NAME')

  circuit_origin_figure <- circuit_origin %>%
    ggplot() +
    geom_sf(aes(fill = Petitions, geometry = geometry), color = 'black') +
    coord_sf(xlim = c(xmin = -130, xmax = -65), ylim = c(ymin = 25, ymax = 50)) +
    scale_fill_continuous(low = 'gray',
                          high = 'black') +
    labs(fill = 'Petitions Filed (OT2023)') +
    theme_void() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.title = element_text(size = 10), # Adjust the size of the legend title here
      legend.text.align = 0.5,
      legend.text = element_text(size = 10, colour = "gray5"),
      legend.position = "bottom",
      legend.box="horizontal",
      legend.key.width = unit(1.5, "cm"),  # Adjust the width here (e.g., "4cm" for a 4 cm width)
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_rect(fill = "gray", colour = "gray5"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5)) +
    guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
           size = guide_legend(title.position="top", title.hjust = 0.5)) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

  ggsave(circuit_origin_figure, file = "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/stat_pack_OT23/Figures/circuit_origin_figure.png", width = 6, height = 3.5) # Adjust width and height as needed


} #Circuit Map

{

  load('docket_parser/OT18_OT22_dockets.rdata') #Load (Already Compiled...)

  dockets_ot_23 <- dockets %>%
    mutate(docket_type = case_when(
      .default = 'Petitions',
      grepl('A', docket_number) ~ 'Applications',
      grepl('M', docket_number) ~ 'Motions'),
      docketed = lubridate::mdy(docketed),
      docketed = format(docketed, "%Y-%m")) %>%
    group_by(docketed, docket_type) %>%
    summarise(count = n()) %>%
    rename(filing_type = docket_type) %>%
    group_by(filing_type) %>%
    summarise(count = sum(count)) %>%
    mutate(docketed = 2023)


  longitudinal_docketing_trends_2018_2023 <- ot18_ot22_dockets %>%
    mutate(filing_type = case_when(
      .default = 'Petitions',
      grepl('(M|m)', docket_number) ~ 'Motions',
      grepl('(a|A)', docket_number) ~ 'Applications')) %>%
    mutate(filing_term = as.numeric(gsub('(\\-.*|a.*|A.*|m.*|M.*)', '', docket_number))) %>%
    select(filing_term, filing_type)  %>%
    group_by(filing_term, filing_type) %>%
    summarise(count = n()) %>%
    unique() %>%
    rename(docketed = filing_term) %>%
    mutate(docketed = paste0('20', docketed),
           docketed = as.numeric(docketed)) %>%
    bind_rows(dockets_ot_23) %>%
    ggplot(aes(x = factor(docketed), y = count)) +
    geom_bar(stat = 'identity', color = 'gray5', aes(fill = filing_type), position = position_dodge2()) +
    scale_fill_manual(values = c('coral3', 'deepskyblue4', 'gray50')) +
    theme_minimal() +
    facet_wrap(~ filing_type, scales = "free_y", nrow = 3) +
    geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.2)))  +
    geom_hline(yintercept = 0) +
    labs(
      x = '\n',
      y = '\n') +
    theme(
      panel.border = element_rect(size = 1, colour = 'gray5', fill = NA),
      axis.text = element_text(size = 12, colour = 'black'),
      axis.title = element_text(size = 14, colour = 'black'),
      plot.title = element_text(size = 16, colour = 'black', face = 'bold'),
      plot.subtitle = element_text(size = 14, colour = 'black'),
      legend.position = 'none',
      legend.title = element_blank(),
      legend.text = element_text(size = 10, colour = 'black'),
      strip.text = element_text(size = 12, colour = 'black', face = 'bold'),
      strip.background = element_rect(size = 1, colour = 'black', fill = 'gray'),
      panel.background = element_rect(size = 1, fill = 'white', colour = 'white'),
      plot.background = element_rect(size = 1, fill = 'white', colour = 'white'))


  ggsave("stat_pack_OT23/Figures/statpack_figures/longitudinal_docketing_trends_2018_2023.png", longitudinal_docketing_trends_2018_2023 , dpi = 300)


  longitudinal_docketing_trends_2018_2023 <- ot18_ot22_dockets %>%
    mutate(filing_type = case_when(
      .default = 'Petitions',
      grepl('(M|m)', docket_number) ~ 'Motions',
      grepl('(a|A)', docket_number) ~ 'Applications')) %>%
    mutate(filing_term = as.numeric(gsub('(\\-.*|a.*|A.*|m.*|M.*)', '', docket_number))) %>%
    select(filing_term, filing_type)  %>%
    group_by(filing_term, filing_type) %>%
    summarise(count = n()) %>%
    unique() %>%
    rename(docketed = filing_term) %>%
    mutate(docketed = paste0('20', docketed),
           docketed = as.numeric(docketed)) %>%
    bind_rows(dockets_ot_23) %>%
    pivot_wider(names_from = 'filing_type', values_from = 'count')

  write.csv(longitudinal_docketing_trends_2018_2023, file = 'stat_pack_OT23/Statpack Replication Data/Docket/filing_trends_OT18_OT23.csv', row.names = F)


} #Combined Docket Trends (18-23)
