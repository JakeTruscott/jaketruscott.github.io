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
  ROBERTS <- readPNG("justice_images/Roberts.png")
  ALITO <- readPNG("justice_images/Alito.png")
  THOMAS <- readPNG("justice_images/Thomas.png")
  SOTOMAYOR <- readPNG("justice_images/Sotomayor.png")
  KAGAN <- readPNG("justice_images/Kagan.png")
  GORSUCH <- readPNG("justice_images/Gorsuch.png")
  KAVANAUGH <- readPNG("justice_images/Kavanaugh.png")
  BARRETT <- readPNG("justice_images/Barrett.png")
  JACKSON <- readPNG("justice_images/Jackson.png")
  BREYER <- readPNG("justice_images/Breyer.png")
  GINSBURG <- readPNG("justice_images/Ginsburg.png")
  KENNEDY <- readPNG("justice_images/Kennedy.png")

  justice_image_labels <- c(
    ALITO = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Alito.png' width='75' /><br>",
    ROBERTS = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Roberts.png' width='75' /><br>",
    THOMAS = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Thomas.png' width='75' /><br>",
    SOTOMAYOR = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Sotomayor.png' width='75' /><br>",
    KAGAN = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Kagan.png' width='75' /><br>",
    GORSUCH = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Gorsuch.png' width='75' /><br>",
    KAVANAUGH = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Kavanaugh.png' width='75' /><br>",
    BARRETT = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Barrett.png' width='75' /><br>",
    JACKSON = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Jackson.png' width='75' /><br>",
    BREYER = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Breyer.png' width='75' /><br>",
    GINSBURG = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Ginsburg.png' width='75' /><br>",
    KENNEDY = "<img src='C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/justice_images/Kennedy.png' width='75' /><br>"
  )
} #Justice Images
{

  shorthand_case_names <- read.csv('C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/ot23_decisions/shorthand_case_names.csv', as.is = T)


} #Shorthand Case Names...

################################################################################
# Decisions
################################################################################


{

  decisions_ot_23 <- read_xlsx(path = "ot23_decisions/OT_23_Decisions.xlsx")
  decisions_ot_23 <- decisions_ot_23 %>%
    mutate(`Case` = gsub('\\,', '', `Case`),
           Decision = gsub('\\,', '', Decision))
  write.table(decisions_ot_23, file = "ot23_decisions/OT_23_Decisions.csv", row.names = F, quote = F, sep = ',')


} # Decision Breakdowns

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




} # Justice Agreement Vote Matrix (OT23)

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



} #Cases by Coalition Type

{


} #Distribution of Coalitions


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

    output_path = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/stat_pack_OT23/Tables/oral_argument_speaking/attorney_participation_", i, ".csv")

    write.table(attorneys, file = output_path,  row.names = F, quote = F, sep = ',')

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



    output_path = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/stat_pack_OT23/oral_argument_speaking/argument_calendar_", i, ".csv")

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

    word_count_plot

  } #Totals Graph (Active)
  ggsave("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/word_count_plot_OT23.png", word_count_plot, dpi = 300)

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

    write.table(oa_data_temp, file = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/stat_pack_OT23/oral_argument_speaking/oral_argument_participation_", i, ".csv"), row.names = F, quote = F, sep = ',')

  }


} #Word Count by Month

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
  ggsave("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/total_time_spoken_plot_OT23.png", total_time_spoken_plot, dpi = 300)


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



    output_path = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/stat_pack_OT23/oral_argument_speaking/oral_argument_speaking_times_", i, '.csv')

    write.table(time_spoken_total, file = output_path, row.names = F, quote = F, sep = ',')


  }




} #Speaking Times (Total & By Sitting)


################################################################################
# Docket
################################################################################

docket_path <- list.files("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/docket_parser/OT23_docket_sheets", full.names = T)

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

  ggsave(dockets_ot_23, file = "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/stat_pack_OT23/Figures/dockets_ot_23.png")


} #Main Filing Trends (OT23) Figure

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

  fifth_circuit_petitions <- sum(dockets_ot_23_origin$Petitions[dockets_ot_23_origin$Origin == 'Fifth Circuit'])
  total_petitions <- sum(dockets_ot_23_origin$Petitions)
  (fifth_circuit_petitions / total_petitions) * 100

  min <- seq(1, nrow(dockets_ot_23_origin), by = 20)
  max = ifelse(min + 20 > nrow(dockets_ot_23_origin), nrow(dockets_ot_23_origin), min + 20)

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
