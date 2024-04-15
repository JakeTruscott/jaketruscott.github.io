################################################################################
# SCOTUSWatch - Oral Argument Summary Statistics
# Author: Jake S. Truscott, Ph.D
# Updated March 2024
################################################################################

################################################################################
#Load Packages & Libraries
################################################################################
library(kableExtra); library(dplyr); library(tidyr); library(scotustext); library(htmltools); library(ggplot2); library(png); library(dplyr); library(stringi); library(stringr); library(ggplot2); library(ggthemes); library(anytime); library(tm); library(scotustext); library(readxl); library(ggpattern); library(png); library(ggtext); library(grid); library(wesanderson); library(tidyr)


################################################################################
# Justice Images
# Note: Set Directory to Folder Containing Data Repository (Will Include 'justice_images' folder...)
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


################################################################################
# OT 2023 (Aggregate Total Term Stats - Words)
################################################################################
{

  base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
  rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
  load(url(rdata_url))

  {
    oa <- scotus_OT23 %>%
      filter(speaker_type == 'Justice') %>%
      group_by(speaker, case_name) %>%
      summarise(total_word_count = sum(word_count)) %>%
      pivot_wider(names_from = speaker, values_from = total_word_count, names_prefix = "word_count_")

    names(oa) <- gsub('(CHIEF JUSTICE |JUSTICE )', '', names(oa))
    names(oa) <- gsub('word_count_', '', names(oa))


    totals <- oa %>%
      select(-case_name) %>%
      summarise_all(.funs = sum, na.rm = T)

    totals <- cbind(data.frame('case_name' = 'Totals'), totals)

    oa <- oa %>%
      mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa (Consolidated w/ 23A350, 23A351, 23A384)', case_name)) %>%
      rename(' ' = case_name)
  } #Process Data

  {
    oa_data <- oa
    matching_columns <- intersect(colnames(oa_data), names(justice_image_labels))

    original_column_names <- colnames(oa_data)

    oa_data <- oa_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])


    oa_table <- oa_data %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      add_header_above( original_column_names) %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
      row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
      kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
      column_spec(2, color = "white",
                  background = spec_color(oa$ROBERTS, end = 0.5),
                  popover = paste("am:", oa$ROBERTS)) %>%
      column_spec(3, color = "white",
                  background = spec_color(oa$ALITO, end = 0.5),
                  popover = paste("am:", oa$ALITO)) %>%
      column_spec(4, color = "white",
                  background = spec_color(oa$BARRETT, end = 0.5),
                  popover = paste("am:", oa$BARRETT)) %>%
      column_spec(5, color = "white",
                  background = spec_color(oa$GORSUCH, end = 0.5),
                  popover = paste("am:", oa$GORSUCH)) %>%
      column_spec(6, color = "white",
                  background = spec_color(oa$JACKSON, end = 0.5),
                  popover = paste("am:", oa$JACKSON)) %>%
      column_spec(7, color = "white",
                  background = spec_color(oa$KAGAN, end = 0.5),
                  popover = paste("am:", oa$KAGAN)) %>%
      column_spec(8, color = "white",
                  background = spec_color(oa$KAVANAUGH, end = 0.5),
                  popover = paste("am:", oa$KAVANAUGH)) %>%
      column_spec(9, color = "white",
                  background = spec_color(oa$SOTOMAYOR, end = 0.5),
                  popover = paste("am:", oa$SOTOMAYOR)) %>%
      column_spec(10, color = "white",
                  background = spec_color(oa$THOMAS, end = 0.5),
                  popover = paste("am:", oa$THOMAS))


    oa_table

  } #By Argument Table
  save_kable(oa_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa_table_23_active.html")
  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa_table_23_active.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa_table_23_active.png", vwidth = 900, vheight = 70)

  {



    min = seq(1, nrow(oa_data), by = 10)
    max = min + 10
    max = ifelse(max > nrow(oa), nrow(oa), max)

    oa_data <- oa
    matching_columns <- intersect(colnames(oa_data), names(justice_image_labels))

    original_column_names <- colnames(oa_data)

    oa_data <- oa_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])

    for (i in 1:length(min)){

      start = min[i]
      end = max[i]

      oa_data_temp = oa_data[c(start:end),]

      oa_table_temp <- oa_data_temp %>%
        kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
        add_header_above(original_column_names) %>%
        column_spec(1, bold = TRUE, border_right = TRUE, color = "black") %>%
        row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
        row_spec(seq(1, nrow(oa_data_temp), 1), align = 'center') %>%
        kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
        column_spec(2, color = "white",
                    background = spec_color(oa$ROBERTS, end = 0.5),
                    popover = paste("am:", oa$ROBERTS), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(3, color = "white",
                    background = spec_color(oa$ALITO, end = 0.5),
                    popover = paste("am:", oa$ALITO), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(4, color = "white",
                    background = spec_color(oa$BARRETT, end = 0.5),
                    popover = paste("am:", oa$BARRETT), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(5, color = "white",
                    background = spec_color(oa$GORSUCH, end = 0.5),
                    popover = paste("am:", oa$GORSUCH), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(6, color = "white",
                    background = spec_color(oa$JACKSON, end = 0.5),
                    popover = paste("am:", oa$JACKSON), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(7, color = "white",
                    background = spec_color(oa$KAGAN, end = 0.5),
                    popover = paste("am:", oa$KAGAN), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(8, color = "white",
                    background = spec_color(oa$KAVANAUGH, end = 0.5),
                    popover = paste("am:", oa$KAVANAUGH), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(9, color = "white",
                    background = spec_color(oa$SOTOMAYOR, end = 0.5),
                    popover = paste("am:", oa$SOTOMAYOR), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
        column_spec(10, color = "white",
                    background = spec_color(oa$THOMAS, end = 0.5),
                    popover = paste("am:", oa$THOMAS), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;")


      oa_table_temp

      output_dir = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/totals_table_OT23_active_", i, ".html")

      save_kable(oa_table_temp, file = output_dir)


    }


    for (i in 1:length(min)){
      html_file_path <- paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/totals_table_OT23_active_", i, ".html")
      webshot::webshot(html_file_path, paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/totals_table_OT23_active_", i, ".png"), vwidth = 900, vheight = 70)

    }


  } #Argument Table by Month - For Statpack

  {



    totals_data <- totals
    matching_columns <- intersect(colnames(totals_data), names(justice_image_labels))
    original_column_names <- colnames(totals_data)
    original_column_names[1] <- " "

    totals_data <- totals_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])

    totals_table <- totals_data %>%
      rename(' ' = case_name) %>%
      kbl(longtable = T, escape = F, booktabs =  T, align = "c") %>%
      add_header_above( original_column_names) %>%
      row_spec(1, align = 'center') %>%
      column_spec(1, bold = T, border_right = T) %>%
      row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
      kable_styling(full_width = T, font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"), latex_options = "striped") %>%
      column_spec(1, bold = T, border_right = T) %>%
      row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
      #row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
      column_spec(2, color = "white",
                  background = spec_color(totals$ROBERTS, end = 0.5),
                  popover = paste("am:", totals$ROBERTS)) %>%
      column_spec(3, color = "white",
                  background = spec_color(totals$ALITO, end = 0.5),
                  popover = paste("am:", totals$ALITO)) %>%
      column_spec(4, color = "white",
                  background = spec_color(totals$BARRETT, end = 0.5),
                  popover = paste("am:", totals$BARRETT)) %>%
      column_spec(5, color = "white",
                  background = spec_color(totals$GORSUCH, end = 0.5),
                  popover = paste("am:", totals$GORSUCH)) %>%
      column_spec(6, color = "white",
                  background = spec_color(totals$JACKSON, end = 0.5),
                  popover = paste("am:", totals$JACKSON)) %>%
      column_spec(7, color = "white",
                  background = spec_color(totals$KAGAN, end = 0.5),
                  popover = paste("am:", totals$KAGAN)) %>%
      column_spec(8, color = "white",
                  background = spec_color(totals$KAVANAUGH, end = 0.5),
                  popover = paste("am:", totals$KAVANAUGH)) %>%
      column_spec(9, color = "white",
                  background = spec_color(totals$SOTOMAYOR, end = 0.5),
                  popover = paste("am:", totals$SOTOMAYOR)) %>%
      column_spec(10, color = "white",
                  background = spec_color(totals$THOMAS, end = 0.5),
                  popover = paste("am:", totals$THOMAS))



    totals_table
  } #Totals Table (Active)
  save_kable(totals_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/totals_table_OT23_active.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/totals_table_OT23_active.html"
  #webshot::install_phantomjs(force = T)
  #phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/totals_table_OT23_active.png", vwidth = 900, vheight = 70)

  {
    custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

    scotus_term <- scotus_OT23 %>%
      filter(speaker_type == 'Justice') %>%
      group_by(speaker, case_name) %>%
      summarise(total_word_count = sum(word_count)) %>%
      group_by(speaker) %>%
      summarise(total_word_count = sum(total_word_count)) %>%
      mutate(speaker = gsub("(CHIEF JUSTICE |JUSTICE )", "", speaker))

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

  {


    attorneys <- scotus_OT23 %>%
      mutate(response_to = ifelse(lag(speaker_type) == 'Justice', lag(speaker), NA)) %>%
      filter(speaker_type == "Attorney") %>%
      mutate(speaker = ifelse(speaker == 'MR. SYNDER', 'MR. SNYDER', speaker)) %>%
      filter(!is.na(response_to)) %>%
      group_by(case_name, speaker, response_to) %>%
      summarise(total_words = sum(word_count, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(response_to = gsub("(CHIEF JUSTICE |JUSTICE )", "", response_to)) %>%
      pivot_wider(names_from = response_to, values_from = total_words) %>%
      mutate('Total Words' = rowSums(select(., -c(case_name, speaker)), na.rm = TRUE)) %>%
      relocate('Total Words', .after = speaker) %>%
      rename(' ' = case_name,
             'Attorney' = speaker)


    attorney_participation <- attorneys
    original_column_names <- colnames(attorney_participation)

    original_column_names[1] <- " "
    original_column_names[2] <- " "
    original_column_names[3] <- " "

    attorney_participation <- attorney_participation %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])


    attorney_participation_table <- attorney_participation %>%
      kbl(longtable = T, escape = F, booktabs =  T, align = "l") %>%
      add_header_above( original_column_names) %>%
      row_spec(1, align = 'center') %>%
      column_spec(1, bold = T, border_right = T) %>%
      row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
      kable_styling(full_width = T, font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"), latex_options = c("striped", "HOLD_position")) %>%
      column_spec(1, bold = T, border_right = T) %>%
      row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
      row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
      column_spec(4, color = "white",
                  background = spec_color(attorneys$ROBERTS, end = 0.5),
                  popover = paste("am:", attorneys$ROBERTS),
                  latex_valign = 'p') %>%
      row_spec(1:dim(attorneys)[1],  align = "c") %>%
      column_spec(5, color = "white",
                  background = spec_color(attorneys$ALITO, end = 0.5),
                  popover = paste("am:", attorneys$ALITO)) %>%
      column_spec(6, color = "white",
                  background = spec_color(attorneys$BARRETT, end = 0.5),
                  popover = paste("am:", attorneys$BARRETT)) %>%
      column_spec(7, color = "white",
                  background = spec_color(attorneys$GORSUCH, end = 0.5),
                  popover = paste("am:", attorneys$GORSUCH)) %>%
      column_spec(8, color = "white",
                  background = spec_color(attorneys$JACKSON, end = 0.5),
                  popover = paste("am:", attorneys$JACKSON)) %>%
      column_spec(9, color = "white",
                  background = spec_color(attorneys$KAGAN, end = 0.5),
                  popover = paste("am:", attorneys$KAGAN)) %>%
      column_spec(10, color = "white",
                  background = spec_color(attorneys$KAVANAUGH, end = 0.5),
                  popover = paste("am:", attorneys$KAVANAUGH)) %>%
      column_spec(11, color = "white",
                  background = spec_color(attorneys$SOTOMAYOR, end = 0.5),
                  popover = paste("am:", attorneys$SOTOMAYOR)) %>%
      column_spec(12, color = "white",
                  background = spec_color(attorneys$THOMAS, end = 0.5),
                  popover = paste("am:", attorneys$THOMAS))


    attorney_participation_table

  } #Attorneys OA Participation Figure
  save_kable(attorney_participation_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/attorney_participation_23_active.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/attorney_participation_23_active.html"
  #webshot::install_phantomjs(force = T)
  #phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/attorney_participation_23_active.png", vwidth = 800, vheight = 70)

  {

    attorney_sp <- attorney_participation[,c(1:3)]
    names(attorney_sp) = c('Argument', 'Attorney', 'Total_Words')

    min = seq(1, nrow(attorney_sp), by = 25)
    max = min + 25
    max = ifelse(max > nrow(attorney_sp), nrow(attorney_sp), max)


    temp <- attorney_sp[c(min[6]:max[6]),] %>%
      arrange(desc(Argument)) %>%
      mutate(Attorney = gsub('(Jr\\.|| I|| II)', '', Attorney)) %>%
      mutate(Attorney = sapply(strsplit(as.character(Attorney), '\\s+'), tail, 1)) %>%
      group_by(Argument) %>%
      mutate(attorney_count = max(row_number())) %>%
      mutate(Argument = paste0('multirow{', attorney_count, '}{=}{', Argument, "} &")) %>%
      reframe(combined = paste0(Attorney, ' (', Total_Words, ') \\ &')) %>%
      ungroup() %>%
      group_by(Argument) %>%
      summarise(combined = paste0(combined, collapse = ' ')) %>%
      ungroup() %>%
      mutate(combined = paste0(Argument, ' ', combined),
             combined = gsub('\\&$', 'addlinespace', combined)) %>%
      select(combined)


    cat(temp$combined)

  } #Attorney Participation Table - Shortened for StatPack


} #OT 2023 (Totals)

################################################################################
# OT 2023 (By Active Sitting - Words)
# Currently *February Sitting*
################################################################################
{

  base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
  rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
  load(url(rdata_url))

  {
    oa <- scotus_OT23 %>%
      filter(speaker_type == 'Justice') %>%
      filter(sitting == 'March') %>%
      group_by(speaker, case_name) %>%
      summarise(total_word_count = sum(word_count)) %>%
      pivot_wider(names_from = speaker, values_from = total_word_count, names_prefix = "word_count_")

    names(oa) <- gsub('(CHIEF JUSTICE |JUSTICE )', '', names(oa))
    names(oa) <- gsub('word_count_', '', names(oa))


    totals <- oa %>%
      select(-case_name) %>%
      summarise_all(.funs = sum, na.rm = T)

    totals <- cbind(data.frame('case_name' = 'Totals'), totals)

    oa <- oa %>%
      mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa (Consolidated w/ 23A350, 23A351, 23A384)', case_name)) %>%
      rename(' ' = case_name)
  } #Process Data

  {
    oa_data <- oa
    matching_columns <- intersect(colnames(oa_data), names(justice_image_labels))

    original_column_names <- colnames(oa_data)

    oa_data <- oa_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])


    oa_table <- oa_data %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      add_header_above( original_column_names) %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
      row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
      kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
      column_spec(2, color = "white",
                  background = spec_color(oa$ROBERTS, end = 0.5),
                  popover = paste("am:", oa$ROBERTS)) %>%
      column_spec(3, color = "white",
                  background = spec_color(oa$ALITO, end = 0.5),
                  popover = paste("am:", oa$ALITO)) %>%
      column_spec(4, color = "white",
                  background = spec_color(oa$BARRETT, end = 0.5),
                  popover = paste("am:", oa$BARRETT)) %>%
      column_spec(5, color = "white",
                  background = spec_color(oa$GORSUCH, end = 0.5),
                  popover = paste("am:", oa$GORSUCH)) %>%
      column_spec(6, color = "white",
                  background = spec_color(oa$JACKSON, end = 0.5),
                  popover = paste("am:", oa$JACKSON)) %>%
      column_spec(7, color = "white",
                  background = spec_color(oa$KAGAN, end = 0.5),
                  popover = paste("am:", oa$KAGAN)) %>%
      column_spec(8, color = "white",
                  background = spec_color(oa$KAVANAUGH, end = 0.5),
                  popover = paste("am:", oa$KAVANAUGH)) %>%
      column_spec(9, color = "white",
                  background = spec_color(oa$SOTOMAYOR, end = 0.5),
                  popover = paste("am:", oa$SOTOMAYOR)) %>%
      column_spec(10, color = "white",
                  background = spec_color(oa$THOMAS, end = 0.5),
                  popover = paste("am:", oa$THOMAS))


    oa_table

  } #By Argument Table
  save_kable(oa_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/March Sitting 2023/oa_table_23_march.html")
  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/March Sitting 2023/oa_table_23_march.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/March Sitting 2023/oa_table_23_march.png", vwidth = 1250, vheight = 100)

  {



    totals_data <- totals
    matching_columns <- intersect(colnames(totals_data), names(justice_image_labels))
    original_column_names <- colnames(totals_data)
    original_column_names[1] <- " "

    totals_data <- totals_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])

    totals_table <- totals_data %>%
      rename(' ' = case_name) %>%
      kbl(longtable = T, escape = F, booktabs =  T, align = "c") %>%
      add_header_above( original_column_names) %>%
      row_spec(1, align = 'center') %>%
      column_spec(1, bold = T, border_right = T) %>%
      row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
      kable_styling(full_width = T, font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"), latex_options = "striped") %>%
      column_spec(1, bold = T, border_right = T) %>%
      row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
      #row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
      column_spec(2, color = "white",
                  background = spec_color(totals$ROBERTS, end = 0.5),
                  popover = paste("am:", totals$ROBERTS)) %>%
      column_spec(3, color = "white",
                  background = spec_color(totals$ALITO, end = 0.5),
                  popover = paste("am:", totals$ALITO)) %>%
      column_spec(4, color = "white",
                  background = spec_color(totals$BARRETT, end = 0.5),
                  popover = paste("am:", totals$BARRETT)) %>%
      column_spec(5, color = "white",
                  background = spec_color(totals$GORSUCH, end = 0.5),
                  popover = paste("am:", totals$GORSUCH)) %>%
      column_spec(6, color = "white",
                  background = spec_color(totals$JACKSON, end = 0.5),
                  popover = paste("am:", totals$JACKSON)) %>%
      column_spec(7, color = "white",
                  background = spec_color(totals$KAGAN, end = 0.5),
                  popover = paste("am:", totals$KAGAN)) %>%
      column_spec(8, color = "white",
                  background = spec_color(totals$KAVANAUGH, end = 0.5),
                  popover = paste("am:", totals$KAVANAUGH)) %>%
      column_spec(9, color = "white",
                  background = spec_color(totals$SOTOMAYOR, end = 0.5),
                  popover = paste("am:", totals$SOTOMAYOR)) %>%
      column_spec(10, color = "white",
                  background = spec_color(totals$THOMAS, end = 0.5),
                  popover = paste("am:", totals$THOMAS))



    totals_table
  } #Totals Table (Active)
  save_kable(totals_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/March Sitting 2023/totals_table_OT23_march.html")
  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/March Sitting 2023/totals_table_OT23_march.html"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/March Sitting 2023/totals_table_OT23_march.png", vwidth = 1250, vheight = 100)

  {
    custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

    scotus_term <- scotus_OT23 %>%
      filter(sitting == 'March') %>%
      filter(speaker_type == 'Justice') %>%
      group_by(speaker, case_name) %>%
      summarise(total_word_count = sum(word_count)) %>%
      group_by(speaker) %>%
      summarise(total_word_count = sum(total_word_count)) %>%
      mutate(speaker = gsub("(CHIEF JUSTICE |JUSTICE )", "", speaker))

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
  ggsave("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/March Sitting 2023/word_count_plot_OT23_march.png", word_count_plot, dpi = 300)

  {


    attorneys <- scotus_OT23 %>%
      filter(sitting == "March") %>%
      mutate(response_to = ifelse(lag(speaker_type) == 'Justice', lag(speaker), NA)) %>%
      filter(speaker_type == "Attorney") %>%
      mutate(speaker = ifelse(speaker == 'MR. SYNDER', 'MR. SNYDER', speaker)) %>%
      filter(!is.na(response_to)) %>%
      group_by(case_name,  speaker, response_to) %>%
      summarise(total_words = sum(word_count, na.rm = TRUE)) %>%
      unique() %>%
      ungroup() %>%
      mutate(response_to = gsub("(CHIEF JUSTICE |JUSTICE )", "", response_to)) %>%
      pivot_wider(names_from = response_to, values_from = total_words) %>%
      unique() %>%
      mutate('Total Words' = rowSums(select(., -c(case_name, speaker)), na.rm = TRUE)) %>%
      relocate('Total Words', .after = speaker) %>%
      group_by(case_name, speaker) %>%
      mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa (Consolidated w/ 23A350, 23A351, 23A384)', case_name)) %>%
      rename(' ' = case_name,
             'Attorney' = speaker)

    attorney_participation <- attorneys
    original_column_names <- colnames(attorney_participation)

    original_column_names[1] <- " "
    original_column_names[2] <- " "
    original_column_names[3] <- " "

    attorney_participation <- attorney_participation %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])


    attorney_participation_table <- attorney_participation %>%
      kbl(longtable = T, escape = F, booktabs =  T, align = "l") %>%
      add_header_above( original_column_names) %>%
      row_spec(1, align = 'center') %>%
      column_spec(1, bold = T, border_right = T) %>%
      row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
      kable_styling(full_width = T, font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive"), latex_options = c("striped", "HOLD_position")) %>%
      column_spec(1, bold = T, border_right = T) %>%
      row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
      row_spec(seq(1, nrow(oa_data), 1), align = 'center') %>%
      column_spec(4, color = "white",
                  background = spec_color(attorneys$ROBERTS, end = 0.5),
                  popover = paste("am:", attorneys$ROBERTS),
                  latex_valign = 'p') %>%
      row_spec(1:dim(attorneys)[1],  align = "c") %>%
      column_spec(5, color = "white",
                  background = spec_color(attorneys$ALITO, end = 0.5),
                  popover = paste("am:", attorneys$ALITO)) %>%
      column_spec(6, color = "white",
                  background = spec_color(attorneys$BARRETT, end = 0.5),
                  popover = paste("am:", attorneys$BARRETT)) %>%
      column_spec(7, color = "white",
                  background = spec_color(attorneys$GORSUCH, end = 0.5),
                  popover = paste("am:", attorneys$GORSUCH)) %>%
      column_spec(8, color = "white",
                  background = spec_color(attorneys$JACKSON, end = 0.5),
                  popover = paste("am:", attorneys$JACKSON)) %>%
      column_spec(9, color = "white",
                  background = spec_color(attorneys$KAGAN, end = 0.5),
                  popover = paste("am:", attorneys$KAGAN)) %>%
      column_spec(10, color = "white",
                  background = spec_color(attorneys$KAVANAUGH, end = 0.5),
                  popover = paste("am:", attorneys$KAVANAUGH)) %>%
      column_spec(11, color = "white",
                  background = spec_color(attorneys$SOTOMAYOR, end = 0.5),
                  popover = paste("am:", attorneys$SOTOMAYOR)) %>%
      column_spec(12, color = "white",
                  background = spec_color(attorneys$THOMAS, end = 0.5),
                  popover = paste("am:", attorneys$THOMAS))


    attorney_participation_table

  } #Attorneys OA Participation Figure
  save_kable(attorney_participation_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/March Sitting 2023/attorney_participation_23_march.html")
  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/March Sitting 2023/attorney_participation_23_march.html"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/March Sitting 2023/attorney_participation_23_march.png", vwidth = 1250, vheight = 100)

} #OT 2023 (By Sitting) -- Currently march


################################################################################
# OT 2023 (Aggregate Total Term Stats - Speaking Time)
################################################################################

{

  base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
  rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
  load(url(rdata_url))
} #Compile Data from Github (If OT23 Data Not Already Loaded...)

{
  time_spoken_total <- scotus_OT23 %>%
    filter(speaker_type == 'Justice') %>%
    mutate(time_spoken = text_stop - text_start) %>%
    group_by(speaker, case_name) %>%
    summarise(total_time_spoken = sum(time_spoken)) %>%
    mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
    pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
    select(-case_name) %>%
    summarise_all(.funs = sum, na.rm = T) %>%
    mutate(total_time_spoken = rowSums(across(-total_time_spoken))) %>%
    rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
    rename("Total Time\n(Minutes)" = total_time_spoken)


  speaking_data <- time_spoken_total
  matching_columns <- intersect(colnames(speaking_data), names(justice_image_labels))

  original_column_names <- colnames(speaking_data)
  original_column_names[1] <- ' '

  speaking_data <- speaking_data %>%
    rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])
} #Compile Speaking Times from scotus_OT23

{
  speaking_times_total_OT23 <- speaking_data %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    add_header_above( original_column_names) %>%
    column_spec(1, bold = TRUE, border_right = TRUE) %>%
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(speaking_data), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    column_spec(2, color = "white",
                background = spec_color(time_spoken_total$ROBERTS, end = 0.5),
                popover = paste("am:", time_spoken_total$ROBERTS)) %>%
    column_spec(3, color = "white",
                background = spec_color(time_spoken_total$ALITO, end = 0.5),
                popover = paste("am:", time_spoken_total$ALITO)) %>%
    column_spec(4, color = "white",
                background = spec_color(time_spoken_total$BARRETT, end = 0.5),
                popover = paste("am:", time_spoken_total$BARRETT)) %>%
    column_spec(5, color = "white",
                background = spec_color(time_spoken_total$GORSUCH, end = 0.5),
                popover = paste("am:", time_spoken_total$GORSUCH)) %>%
    column_spec(6, color = "white",
                background = spec_color(time_spoken_total$JACKSON, end = 0.5),
                popover = paste("am:", time_spoken_total$JACKSON)) %>%
    column_spec(7, color = "white",
                background = spec_color(time_spoken_total$KAGAN, end = 0.5),
                popover = paste("am:", time_spoken_total$KAGAN)) %>%
    column_spec(8, color = "white",
                background = spec_color(time_spoken_total$KAVANAUGH, end = 0.5),
                popover = paste("am:", time_spoken_total$KAVANAUGH)) %>%
    column_spec(9, color = "white",
                background = spec_color(time_spoken_total$SOTOMAYOR, end = 0.5),
                popover = paste("am:", time_spoken_total$SOTOMAYOR)) %>%
    column_spec(10, color = "white",
                background = spec_color(time_spoken_total$THOMAS, end = 0.5),
                popover = paste("am:", time_spoken_total$THOMAS))
} #Compile Table

speaking_times_total_OT23  #Print Preview
save_kable(speaking_times_total_OT23, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa23_speaking_times_active.html")
html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa23_speaking_times_active.html"
phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa23_speaking_times_active.png", vwidth = 1250, vheight = 100)



################################################################################
# OT 2023 (By Active Sitting - Speaking Time)
# Currently *February Sitting*
################################################################################

{
  time_spoken_total_february <- scotus_OT23 %>%
    filter(speaker_type == 'Justice') %>%
    filter(sitting == 'February') %>%
    mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa (Consolidated w/ 23A350, 23A351, 23A384)', case_name)) %>%
    mutate(time_spoken = text_stop - text_start) %>%
    group_by(speaker, case_name) %>%
    summarise(total_time_spoken = sum(time_spoken)) %>%
    mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
    group_by(case_name) %>%
    pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
    summarise_all(.funs = sum, na.rm = T) %>%
    mutate(total_time_spoken = rowSums(across(-c(total_time_spoken, case_name)))) %>%
    rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
    rename("Total Time\n(Minutes)" = total_time_spoken)


  speaking_data <- time_spoken_total_february
  matching_columns <- intersect(colnames(speaking_data), names(justice_image_labels))

  original_column_names <- colnames(speaking_data)
  original_column_names[1:2] <- ' '

  speaking_data <- speaking_data %>%
    rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.]) %>%
    rename(`Case` = case_name)
} #Compile Speaking Times from scotus_OT23

{
  speaking_times_february <- speaking_data %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    add_header_above( original_column_names) %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "350px") %>%
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(speaking_data), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    column_spec(3, color = "white",
                background = spec_color(time_spoken_total_february$ROBERTS, end = 0.5),
                popover = paste("am:", time_spoken_total_february$ROBERTS)) %>%
    column_spec(4, color = "white",
                background = spec_color(time_spoken_total_february$ALITO, end = 0.5),
                popover = paste("am:", time_spoken_total_february$ALITO)) %>%
    column_spec(5, color = "white",
                background = spec_color(time_spoken_total_february$BARRETT, end = 0.5),
                popover = paste("am:", time_spoken_total_february$BARRETT)) %>%
    column_spec(6, color = "white",
                background = spec_color(time_spoken_total_february$GORSUCH, end = 0.5),
                popover = paste("am:", time_spoken_total_february$GORSUCH)) %>%
    column_spec(7, color = "white",
                background = spec_color(time_spoken_total_february$JACKSON, end = 0.5),
                popover = paste("am:", time_spoken_total_february$JACKSON)) %>%
    column_spec(8, color = "white",
                background = spec_color(time_spoken_total_february$KAGAN, end = 0.5),
                popover = paste("am:", time_spoken_total_february$KAGAN)) %>%
    column_spec(9, color = "white",
                background = spec_color(time_spoken_total_february$KAVANAUGH, end = 0.5),
                popover = paste("am:", time_spoken_total_february$KAVANAUGH)) %>%
    column_spec(10, color = "white",
                background = spec_color(time_spoken_total_february$SOTOMAYOR, end = 0.5),
                popover = paste("am:", time_spoken_total_february$SOTOMAYOR)) %>%
    column_spec(11, color = "white",
                background = spec_color(time_spoken_total_february$THOMAS, end = 0.5),
                popover = paste("am:", time_spoken_total_february$THOMAS))
} #Compile Table

speaking_times_february #Print Preview
save_kable(speaking_times_february, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/total_speaking_time.html")
html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/total_speaking_time.html"
phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/total_speaking_time.png", vwidth = 1250, vheight = 100)

################################################################################
# OT 2023 (By Active Sitting - Speaking Time)
# By Sitting
################################################################################

{

  for (i in unique(scotus_OT23$sitting)){

    time_spoken_total <- scotus_OT23 %>%
      filter(speaker_type == 'Justice') %>%
      filter(sitting == i) %>%
      mutate(case_name = ifelse(grepl('Kinder Morgan', case_name), 'Ohio, Et Al. Applicants v. Epa', case_name)) %>%
      mutate(time_spoken = text_stop - text_start) %>%
      group_by(speaker, case_name) %>%
      summarise(total_time_spoken = sum(time_spoken)) %>%
      mutate(total_time_spoken_minutes = round(total_time_spoken/60, 2)) %>%
      group_by(case_name) %>%
      pivot_wider(names_from = speaker, values_from = total_time_spoken_minutes, names_prefix = "time_spoken_") %>%
      summarise_all(.funs = sum, na.rm = T) %>%
      mutate(total_time_spoken = rowSums(across(-c(total_time_spoken, case_name)))) %>%
      rename_with(~str_replace(., "time_spoken_", ""), -total_time_spoken) %>%
      rename("Total Time\n(Minutes)" = total_time_spoken)


    speaking_data <- time_spoken_total
    matching_columns <- intersect(colnames(speaking_data), names(justice_image_labels))

    original_column_names <- colnames(speaking_data)
    original_column_names[1:2] <- ' '

    speaking_data <- speaking_data %>%
      rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.]) %>%
      rename(`Case` = case_name)


    speaking_times <- speaking_data %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      add_header_above( original_column_names) %>%
      column_spec(1, bold = TRUE, border_right = TRUE, width = "1000px", extra_css = 'font-size: 14px;') %>%
      row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
      row_spec(seq(1, nrow(speaking_data), 1), align = 'center') %>%
      kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
      column_spec(1, color = 'black') %>%
      column_spec(2, color = "black",
                  popover = paste("am:", time_spoken_total$`Total Time
(Minutes)`), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;")  %>%
      column_spec(3, color = "white",
                  background = spec_color(time_spoken_total$ROBERTS, end = 0.5),
                  popover = paste("am:", time_spoken_total$ROBERTS), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
      column_spec(4, color = "white",
                  background = spec_color(time_spoken_total$ALITO, end = 0.5),
                  popover = paste("am:", time_spoken_total$ALITO), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
      column_spec(5, color = "white",
                  background = spec_color(time_spoken_total$BARRETT, end = 0.5),
                  popover = paste("am:", time_spoken_total$BARRETT), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
      column_spec(6, color = "white",
                  background = spec_color(time_spoken_total$GORSUCH, end = 0.5),
                  popover = paste("am:", time_spoken_total$GORSUCH), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
      column_spec(7, color = "white",
                  background = spec_color(time_spoken_total$JACKSON, end = 0.5),
                  popover = paste("am:", time_spoken_total$JACKSON), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
      column_spec(8, color = "white",
                  background = spec_color(time_spoken_total$KAGAN, end = 0.5),
                  popover = paste("am:", time_spoken_total$KAGAN), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
      column_spec(9, color = "white",
                  background = spec_color(time_spoken_total$KAVANAUGH, end = 0.5),
                  popover = paste("am:", time_spoken_total$KAVANAUGH), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
      column_spec(10, color = "white",
                  background = spec_color(time_spoken_total$SOTOMAYOR, end = 0.5),
                  popover = paste("am:", time_spoken_total$SOTOMAYOR), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;") %>%
      column_spec(11, color = "white",
                  background = spec_color(time_spoken_total$THOMAS, end = 0.5),
                  popover = paste("am:", time_spoken_total$THOMAS), extra_css = "font-size: 18px; text-align: center; vertical-align: middle; font-weight: bold;")

    output_path = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/statpack_oa_times/oa_speaking_times_", i, '.html')

     save_kable(speaking_times, file = output_path)


  }


  for (i in unique(scotus_OT23$sitting)){

    html_file_path = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/statpack_oa_times/oa_speaking_times_", i, '.html')
    png_file_path = paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/statpack_oa_times/oa_speaking_times_", i, '.png')
    webshot::webshot(html_file_path, png_file_path, vwidth = 1000, vheight = 100)

  }



} #Speaking Times by Sitting - For Statpack




################################################################################
#Earlier Terms - Words
################################################################################
{

  base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
  rdata_url <- paste0(base_url, "scotus_transcripts_04-22.rdata")
  load(url(rdata_url))

  oa <- scotus %>%
    filter(term == 2022) %>%
    mutate(case_name = ifelse(argument == '21-376', "Chad Everet Brackeen, et al., Petitioners v. Deb Haaland, Secretary of the Interior, et al.", case_name)) %>%
    filter(type == 'Justice') %>%
    group_by(speaker, case_name) %>%
    summarise(total_word_count = sum(word_count)) %>%
    pivot_wider(names_from = speaker, values_from = total_word_count, names_prefix = "word_count_")

  names(oa) <- gsub('(CHIEF JUSTICE |JUSTICE )', '', names(oa))
  names(oa) <- gsub('word_count_', '', names(oa))


  totals <- oa %>%
    select(-case_name) %>%
    summarise_all(.funs = sum, na.rm = T)

  totals <- cbind(data.frame('case_name' = 'Totals'), totals)

  oa <- oa %>%
    rename(' ' = case_name)


  oa_table <- oa %>%
    kbl(longtable = T, escape = F, booktabs =  T, align = "c") %>%
    column_spec(1, bold = T, border_right = T) %>%
    row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(oa), 1), align = 'center') %>%
    kable_styling(font_size = 10, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    column_spec(2, color = "white",
                background = spec_color(oa$ROBERTS, end = 0.7),
                popover = paste("am:", oa$ROBERTS)) %>%
    column_spec(3, color = "white",
                background = spec_color(oa$ALITO, end = 0.7),
                popover = paste("am:", oa$ALITO)) %>%
    column_spec(4, color = "white",
                background = spec_color(oa$BARRETT, end = 0.7),
                popover = paste("am:", oa$BARRETT)) %>%
    column_spec(5, color = "white",
                background = spec_color(oa$GORSUCH, end = 0.7),
                popover = paste("am:", oa$GORSUCH)) %>%
    column_spec(6, color = "white",
                background = spec_color(oa$JACKSON, end = 0.7),
                popover = paste("am:", oa$JACKSON)) %>%
    column_spec(7, color = "white",
                background = spec_color(oa$KAGAN, end = 0.7),
                popover = paste("am:", oa$KAGAN)) %>%
    column_spec(8, color = "white",
                background = spec_color(oa$KAVANAUGH, end = 0.7),
                popover = paste("am:", oa$KAVANAUGH)) %>%
    column_spec(9, color = "white",
                background = spec_color(oa$SOTOMAYOR, end = 0.7),
                popover = paste("am:", oa$SOTOMAYOR)) %>%
    column_spec(10, color = "white",
                background = spec_color(oa$THOMAS, end = 0.7),
                popover = paste("am:", oa$THOMAS))

  oa_table

  save_kable(oa_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/oa_table.html")



  totals_table <- totals %>%
    rename(' ' = case_name) %>%
    kbl(title = 'October Term 2022 - Oral Argument Rhetoric') %>%
    row_spec(1, align = 'center') %>%
    column_spec(1, bold = T, border_right = T) %>%
    row_spec(0, bold = T, color = 'white', background = '#080808', align = 'center') %>%
    kable_styling(full_width = T, font_size = 10, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    column_spec(2, color = "white",
                background = spec_color(oa$ROBERTS, end = 0.7),
                popover = paste("am:", oa$ROBERTS)) %>%
    column_spec(3, color = "white",
                background = spec_color(oa$ALITO, end = 0.7),
                popover = paste("am:", oa$ALITO)) %>%
    column_spec(4, color = "white",
                background = spec_color(oa$BARRETT, end = 0.7),
                popover = paste("am:", oa$BARRETT)) %>%
    column_spec(5, color = "white",
                background = spec_color(oa$GORSUCH, end = 0.7),
                popover = paste("am:", oa$GORSUCH)) %>%
    column_spec(6, color = "white",
                background = spec_color(oa$JACKSON, end = 0.7),
                popover = paste("am:", oa$JACKSON)) %>%
    column_spec(7, color = "white",
                background = spec_color(oa$KAGAN, end = 0.7),
                popover = paste("am:", oa$KAGAN)) %>%
    column_spec(8, color = "white",
                background = spec_color(oa$KAVANAUGH, end = 0.7),
                popover = paste("am:", oa$KAVANAUGH)) %>%
    column_spec(9, color = "white",
                background = spec_color(oa$SOTOMAYOR, end = 0.7),
                popover = paste("am:", oa$SOTOMAYOR)) %>%
    column_spec(10, color = "white",
                background = spec_color(oa$THOMAS, end = 0.7),
                popover = paste("am:", oa$THOMAS))

  totals_table
  save_kable(totals_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/totals_table.html")

  custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

  scotus_term <- scotus %>%
    filter(term == 2022) %>%
    mutate(case_name = ifelse(argument == '21-376', "Chad Everet Brackeen, et al., Petitioners v. Deb Haaland, Secretary of the Interior, et al.", case_name)) %>%
    filter(type == 'Justice') %>%
    group_by(speaker, case_name) %>%
    summarise(total_word_count = sum(word_count)) %>%
    group_by(speaker) %>%
    summarise(total_word_count = sum(total_word_count)) %>%
    mutate(speaker = gsub("(CHIEF JUSTICE |JUSTICE )", "", speaker)) %>%
    mutate(speaker = str_to_title(speaker))

  word_count_plot <- ggplot(data = scotus_term, aes(y = total_word_count, x = speaker, fill = total_word_count)) +
    scale_y_continuous(labels = scales::comma, breaks = seq(10000, 80000, 10000)) +
    geom_hline(yintercept = 0, colour = 'gray5') +
    geom_col(colour = 'gray5') +
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
         title = "Total Words Spoken in 2022 Term Oral Arguments",
         subtitle = paste0("(As of 2023-07-01)")) +
    scale_x_discrete(labels = justice_image_labels) +
    theme_classic() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = ggtext::element_markdown(),
      axis.text.y = element_text(size = 15),
      axis.ticks.x = element_blank(),
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

  ggsave("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/OT23 Totals/word_count_plot_OT22.png", word_count_plot, dpi = 300)

} #OT 2022
