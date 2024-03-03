library(kableExtra)
library(dplyr)
library(tidyr)
library(scotustext)
library(htmltools)
library(ggplot2)
library(png)
library(dplyr); library(stringi); library(stringr); library(ggplot2); library(ggthemes); library(anytime); library(tm); library(scotustext); library(readxl); library(ggpattern); library(png); library(ggtext); library(grid); library(wesanderson); library(tidyr)

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
  save_kable(oa_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/oa_table_23_active.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/oa_table_23_active.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/oa_table_23_active.png", vwidth = 900, vheight = 70)

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
  save_kable(totals_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/totals_table_OT23_active.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/totals_table_OT23_active.html"
  #webshot::install_phantomjs(force = T)
  #phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/totals_table_OT23_active.png", vwidth = 900, vheight = 70)

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
  ggsave("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/word_count_plot_OT23.png", word_count_plot, dpi = 300)

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
  save_kable(attorney_participation_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/attorney_participation_23_active.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/attorney_participation_23_active.html"
  #webshot::install_phantomjs(force = T)
  #phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/attorney_participation_23_active.png", vwidth = 800, vheight = 70)

} #OT 2023 (Totals)

{

  base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
  rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
  load(url(rdata_url))

  {
    oa <- scotus_OT23 %>%
      filter(speaker_type == 'Justice') %>%
      filter(sitting == 'February') %>%
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
  save_kable(oa_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/oa_table_23_February.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/oa_table_23_February.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/oa_table_23_February.png", vwidth = 900, vheight = 70)

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
  save_kable(totals_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/totals_table_OT23_February.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/totals_table_OT23_February.html"
  #webshot::install_phantomjs(force = T)
  #phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/totals_table_OT23_February.png", vwidth = 900, vheight = 70)

  {
    custom_colors <- c("#5FA934", "#3B5E8B", "#522559")

    scotus_term <- scotus_OT23 %>%
      filter(sitting == 'February') %>%
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
  ggsave("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/word_count_plot_OT23_February.png", word_count_plot, dpi = 300)

  {


    attorneys <- scotus_OT23 %>%
      #select(-id) %>%
      filter(sitting == "February") %>%
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
  save_kable(attorney_participation_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/attorney_participation_23_February.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/attorney_participation_23_February.html"
  #webshot::install_phantomjs(force = T)
  #phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/February Sitting 2023/attorney_participation_23_February.png", vwidth = 1100, vheight = 70)

} #OT 2023 (By Sitting) -- Currently February

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

  save_kable(oa_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/oa_table.html")



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
  save_kable(totals_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/totals_table.html")

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

  ggsave("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/word_count_plot_OT22.png", word_count_plot, dpi = 300)

  } #OT 2022





