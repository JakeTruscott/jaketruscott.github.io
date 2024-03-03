###############################################################################
#Decisions Table
#Jake S. Truscott
#Code Developed December 2023
###############################################################################

###############################################################################
#Load Packages
###############################################################################
library(kableExtra); library(dplyr);  library(tidyr); library(scotustext); library(htmltools); library(ggplot2); library(png); library(dplyr); library(stringi); library(stringr); library(ggplot2); library(ggthemes); library(anytime); library(tm); library(scotustext); library(readxl); library(ggpattern); library(png); library(ggtext); library(grid); library(wesanderson); library(tidyr); library(readxl); library(anytime)


###############################################################################
#Load Active Excel
###############################################################################

decisions <- read_xlsx(path = "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/_scotuswatch/ot23_decisions/OT_23_Decisions.xlsx")

decisions <- decisions %>%
  mutate(Date_Argued = anydate(Date_Argued),
         Date_Decided = anydate(Date_Decided)) %>%
  rename('Date Decided' = Date_Decided,
         'Date Argued' = Date_Argued,
         'Lower Court' = Lower_Court) %>%
  mutate(across(Coalition:ncol(.), ~ case_when(
    . == 1 ~ "M",
    . == 2 ~ 'RC',
    . == 3 ~ 'SC',
    . == -1 ~ 'D',
    . == 4 ~ 'CJ',
    . == 100 ~ 'M*',
    TRUE ~ as.character(.)
  )))


decisions$`Date Argued` <- format(decisions$`Date Argued`, "%m/%d/%y")
decisions$`Date Decided` <- format(decisions$`Date Decided`, "%m/%d/%y")



decisions

###############################################################################
#Justice Images
###############################################################################
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

###############################################################################
#Table
###############################################################################

decisions_data <- decisions
matching_columns <- intersect(colnames(decisions_data), names(justice_image_labels))

original_column_names <- colnames(decisions_data)

matching_columns <- colnames(decisions_data)[8:16]

get_cell_color <- function(value) {
  if (value == "M") {
    return("deepskyblue")   # Set the color for factor value "A"
  } else if (value == "D") {
    return("tomato")  # Set the color for factor value "B"
  } else if (value == "RC") {
    return("steelblue") # Set the color for factor value "C"
  } else if (value == 'SC'){
    return("purple4")
  } else if (value == "M*"){
    return ("darkolivegreen")
  } else if (value == 'CJ'){
    return("dodgerblue")
  } else {
    return("white") # Set the default color for other values
  }
}

decisions_data <- decisions_data %>%
  mutate_at(vars(all_of(matching_columns)),
            ~cell_spec(., background = get_cell_color(.))) %>%
  mutate(across(all_of(matching_columns),
                ~gsub('border-radius: 4px;',
                      'border-radius: 4px; color: white; ', .))) %>%
  rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])

decisions_data

decisions_info <- decisions_data[,1:7] %>%
  arrange(`Date Argued`) %>%
  kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
  column_spec(c(1:6), bold = TRUE) %>%
  column_spec(7, bold = TRUE, border_right = TRUE) %>%
  row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
  row_spec(seq(1, nrow(decisions_data), 1), align = 'center') %>%
  row_spec(nrow(decisions_data), extra_css = "border-bottom: 2px solid;") %>%
  kable_styling(font_size = 18, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

decision_table <- decisions_data[, c(1:3, 8:16)] %>%
  arrange(`Date Argued`) %>%
  select(-c(`Date Argued`)) %>%
  kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
  add_header_above(c(" ", " ", original_column_names[8:16])) %>%
  column_spec(1, width = "3cm", bold = TRUE, border_right = TRUE) %>%
  column_spec(2, bold = TRUE, border_right = TRUE) %>%
  column_spec(c(2:11), width = "1.25cm", border_right = TRUE) %>%
  column_spec(2:11, width = "1.25cm", border_right = TRUE, extra_css = "vertical-align: middle; font-size: 18px;") %>%
  row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
  row_spec(seq(1, nrow(decisions_data), 1), align = 'center') %>%
  row_spec(nrow(decisions_data), extra_css = "border-bottom: 2px solid;") %>%
  kable_styling(font_size = 15, bootstrap_options = c("striped", "hover", "responsive")) %>%
  add_footnote(
    c(
      "<span style=\"border-radius: 0px; padding: 0px; background-color: white !important; color: white;\">RC</span>",
      "<span style=\"border-radius: 1px; padding: 1px; background-color: darkolivegreen !important; color: white;\">M*</span> = Majority Author",
      "<span style=\"border-radius: 0px; padding: 0px; background-color: white !important; color: white;\">RC</span>",
      "<span style=\"border-radius: 1px; padding: 1px; background-color: deepskyblue !important; color: white;\">M</span> = Majority",
      "<span style=\"border-radius: 0px; padding: 0px; background-color: white !important; color: white;\">RC</span>",
      "<span style=\"border-radius: 1px; padding: 1px; background-color: steelblue !important; color: white;\">RC</span> = Regular Concurrence",
      "<span style=\"border-radius: 0px; padding: 0px; background-color: white !important; color: white;\">RC</span>",
      "<span style=\"border-radius: 1px; padding: 1px; background-color: dodgerblue !important; color: white;\">CJ</span> = Concurrence in Judgement",
      "<span style=\"border-radius: 0px; padding: 0px; background-color: white !important; color: white;\">RC</span>",
      "<span style=\"border-radius: 1px; padding: 1px; background-color: slateblue !important; color: white;\">SC</span> = Special Concurrence (In Part)",
      "<span style=\"border-radius: 0px; padding: 0px; background-color: white !important; color: white;\">RC</span>",
      "<span style=\"border-radius: 1px; padding: 1px; background-color: tomato !important; color: white;\">D</span> = Dissent"
    ),
    notation = "none",
    escape = FALSE  # Add escape parameter to allow HTML formatting
  )


save_kable(decisions_info, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Info.html")
html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Info.html"
#webshot::install_phantomjs(force = T)
phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Info.png", vwidth = 1300, vheight = 70)


save_kable(decision_table, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Table.html")
html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Table.html"
#webshot::install_phantomjs(force = T)
phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Table.png", vwidth = 1000, vheight = 70)


