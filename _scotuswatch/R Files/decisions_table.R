###############################################################################
# SCOTUSWatch - Decisions Table (OT2023)
# Author: Jake S. Truscott, Ph.D
# Updated December 2023
###############################################################################

###############################################################################
#Load Packages
###############################################################################
library(kableExtra); library(dplyr);  library(tidyr); library(scotustext); library(htmltools); library(ggplot2); library(png); library(dplyr); library(stringi); library(stringr); library(ggplot2); library(ggthemes); library(anytime); library(tm); library(scotustext); library(readxl); library(ggpattern); library(png); library(ggtext); library(grid); library(wesanderson); library(tidyr); library(readxl); library(anytime)


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
#Load Active Excel

'Notes:
100 = Wrote Majority
1 = Joined Majority
2 = Wrote Regular Concurrence
3 = Joined Regular Concurrence
4 = Wrote Regular Concurrence & Joined Regular Concurrence
5 = Wrote Concurrence in Judgement
6 = Joined Concurrence in Judgement
7 = Wrote Special Concurrence
8 = Joined Special Concurrence
-1 = Wrote Dissenting Opinion
-2 = Joined Dissenting Opinion
-3 = Wrote Dissent & Joined Dissent'

###############################################################################
decisions <- read.csv(file = "ot23_decisions/OT_23_Decisions.csv", as.is = T)

{

  decisions <- decisions %>%
    mutate(Date_Argued = anydate(Date_Argued),
           Date_Decided = anydate(Date_Decided)) %>%
    rename('Date Decided' = Date_Decided,
           'Date Argued' = Date_Argued,
           'Lower Court' = Lower_Court) %>%
    mutate(across(Coalition:ncol(.), ~ case_when(
      . == 100 ~ 'M*',
      . == 1 ~ 'M',
      . == 2 ~ 'RC',
      . == 3 ~ 'JRC',
      . == 4 ~ 'RC & JCR',
      . == 5 ~ 'CJ',
      . == 6 ~ 'JCJ',
      . == 7 ~ 'SC',
      . == 8 ~ 'JSC',
      . == -1 ~ 'D',
      . == -2 ~ 'JD',
      . == -3 ~ 'D & JD',
      is.na(.) ~ 'DNP',
      TRUE ~ as.character(.)
    )))


  decisions$`Date Argued` <- format(decisions$`Date Argued`, "%m/%d/%y")
  decisions$`Date Decided` <- format(decisions$`Date Decided`, "%m/%d/%y")

} #Convert Values to DF



###############################################################################
#Compile Tables (Vote Matrix & Summary Info)
###############################################################################

decisions_data <- decisions
matching_columns <- intersect(colnames(decisions_data), names(justice_image_labels))
original_column_names <- colnames(decisions_data)
matching_columns <- colnames(decisions_data)[8:16]

get_vote_color <- function(value){
  if (value == "M*") {
    return('darkolivegreen')
  } else if (value == 'M'){
    return('#99CCFF')
  } else if (value == "RC"){
    return('#66B2FF')
  } else if (value == 'JRC'){
    return('#3399FF')
  } else if (value == 'RC & JRC'){
    return('#3399FF')
  } else if (value == 'CJ'){
    return('#FF9933')
  } else if(value == 'JCJ'){
    return('#FFCC99')
  } else if (value == 'SC'){
    return('#B265FF')
  } else if(value == 'JSC'){
    return('#6600CC')
  } else if (value == 'D'){
    return('#FF3333')
  } else if (value == 'JD'){
    return('#CC0000')
  } else if (value == 'D & JD'){
    return('#990000')
  } else if (value == 'DNP'){
      return('#FFFFFF')
    }
  } #Assign Color to Box by Vote Type

{
  decisions_data <- decisions_data %>%
    rowwise() %>%
    mutate_at(vars(all_of(matching_columns)),
              ~cell_spec(., background = get_vote_color(.))) %>%
    mutate(across(all_of(matching_columns),
                  ~gsub('border-radius: 4px;',
                        'border-radius: 4px; color: white; ', .))) %>%
    rename_at(.vars = matching_columns, .funs = ~ justice_image_labels[.])
} #Subset Decision Data - Match w/ Justice Images

{
  decisions_info_1 <- decisions_data[1:29, 1:7]
  decisions_info_2 <- decisions_data[30:nrow(decisions_data), 1:7]

  decisions_info_1 <- decisions_info_1 %>%
      mutate(`Date Decided` = as.Date(`Date Decided`, "%m/%d/%y"),
           `Date Argued` = as.Date(`Date Argued`, "%m/%d/%y")) %>%
    arrange(`Date Decided`) %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(c(1:6), bold = TRUE) %>%
    column_spec(7, bold = TRUE, border_right = TRUE) %>%
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(decisions_info_1), 1), align = 'center') %>%
    row_spec(nrow(decisions_info_1), extra_css = "border-bottom: 2px solid;") %>%
    kable_styling(font_size = 18, bootstrap_options = c("striped", "hover", "condensed", "responsive"))


  decisions_info_2 <- decisions_info_2 %>%
    mutate(`Date Decided` = as.Date(`Date Decided`, "%m/%d/%y"),
           `Date Argued` = as.Date(`Date Argued`, "%m/%d/%y")) %>%
    arrange(`Date Decided`) %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(c(1:6), bold = TRUE) %>%
    column_spec(7, bold = TRUE, border_right = TRUE) %>%
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(decisions_info_2), 1), align = 'center') %>%
    row_spec(nrow(decisions_info_2), extra_css = "border-bottom: 2px solid;") %>%
    kable_styling(font_size = 18, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

  } #Compile Summary Info Table

{

  chunk_size <- 10
  num_rows <- nrow(decisions_data)


  for (start_row in seq(1, num_rows, by = chunk_size)) {
    end_row <- min(start_row + chunk_size - 1, num_rows)

    chunk <- decisions_data[start_row:end_row, c(1:3, 8:16)]

    chunk <- chunk %>% mutate(`Date Decided` = as.Date(`Date Decided`, "%m/%d/%y"),
             `Date Argued` = as.Date(`Date Argued`, "%m/%d/%y")) %>%
      arrange(`Date Decided`) %>%
      select(-c(`Date Argued`)) %>%
      kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
      add_header_above(c(" ", " ", original_column_names[8:16])) %>%
      column_spec(1, width = "3cm", bold = TRUE, border_right = TRUE) %>%
      column_spec(2, bold = TRUE, border_right = TRUE) %>%
      column_spec(c(2:11), width = "1.25cm", border_right = TRUE) %>%
      column_spec(2:11, width = "1.25cm", border_right = TRUE, extra_css = "vertical-align: middle; font-size: 18px;") %>%
      row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
      row_spec(seq(1, nrow(chunk), 1), align = 'center') %>%
      row_spec(nrow(chunk), extra_css = "border-bottom: 2px solid;") %>%
      kable_styling(font_size = 15, bootstrap_options = c("striped", "hover", "responsive")) %>%
      add_footnote(
        c(
          "<span style=\"border-radius: 0px; padding: 1px; background-color: white !important; color: white;\"></span>",
          "<span style=\"border-radius: 0px; padding: 0px; background-color: white !important; color: white;\"></span>",

          "<span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: darkolivegreen !important; color: white;\">M*</span> = Majority Author
         <span style=\"margin-left: 10px;\"></span>
         <span style=\"border-radius: 1px; padding: 1px; background-color: #99CCFF !important; color: white;\">M</span> = Joined Majority
         <span style=\"margin-left: 10px;\"></span>
         <span style=\"border-radius: 1px; padding: 1px; background-color: #66B2FF !important; color: white;\">RC</span> = Wrote Concurrence
         <span style=\"margin-left: 10px;\"></span>
         <span style=\"border-radius: 1px; padding: 1px; background-color: #3399FF !important; color: white;\">JRC</span> = Joined Concurrence
         <span style=\"margin-left: 10px;\"></span>
         <span style=\"border-radius: 1px; padding: 1px; background-color: #66B2FF !important; color: white;\">RC & JRC</span> = Wrote & Joined Concurrence",

          "<span style=\"border-radius: 3px; padding: 1px; background-color: white !important; color: white;\"> </span>",

          "<span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #FF9933 !important; color: white;\">CJ</span> = Wrote Concurrence In Judgement
         <span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #FFCC99 !important; color: white;\">JCJ</span> = Joined Concurrence In Judgement
         <span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #B265FF !important; color: white;\">SC</span> = Wrote Special Concurrence
         <span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #6600CC !important; color: white;\">JSC</span> = Joined Special Concurrence
         <span style=\"margin-left: 10px;\"></span>",

          "<span style=\"border-radius: 3px; padding: 1px; background-color: white !important; color: white;\"> </span>",

          "<span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #FF3333 !important; color: white;\">D</span> = Wrote Dissent
         <span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #CC0000 !important; color: white;\">JD</span> = Joined Dissent
         <span style=\"margin-left: 10px;\"></span>
        <span style=\"border-radius: 1px; padding: 1px; background-color: #990000 !important; color: white;\">D & JD</span> = Wrote & Joined Dissent
         <span style=\"margin-left: 10px;\"></span>"


        ),
        notation = "none",
        escape = FALSE  # Add escape parameter to allow HTML formatting
      )

    save_kable(chunk, paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Table_", start_row, ".html"))
    html_file_path <- paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Table_", start_row, ".html")
    #webshot::install_phantomjs(force = T)
    phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
    webshot::webshot(html_file_path, paste0("C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Table_", start_row, ".png"), vwidth = 1000, vheight = 300)
  }



} #Compile Vote Matrix Table

###############################################################################
#Save to Local Machine
###############################################################################

save_kable(decisions_info_1, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Info_1.html")
html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Info_1.html"
#webshot::install_phantomjs(force = T)
phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Info_1.png", vwidth = 1300, vheight = 1000)

save_kable(decisions_info_2, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Info_2.html")
html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Info_2.html"
#webshot::install_phantomjs(force = T)
phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/Decisions/OT_23_Decisions_Info_2.png", vwidth = 1300, vheight = 1000)

