################################################################################
# SCOTUSWatch - Oral Argument Sittings OT 2023
# Author: Jake S. Truscott, Ph.D
# Updated March 2024
################################################################################

###############################################################################
#Load Packages & Libraries
###############################################################################
library(kableExtra); library(dplyr);  library(tidyr); library(scotustext); library(htmltools); library(ggplot2); library(png); library(dplyr); library(stringi); library(stringr); library(ggplot2); library(ggthemes); library(anytime); library(tm); library(scotustext); library(readxl); library(ggpattern); library(png); library(ggtext); library(grid); library(wesanderson); library(tidyr); library(readxl)

###############################################################################
# Tables by Sitting
###############################################################################

{
  case <- c("Pulsifer v. U.S. ", "CFPB v. Community Financial Services", "Acheson Hotels v. Laufer", "Murray v. UBS Securities", "Great Lakes Insurance v. Raiders Resort Realty", "Alexander v. South Carolina State Conference of the NAACP")
  argued <- c("10/02/2023", "10/03/2023", "10/04/2023", "10/10/2023", "10/10/2023", "10/11/2023")
  petitioner <- c("Shay Dvoretzky", "Elizabeth Prelogar", "Adam Unikowsky", "Easha Anand", "Jeffrey Wall", "John Gore")
  respondent <- c("Frederick Liu", "Noel Francisco", "Kelsi Corkran", "Eugene Scalia", "Howard Bashman", "Leah Aden")
  amici <- c("", "", "Erica Ross", "Anthony Yang", "", "Caroline Flynn")
  lower_court <- c("CA8", "CA5", "CA1", "CA2", "CA3", "Dist S.C.")
  cert_amici <- c("", 2, 5, 2, "", 2)
  merits_amici <- c(4, 29, 15, 12, 7, 14)

  october_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(october_sitting) <- gsub("\\.", " ", names(october_sitting))

  october_sitting_cases <- october_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(october_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))



  save_kable(october_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/october_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/october_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/october_sitting_cases.png", vwidth = 900, vheight = 70)




} #October Sitting

{
  case <- c("Culley v. Marshall", "O'Connor Ratcliff v. Garnier", "Lindke v. Freed", "Vidal v. Elster", "Dept. of Agric. Rural Div. v. Kirtz", "U.S. v. Rahimi", "Rudisill v. McDonough")
  argued <- c("10/30/2023", "10/31/2023", "10/31/2023", "11/01/2023", "11/06/2023", "11/07/2023", "11/08/2023")
  petitioner <- c("Shay Dvoretzky", "Hashim Moonpan", "Allon Kedem", "Malcolm Stewart", "Benjamin Snyder", "Elizabeth Prelogar", "Misha Tseytlin")
  respondent <- c("Edmund LaCour, Jr.", "Pamela Karlan", "Victoria Ferres", "Jonathan Taylor", "Nandan Joshi", "J. Matthew Wright", "Vivek Suri")
  amici <- c("Nicole Reaves", "Sopan Joshi", "Masha Hansford", "", "", "", "")
  lower_court <- c("CA11", "CA9", "CA6", "CAFed", "CA3", "CA5", "CAFed")
  cert_amici <- c(3, 0, 0, 1, 0, 7, 4)
  merits_amici <-  c(11, 14, 11, 7, 2, 60, 11)

  november_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(november_sitting) <- gsub("\\.", " ", names(november_sitting))

  november_sitting_cases <- november_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(november_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))

  november_sitting_cases

  save_kable(november_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/november_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/november_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/november_sitting_cases.png", vwidth = 900, vheight = 70)
} #November Sitting

{
  case <- c("Brown v. United States &\n Jackson v. United States", 'McElrath v. Georgia', "Wilkinson v. Garland, Att'y Gen.", 'SEC v. Jarkesy', 'Harrington v. Purdue Pharma L.P.', 'Moore v. United States', 'Muldrow v. St. Louis')
  argued <- c('11/27/2023', '11/28/2023', '11/28/2023', '11/29/2023', '12/04/2023', '12/05/2023', '12/06/2023')
  petitioner <- c('Jeffrey T. Green \n(For Brown) & \n Andrew Lee Adler \n(For Jackson)', 'Richard A. Simpson', 'Jamie A. Santos', 'Brian H. Fletcher', 'Curtis E. Gannon', 'Andrew M. Grossman', 'Brian Wolfman')
  respondent <- c('Austin Raynor', 'Stephen J. Petrany', 'Colleen R. Sinzdak', 'S. Michael McColloch', 'Gregory G. Garre (For Purdue Pharma L.P) & Pratik A. Shah \n(For Unsecured Creditors', 'Elizabeth B. Prelogar', 'Robert M. Loeb')
  amici <- c("", "", "", "", "", "", 'Aimee W. Brown')
  lower_court <- c("CA3 \n& CA11", "SCGA", "CA3", "CA5", "CA2", "CA9", "CA8")
  cert_amici <- c(1, 1, 0, 0, 0, 8, 3)
  merits_amici <-  c(4, 4, 3, 38, 26, 43, 12)

  december_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(december_sitting) <- gsub("\\.", " ", names(december_sitting))

  december_sitting_cases <- december_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2, width = "4cm") %>%  # Set the width of columns 2 to 8
    column_spec(3:4, width = "6cm") %>%  # Set the width of the first column
    column_spec(5:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(december_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))

  december_sitting_cases

  save_kable(december_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/december_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/december_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/december_sitting_cases.png", vwidth = 900, vheight = 70)
} #December Sitting

{
  case <- c('CAMPOS-CHAVES V. GARLAND, ATT’Y GEN &\n GARLAND, ATT’Y. GEN. V. SINGH\n(Consolidated)', 'FBI V. FIKRE', 'SHEETZ V. COUNTY OF EL DORADO', 'UNITED STATES TRUSTEE V. JOHN Q. HAMMONS FALL 2006, LLC', 'SMITH V. ARIZONA', 'MACQUAIRE INFRASTRUCTURE CORP. V. MOAB PARTNERS, L.P.', 'DEVILLIER V. TEXAS', 'RELENTLESS, INC. V. DEPT. OF COMMERCE', 'LOPER BRIGHT ENTERPRISES, INC. RAIMONDO, SEC. OF COMMERCE')
  argued <- c('01/08/2024', '01/08/2024', '01/09/2024', '01/09/2024', '01/10/2024', '01/16/2024', '01/16/2024', '01/17/2024', '01/17/2024')
  petitioner <- c('Easha Anand', 'Sopan Joshi', 'Paul J. Beard II', 'Masha G. Hansford', 'Hari Santhanam', 'Linda T. Coberly', 'Robert J. McNamara', 'Roman Martinez', 'Paul D. Clement')
  respondent <- c('Charles L. McCloud', 'Gadeir Abbas', 'Aileen M. McGrath', 'Daniel L. Geyser', 'Alexander W. Samuels', 'David C. Frederick', 'Aaron L. Nielson', 'Elizabeth B. Prelogar', 'Elizabeth B. Prelogar')
  amici <- c("", "", 'Erica L. Ross', '', 'Eric J. Feigin', 'Ephraim McDowell', 'Edwin S. Kneedler', '', '')
  lower_court <- c('CA5', 'CA9', 'Cal. Ct. App.', 'CA10', 'Ariz. Ct. App.', 'CA2', 'CA5', 'CA1', 'CADC')
  cert_amici <- c(0, 0, 5, 0, 0, 2, 2, 2, 8)
  merits_amici <-  c(5, 10, 23, 6, 10, 9, 12, 11, 62)

  january_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(january_sitting) <- gsub("\\.", " ", names(january_sitting))

  january_sitting_cases <- january_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2, width = "4cm") %>%  # Set the width of columns 2 to 8
    column_spec(3:4, width = "6cm") %>%  # Set the width of the first column
    column_spec(5:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(january_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))

  january_sitting_cases

  save_kable(january_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/january_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/january_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/january_sitting_cases.png", vwidth = 900, vheight = 70)
} #January Sitting

{
  case <- c('TRUMP V. ANDERSON', 'CORNER POST, INC. V. BD. OF GOVERNORS, FRS', 'BISSONNETTE V. LEPAGE BAKERIES PARK ST., LLC', 'OHIO V. EPA | KINDER MORGAN, INC. V. EP | AMERICAN FOREST & PAPER ASSN. V. EPA | U.S. STEEL CORP. V. EPA (Consolidated)', 'WARNER CHAPPELL MUSIC, INC. V. NEALY', 'MOODY V. NETCHOICE (Linked w/ Paxton)', 'NETCHOICE V. PAXTON', 'McINTOSH V. UNITED STATES', 'CANTERO V. BANK OF AMERICA, N.A.', "GARLAND ATT'Y GEN. V. CARGILL", 'COINBASE, INC. V. SUSKI')
  argued <- c('02/09/2024', '02/20/2024', '02/20/2024', '02/21/2024', '02/21/2024', '02/26/2024', '02/26/2024', '02/27/2024', '02/27/2024', '02/28/2024', '02/28/2024')
  petitioner <- c('Jonathan F. Mitchell', 'Bryan Weir', 'Jennifer Bennett', 'Mathura Sridharan (State App.) & Catherine Stetson (Industry App.)', 'Kannon Shanmugam', 'Henry Whitaker', 'Paul Clement', 'Steven Yurowitz', 'Jonathan Taylor', 'Brian Fletcher', 'Jessica Ellsworth')
  respondent <- c('Jason C. Murray (Anderson) & Shannon W. Stevenson (Griswold)', 'Benjamin Snyder', 'Traci Lovett', 'Malcolm Stewart (Federal Resp.) & Judith Vale (State Resp.)', 'Joe Wesley', 'Paul Clement', 'Aaron Nielson', 'Mattherw Guarnieri', 'Lisa Blatt', 'Jonathan Mitchell', 'David Harris')
  amici <- c('', '', '', '', 'Yaira Dubin', 'Elizabeth Prelogar', 'Elizabeth Prelogar', '', 'Malcolm Stewart', '', '')
  lower_court <- c('SCCO', 'CA8', 'CA2', 'CADC', 'CA11', 'CA11', 'CA5', 'CA2', 'CA2', 'CA5', 'CA9')
  cert_amici <- c(1, 3, 0, 1, 3, 7, 1, 0, 1, 1, 0)
  merits_amici <-  c(76, 12, 14, 0, 12, 76, 80, 2, 13, 20, 6)

  february_sitting <- data.frame(
    'Case' = case,
    'Argued' = argued,
    'Petitioner Counsel' = petitioner,
    'Respondent Counsel' = respondent,
    'Arguing Amici' = amici,
    'Lower Court' = lower_court,
    'Cert-Stage Amici' = cert_amici,
    'Merits-Stage Amici' = merits_amici
  )
  names(february_sitting) <- gsub("\\.", " ", names(february_sitting))

  february_sitting_cases <- february_sitting %>%
    kbl(longtable = TRUE, escape = FALSE, booktabs = TRUE, align = "c") %>%
    column_spec(1, bold = TRUE, border_right = TRUE, width = "6cm") %>%  # Set the width of the first column
    column_spec(2, width = "4cm") %>%  # Set the width of columns 2 to 8
    column_spec(3:4, width = "6cm") %>%  # Set the width of the first column
    column_spec(5:8, width = "4cm") %>%  # Set the width of columns 2 to 8
    row_spec(0, bold = TRUE, color = 'white', background = '#080808', align = 'center') %>%
    row_spec(seq(1, nrow(february_sitting), 1), align = 'center') %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed"))

  february_sitting_cases

  save_kable(february_sitting_cases, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/february_sitting_cases.html")

  html_file_path <- "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/february_sitting_cases.html"
  #webshot::install_phantomjs(force = T)
  phantomjs <- "C:/Users/Jake Truscott/AppData/Roaming/PhantomJS"
  webshot::webshot(html_file_path, "C:/Users/Jake Truscott/Documents/GitHub/jaketruscott.github.io/images/scotuswatch_tables/argument_sittings/february_sitting_cases.png", vwidth = 900, vheight = 70)
} #February Sitting
