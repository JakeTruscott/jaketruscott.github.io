###############################################################################
#Wordshoal Test - OAs
###############################################################################

library(quanteda.textmodels); library(quanteda); library(tidyr); library(scotustext); library(stringr); library(stringi); library(tm); library(pdftools); library(pscl); library(texreg); library(MASS); library(rjags); library(KernSmooth); library(dplyr); library(ggplot2)


base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
rdata_url <- paste0(base_url, "scotus_transcripts_23.rdata")
load(url(rdata_url))

oa <- scotus_OT23 %>%
  filter(speaker_type == 'Justice') %>%
  group_by(speaker, argument) %>%
  reframe(text = paste(text, collapse = " "))

process_text <- function(input_text){
  cleaned_text <- tolower(input_text)
  cleaned_text <- gsub("[[:punct:]]", "", cleaned_text)
  cleaned_text <- tm::removeWords(cleaned_text, stopwords("english"))
  return(cleaned_text)
}

scotusWFM <- corpus(process_text(oa$text)) #Convert Text to Large corpus Object

{
  docvars(scotusWFM)$document_id <- c(1:length(oa$argument))
  docvars(scotusWFM)$justice <- oa$speaker
  docvars(scotusWFM)$docket_id <- oa$argument
} #Docvars

{
  textmodel_wordshoal <- function(x, groups, authors, dir = c(1,2), tol = 1e-3) {
    UseMethod("textmodel_wordshoal")
  }

  textmodel_wordshoal.dfm <- function(x, groups, authors, dir = c(1,2), tol = 1e-3, use_weights = TRUE) {

    startTime <- proc.time()

    x <- quanteda::as.dfm(x)
    groups <- as.factor(groups)
    authors <- as.factor(authors)

    # check that no groups or author partitions are a single row
    if (length(not_enough_rows <- which(lengths(split(quanteda::docnames(x), groups)) < 2)))
      stop("only a single case for the following groups: \n",
           paste(levels(groups)[not_enough_rows], collapse = "\n"))

    S <- quanteda::ndoc(x)
    psi <- rep(NA, S)

    N <- nlevels(authors)
    M <- nlevels(groups)

    ## FIRST-LEVEL SCALING ##

    cat("\nScaling ", M, " document groups", sep="")
    for (j in 1:M) {

      # Extract dfm rows for current document group
      groupdfm <- x[groups == levels(groups)[j], ]

      # Remove features that do not appear XX_in at leastone document_XX at least twice
      groupdfm <- quanteda::dfm_trim(groupdfm, min_docfreq = 1)

      # Run wordfish on document group
      # wfresult <- wordfishcpp(as.matrix(groupdfm), c(1, 2), c(0, 0, 1/9, 1), c(1e-2, 1e-4), 1L, 0L)
      wfresult <- textmodel_wordfish(groupdfm, tol = c(tol, 1e-8))

      # Save the results
      psi[groups == levels(groups)[j]] <-
        if(isS4(wfresult)){ wfresult@theta } else { wfresult$theta }

      if (j %% 20 == 0)
        cat(j, " ", sep="")
      else
        cat(".")
    }

    ## SECOND-LEVEL SCALING ##

    cat("\nFactor Analysis on Debate-Level Scales")

    psi <- replace(psi,is.na(psi),0) # debates that failed to scale
    jVec <- as.integer(groups)
    iVec <- as.integer(authors)

    ## Factor Analysis on Debate Score Matrix ##

    prioralpha <- 0.5
    priorbeta <- 0.5
    priortheta <- 1
    priortau <- 1

    # Dumb (but deterministic!) initial values

    alpha <- rep(0,M)
    beta <- rep(0,M)
    theta <- seq(-2,2,length.out=N)
    tau <- rep(1,N)

    # Calculate initial log-posterior... (Depending on Weight Classification)

    if (use_weights == TRUE){
      lastlp <- -Inf
      lp <- sum(dnorm(alpha,0,prioralpha,log=TRUE))
      lp <- lp + sum(dnorm(beta,0,priorbeta,log=TRUE))
      lp <- lp + sum(dnorm(theta,0,priortheta,log=TRUE))
      lp <- lp + sum(dgamma(tau,1,1,log=TRUE))
      for (s in 1:S) {
        weight <- docvars(x)$weight[s]
        lps <- alpha[jVec[s]] + beta[jVec[s]] * theta[iVec[s]]
        lp <- lp + dnorm(psi[s], lps, (tau[iVec[s]])^(-1/2), log = TRUE) * weight
      }
    } else {
      lastlp <- -Inf
      lp <- sum(dnorm(alpha,0,prioralpha,log=TRUE))
      lp <- lp + sum(dnorm(beta,0,priorbeta,log=TRUE))
      lp <- lp + sum(dnorm(theta,0,priortheta,log=TRUE))
      lp <- lp + sum(dgamma(tau,1,1,log=TRUE))
      for (s in 1:S) {
        lps <- alpha[jVec[s]] + beta[jVec[s]] * theta[iVec[s]]
        lp <- lp + dnorm(psi[s], lps, (tau[iVec[s]])^(-1/2), log=TRUE)
      }
    }



    # Until log-posterior stops changing...

    while((lp - lastlp) > abs(tol)){

      cat(".")

      # Update debate parameters

      priordebate <- solve(matrix(c(prioralpha^2,0,0,priorbeta^2),2,2))

      for (j in 1:M){
        locs <- which(jVec == j)
        Ytmp <- psi[locs]
        Xtmp <- cbind(1,theta[iVec[locs]])
        Wtmp <- diag(tau[iVec[locs]])
        coeftmp <- solve(t(Xtmp) %*% Wtmp %*% Xtmp + priordebate) %*% t(Xtmp) %*% Wtmp %*% Ytmp
        alpha[j] <- coeftmp[1]
        beta[j] <- coeftmp[2]
      }

      # Update speaker parameters

      for (i in 1:N){
        locs <- which(iVec == i)
        Ytmp <- matrix(psi[locs] - alpha[jVec[locs]],ncol=1)
        Xtmp <- matrix(beta[jVec[locs]],ncol=1)
        coeftmp <- solve(t(Xtmp) %*% Xtmp + priortheta^(-2)) %*% t(Xtmp) %*% Ytmp
        theta[i] <- coeftmp[1,1]
        mutmp <- solve(t(Xtmp) %*% Xtmp + priortheta^(-2)) %*% t(Xtmp) %*% Xtmp %*% coeftmp
        tau[i] <- (priortau + 0.5 * length(Ytmp)) /
          (priortau + 0.5 * (sum(Ytmp^2) - mutmp*(priortheta^(-2)) * mutmp))
      }

      # Recalculate log-posterior (Depending on Weights Classification)

      if (use_weights == TRUE){
        lastlp <- lp
        lp <- sum(dnorm(alpha, 0, prioralpha, log = TRUE))
        lp <- lp + sum(dnorm(beta, 0, priorbeta, log = TRUE))
        lp <- lp + sum(dnorm(theta, 0, priortheta, log = TRUE))
        lp <- lp + sum(dgamma(tau, priortau, priortau, log = TRUE))
        for (s in 1:S){
          # Access the 'weight' docvar from docvars
          weight <- docvars(x)$weight[s]

          lps <- alpha[jVec[s]] + beta[jVec[s]] * theta[iVec[s]]
          lp <- lp + dnorm(psi[s], lps, (tau[iVec[s]])^(-1/2), log = TRUE) * weight
        }
      } else {
        lastlp <- lp
        lp <- sum(dnorm(alpha,0,prioralpha, log = TRUE))
        lp <- lp + sum(dnorm(beta, 0, priorbeta, log = TRUE))
        lp <- lp + sum(dnorm(theta, 0, priortheta, log = TRUE))
        lp <- lp + sum(dgamma(tau, priortau, priortau, log = TRUE))
        for (s in 1:S){
          lps <- alpha[jVec[s]] + beta[jVec[s]] * theta[iVec[s]]
          lp <- lp + dnorm(psi[s], lps, (tau[iVec[s]])^(-1/2), log = TRUE)
        }
      }



    } # end while


    thetaSE <- rep(NA,N)
    for (i in 1:N){
      locs <- which(iVec == i)
      Xtmp <- matrix(beta[jVec[locs]],ncol=1)
      thetaSE[i] <- sqrt(solve(t(Xtmp) %*% Xtmp + priortheta^(-2)) / tau[i])
    }


    cat("\nElapsed time:", (proc.time() - startTime)[3], "seconds.\n")

    result <- list(
      tol = tol,
      authors = authors,
      groups = groups,
      theta = theta,
      beta = beta,
      alpha = alpha,
      psi = psi,
      se.theta = thetaSE,
      call = match.call()
    )

    class(result) <- c("textmodel_wordshoal", "textmodel", "list")
    result

  }

  print.textmodel_wordshoal <- function(x, ...) {
    cat("Call:\n\t")
    print(x$call)
    cat("\n",
        length(unique(x$authors)), " authors; ",
        length(unique(x$groups)), " groups.",
        "\n",
        sep = "")
  }

  summary.textmodel_wordshoal <- function(object, ...) {
    stat <- data.frame(
      theta = object$theta,
      se = object$se.theta,
      row.names = levels(object$authors),
      check.rows = FALSE,
      stringsAsFactors = FALSE
    )

    result <- list(
      'call' = object$call,
      'estimated.author.positions' = as.statistics_textmodel(stat)#,
    )
    return(as.summary.textmodel(result))

  }
} #Wordshoal

scotusDFM <- dfm(scotusWFM) #Convert to DFM


wordshoalfit_noweights <- textmodel_wordshoal.dfm(scotusDFM, dir = c(7,1),
                                                  groups = docvars(scotusWFM, "docket_id"),
                                                  authors = docvars(scotusWFM, "justice"),
                                                  use_weights = FALSE) #Fit Wordshoal

authors_noweights <- levels(wordshoalfit_noweights$authors)
thetas_noweights  <- wordshoalfit_noweights$theta
se_noweights  <- wordshoalfit_noweights$se.theta
estimates_noweights <- data.frame(author = authors_noweights,
                                  theta = thetas_noweights,
                                  se = se_noweights,
                                  upper = thetas_noweights + se_noweights,
                                  lower = thetas_noweights - se_noweights)

estimates_noweights <- estimates_noweights %>%
  arrange(theta) %>%
  mutate(ideology = case_when(
    .default = 'Conservative',
    author == "JUSTICE KAGAN" ~ 'Liberal',
    author == 'JUSTICE SOTOMAYOR' ~ 'Liberal',
    author == 'JUSTICE JACKSON' ~ 'Liberal'))

ggplot(data = estimates_noweights, aes(x = theta, y = reorder(author, theta))) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.25, linewidth = 1, position = position_dodge(width = 1)) +
  geom_point( size = 3 , color = 'black') +
  geom_vline(xintercept = 0, linetype = 2, color = 'gray5', alpha = 1/5) +
  labs(x = expression(theta),
       y = " ",
       color = 'Ideology',
       shape = 'Ideology',
       linetype = 'Ideology',
       title = 'Wordshoal Estimation of SCOTUS Decisions',
       subtitle = '2016 to 2019 Terms') +
  scale_color_manual(values = c('Weights' = 'gray5', 'No Weights' = 'gray5')) +
  scale_shape_manual(values = c('Weights' = 16, 'No Weights' = 17)) +
  scale_linetype_manual(values = c('Weights' = 1, 'No Weights' = 2)) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    panel.border = element_rect(linewidth = 1, color = "gray5", fill = NA),
    legend.title.align = 0.5,
    legend.text.align = 0.25,
    legend.title = element_blank(),
    legend.text = element_text(size = 15, color = "gray5"),
    legend.box.background = element_rect(size = 1, color = 'gray5', fill = NA),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "gray", color = "gray5"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5)
  )

