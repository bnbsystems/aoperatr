

valudate.average.adj <-  function(rawdata, weights_data, powierzchnia) {

  data <- rawdata %>%
    dplyr::mutate(UdzialWPrawie = as.numeric(UdzialWPrawie)) %>%
    dplyr::mutate(Powierzchnia = as.numeric(Powierzchnia)) %>%
    dplyr::mutate(CenaTransakcyjna = as.numeric(gsub("\\s", "", CenaTransakcyjna))) %>%
    dplyr::mutate(CenaNieruchomosci = CenaTransakcyjna / UdzialWPrawie) %>%
    dplyr::mutate(CenaZaMetr = round(CenaNieruchomosci / Powierzchnia, digits = 2))

  data <- data[!data$Data == "", ]
  data.cena <- Sys.Date()

  data$CenaTransakcyjnaStr <- format(data$CenaTransakcyjna, big.mark = " ", scientific = FALSE, nsmall = 0)
  data$CenaZaMetrStr <- format(data$CenaZaMetr, big.mark = " ", scientific = FALSE, nsmall = 0)
  data$PowierzchniaStr <- sprintf("%.2f", data$Powierzchnia)
  data$DaysToLatest <- as.numeric(lubridate::ymd(data.cena) - lubridate::ymd(data$Data))
  data$MonthsToLatest <- floor(data$DaysToLatest / 30)
  data$CenaTransakcyjnaAdj <- round(data$CenaTransakcyjna * (1 + wzrost.miesieczny * data$MonthsToLatest))
  data$CenaTransakcyjnaAdjStr <- format(data$CenaTransakcyjnaAdj, big.mark = " ", scientific = FALSE, nsmall = 0)
  data$CenaZaMetrAdj <- round(data$CenaTransakcyjnaAdj / data$Powierzchnia, digits = 2)
  data$CenaZaMetrAdjStr <- format(data$CenaZaMetrAdj, big.mark = " ", scientific = FALSE, nsmall = 0)

  WK <- list()

  WK$DataMax <- max(data$Data)
  WK$DataMin <- min(data$Data)

  WK$Csr <- mean(data$CenaZaMetrAdj)
  WK$Cmin <- min(data$CenaZaMetrAdj)
  WK$Cmax <-max(data$CenaZaMetrAdj)
  WK$CminCsr <- round((WK$Cmin / WK$Csr), digits = 3)
  WK$CmaxCsr <- round((WK$Cmax / WK$Csr), digits = 3)
  WK$CmaxminCsr <- round((WK$CminCsr + WK$CmaxCsr) / 2, digits = 3)
  WK$DeltaCena <- WK$Cmax - WK$Cmin
  WK$Cx <- round(((WK$Csr - WK$Cmin) / WK$DeltaCena), digits = 3)

  WK$WKData <- list()
  WK$WeightsTable <- list()
  WK$WeightsTable$Description <- list()
  WK$WeightsTable$Participation <- list()
  WK$WeightsTable$Min <- list()
  WK$WeightsTable$Max <- list()
  WK$SumaC <- 0

  counter <- 1
  for (weight in weights_data$weights) {
    wk_item <- list()
    wk_item$scale_range <- length(weight$ratings)

    value_min <- round(WK$CminCsr * (weight$participation / 100), digits = 3)
    value_max <- round(WK$CmaxCsr * (weight$participation / 100), digits = 3)

    if (wk_item$scale_range == 2) {
      wk_item$values <- c(value_min, value_max)
    } else {
      intervals <- wk_item$scale_range - 1
      wk_item$values <- seq(value_min, value_max, by = (value_max - value_min) / intervals)
      wk_item$values <- round(wk_item$values, digits = 3)
    }

    for (rating in weight$ratings) {
      if (rating$rate == weight$rate) {
        wk_item$rateText <- rating$rateText
        wk_item$rateValue <- wk_item$values[[rating$rate + 1]]
        wk_item$weightDescription <- weight$description
        WK$SumaC <- WK$SumaC + wk_item$rateValue
        break
      }
    }

    ## Pair WK with ratings
    WK$WeightsTable$Description[[counter]] <- weight$description
    WK$WeightsTable$Participation[[counter]] <- weight$participation
    WK$WeightsTable$Min[[counter]] <- value_min
    WK$WeightsTable$Max[[counter]] <- value_max
    WK$WeightsTable$rateValue[[counter]] <- wk_item$rateValue

    WK$WKData[[weight$name]] <- wk_item

    counter <- counter + 1
  }

  WK$WeightsTable$Description <- unlist(WK$WeightsTable$Description)
  WK$WeightsTable$Participation <- unlist(WK$WeightsTable$Participation)
  WK$WeightsTable$Min <- unlist(WK$WeightsTable$Min)
  WK$WeightsTable$Max <- unlist(WK$WeightsTable$Max)
  WK$WeightsTable$rateValue <- unlist(WK$WeightsTable$rateValue)

  WK$WeightsTable$Description <- append(WK$WeightsTable$Description, "")
  WK$WeightsTable$Participation <- append(WK$WeightsTable$Participation, 100)
  WK$WeightsTable$Min <- append(WK$WeightsTable$Min, sum(WK$CminCsr))
  WK$WeightsTable$Max <- append(WK$WeightsTable$Max, sum(WK$CmaxCsr))
  WK$WeightsTable$rateValue <- append(WK$WeightsTable$rateValue, WK$SumaC)


  WK$CenaZaMetr <- round(WK$SumaC * WK$Csr, digits = 2)
  WK$Cena <- round(WK$CenaZaMetr * as.numeric(powierzchnia), digits = 2)
  WK$CenaRound <- round(WK$Cena)
  return(list(
    CenaZaMetrAdj = WK$CenaZaMetrAdj,
    CenaZaMetrAdjPriceMin=price(min(data$CenaZaMetrAdj)),
    CenaZaMetrAdjPrice=price(max(data$CenaZaMetrAdj)),
    CminCsr = WK$CminCsr,
    CenaZaMetr=WK$CenaZaMetr,
    Cena = WK$Cena,
    CenaPrice = price(WK$Cena),
    Csr = WK$Csr,
    CsrPrice = price(round(WK$Csr, 2)),
    SumaC = WK$SumaC,
    WeightsTable = WK$WeightsTable,
    CenaRound = WK$CenaRound
  ))
}
