#' get data of ratios of mixing ratios of 2 species over each NOx-Condition, every Mechanism and Run
#' 
#' Get the raw data of the budgets of a Species in a Mechanisms from a Run, that was processed at a Date
#' @param dataframe The data frame with the raw data of all mixing ratios
#' @param Numerator The species whose mixing ratios are the numerator or ratio
#' @param Denominator The species whose mixing ratios are the denominator or ratio
#' @return Data frame containing ratios of mixing ratio data over each NOx-Conditions
#' @export

get_ratio_data <- function (dataframe, Numerator, Denominator) {
  num.colnums <- match(Numerator, names(dataframe))
  den.colnums <- match(Denominator, names(dataframe))
  numerator <- dataframe[[num.colnums]]
  denominator <- dataframe[[den.colnums]]
  dataframe$Ratio = numerator / denominator
  data <- dataframe %>% select(Mechanism, Run, NOx.Emissions, Temperature, H2O2, HNO3, Ratio) %>%
    arrange(Temperature) %>%
    mutate(Temperature.C = Temperature - 273) %>%
    rowwise() %>%
    mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
    select(-Temperature, -H2O2, -HNO3, -NOx.Emissions) %>%
    group_by(Mechanism, Run, Temperature.C, NOx.Condition) %>%
    summarise(Ratio = mean(Ratio))
  return(data)
}