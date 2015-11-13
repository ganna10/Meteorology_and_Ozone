#' get data for net budget plot
#' 
#' Select data for Net Budget plot, determine NOx-Condition and output mean Net Budget for each NOx-condition, Mechanism and Temperature
#' @param dataframe The data frame containing full data
#' @return The data frame with the data necessary for net budget plot
#' @export

get_net_budget_data <- function (dataframe) {
  df <- dataframe %>% select(Mechanism, Temperature, NOx.Emissions, Net.Reaction.Rate, Run, H2O2, HNO3) %>%
     mutate(Temperature.C = Temperature - 273) %>%
     arrange(Temperature) %>%
     rowwise() %>%
     mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
     select(Mechanism, Temperature.C, Run, Net.Reaction.Rate, NOx.Condition) %>%
    group_by(Mechanism, Run, Temperature.C, NOx.Condition) %>%
    summarise(Net.Rate = mean(Net.Reaction.Rate))
  return(df)
}