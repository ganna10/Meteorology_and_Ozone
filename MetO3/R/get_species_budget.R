#' Get budget allocated to reactions of species from raw data
#' 
#' Input of raw data frame is teased to output the reactions and their rates that contribute to the species production and consumption data. Reaction switch to output full reaction label or just reactants. Absolute or Fraction of total also calculated based on switch.
#' @param dataframe The data frame with raw budget data
#' @param Reactants FALSE => full reaction returned, TRUE => Reactants are returned
#' @param Absolute TRUE => Absolute data returned, FALSE => Fractional contributions to total are returned
#' @return Data frame with species budget data
#' @export

get_species_budget <- function (dataframe, Reactants, Absolute) {
  df = dataframe %>% select(Mechanism, Temperature, NOx.Emissions, Run, Reaction, Rate, H2O2, HNO3) %>%
    mutate(Temperature.C = Temperature - 273) %>%
    arrange(Temperature) %>% 
    rowwise() %>%
    mutate(NOx.Condition = get_NOx_condition(H2O2/HNO3)) %>%
    select(Mechanism, Run, Temperature.C, Reaction, Rate, NOx.Condition) %>%
    group_by(Mechanism, Run, Temperature.C, Reaction, NOx.Condition) %>%
    summarise(Rate = mean(Rate))
  
  if (Reactants == TRUE) {
    df = df %>% rowwise() %>% 
      mutate(Reaction = ifelse( is.na(str_match(Reaction, " = ")[1,1]), Reaction, sapply(str_split(Reaction, " = "), "[", 1)))
  }
  
  if (Absolute == FALSE) {
    df = df %>% filter(Rate > 0) %>%
      group_by(Run, Mechanism, Temperature.C, NOx.Condition) %>%
      mutate(Sum = sum(Rate), Fraction = Rate / Sum) %>%
      select(-Sum, -Rate)
  }
  return(df)
}