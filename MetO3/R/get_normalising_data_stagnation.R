#' Get data for which to normalise the budget reaction rates. Using Total loss rate of emitted VOC
#' 
#' Data of normalising factors for budget reaction rates
#' @param df The df containing all the extracted normalising factors
#' @return total.loss.initial Data frame with total voc loss rates grouped by Mechanism, Run, NOx.Condition and Temperature
#' @export

get_normalising_data_stagnation <- function (df) {
  total.loss.initial <- df %>%
    mutate(Temperature.C = Temperature - 273) %>%
    select(Mechanism, Run, Temperature.C, NOx.Condition, VOC.initial.losses) %>%
    group_by(Mechanism, Temperature.C, NOx.Condition, Run) %>%
    summarise(VOC.initial.losses = mean(VOC.initial.losses))
  return(total.loss.initial)
}