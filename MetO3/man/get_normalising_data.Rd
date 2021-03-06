% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_normalising_data.R
\name{get_normalising_data}
\alias{get_normalising_data}
\title{Get data for which to normalise the budget reaction rates. Using Total loss rate of emitted VOC}
\usage{
get_normalising_data(df)
}
\arguments{
\item{df}{The df containing all the extracted normalising factors}
}
\value{
total.loss.initial Data frame with total voc loss rates grouped by Mechanism, Run, NOx.Condition and Temperature
}
\description{
Data of normalising factors for budget reaction rates
}

