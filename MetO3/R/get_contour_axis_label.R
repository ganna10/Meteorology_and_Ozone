#' Axis Labels for Contour Plots
#' 
#' Axis labels from original data to be used for labelling plot axes
#' @param break.points Sequence at which the axis will be labelled
#' @param orig.data Original data frame containing the contour data for plotting
#' @param digits Number of digits to round the label to.
#' @return Sequence of labels for axis labelling

get_contour_axis_label = function (break.points, orig.data, digits) {
  labels = lapply(break.points, function (i) round ((i * (max(orig.data) - min(orig.data))) + min(orig.data), digits ))
  return (labels)
}