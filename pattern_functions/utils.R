#' Determine Dart Width
#' 
#' @description 
#' Helper function to calculate proper dart width from waist-hip ratio
#' 
#' @inheritParams straight_skirt
#' 
#' @return
#' @export
#'
#' @examples
dart_width <- function(waist,
                       hip) {
  # Determine Dart Width
  dart_width <- dplyr::case_when(hip - waist >= 11 ~ 1,
                                 hip - waist >= 9 ~ 0.75,
                                 hip - waist >= 7 ~ 0.5,
                                 hip - waist < 7 ~ 0.25)
  return(dart_width)
}