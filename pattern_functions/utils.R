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
calc_dart_width <- function(waist,
                       hip) {
  # Determine Dart Width
  dart_width <- dplyr::case_when(hip - waist >= 11 ~ 1,
                                 hip - waist >= 9 ~ 0.75,
                                 hip - waist >= 7 ~ 0.5,
                                 hip - waist < 7 ~ 0.25)
  return(dart_width)
}

#' Shoulder Type Adjustment
#' 
#' @description 
#' Helper function to sdjustment for differing shoulder shapes
#' 
#' @inheritParams basic_bodice_front
#'
#' @return
#' @export
#'
#' @examples
calc_shoulder_type_adj <- function(shoulder_type) {
  # Set Shoulder Adjustment
  shoulder_type_adj <- dplyr::case_when(shoulder_type == "normal" ~ 1,
                                        shoulder_type == "sloped" ~ 1.25,
                                        shoulder_type == "flat" ~ 0.75)
  return(shoulder_type_adj)
}