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


#' Save to Projector File
#'
#' @param file_name character; the name and path of the file to save the pattern to
#' @param pattern character; the pattern object returned by the pattern function
#'
#' @return
#' @export
#'
#' @examples
save_to_projector_file <- function(file_name, pattern) {
  pdf(file = file_name,   # The directory you want to save the file in
      width = 12, # The width of the plot in inches
      height = 12) # The height of the plot in inches
  
  # Step 2: Create the plot with R code
  pattern$pattern 
  
  # Step 3: Run dev.off() to create the file!
  dev.off()
}


#' Save to Paginated PDF
#'
#' @param pattern 
#'
#' @return
#' @export
#'
#' @examples
save_to_paginated_pdf <- function(file_name, pattern){
  points <- pattern$points
  
  x_min <- floor(min(points$x)) - 5
  x_max <- ceiling(max(points$x)) + 5
  y_min <- floor(min(points$y)) - 5
  y_max <- ceiling(max(points$y)) + 5
  
  # width <- max(points$x) - min(points$x)
  # height <- max(points$y) - min(points$y)
  width = x_max - x_min
  height = y_max - y_min
  paper_x <- 8.5 # 8 printing width
  paper_y <- 11 # 10.5 printing width
  splits_x <- ceiling(width/8)
  splits_y <- ceiling(height/10.5)
  
  ggsave(file = paste0(file_name, ".tiff"), # this will need to be a tmp directory in the app
         plot = pattern,
         width = width,
         height = height,
         units = "in")
  
  pattern_image <- raster::raster(paste0(file_name, ".tiff"))

  h        <- ceiling(ncol(pattern_image)/splits_x)
  v        <- ceiling(nrow(pattern_image)/splits_y)
  agg      <- aggregate(pattern_image,fact=c(h,v))
  agg[]    <- 1:ncell(agg)
  agg_poly <- rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()
  for(i in 1:ncell(agg)){
    e1          <- extent(agg_poly[agg_poly$polis==i,])
    r_list[[i]] <- crop(pattern_image,e1)
  }
  
  render_x <- round(width/splits_x, 4)
  render_y <- round(height/splits_y, 4)
  
}

raster_plot_pattern <- function(raster_plot_chunk){
  plot(raster_plot_chunk, 
       col = c("black", "white"), 
       legend = FALSE,
       xaxt="n", yaxt = "n", # axis ticks
       par(mar = c(0, 0, 0, 0)),  #par(mar = c(bottom, left, top, right)) #plot margins
       legend.mar =  0
       )
}

pdf(paste0(file_name, "pages.pdf"),
    width = render_x,
    height = render_y)
lapply(r_list, function(n) raster_plot_pattern(n))
dev.off()

