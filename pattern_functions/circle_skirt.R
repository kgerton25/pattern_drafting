#' Circle Skirt
#' 
#' @description 
#' This function returns the major pattern points of a circle skirt 
#' as well as the ggplot object of the pattern to print.
#'
#' @param waist numeric; waist circumference
#' @param skirt_length numeric; desired skirt length, from waist
#' @param n_circles numeric; typically 1 or 2 to make a single or double circle skirt
#'
#' @return list; a list containing the point set used to draft the pattern and the pattern object
#' @export
#'
#' @examples
circle_skirt <- function(waist,
                         skirt_length,
                         n_circles = 1) {
  
  A = c(0, 0)
  B = c(0, waist/(2*pi*n_circles))
  C = c(waist/(2*pi*n_circles), 0)
  F = c(0, skirt_length) + B
  G = c(skirt_length, 0) + C
  
  point_data <- c(A, B, C, F, G)
  points <- data.frame(point = c("A", "B", "C", "F", "G"),
                       x = point_data[c(TRUE, FALSE)],
                       y = point_data[c(FALSE, TRUE)])
  # Pattern Plot
  x_min <- floor(min(points$x)) - 5
  x_max <- ceiling(max(points$x)) + 5
  y_min <- floor(min(points$y)) - 5
  y_max <- ceiling(max(points$y)) + 5
  
  arcs <- data.frame(
    start = -3*pi/2,
    end = -pi,
    r = c(C[1], G[1])
  )
  
  pattern <- points %>%
    ggplot(aes(x = x, y = y)) + 
    xlim(x_min, x_max) +
    ylim(y_max, y_min) +
    geom_point() + 
    # geom_text(aes(label = point)) +
    ggforce::geom_arc(data = arcs,
             mapping = aes(x0 = A[1], y0 = A[2],
                           r = r, 
                           start = start, 
                           end = end),
             inherit.aes = FALSE,
             n = 90) +
    geom_segment(aes(x = B[1], y = B[2], xend = F[1], yend = F[2])) + 
    geom_segment(aes(x = C[1], y = C[2], xend = G[1], yend = G[2])) +  
    # Calibration Grid
    geom_segment(aes(x = -4, y = 0, xend = 0, yend = 0)) + 
    geom_segment(aes(x = -4, y = -1, xend = 0, yend = -1)) +
    geom_segment(aes(x = -4, y = -2, xend = 0, yend = -2)) +
    geom_segment(aes(x = -4, y = -3, xend = 0, yend = -3)) +
    geom_segment(aes(x = -4, y = -4, xend = 0, yend = -4)) +
    geom_segment(aes(y = -4, x = 0, yend = 0, xend = 0)) + 
    geom_segment(aes(y = -4, x = -1, yend = 0, xend = -1)) +
    geom_segment(aes(y = -4, x = -2, yend = 0, xend = -2)) +
    geom_segment(aes(y = -4, x = -3, yend = 0, xend = -3)) +
    geom_segment(aes(y = -4, x = -4, yend = 0, xend = -4)) +
    cowplot::theme_nothing()
  
  
  return(list("points" = points,
              "pattern" = pattern))
}