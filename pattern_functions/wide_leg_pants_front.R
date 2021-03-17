#' Wide Leg Pants Front
#' 
#' @description 
#' This function returns the major pattern points of a wide leg pants front
#' as well as the ggplot object of the pattern to print.
#'
#' @param crotch_length numeric; the difference between the inseam and outseam measurement
#' @param waist numeric; waist circumference
#' @param hip numeric; hip circumference
#' @param inseam numeric; inseam length
#' @param outseam numeric; outseam length
#' @param leg_opening numeric; foot circumference around heel and top of pointed foot
#' @param movement_ease numeric; the amount of ease between the body measurement and the garment measurement
#' @param sway_back_adj numeric; amount to adjust the pattern to account for a sway back. If you have a sway back, set this value to 0.25 - 0.75
#'
#' @return list; a list containing the point set used to draft the pattern and the pattern object
#' @export
#'
#' @examples
wide_leg_pants_front <- function(crotch_length,
                        waist,
                        hip,
                        inseam,
                        outseam,
                        leg_opening,
                        movement_ease = 0.25,
                        leg_slimming_amt = 0.25,
                        sway_back_adj = 0) {
  
  # PANTS FRONT
  
  A = c(0, 0)
  B = c(0, crotch_length)
  C = c((hip/4) + movement_ease, crotch_length)
  D = c((hip/4) + movement_ease, 0)
  E = C + c(max(D[1]/4, 2), 0)
  F = D + c(0, C[2]*(2/3))
  
  ## Crotch Curve
  len_CE = E[1] - C[1]
  len_CF = C[2]*(1/3)
  len_EF = sqrt((len_CE)^2 + (C[2]*(1/3))^2)
  ang_CEF = atan(len_CF/len_CE)
  len_CG = len_CE * sin(ang_CEF)
  len_CH = len_CG*(2/3)
  len_CI = len_CG*(1/3)
  ang_c1 = acos(len_CG/len_CF)
  x1G = len_CG*sin(ang_c1)
  y1G = len_CG*cos(ang_c1)
  x1H = len_CH*sin(ang_c1)
  y1H = len_CH*cos(ang_c1)
  x1I = len_CI*sin(ang_c1)
  y1I = len_CI*cos(ang_c1) 
  
  G = c(C[1] + x1G, C[2] - y1G)
  H = c(C[1] + x1H, C[2] - y1H)
  I = c(C[1] + x1I, C[2] - y1I)
  
  ## LEG
  J = B + c(E[1]/2, 0)
  K = c(J[1], 0)
  L = c(K[1], outseam)
  M = L + c(leg_opening/4, 0)
  N = L - c(leg_opening/4, 0)
  O = E - c(0.5, 0)
  P = A + c(0, C[2]*(2/3))
  Q = D - c((waist/4) + (dart_width*2), 0)
  p1 = J + c(0, (L[2] - J[2])/2)
  knee = p1 - c(0, 2)
  
  ##  FRONT DARTS
  d1 = c(knee[1] + dart_width/2, 0)
  d2 = d1 - c(dart_width, 0)
  d1m = d1 - c(dart_width/2, -4)
  d3 = d2 - c(dart_width, 0)
  d4 = d3 - c(dart_width, 0)
  d2m = d3 - c(dart_width/2, -3.5)
  
  ## Supplemental Points
  Q1 = c(Q[1], 2*P[2])
  E1 = c(E[1], E[2] - 2*(E[2] - H[2]))
  H1 = c(H[1], H[2] - 2*(H[2] - F[2]))
  
  # Rename M and N for Wide Leg
  M = c(E[1], L[2])
  N = c(P[1], L[2])
  
  
  # Assemble Data
  point_data <- c(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Q1, E1, H1, p1, knee, d1, d2, d3, d4, d1m, d2m)
  points <- data.frame(point = c(LETTERS[1:17], "Q1", "E1", "H1", "p1", "knee", 'd1', 'd2', 'd3', 'd4', 'd1m', 'd2m'),
                       x = point_data[c(TRUE, FALSE)],
                       y = point_data[c(FALSE, TRUE)])
  
  ## HIP CURVE
  hip_points <- points %>%
    dplyr::filter(point %in% c("Q", "P", "Q1"))
  hip_curve <- solve(cbind(1, hip_points$y, hip_points$y^2), hip_points$x)
  hip_curve_points <- data.frame(y = seq(Q[2], P[2], length.out = 10))
  hip_curve_points$x <- hip_curve[1] + hip_curve[2]*hip_curve_points$y + hip_curve[3]*hip_curve_points$y^2
  
  ## Inseam Curve
  inseam_points <- points %>%
    dplyr::filter(point %in% c("E", "p5", 'W1'))
  inseam_curve <- solve(cbind(1, inseam_points$y, inseam_points$y^2), inseam_points$x)
  inseam_curve_points <- data.frame(y = seq(E[2], p5[2], length.out = 10))
  inseam_curve_points$x <- inseam_curve[1] + inseam_curve[2]*inseam_curve_points$y + inseam_curve[3]*inseam_curve_points$y^2
  
  
  ## CROTCH CURVE 1
  
  CC1_points <- points %>%
    dplyr::filter(point %in% c("E", "H", "E1"))
  CC1_curve <- solve(cbind(1, CC1_points$y, CC1_points$y^2), CC1_points$x)
  CC1_curve_points <- data.frame(y = seq(E[2], H[2], length.out = 10))
  CC1_curve_points$x <- CC1_curve[1] + CC1_curve[2]*CC1_curve_points$y + CC1_curve[3]*CC1_curve_points$y^2
  
  ## CROTCH CURVE 2
  
  CC2_points <- points %>%
    dplyr::filter(point %in% c("H", "F", "H1"))
  CC2_curve <- solve(cbind(1, CC2_points$y, CC2_points$y^2), CC2_points$x)
  CC2_curve_points <- data.frame(y = seq(H[2], F[2], length.out = 10))
  CC2_curve_points$x <- CC2_curve[1] + CC2_curve[2]*CC2_curve_points$y + CC2_curve[3]*CC2_curve_points$y^2
  
  # Pattern Plot
  
  x_min <- floor(min(points$x)) - 1
  x_max <- ceiling(max(points$x)) + 1
  y_min <- floor(min(points$y)) - 1
  y_max <- ceiling(max(points$y)) + 1
  
  pattern <- points %>%
    dplyr::filter(point %in% c('D', 'Q', 'P', 'knee', 'p6', 'N', 'L', 'M', 'p5', 'O', 'E', 'H', 'F')) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    xlim(x_max, x_min) +
    ylim(y_max, y_min) + 
    geom_text(aes(label = point)) +
    geom_segment(aes(x = D[1], y = D[2], xend = Q[1], yend = Q[2])) +
    geom_segment(aes(x = D[1], y = D[2], xend = F[1], yend = F[2])) +
    stat_smooth(data = hip_curve_points, 
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "loess", 
                se = FALSE)  +
    stat_smooth(data = CC1_curve_points, 
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "loess", 
                se = FALSE)  +
    stat_smooth(data = CC2_curve_points, 
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "loess", 
                se = FALSE) + 
    geom_segment(aes(x = E[1], y = E[2], xend = M[1], yend = M[2])) +
    geom_segment(aes(x = P[1], y = P[2], xend = N[1], yend = N[2])) +
    geom_segment(aes(x = M[1], y = M[2], xend = N[1], yend = N[2])) +
    geom_segment(aes(x = d1[1], y = d1[2], xend = d1m[1], yend = d1m[2])) +
    geom_segment(aes(x = d2[1], y = d2[2], xend = d1m[1], yend = d1m[2])) +
    geom_segment(aes(x = d3[1], y = d3[2], xend = d2m[1], yend = d2m[2])) +
    geom_segment(aes(x = d4[1], y = d4[2], xend = d2m[1], yend = d2m[2])) + 
    cowplot::theme_nothing()
  
  return(list("points" = points,
              "pattern" = pattern))
  
}
