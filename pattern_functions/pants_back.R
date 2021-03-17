#' Basic Pants Back
#' 
#' @description 
#' This function returns the major pattern points of a pants back
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
pants_back <- function(crotch_length = 11,
                        waist = 33,
                        hip = 47,
                        inseam = 32,
                        outseam = 43,
                        leg_opening = 13.5,
                        movement_ease = 0.25,
                        leg_slimming_amt = 0.25,
                        sway_back_adj = 0) {
  
  # PANTS BACK
  
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
  R = K + c((D[1] - K[1])/2, 0)
  
  cb_X = ((R[1] - C[1])/sqrt((R[1] - C[1])^2 + (R[2] - C[2])^2))*(0.75 + large_seat_adj)
  cb_Y = ((R[2] - C[2])/sqrt((R[1] - C[1])^2 + (R[2] - C[2])^2))*(0.75 + large_seat_adj)  
  
  S = R + c(cb_X, cb_Y)
  
  center_back_points = data.frame(point = c("C", "S"),
                                  x = c(C[1], S[1]),
                                  y = c(C[2], S[2]))
  center_back_line = solve(cbind(1, center_back_points$x), center_back_points$y)
  
  F1 = c(((F[2] - center_back_line[1])/center_back_line[2]), F[2])
  
  center_back_points <- data.frame(x = seq(F1[1], S[1], length.out = 10))
  center_back_points$y <- center_back_line[1] + center_back_line[2]*center_back_points$x 
  
  ang_S = acos(abs(S[2] - R[2])/((waist/4) + (dart_width*2)))
  len_SxT = ((waist/4) + (dart_width*2)) * sin(ang_S)
  
  T = c(S[1] - len_SxT, 0)
  U = P - c(0.25 + movement_ease, 0)
  V = E + c(1.25 + large_seat_adj, 0)
  W = V + c(0, 0.25)
  X = W - c(0.5, 0)
  
  ## LEG SLIMMING POINTS
  inseam_points = data.frame(point = c("X", "M"),
                             x = c(X[1], M[1]),
                             y = c(X[2], M[2]))
  inseam_line = solve(cbind(1, inseam_points$x), inseam_points$y)
  
  p3 = c(((knee[2] - inseam_line[1])/inseam_line[2]), knee[2])
  p5 = p3 - c(leg_slimming_amt, 0)
  
  outseam_points = data.frame(point = c("U", "N"),
                              x = c(U[1], N[1]),
                              y = c(U[2], N[2]))
  outseam_line = solve(cbind(1, outseam_points$x), outseam_points$y)
  
  p4 = c(((knee[2] - outseam_line[1])/outseam_line[2]), knee[2])
  p6 = p4 + c(leg_slimming_amt, 0)
  
  W1 = c(W[1], 2*W[2] + L[2])
  U1 = c(U[1], 2*U[2] + L[2])
  
  ##  BACK DARTS
  dart_start_from_cb = (D[1] - Q[1])/3
  
  ### Center Dart
  d1 = c(S[1] - (dart_start_from_cb * sin(ang_S)), S[2] + (dart_start_from_cb * cos(ang_S)))
  d2 = c(d1[1] - (dart_width * sin(ang_S)), d1[2] + (dart_width * cos(ang_S)))
  d1m = c(((d1[2] + (dart_start_from_cb + dart_width) - center_back_line[1])/center_back_line[2]) - (dart_start_from_cb * sin(ang_S)), d1[2] + 4)
  
  ### Side Dart
  d3 = c(d2[1] - (dart_width * sin(ang_S)), d2[2] + (dart_width * cos(ang_S)))
  d4 = c(d3[1] - (dart_width * sin(ang_S)), d3[2] + (dart_width * cos(ang_S)))
  d2m = c(d1m[1] - (2 * dart_width), d3[2] + 3.5)
  
  # Data Assembly
  point_data <- c(I, L, M, N, R, S, T, U, W, X, knee, F1, p5, p6, d1, d2, d1m, d3, d4, d2m, W1, U1)
  points <- data.frame(point = c('I', 'L', 'M', 'N', 'R', 'S', 'T', 'U', 'W', 'X', "knee", "F1", "p5", "p6", 'd1', 'd2', 'd1m', 'd3', 'd4', 'd2m', 'W1', "U1"),
                       x = point_data[c(TRUE, FALSE)],
                       y = point_data[c(FALSE, TRUE)])
  
  ## HIP CURVE
  hip_points <- points %>%
    dplyr::filter(point %in% c("N", "U", "T"))
  hip_curve <- solve(cbind(1, hip_points$y, hip_points$y^2), hip_points$x)
  hip_curve_points <- data.frame(y = seq(U[2], T[2], length.out = 10))
  hip_curve_points$x <- hip_curve[1] + hip_curve[2]*hip_curve_points$y + hip_curve[3]*hip_curve_points$y^2
  
  ## Inseam Curve
  inseam_points <- points %>%
    dplyr::filter(point %in% c("W", "p5", 'W1'))
  inseam_curve <- solve(cbind(1, inseam_points$y, inseam_points$y^2), inseam_points$x)
  inseam_curve_points <- data.frame(y = seq(W[2], p5[2], length.out = 10))
  inseam_curve_points$x <- inseam_curve[1] + inseam_curve[2]*inseam_curve_points$y + inseam_curve[3]*inseam_curve_points$y^2
  
  ## Outseam Curve
  outseam_points <- points %>%
    dplyr::filter(point %in% c("T", "U", 'p6', 'U1'))
  outseam_curve <- solve(cbind(1, outseam_points$y, outseam_points$y^2, outseam_points$y^3), outseam_points$x)
  outseam_curve_points <- data.frame(y = seq(U[2], p6[2], length.out = 10))
  outseam_curve_points$x <- outseam_curve[1] + outseam_curve[2]*outseam_curve_points$y + outseam_curve[3]*outseam_curve_points$y^2 + outseam_curve[4]*outseam_curve_points$y^3
  
  # Pattern Plot
  
  x_min <- floor(min(points$x)) - 1
  x_max <- ceiling(max(points$x)) + 1
  y_min <- floor(min(points$y)) - 1
  y_max <- ceiling(max(points$y)) + 1
  
  pattern <- points %>%
    dplyr::filter(!(point %in% c("W1", "U1", "d1", 'd2', 'd3', 'd4', 'd1m', 'd2m'))) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    xlim(x_max, x_min) +
    ylim(y_max, y_min) + 
    geom_text(aes(label = point)) +
    geom_segment(aes(x = S[1], y = S[2], xend = T[1], yend = T[2])) +
    stat_smooth(data = hip_curve_points,
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "loess",
                se = FALSE)  +
    stat_smooth(data = outseam_curve_points,
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "loess",
                se = FALSE)  +
    stat_smooth(data = inseam_curve_points,
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "loess",
                se = FALSE)  +
    geom_smooth(data = points %>%
                  dplyr::filter(point %in% c("W", "X", "I", "F1", "R", "S")),
                colour = "black",
                size = 0.5,
                method = "loess",
                se = FALSE)  +
    geom_segment(aes(x = p5[1], y = p5[2], xend = M[1], yend = M[2])) +
    geom_segment(aes(x = p6[1], y = p6[2], xend = N[1], yend = N[2])) +
    geom_segment(aes(x = M[1], y = M[2], xend = N[1], yend = N[2])) +
    geom_segment(aes(x = d1[1], y = d1[2], xend = d1m[1], yend = d1m[2])) +
    geom_segment(aes(x = d2[1], y = d2[2], xend = d1m[1], yend = d1m[2])) +
    geom_segment(aes(x = d3[1], y = d3[2], xend = d2m[1], yend = d2m[2])) +
    geom_segment(aes(x = d4[1], y = d4[2], xend = d2m[1], yend = d2m[2])) + 
    cowplot::theme_nothing()
  
  return(list("points" = points,
              "pattern" = pattern))
  
}
