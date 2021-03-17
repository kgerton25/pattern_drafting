#' Straight Skirt
#' 
#' @description 
#' This function returns the major pattern points of a straight skirt 
#' as well as the ggplot object of the pattern to print.
#'
#' @param waist numeric; waist circumference
#' @param hip numeric; hip circumference
#' @param hip_point numeric; vertical length between waist and hip
#' @param skirt_length numeric; desired skirt length, from waist
#' @param movement_ease numeric; the amount of ease between the body measurement and the garment measurement
#' @param sway_back_adj numeric; amount to adjust the pattern to account for a sway back. If you have a sway back, set this value to 0.25 - 0.75
#'
#' @return list; a list containing the point set used to draft the pattern and the pattern object
#' @export
#'
#' @examples
straight_skirt <- function(waist,
                           hip,
                           hip_point,
                           skirt_length,
                           movement_ease = 0.25,
                           sway_back_adj = 0) {
  
  # Determine Dart Width
  dart_width <- dplyr::case_when(hip - waist >= 11 ~ 1,
                                 hip - waist >= 9 ~ 0.75,
                                 hip - waist >= 7 ~ 0.5,
                                 hip - waist < 7 ~ 0.25)
  
  # Create Major Pattern Points
  ## Skirt Body
  A = c(0, 0)
  B = c(0, skirt_length)
  C = c(0.5*hip + movement_ease, 0)
  D = C + B
  E = C/2
  F = c(D[1]/2, D[2])
  G = c(0, hip_point)
  H = C + G
  I = E + G
  J = c(0.25 * waist + 2, 0)
  K = C - J
  ## Front Darts
  L = A + c(0, 0.5)
  M = L + c(3, -0.01)
  N = M + c(dart_width, -0.01)
  O = N + c(dart_width, -0.01)
  P = O + c(dart_width, -0.01)
  Q = M + c(dart_width/2, 4)
  R = O + c(dart_width/2, 3.5)
  ## Back Darts & Kick Pleat
  S = C + c(0, 0.75 + sway_back_adj)
  T = S + c(4, 0)
  U = D + c(4, 0)
  V = D - c(0, 7)
  W = S - c(3, 0.1*(0.75 + sway_back_adj))
  X = W - c(dart_width, 0.1*(0.75 + sway_back_adj))
  Y = X - c(dart_width, 0.1*(0.75 + sway_back_adj))
  Z = Y - c(dart_width, 0.1*(0.75 + sway_back_adj))
  A1 = X + c(0.5, 7)
  B1 = Z + c(0.5, 6.5)
  back_hip = K + c(0, 2*I[2])
  front_hip = J + c(0, 2*I[2])
  
  # Assemble Pattern Points
  point_data <- c(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1, back_hip, front_hip)
  points <- data.frame(point = c(LETTERS, "A1", "B1", "BH", "FH"),
                       x = point_data[c(TRUE, FALSE)],
                       y = point_data[c(FALSE, TRUE)])
  
  # Fit Hip Curves
  ## Back
  hip_back_points <- points %>%
    dplyr::filter(point %in% c("K", "I", "BH"))
  hip_back_curve <- solve(cbind(1, hip_back_points$y, hip_back_points$y^2), hip_back_points$x)
  hip_back_curve_points <- data.frame(y = seq(K[2], I[2], length.out = 10))
  hip_back_curve_points$x <- hip_back_curve[1] + hip_back_curve[2]*hip_back_curve_points$y + hip_back_curve[3]*hip_back_curve_points$y^2
  ## Front
  hip_front_points <- points %>%
    dplyr::filter(point %in% c("J", "I", "FH"))
  hip_front_curve <- solve(cbind(1, hip_front_points$y, hip_front_points$y^2), hip_front_points$x)
  hip_front_curve_points <- data.frame(y = seq(J[2], I[2], length.out = 10))
  hip_front_curve_points$x <- hip_front_curve[1] + hip_front_curve[2]*hip_front_curve_points$y + hip_front_curve[3]*hip_front_curve_points$y^2
  
  # Plot Pattern
  x_min <- floor(min(points$x)) - 1
  x_max <- ceiling(max(points$x)) + 1
  y_min <- floor(min(points$y)) - 1
  y_max <- ceiling(max(points$y)) + 1
  
  pattern <- points %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    xlim(x_max, x_min) +
    ylim(y_max, y_min) + 
    # geom_text(aes(label = point)) +
    # Back Waist
    geom_segment(aes(x = T[1], y = T[2], xend = S[1], yend = S[2])) +
    stat_smooth(data = points %>%
                  dplyr::filter(point %in% c("S", "W", "X", "Y", "Z", "K")), 
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "lm", 
                formula = y ~ poly(x, 3),
                se = FALSE) +
    # Front Waist
    stat_smooth(data = points %>%
                  dplyr::filter(point %in% c("L", "M", "N", "O", "P", "J")), 
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "lm", 
                formula = y ~ poly(x, 3),
                se = FALSE) + 
    # Front Darts
    geom_segment(aes(x = M[1], y = M[2], xend = Q[1], yend = Q[2])) + 
    geom_segment(aes(x = N[1], y = N[2], xend = Q[1], yend = Q[2])) + 
    geom_segment(aes(x = O[1], y = O[2], xend = R[1], yend = R[2])) + 
    geom_segment(aes(x = P[1], y = P[2], xend = R[1], yend = R[2])) + 
    # Back Darts
    geom_segment(aes(x = W[1], y = W[2], xend = A1[1], yend = A1[2])) + 
    geom_segment(aes(x = X[1], y = X[2], xend = A1[1], yend = A1[2])) + 
    geom_segment(aes(x = Y[1], y = Y[2], xend = B1[1], yend = B1[2])) + 
    geom_segment(aes(x = Z[1], y = Z[2], xend = B1[1], yend = B1[2])) + 
    # Hip Line
    geom_segment(aes(x = F[1], y = F[2], xend = I[1], yend = I[2])) +
    stat_smooth(data = hip_front_curve_points, 
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "loess", 
                se = FALSE)  +
    stat_smooth(data = hip_back_curve_points, 
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "loess", 
                se = FALSE)  +
    # Final Lines
    geom_segment(aes(x = T[1], y = T[2], xend = U[1], yend = U[2])) + 
    geom_segment(aes(x = U[1], y = U[2], xend = B[1], yend = B[2])) + 
    geom_segment(aes(x = B[1], y = B[2], xend = L[1], yend = L[2])) + 
    geom_segment(aes(x = D[1], y = D[2], xend = S[1], yend = S[2])) + 
    # Kick Pleat
    geom_rect(aes(xmin = D[1], xmax = U[1], ymin = V[2], ymax = D[2]),
              alpha = 0.25) + 
    cowplot::theme_nothing()
  
  return(list("points" = points,
              "pattern" = skirt_pattern))
  
}
