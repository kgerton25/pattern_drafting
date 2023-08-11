#' Foundation Top Front
#'
#' @description 
#' This function returns the major pattern points of a foundation top 
#' as well as the ggplot object of the pattern to print.
#'
#' @param bust numeric; bust circumference
#' @param shoulder_width  numeric; length between shouldersm across the back
#' @param neck numeric; neck circumference
#' @param first_line_front numeric; width of front chest from armpit to armpit
#' @param bp_to_bp numeric; length between bust points
#' @param shoulder_to_bp numeric; length from shoulder top to bust point
#' @param shoulder_to_waist numeric; length from shoulder top to waistline over the bust
#' @param waist numeric; waist circumference
#' @param shoulder_type character; "normal", "flat" or "sloped" 
#' @param large_bust_adj logical; add extra movement room for a full chest
#' @param movement_ease numeric; the amount of ease between the body measurement and the garment measurement
#'
#' @return list; a list containing the point set used to draft the pattern and the pattern object
#' @export
#'
#' @examples
foundation_top_front <- function(bust,
                                 shoulder_width,
                                 neck,
                                 first_line_front,
                                 bp_to_bp,
                                 shoulder_to_bp,
                                 shoulder_to_waist,
                                 waist,
                                 shoulder_type = "normal",
                                 large_bust_adj = FALSE,
                                 movement_ease = 0.5) {
  
  # Set Shoulder Adjustment
  shoulder_type_adj <- calc_shoulder_type_adj(shoulder_type)
  
  # Points
  A = c(0, 0)
  B = c(ifelse(large_bust_adj == TRUE, (neck/6) + 0.25, (bust/12) - 0.25), 0)
  C = B + c(2.75, 0)
  D = c(C[1], shoulder_type_adj)
  E = c(shoulder_width/2, 0)
  
  shoulder_points <- data.frame(point = c('B', 'D'),
                                x = c(B[1], D[1]),
                                y = c(B[2], D[2]))
  shoulder_line <- solve(cbind(1, shoulder_points$x), shoulder_points$y)
  
  F = c(E[1], shoulder_line[1] + E[1]*shoulder_line[2])
  
  G = c(0, B[1] + 0.25)
  H = c(B[1], G[2])
  
  len_GH = H[1]
  len_BH = H[2]
  ang_BGH = atan(len_BH/len_GH)
  len_HI = len_GH * sin(ang_BGH)
  ang_IHG = (pi/2) - ang_BGH
  Ix = B[1] - (cos(ang_IHG) * len_HI)
  Iy = G[2] - (sin(ang_IHG) * len_HI)
  
  I = c(Ix, Iy)
  
  Jx = B[1] - (cos(ang_IHG) * (2/3) * len_HI)
  Jy = G[2] - (sin(ang_IHG) * (2/3) * len_HI)
  
  J = c(Jx, Jy)
  
  K = c(0, ((1/6)*bust + ifelse(large_bust_adj, 1.5, 2)))
  L = c((0.25 * bust) + movement_ease, K[2])
  M = c(0, shoulder_to_waist)
  N = c(L[1], M[2])
  O = c(0, K[2]/2)
  P = c(first_line_front/2, O[2])
  Q = c(P[1], K[2])
  R = c(P[1], P[2] + ((Q[2] - P[2]))/3)
  S = c(R[1], R[2] + ((Q[2] - P[2]))/3)
  
  ## Armpit Curve
  len_QL = L[1] - Q[1]
  len_RQ = Q[2] - R[2]
  len_RL = sqrt((len_QL)^2 + (len_RQ)^2)
  ang_LRQ = atan(len_QL/len_RQ)
  len_QT = len_RQ * sin(ang_LRQ)
  len_QU = len_QT * (2/3)
  ang_RQU = (pi/2) - ang_LRQ
  
  x1T = len_QT*sin(ang_RQU)
  y1T = len_QT*cos(ang_RQU)
  x1U = len_QU*sin(ang_RQU)
  y1U = len_QU*cos(ang_RQU)
  
  T = c(Q[1] + x1T, Q[2] - y1T)
  U = c(Q[1] + x1U, Q[2] - y1U)
  
  V = c(bp_to_bp/2, K[2])
  X = c(V[1], shoulder_to_bp)
  
  # Supplemental Points
  ## Armpit
  F1 = c(F[1], R[2] + (R[2] - F[2]))
  U1 = c(U[1], R[2] - (U[2] - R[2]))
  U2 = c(L[1] + (L[1] - U[1]), U[2])
  
  ## Waist Dart
  Dw = c((waist/4) + 2, M[2])
  Ew = c(X[1], M[2])
  Fw = c(Ew[1] - 1, Ew[2])
  Gw = c(Ew[1] + 1, Ew[2])
  Hw = c(X[1], X[2] + 1.5)
  
  
  # Assemble Data
  point_data <- c(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, X, F1, U1, U2, Dw, Ew, Fw, Gw, Hw)
  points <- data.frame(point = c(LETTERS[1:22], 'X', 'F1', 'U1', 'U2', 'Dw', 'Ew', 'Fw', 'Gw', 'Hw'),
                       x = point_data[c(TRUE, FALSE)],
                       y = point_data[c(FALSE, TRUE)])
  ## ARMPIT CURVE 1
  AC1_points <- points %>%
    dplyr::filter(point %in% c("F", "R", "F1"))
  AC1_curve <- solve(cbind(1, AC1_points$y, AC1_points$y^2), AC1_points$x)
  AC1_curve_points <- data.frame(y = seq(F[2], R[2], length.out = 10))
  AC1_curve_points$x <- AC1_curve[1] + AC1_curve[2]*AC1_curve_points$y + AC1_curve[3]*AC1_curve_points$y^2
  
  ## ARMPIT CURVE 2
  AC2_points <- points %>%
    dplyr::filter(point %in% c("U1", "R", "U"))
  AC2_curve <- solve(cbind(1, AC2_points$y, AC2_points$y^2), AC2_points$x)
  AC2_curve_points <- data.frame(y = seq(U[2], R[2], length.out = 10))
  AC2_curve_points$x <- AC2_curve[1] + AC2_curve[2]*AC2_curve_points$y + AC2_curve[3]*AC2_curve_points$y^2
  
  ## ARMPIT CURVE 3
  AC3_points <- points %>%
    dplyr::filter(point %in% c("U", "L", "U2"))
  AC3_curve <- solve(cbind(1, AC3_points$x, AC3_points$x^2), AC3_points$y)
  AC3_curve_points <- data.frame(x = seq(U[1], L[1], length.out = 10))
  AC3_curve_points$y <- AC3_curve[1] + AC3_curve[2]*AC3_curve_points$x + AC3_curve[3]*AC3_curve_points$x^2
  
  
  # Neck Ellipse
  xc <- A[1] # center x_c or h
  yc <- A[2] # y_c or k
  a <- B[1] # major axis length
  b <- G[2] # minor axis length
  
  x <- seq(G[1], B[1], length.out = 25)
  y <- sqrt((1 - ((x - xc)^2/(a^2)))*(b^2) + yc)
  neck_ellipse = data.frame(x = x, 
                            y = y)
  
  # PATTERN PLOT
  scale_points <- points %>%
    dplyr::filter(point %in% c('B', 'G', 'K', 'M', 'L', 'U', 'R', 'F', 'X', 'Dw')) 
  
  x_min <- floor(min(scale_points$x)) - 5
  x_max <- ceiling(max(scale_points$x)) + 5
  y_min <- floor(min(scale_points$y)) - 5
  y_max <- ceiling(max(scale_points$y)) + 5
  
  pattern <- points %>%
    dplyr::filter(point %in% c('B', 'G', 'K', 'M', 'L', 'U', 'R', 'F', 'X', 'Dw')) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    xlim(x_min, x_max) +
    ylim(y_max, y_min) + 
    coord_equal() + 
    geom_text(aes(label = point)) +
    stat_smooth(data = AC1_curve_points, 
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "loess", 
                se = FALSE) +
    stat_smooth(data = AC2_curve_points, 
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "loess", 
                se = FALSE) +
    stat_smooth(data = AC3_curve_points,
                aes(x = x, y = y),
                colour = "black",
                size = 0.5,
                method = "loess",
                se = FALSE) +
    geom_line(data = neck_ellipse, 
              aes(x = x, y = y),
              colour = "black",
              size = 0.1) +
    geom_segment(aes(x = G[1], y = G[2], xend = M[1], yend = M[2])) +
    geom_segment(aes(x = B[1], y = B[2], xend = F[1], yend = F[2])) +
    geom_segment(aes(x = M[1], y = M[2], xend = Dw[1], yend = Dw[2])) +
    geom_segment(aes(x = Dw[1], y = Dw[2], xend = L[1], yend = L[2])) + 
    geom_segment(aes(x = Fw[1], y = Fw[2], xend = Hw[1], yend = Hw[2])) + 
    geom_segment(aes(x = Gw[1], y = Gw[2], xend = Hw[1], yend = Hw[2])) +  
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