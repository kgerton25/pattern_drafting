#' Basic Bodice Back
#'
#' @description 
#' This function returns the major pattern points of a foundation top 
#' as well as the ggplot object of the pattern to print.
#'
#' @param bust numeric; bust circumference
#' @param shoulder_width  numeric; length between shouldersm across the back
#' @param neck numeric; neck circumference
#' @param first_line_back numeric; width of back from armpit to armpit
#' @param back length numeric; length from shoulder top to waistline
#' @param waist numeric; waist circumference
#' @param shoulder_type character; "normal", "flat" or "sloped" 
#' @param movement_ease numeric; the amount of ease between the body measurement and the garment measurement
#'
#' @return list; a list containing the point set used to draft the pattern and the pattern object
#' @export
#'
#' @examples
basic_bodice_back <- function(bust,
                                 shoulder_width,
                                 neck,
                                 first_line_back,
                                 back_length,
                                 waist,
                                 shoulder_type = "normal",
                                 large_bust_adj = FALSE,
                                 movement_ease = 0.5) {

  
  # Set Shoulder Adjustment
  shoulder_type_adj <- dplyr::case_when(shoulder_type == "normal" ~ 1,
                                        shoulder_type == "sloped" ~ 1.25,
                                        shoulder_type == "flat" ~ 0.75)
  
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
  
  G = c(0, 0.75)
  H = c(B[1], G[2])
  I = c((2/3)*H[1], H[2])
  
  J = c(0, G[2] + ((1/6)*bust + ifelse(large_bust_adj, 1.5, 2)))
  
  K = c((0.25 * bust) + movement_ease, J[2])
  L = c(0, G[2] + back_length)
  M = c(K[1], L[2])
  N = c(0, G[2] + ((J[2] - G[2])/2))
  O = c(first_line_back/2, N[2])
  P = c(O[1], J[2])
  Q = c(O[1], O[2] + ((P[2] - O[2])/3))
  
  ## Armpit Curve
  len_PK = K[1] - P[1]
  len_PQ = P[2] - Q[2]
  len_QK = sqrt((len_PK)^2 + (len_PQ)^2)
  ang_KQP = atan(len_PK/len_PQ)
  len_PR = len_PQ * sin(ang_KQP)
  len_PS = len_PR * (2/3)
  ang_QPR = (pi/2) - ang_KQP
  
  x1R = len_PR*sin(ang_QPR)
  y1R = len_PR*cos(ang_QPR)
  x1S = len_PS*sin(ang_QPR)
  y1S = len_PS*cos(ang_QPR)
  
  R = c(P[1] + x1R, P[2] - y1R)
  S = c(P[1] + x1S, P[2] - y1S)
  
  # Supplemental Points
  ## Armpit
  F1 = c(F[1], Q[2] + (Q[2] - F[2]))
  S1 = c(S[1], Q[2] - (S[2] - Q[2]))
  S2 = c(K[1] + (K[1] - Q[1]), Q[2])
  ## Neck
  J1 = c(-J[1], J[2])
  J2 = c(J[1], -J[2])
  
  # Assemble Data
  point_data <- c(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, F1, S1, S2)
  points <- data.frame(point = c(LETTERS[1:19], 'F1', 'S1', 'S2'),
                       x = point_data[c(TRUE, FALSE)],
                       y = point_data[c(FALSE, TRUE)])
  ## ARMPIT CURVE 1
  AC1_points <- points %>%
    dplyr::filter(point %in% c("F", "Q", "F1"))
  AC1_curve <- solve(cbind(1, AC1_points$y, AC1_points$y^2), AC1_points$x)
  AC1_curve_points <- data.frame(y = seq(F[2], Q[2], length.out = 10))
  AC1_curve_points$x <- AC1_curve[1] + AC1_curve[2]*AC1_curve_points$y + AC1_curve[3]*AC1_curve_points$y^2
  
  ## ARMPIT CURVE 2
  AC2_points <- points %>%
    dplyr::filter(point %in% c("S1", "Q", "S"))
  AC2_curve <- solve(cbind(1, AC2_points$y, AC2_points$y^2), AC2_points$x)
  AC2_curve_points <- data.frame(y = seq(S[2], Q[2], length.out = 10))
  AC2_curve_points$x <- AC2_curve[1] + AC2_curve[2]*AC2_curve_points$y + AC2_curve[3]*AC2_curve_points$y^2
  
  ## ARMPIT CURVE 3
  AC3_points <- points %>%
    dplyr::filter(point %in% c("S", "K", "S2"))
  AC3_curve <- solve(cbind(1, AC3_points$x, AC3_points$x^2), AC3_points$y)
  AC3_curve_points <- data.frame(x = seq(S[1], K[1], length.out = 10))
  AC3_curve_points$y <- AC3_curve[1] + AC3_curve[2]*AC3_curve_points$x + AC3_curve[3]*AC3_curve_points$x^2
  
  # ## Neck Curve 1
  # NC1_points <- points %>%
  #   dplyr::filter(point %in% c("J2", "B", "J"))
  # NC1_curve <- solve(cbind(1, NC1_points$y, NC1_points$y^2), NC1_points$x)
  # NC1_curve_points <- data.frame(y = seq(J[2], B[2], length.out = 10))
  # NC1_curve_points$x <- NC1_curve[1] + NC1_curve[2]*NC1_curve_points$y + NC1_curve[3]*NC1_curve_points$y^2
  # 
  # ## Neck Curve 2
  # NC2_points <- points %>%
  #   dplyr::filter(point %in% c("J1", "G", "J"))
  # NC2_curve <- solve(cbind(1, NC2_points$x, NC2_points$x^2), NC2_points$y)
  # NC2_curve_points <- data.frame(x = seq(J[1], G[1], length.out = 10))
  # NC2_curve_points$y <- NC2_curve[1] + NC2_curve[2]*NC2_curve_points$x + NC2_curve[3]*NC2_curve_points$x^2
  
  # Neck Ellipse
  xc <- I[1] # center x_c or h
  yc <- B[2] # y_c or k
  a <- B[1] - I[1] # major axis length
  b <- I[2] # minor axis length
  
  x <- seq(I[1], B[1], length.out = 25)
  y <- sqrt((1 - ((x - xc)^2/(a^2)))*(b^2) + yc)
  neck_ellipse = data.frame(x = x,
                            y = y)
  
  # PATTERN PLOT
  scale_points <- points %>%
    dplyr::filter(point %in% c('B', 'I', 'G', 'L', 'M', 'J', 'K', 'S', 'Q', 'F'))
  x_min <- floor(min(scale_points$x)) - 1
  x_max <- ceiling(max(scale_points$x)) + 1
  y_min <- floor(min(scale_points$y)) - 1
  y_max <- ceiling(max(scale_points$y)) + 1
  
  pattern <- points %>%
    dplyr::filter(point %in% c('B', 'I', 'G', 'L', 'M', 'J', 'K', 'S', 'Q', 'F')) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    xlim(x_min, x_max) +
    ylim(y_max, y_min) + 
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
    geom_segment(aes(x = G[1], y = G[2], xend = L[1], yend = L[2])) +
    geom_segment(aes(x = G[1], y = G[2], xend = I[1], yend = I[2])) +
    geom_segment(aes(x = B[1], y = B[2], xend = F[1], yend = F[2])) +
    geom_segment(aes(x = M[1], y = M[2], xend = L[1], yend = L[2])) +
    geom_segment(aes(x = K[1], y = K[2], xend = M[1], yend = M[2])) + 
    cowplot::theme_nothing()
  
  
  return(list("points" = points,
              "pattern" = pattern))
}