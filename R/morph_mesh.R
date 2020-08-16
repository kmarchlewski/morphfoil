
create_covM <- function (X1, X2, X_constr = NULL, theta = 1, theta_constr = 1) {

  if (is.null(X_constr)) {
    X1 <- t(t(X1) / theta)
    X2 <- t(t(X2) / theta)

    r <- fields::rdist(X1, X2)

    K <- exp(-0.5 * r^2)
  } else {
    X1_sc <- t(t(X1) / theta_constr)
    X2_sc <- t(t(X2) / theta_constr)
    X_constr <- t(t(X_constr) / theta_constr)

    Dist1 <- fields::rdist(X1_sc, X_constr)
    Dist2 <- fields::rdist(X2_sc, X_constr)

    lim1 <- apply(1 - exp(-0.5 * Dist1^2), 1, prod)
    lim2 <- apply(1 - exp(-0.5 * Dist2^2), 1, prod)

    X1 <- t(t(X1) / theta)
    X2 <- t(t(X2) / theta)

    Dist <- fields::rdist(X1, X2)

    K <- exp(-0.5 * Dist^2) * (lim1 %o% lim2)
  }

  return (K)
}

create_normals <- function(X, sel) {

  normals <- matrix(0, sum(sel), 2)
  k <- 1

  for (i in 1:length(sel)) {
    if (sel[i]) {
      n0 <- c(-(X[i, 2] - X[i-1, 2]), X[i, 1] - X[i-1, 1])
      n2 <- c(-(X[i+1, 2] - X[i, 2]), X[i+1, 1] - X[i, 1])
      normals[k, ] <- (n0 + n2) / 2
      k <- k + 1
    }
  }

  n <- apply(normals, 1, function (vec) sqrt(vec[1]^2 + vec[2]^2))
  normals <- normals / cbind(n, n)

  colnames(normals) <- c("x", "y")

  return (normals)
}

#' Select points constraining morphing
#'
#' The function selects points from airfoil points which
#' should not be moved during morphing of a mesh.
#'
#' @param airfoil A data frame containing coordinates of airfoil points.
#' @param surf A character string indicating upper or lower curve
#'    of the airfoil.
#' @param limits A matrix which in each row contains limits within wich
#'    the constraint points should lie.
#'
#' @return A list of two elements: a logical vector (\code{$sel}) indicating
#'    which points are selected and a data frame (\code{$coord}) containing
#'    coordinates of the selected points.
#'
#' @examples
#' constr_up <- select_constr(rae2822_airfoil, "up", rbind(c(0, 0), c(1, 1)))
#' constr_lo <- select_constr(rae2822_airfoil, "lo", rbind(c(0, 0), c(1, 1)))
#'
#' @export

select_constr <- function (airfoil, surf, limits) {

  if ((surf != "up" & surf != "lo") || length(surf) != 1) {
    stop("surf = c(\"up\", \"lo\")")
  } else {
    constr <- rep(FALSE, nrow(airfoil))

    for (i in 1:nrow(limits)) {
      constr <- constr |
        (airfoil$x >= limits[i, 1] & airfoil$x <= limits[i, 2])
    }

    res <- list(
      sel = constr,
      coord = data.frame(
        x = airfoil$x[constr],
        y = airfoil[constr, paste0("y", surf)]
      )
    )

    return(res)
  }
}

#' Select morphing points
#'
#' The function selects the morphing points (which control a mesh morphing)
#'    from airfoil points.
#'    Each morphing point is selected such that it is neares to a corresponding
#'    point from \code{xcoord} vector.
#'
#' @param airfoil A data frame containing coordinates of airfoil points.
#' @param surf A character string indicating upper or lower curve
#'    of the airfoil.
#' @param xcoord A vector of x-coordinates of the requested points.
#'
#' @return A list of two elements: a logical vector (\code{$sel}) indicating
#'    which points are selected and a data frame (\code{$coord}) containing
#'    coordinates of the selected points.
#'
#' @examples
#' x_coord <- seq(0.15, 0.9, len = 5)
#' morph_up <- select_morphp(rae2822_airfoil, "up", x_coord)
#' morph_lo <- select_morphp(rae2822_airfoil, "lo", x_coord)
#'
#' @export

select_morphp <- function (airfoil, surf, xcoord) {

  if ((surf != "up" & surf != "lo") || length(surf) != 1) {
    stop("surf = c(\"up\", \"lo\")")
  } else {
    sel <- rep(FALSE, nrow(airfoil))
    sel[apply(fields::rdist(airfoil$x, xcoord), 2, which.min)] <- TRUE

    normals <- create_normals(
      cbind(airfoil$x, airfoil[, paste0("y", surf)]), sel
    )

    if (surf == "lo") {
      normals <- -normals
    }

    res <- list(
      sel = sel,
      coord = data.frame(
        x = airfoil$x[sel],
        y = airfoil[sel, paste0("y", surf)]
      ),
      vec = normals
    )

    return(res)
  }
}

#' Morph a mesh
#'
#' The function morphs the mesh according to movements of morphing points,
#' locations of constrains and parameters.
#' The morphing technique is based on Gaussian Processes with zero mean.
#'
#' @param mesh A list containing three elements: \code{head}, \code{coord}
#'    and \code{tail}.
#' @param airfoil A data frame having three columns: x coordinates of points,
#'    y coordinates of the upper points and y coordinates of the lower points.
#' @param morph_up A list containing a logical vector indicating selected
#'    upper points and a data frame consisting of coordinates of the points.
#' @param morph_lo A list containing a logical vector indicating selected
#'    lower points and a data frame consisting of coordinates of the points.
#' @param constr_up A list of two elements: a logical vector indicating
#'    which points are selected as upper constraints and a data frame containing
#'    coordinates of the selected points.
#' @param constr_lo A list of two elements: a logical vector indicating
#'    which points are selected as lower constraints and a data frame containing
#'    coordinates of the selected points.
#' @param par_up A vector of displacements in the upper morphing points.
#' @param par_lo A vector of displacements in the lower morphing points.
#' @param theta A vector of covariance radii for the morphing points.
#' @param theta_constr A vector of covariance radii for
#'    the constraining points.
#'
#' @return The output list contains: a head of the file (\code{$head}),
#' modified mesh points (\code{$coord}) and a file tail (\code{$tail}).
#'
#' @source \url{https://arxiv.org/abs/1311.6190}
#'
#' @examples
#' # Select constraint points
#' constr_up <- select_constr(rae2822_airfoil, "up", rbind(c(0, 0), c(1, 1)))
#' constr_lo <- select_constr(rae2822_airfoil, "lo", rbind(c(0, 0), c(1, 1)))
#'
#' # Select morphing points
#' x_coord <- seq(0.15, 0.9, len = 5)
#' morph_up <- select_morphp(rae2822_airfoil, "up", x_coord)
#' morph_lo <- select_morphp(rae2822_airfoil, "lo", x_coord)
#'
#' # Set parameters values
#' par_up <- rep(0.02, len = 5)
#' par_lo <- rep(0, len = 5)
#'
#' # Set covariance radii
#' theta <- c(0.1, 0.1)
#' theta_constr <- c(0.01, 0.01)
#'
#' # Create a modified mesh
#' mesh_mod <- morph_mesh(
#'   rae2822_mesh, rae2822_airfoil,
#'   morph_up, morph_lo, constr_up, constr_lo,
#'   par_up, par_lo, theta, theta_constr
#' )
#'
#' # Plot the modified mesh
#' plot_mesh(mesh_mod, rae2822_airfoil)
#'
#' @export

morph_mesh <- function (
  mesh, airfoil,
  morph_up, morph_lo, constr_up, constr_lo,
  par_up, par_lo, theta, theta_constr) {

  N_airp <- nrow(airfoil)

  ## Select "upper" points and "lower" points of the mesh

  sel_up <- (mesh$coord[, 1] <= airfoil$x[1]) & (mesh$coord[, 2] > 0)

  for (i in 2:N_airp) {
    sel_tmp <-
      (mesh$coord[, 1] > airfoil$x[i-1]) &
      (mesh$coord[, 1] <= airfoil$x[i]) &
      (mesh$coord[, 2] >= min(airfoil$yup[i-1], airfoil$yup[i]))

    sel_up <- sel_up | sel_tmp
  }

  sel_up <- sel_up | (mesh$coord[, 1] > airfoil$x[N_airp]) & (mesh$coord[, 2] > 0)

  sel_lo <- (mesh$coord[, 1] <= airfoil$x[1]) & (mesh$coord[, 2] < 0)

  for (i in 2:N_airp) {
    sel_tmp <-
      (mesh$coord[, 1] > airfoil$x[i-1]) &
      (mesh$coord[, 1] <= airfoil$x[i]) &
      (mesh$coord[, 2] <= max(airfoil$ylo[i-1], airfoil$ylo[i]))

    sel_lo <- sel_lo | sel_tmp
  }

  sel_lo <- sel_lo | (mesh$coord[, 1] > airfoil$x[N_airp]) & (mesh$coord[, 2] < 0)

  ## Create tables of points

  Xup <- morph_up$coord
  Xup_msh <- mesh$coord[sel_up, ]
  Xup_constr <- constr_up$coord

  Xlo <- morph_lo$coord
  Xlo_msh <- mesh$coord[sel_lo, ]
  Xlo_constr <- constr_lo$coord

  ## Create covariance matrixes

  K_up <- create_covM(Xup, Xup, Xup_constr, theta, theta_constr)
  K_lo <- create_covM(Xlo, Xlo, Xlo_constr, theta, theta_constr)

  k_up <- create_covM(Xup_msh, Xup, Xup_constr, theta, theta_constr)
  k_lo <- create_covM(Xlo_msh, Xlo, Xlo_constr, theta, theta_constr)

  ## Calculate displacements in the morphing points

  f_upx <- par_up * morph_up$vec[, 1]
  f_upy <- par_up * morph_up$vec[, 2]
  f_lox <- par_lo * morph_lo$vec[, 1]
  f_loy <- par_lo * morph_lo$vec[, 2]

  ## Calculate mesh points locations after morphing airfoil surface

  F_upx <- k_up %*% ((solve(K_up + diag(1e-8, length(par_up))))%*% f_upx)
  F_lox <- k_lo %*% (solve(K_lo + diag(1e-8, length(par_lo))) %*% f_lox)

  F_upy <- k_up %*% ((solve(K_up + diag(1e-8, length(par_up))))%*% f_upy)
  F_loy <- k_lo %*% (solve(K_lo + diag(1e-8, length(par_lo))) %*% f_loy)

  ## Create a modified mesh

  mesh_mod <- mesh
  mesh_mod$coord[sel_up, 1] <- mesh_mod$coord[sel_up, 1] + F_upx
  mesh_mod$coord[sel_up, 2] <- mesh_mod$coord[sel_up, 2] + F_upy
  mesh_mod$coord[sel_lo, 1] <- mesh_mod$coord[sel_lo, 1] + F_lox
  mesh_mod$coord[sel_lo, 2] <- mesh_mod$coord[sel_lo, 2] + F_loy

  return(mesh_mod)
}
