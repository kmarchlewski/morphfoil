
#' @title Plot a mesh
#'
#' @description The function plots a mesh in the .msh2 format.
#'    The function uses \code{\link[graphics]{plot}} and
#'    \code{\link[graphics]{lines}} functions.
#'
#' @param mesh A list of elements \code{$head}, \code{$coord} and \code{$tail}.
#' @param airfoil A data frame containing coordinates of airfoil points.
#' @param xl A vector of limits for the x-axis.
#' @param yl A vector of limits for the y-axis.
#'
#' @examples plot_mesh(rae2822_mesh, rae2822_airfoil)
#'
#' @export

plot_mesh <- function (mesh, airfoil, xl = c(0, 1), yl = c(-0.2, 0.2)) {

  graphics::plot(
    mesh$coord,
    xlim = xl, ylim = yl,
    pch = 1, asp = 1, cex = 0.2
  )

  graphics::lines(
    x = c(airfoil[, 1], rev(airfoil[, 1])),
    y = c(airfoil[, 3], rev(airfoil[, 2])),
    col = "red", lwd = 1.5
  )

}

#' @title Plot an airfoil
#'
#' @description The function plots an airfoil.
#'    The function uses \code{\link[graphics]{plot}},
#'    \code{\link[graphics]{lines}} and
#'    \code{\link[graphics]{arrows}} functions.
#'
#' @param airfoil A data frame containing coordinates of airfoil points.
#' @param morph_up A list of upper morphing points.
#' @param morph_lo A list of lower morphing points.
#' @param constr_up A list of upper constraints.
#' @param constr_lo A list of lower constraints.
#' @param air_points A logical value indicating if the airfoil
#'     coordinate points should be plotted.
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
#' plot_airfoil(rae2822_airfoil, morph_up, morph_lo, constr_up, constr_lo)
#'
#' @export

plot_airfoil <- function (airfoil, morph_up, morph_lo,
                          constr_up, constr_lo, air_points = TRUE) {

  graphics::plot(
    c(rep(min(airfoil$x), 2), rep(max(airfoil$x), 2)),
    rep(c(max(airfoil$yup), min(airfoil$ylo)), 2),
    col = "white", xlab = "", ylab = "", asp = 1
  )

  graphics::lines(airfoil$x, airfoil$yup, type = "l", col = "black")
  graphics::lines(airfoil$x, airfoil$ylo, type = "l", col = "black")

  if (air_points) {
    graphics::lines(airfoil$x, airfoil$yup, type = "p", col = "black", pch = 19, cex = 0.3)
    graphics::lines(airfoil$x, airfoil$ylo, type = "p", col = "black", pch = 19, cex = 0.3)
  }

  graphics::lines(morph_up$coord, type = "p", col = "blue", lwd = 1.5)
  graphics::lines(morph_lo$coord, type = "p", col = "orange", lwd = 1.5)

  graphics::lines(constr_up$coord, type = "p", col = "red", lwd = 1.5)
  graphics::lines(constr_lo$coord, type = "p", col = "red", lwd = 1.5)

  for (i in 1:nrow(morph_up$vec)) {
    graphics::arrows(
      morph_up$coord$x[i], morph_up$coord$y[i],
      morph_up$coord$x[i] + 0.1 * morph_up$vec[i, 1],
      morph_up$coord$y[i] + 0.1 * morph_up$vec[i, 2],
      angle = 10, length = 0.15
    )
  }

  for (i in 1:nrow(morph_lo$vec)) {
    graphics::arrows(
      morph_lo$coord$x[i], morph_lo$coord$y[i],
      morph_lo$coord$x[i] + 0.1 * morph_lo$vec[i, 1],
      morph_lo$coord$y[i] + 0.1 * morph_lo$vec[i, 2],
      angle = 10, length = 0.15
    )
  }
}
