
#' Coordinates of the RAE 2822 airfoil
#'
#' The RAE 2822 airfoil coordinates are stored in a three-column format.
#' A length of the airfoil chord is normalized to one.
#'
#' @format A data frame with 65 rows and three variables.
#' \describe{
#'   \item{x}{x coordinates of points}
#'   \item{yup}{y coordinates of the upper points}
#'   \item{ylo}{y coordinates of the lower points}
#' }
#'
#' @source \url{https://www.grc.nasa.gov/WWW/wind/valid/raetaf/geom.txt}

"rae2822_airfoil"

#' Mesh in the .msh2 format for the RAE 2822 airfoil
#'
#' The mesh is unstructured an is suitable for a \code{xred} solver
#' developed at MEiL PW.
#'
#' @format A list with three elements:
#' \describe{
#'   \item{head}{A character string containing basic information about a file}
#'   \item{coord}{A data frame containing points coordinates}
#'   \item{tail}{A character string describing volumes and boundaries}
#' }

"rae2822_mesh"
