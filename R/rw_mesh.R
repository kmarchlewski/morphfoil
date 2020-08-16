
#' @title Read a file in a msh2 format
#'
#' @description The function reads a .msh2 file and store it in a list.
#'    The function uses \code{\link[base]{scan}} and
#'    \code{\link[utils]{read.table}} functions.
#' @param name A string with a name of the file to read.
#'     The file name is relative to a working directory.
#' @param head_rows A number of rows containing introductory information.
#'     The default value is 4.
#'
#' @return The output list contains: a head of the file (\code{$head}),
#' mesh points (\code{$coord}) and a file tail (\code{$tail}).
#'
#' @export

read_mesh <- function (name, head_rows = 4) {

  if (!is.character(name) | length(name) != 1) {
    stop(" wrong 'name' argument")
  }

  head <- scan(
    file = name, n = head_rows,
    what = "char(0)", sep = "\n",
    blank.lines.skip = FALSE, quiet = TRUE
  )

  tail <- scan(
    file = name, skip = as.numeric(head[4]) + head_rows,
    what = "char(0)", sep = "\n",
    blank.lines.skip = FALSE, quiet = TRUE
  )

  coord <- utils::read.table(
    name,
    skip = head_rows,
    nrows = as.numeric(readLines(name, n = head_rows)[head_rows])
  )

  coord <- as.data.frame(coord)
  colnames(coord) <- c("x", "y")

  return (list(head = head, coord = coord, tail = tail))
}

#' @title Write a file in a msh2 format
#'
#' @description The function writes a .msh2 file.
#'    The function uses \code{\link[base]{write}} and
#'    \code{\link[utils]{write.table}} functions.
#' @param name A string naming a file to write to.
#'    The file name is relative to a working directory.
#' @param mesh A list of three elements: a head of the file (\code{$head}),
#'    mesh points (\code{$coord}) and a file tail (\code{$tail}).
#'
#' @export

write_mesh <- function (name, mesh) {

  write(mesh$head, name, append = FALSE)

  utils::write.table(
    format(mesh$coord, justify = "right", digits = 20, width = 30),
    name,
    append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE
  )

  write(mesh$tail, name, append = TRUE)

}

#' @title Read a file with airfoil coordinates
#'
#' @description The function reads a file in a three-row format
#'    and store it in a data frame.
#'    The function uses \code{\link[utils]{read.table}} function.
#' @param name A string with a name of the file to read.
#'    The file name is relative to a working directory.
#' @param skip_rows A number of rows to skip before reading a table.
#'    The default value is 5.
#' @param ... additional argumets passed to \code{read.table}.
#'
#' @return The output data frame contains: x coordinates of points (\code{$x}),
#'    y coordinates of the lower points (\code{$ylo}) and
#'    y coordinates of the upper points (\code{$yup}).
#'
#' @export

read_airfoil <- function (name, skip_rows = 5, ...) {

  airfoil <- utils::read.table(name, skip = skip_rows, ...)
  airfoil[, 2] = -airfoil[, 2]

  airfoil <- as.data.frame(airfoil)
  colnames(airfoil) <- c("x", "ylo", "yup")

  return (airfoil)
}
