% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/morph_mesh.R
\name{select_morphp}
\alias{select_morphp}
\title{Select morphing points}
\usage{
select_morphp(airfoil, surf, xcoord)
}
\arguments{
\item{airfoil}{A data frame containing coordinates of airfoil points.}

\item{surf}{A character string indicating upper or lower curve
of the airfoil.}

\item{xcoord}{A vector of x-coordinates of the requested points.}
}
\value{
A list of two elements: a logical vector (\code{$sel}) indicating
   which points are selected and a data frame (\code{$coord}) containing
   coordinates of the selected points.
}
\description{
The function selects the morphing points (which control a mesh morphing)
   from airfoil points.
   Each morphing point is selected such that it is neares to a corresponding
   point from \code{xcoord} vector.
}
\examples{
x_coord <- seq(0.15, 0.9, len = 5)
morph_up <- select_morphp(rae2822_airfoil, "up", x_coord)
morph_lo <- select_morphp(rae2822_airfoil, "lo", x_coord)

}
