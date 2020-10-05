
## Load functions from a package
devtools::load_all()

## Document package and prepare NAMESPACE file
# devtools::document()

## Test package

# Creates tests...
# usethis::use_testthat()

# Run package tests:
# devtools::test()

## Full R CMD check:
# devtools::check()

## Load mesh and airfoil data

mesh_name <- ".tmp/rae2822.msh2"
mesh_mod_name <- ".tmp/rae2822_mod.msh2"
airfoil_name <- ".tmp/rae2822_curve.dat"

mesh <- read_mesh(mesh_name)
airfoil <- read_airfoil(airfoil_name)

## Plot mesh

plot_mesh(mesh, airfoil)

## Select constraint points

constr_up <- select_constr(airfoil, "up", rbind(c(0, 0), c(1, 1)))
constr_lo <- select_constr(airfoil, "lo", rbind(c(0, 0), c(1, 1)))

## Select morphing points

x_coord <- seq(0.15, 0.9, len = 6)

morph_up <- select_morphp(airfoil, "up", x_coord)
morph_lo <- select_morphp(airfoil, "lo", x_coord)

## Plot the airfoil with the morphing points and normals

plot_airfoil(airfoil, morph_up, morph_lo, constr_up, constr_lo)

## Read csv file with parameters

parameters <- as.numeric(read.table(file = ".tmp/parameters.csv", sep = ";"))
N_morphp <- length(parameters) / 2
par_up <- parameters[1:N_morphp]
par_lo <- parameters[(N_morphp+1):length(parameters)]

## Define covariance radii

theta <- c(0.08, 0.08)
theta_constr <- c(0.01, 0.01)

## Create a morphed mesh

mesh_mod <- morph_mesh(
  mesh, airfoil,
  morph_up, morph_lo,
  constr_up, constr_lo,
  par_up, par_lo,
  theta, theta_constr
)

## Create a morphed airfoil

airfoil_mod <- morph_airfoil(
  airfoil,
  morph_up, morph_lo,
  constr_up, constr_lo,
  par_up, par_lo,
  theta, theta_constr
)

## Plot the morphed airfoil
plot_airfoil(
  airfoil, morph_up, morph_lo, constr_up, constr_lo,
  control = F, airfoil_points = T
)
plot_airfoil(
  airfoil_mod, morph_up, morph_lo, constr_up, constr_lo,
  control = F, airfoil_points = T, add = T, col = "blue"
)

## Plot the morphed mesh

plot_mesh(mesh_mod, airfoil_mod)

## Write the morphed mesh to the .msh2 file

# write_mesh(mesh_mod_name, mesh_mod)
