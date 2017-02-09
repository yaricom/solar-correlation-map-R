# This is implementation of correlations visualization between dependent and explanatory variables 
# as well as between explanatory variables pairs

# Return angles to the lineary spaced data points on the specified orbit
#
# Args:
#   orbit: the orit number
#   n_data_points: the number of data points to be place on this orbit
# Returns:
#   sequence of angles in radians to lineary spaced data points
makeAngles <- function(orbit, n_data_points) {
  start = (2 * pi) / 11 * (orbit - 1)
  stop = start + (pi / 2)
  angles = seq(start, stop, length.out = n_data_points)
}

# Function to find point's decart coordinates given polar coordinates
#
# Args:
#   angle: the point angle in radians
#   radius: the radius of point's orbit
# Returns:
#   the point's decart coordinates from given polar coordinates
toDecart <- function(angle, radius) {
  y = sin(angle) * radius
  x = cos(angle) * radius
  c(x, y)
}

# Function to plot correlations visualization as solar system based model
#
# Args:
#   data: the data frame with data
#   dv_label: the name of dependent variable (sun)
#   orbits: the number of orbits to depict
plotSolarCorrelation <- function(data, dv_label, orbits = 10) {
  lbls <- names(data)
  # Get dv_label index in array of labels
  center_idx_bool <- lbls == dv_label
  center_idx <- which(center_idx_bool)
  
  all_idx <- !center_idx_bool
  corr_dist <- cor(data, method = "pearson", use = "pairwise")
  positive <- corr_dist[,i] >= 0
  sun_corr_dist <- corr_dist[,center_indx]
  colors <- rainbow(n = length(corr_dist), start = .2, end = .8) # cm.colors(n = length(corr_dist), alpha = 1)
  step <- 1.0 / orbits
  last_orbit <- 0.0
  
  # setup plot
  plot.new()
  symbols(0, 0, circles = 0.05, xlim = c(-1,1), ylim = c(-1,1), bg = colours[center_idx], inches=FALSE)

}