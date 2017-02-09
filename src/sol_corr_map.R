# This is implementation of correlations visualization between dependent and explanatory variables 
# as well as between explanatory variables pairs

# Return angles to the lineary spaced data points on the specified orbit
#
# Args:
#   orbit: the orit number
#   n_data_points: the number of data points to be place on this orbit
# Returns:
#   sequence of angles in radians to lineary spaced data points
findAngles <- function(orbit, n_data_points) {
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
polarToDecart <- function(angle, radius) {
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
  positive <- corr_dist[,center_idx] >= 0
  corr_dist <- abs(corr_dist) # now we need only magnitude
  sun_corr_dist <- corr_dist[,center_idx]
  colors <- rainbow(n = length(sun_corr_dist), start = .2, end = .8) # cm.colors(n = length(corr_dist), alpha = 1)
  step <- 1.0 / orbits
  last_orbit <- 0.0
  
  # setup plot
  plim = c(-orbits, orbits)
  plot.new()
  plot.window(xlim = plim, ylim = plim, asp = 1)
  r <- 0.03 * orbits
  rp <- r * 0.8
  symbols(0, 0, circles = r, xlim = plim, ylim = plim, bg = colors[center_idx], add = TRUE, inches = FALSE)
  text(r, r, lbls[center_idx], adj = c(0.1, 0), cex = 0.8, col = "gray")
  for (orbit in 1:(orbits + 1)) {
    new_orbit <- step * orbit + 0.1
    idx <- (sun_corr_dist >= (1 - last_orbit)) & (sun_corr_dist > (1 - new_orbit)) & all_idx
    idx_int <- which(idx)
    
    # collect correlation distances to the data points on orbit
    corr_dists <- double(length = length(idx_int))
    i <- 1
    for (index in idx_int) {
      corr_dists[i] <- corr_dist[index, center_idx]
      i <- i + 1
    }
    
    planets <- sum(idx) # the number of planets on orbit
    angles <- findAngles(orbit, planets)
    
    # Draw orbit labels
    text(0, orbit - 0.1, sprintf("%.1f", (1.0 - orbit / orbits)), adj = c(0.5, 0.5), cex = 0.8, col = "lightgray")
    
    # Place planets
    while(any(idx)) {
      remaining <- sum(idx)
      current_planet <- planets - remaining + 1 # +1 because in R indices starts from 1
      current_idx <- idx_int[current_planet]
      angle <- angles[current_planet]
      xy <- polarToDecart(angle = angle, orbit)
      # draw planet
      symbols(xy[1], xy[2], circles = rp, bg = colors[current_idx], add = TRUE, inches = FALSE)
      
      planet_idx <- current_idx
      idx[planet_idx] <- FALSE
      all_idx[planet_idx] <- FALSE
      
      # draw planet label
      planet_corr <- corr_dist[planet_idx]
      if (positive[planet_idx]) col <- "#03C03C" else col <- "#FF6961"
      if (orbit == orbits)
        col <- "gray"
      text(xy[1] + rp, xy[2] + rp, lbls[planet_idx], adj = c(0.1, 0), cex = 0.8, col = col)
      
    }
    
    
    
    # move to next orbit
    last_orbit <- new_orbit
  }
  

}