# This is implementation of correlations visualization between dependent and explanatory variables 
# as well as between explanatory variables pairs

# Return angles to the lineary spaced data points on the specified orbit
#
# Args:
#   orbit: the orit number
#   n_data_points: the number of data points to be place on this orbit
# Returns:
#   sequence of angles in radians to lineary spaced data points
#
findAngles <- function(orbit, n_data_points, orbits = 5) {
  start = pi + 2 * pi * orbit / orbits
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
#
polarToDecart <- function(angle, radius) {
  y = sin(angle) * radius
  x = cos(angle) * radius
  c(x, y)
}

# Function to draw celestial body: planet or moon at specified coordinates
#
# Args:
#   xy: the vector c(x, y)
#   radius: the radius of body
#   col: the color to fill
#   lbl: the caption label
#   corr_positive: TRUE when associated explanatory variable has positive correlation with dependent variable
#   last_orbit: if TRUE then body orbit is outmost in the system
#
drawCelestialBody <- function(xy, radius, col, lbl, corr_positive = FALSE, last_orbit = FALSE) {
  symbols(xy[1], xy[2], circles = radius, bg = col, fg = NULL, add = TRUE, inches = FALSE)
  # draw label
  if (last_orbit)
    col <- "gray"
  else if (corr_positive) 
    col <- "#03C03C" 
  else 
    col <- "#FF6961"
  text(xy[1] + radius, xy[2] + radius, lbl, adj = c(0.1, 0), cex = 0.8, col = col)
}

# Function to plot correlations visualization as solar system based model
#
# Args:
#   data: the data frame with data
#   dv_label: the name of dependent variable (sun)
#   orbits: the number of orbits to depict
#
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
  colors_orbit <- heat.colors(orbits)
  colors_orbit[orbits] <- "lightgrey"
  step <- 1.0 / orbits
  last_orbit <- 0.0
  
  # setup plot
  plim = c(-orbits, orbits)
  plot.new()
  plot.window(xlim = plim, ylim = plim, asp = 1)
  r <- 0.03 * orbits
  rp <- r * 0.8
  moon_orbit <- 0.5
  rm <- rp * 0.6
  
  # Draw the sun (dependent variable)
  symbols(0, 0, circles = r, xlim = plim, ylim = plim, bg = colors[center_idx], fg = NULL, add = TRUE, inches = FALSE)
  text(r, r, lbls[center_idx], adj = c(0.1, 0), cex = 0.8, col = grey(0.5, alpha = 1))
  
  # Draw orbital systems with all correlated explanatory variables
  for (orbit in 1:orbits) {
    new_orbit <- step * orbit + 0.1
    idx <- (sun_corr_dist >= (1 - last_orbit)) & (sun_corr_dist > (1 - new_orbit)) & all_idx
    idx_int <- which(idx)
    
    # draw orbit circle
    symbols(0, 0, circles = orbit, fg = colors_orbit[orbit], add = TRUE, inches = FALSE)
    
    # collect correlation distances to the data points on orbit
    corr_dists <- double(length = length(idx_int))
    i <- 1
    for (index in idx_int) {
      corr_dists[i] <- corr_dist[index, center_idx]
      i <- i + 1
    }
    
    planets <- sum(idx) # the number of planets on orbit
    planet_angles <- findAngles(orbit, planets, orbits)
    
    # Draw orbit labels
    text(0, orbit - 0.1, sprintf("%.1f", (1.0 - orbit / orbits)), adj = c(0.5, 0.9), cex = 0.8, col = "lightgrey")
    
    # Place planets
    while (any(idx)) {
      remaining <- sum(idx)
      current_planet <- planets - remaining + 1 # +1 because in R indices starts from 1
      planet_idx <- idx_int[current_planet]
      idx[planet_idx] <- FALSE
      all_idx[planet_idx] <- FALSE

      # proceed with moons if any
      planet_corr <- corr_dist[planet_idx,]
      planet_angle <- planet_angles[current_planet]
      xy <- polarToDecart(angle = planet_angle, orbit)
      
      # add orbit around planet if it has moons
      moon_idx <- (planet_corr >= 0.8) & all_idx
      if (any(moon_idx)) {
        symbols(xy[1], xy[2], circles = moon_orbit, bg = grey(0.8, alpha = 0.8), fg = NULL, add = TRUE, inches = FALSE)
      }
      
      # draw planet and its label
      drawCelestialBody(xy, radius = rp, col = colors[planet_idx], 
                        lbl = lbls[planet_idx], corr_positive = positive[planet_idx], 
                        last_orbit = (orbit == orbits))
      # draw moons if any
      moon_idx_int <- which(moon_idx)
      moons <- sum(moon_idx)
      moon_angles <- findAngles(1, moons)
      while (any(moon_idx)) {
        remaining_moons <- sum(moon_idx)
        current_moon <- moons - remaining_moons + 1 # +1 because in R indices starts from 1
        current_moon_idx <- moon_idx_int[current_moon]
        moon_angle <- moon_angles[current_moon]
        moon_xy <- polarToDecart(angle = moon_angle, radius = moon_orbit) + xy
        color <- colors[current_moon_idx]
        # draw moon and its label
        drawCelestialBody(moon_xy, radius = rm, col = colors[current_moon_idx], 
                          lbl = lbls[current_moon_idx], corr_positive = positive[current_moon_idx], 
                          last_orbit = (orbit == orbits))
        
        moon_idx[current_moon_idx] <- FALSE
        idx[current_moon_idx] <- FALSE
        all_idx[current_moon_idx] <- FALSE
      }
      
    }

    # move to next orbit
    last_orbit <- new_orbit
  }
  

}