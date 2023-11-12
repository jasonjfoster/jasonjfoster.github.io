library(ggplot2)
library(GGally)

theme_jjf <- function(base_size = 11, base_family = "",
                      base_line_size = base_size / 22,
                      base_rect_size = base_size / 22) {
  
  half_line <- base_size / 2
  
  theme_gray(base_size = base_size, base_family = base_family,
             base_line_size = base_line_size,
             base_rect_size = base_rect_size) %+replace%
    theme(line = element_line(colour = rgb(217, 217, 217, max = 255), size = base_line_size,
                              linetype = 1, lineend = "butt"),
          axis.text = element_text(size = rel(0.8), colour = "black"),
          axis.ticks = element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          legend.key = element_blank(),
          legend.text.align = 0,
          legend.title = element_blank(),
          legend.position = "bottom",
          panel.background = element_blank(),
          panel.grid = element_line(),
          strip.background = element_blank(),
          plot.background = element_blank(),
          plot.margin = margin(0, 0, 0, 0))
}

palette_jjf <- function(n) {
  
  colors_jjf <- c(rgb(236, 105,  65, max = 255),
                  rgb(253, 197, 129, max = 255),
                  rgb( 20,  76,  89, max = 255),
                  rgb( 22, 144, 133, max = 255))
  
  result <- rep(colors_jjf, length.out = n)
  return(result)
  
}

scale_color_jjf <- function(...) {
  
  discrete_scale("color", "jjf", palette_jjf, ...)
  
}

scale_fill_jjf <- function(...) {
  
  discrete_scale("fill", "jjf", palette_jjf, ...)
  
}

# https://ggplot2.tidyverse.org/reference/labeller.html
capitalize <- function(string) {
  string <- as.character(string)
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

plot_jjf <- function(dt, x, y, z, decomp, title, xlab, ylab, multiple, palette, stack) {
  
  if (!is.null(palette)) {
    dt[ , (z) := factor(get(z), levels = names(palette))]
  } else {
    dt[ , (z) := capitalize(get(z))]
    dt[ , (z) := factor(get(z), levels = unique(get(z)))]
  }
  
  result <- ggplot() +
    theme_jjf() +
    labs(title = title, x = xlab, y = ylab)
  
  if (stack) {
    
    result <- result +
      geom_area(data = dt[get(z) != decomp],
                aes(x = get(x), y = get(y) * multiple, fill = get(z))) +
      geom_line(data = dt[get(z) == decomp],
                aes(x = get(x), y = get(y) * multiple, color = get(z)))
    
    if (!is.null(palette)) {
      
      result <- result +
        scale_fill_manual(values = palette)
        # scale_fill_manual(values = palette, guide_legend(order = 2)) +
        # scale_color_manual(values = palette, guide_legend(order = 1))
      
    } else {
      
      result <- result +
        scale_fill_jjf(guide_legend(order = 2)) +
        scale_color_manual(values = "black", guide_legend(order = 1))
      
    }
    
  } else {
    
    result <- result +
      geom_line(data = dt,
                aes(x = get(x), y = get(y) * multiple, color = get(z)))
    
    if (!is.null(palette)) {
      
      result <- result +
        scale_color_manual(values = palette)
      
    } else {
      
      result <- result +
        scale_color_jjf()
      
    }
    
  }
  
  return(result)
  
}

plot_ts <- function(dt, x = "index", y = "value", z = "variable",
                    title = NULL, xlab = NULL, ylab = NULL,
                    multiple = 1, palette = NULL, stack = FALSE) {
  
  result <- plot_jjf(dt, x, y, z, NULL, title, xlab, ylab, multiple, palette, stack)
  
  return(result)
  
}

plot_ts_decomp <- function(dt, x = "index", y = "value", z = "variable", decomp = "",
                           title = NULL, xlab = NULL, ylab = NULL,
                           multiple = 100, palette = NULL, stack = TRUE) {
  
  result <- plot_jjf(dt, x, y, z, decomp, title, xlab, ylab, multiple, palette, stack)
  
  return(result)
  
}

plot_scen <- function(dt, x = "shock", y = "value", z = "variable",
                      title = NULL, xlab = NULL, ylab = NULL,
                      multiple = 1, palette = NULL, stack = FALSE) {
  
  result <- plot_jjf(dt, x, y, z, NULL, title, xlab, ylab, multiple, palette, stack)
  
  return(result)
  
}

plot_heatmap <- function(dt, x = "", y = "", z = "",
                         title = NULL, xlab = NULL, ylab = NULL,
                         multiple = 100, color = palette_jjf(1)) {
  
  result <- ggplot(dt, aes(x = get(x), y = get(y), fill = get(z) * multiple)) +
    theme_jjf() +
    theme(legend.position = "none") +
    labs(title = title, x = xlab, y = ylab) +
    geom_tile() +
    geom_text(aes(label = sprintf("%0.0f", get(z) * 100))) +
    scale_fill_gradient(low = "white", high = color)
  
  return(result)
  
}

plot_scatter <- function(dt, x = "", y = "",
                         title = NULL, xlab = x, ylab = y,
                         multiple = 100, color = palette_jjf(1)) {
  
  result <- ggplot() +
    theme_jjf() +
    geom_point(data = dt, aes(x = get(x) * multiple, y = get(y) * multiple),
               col = color, alpha = 0.2) +
    labs(title = title, x = xlab, y = ylab)
  
  return(result)
  
}

plot_density <- function(dt, x = "", y = "",
                         title = NULL, xlab = x, ylab = y,
                         multiple = 1, color = palette_jjf(1)) {
  
  result <- ggplot() +
    theme_jjf() +
    geom_histogram(data = dt, aes(x = get(x) * multiple, y = ..density..), bins = 30) +
    geom_line(data = dt, aes(x = get(x) * multiple, y = get(y) * multiple)) +
    labs(title = title, x = xlab, y = ylab)
  
  return(result)
  
}

plot_pairs <- function(dt,
                       title = NULL, xlab = NULL, ylab = NULL,
                       multiple = 100, color = palette_jjf(1)) {
  
  result <- ggpairs(dt * multiple,
                    diag = list(continuous = wrap("densityDiag", col = color)),
                    upper = list(continuous = wrap("points", size = 0.1, alpha = 0.1, col = color)),
                    lower = list(continuous = wrap("points", size = 0.1, alpha = 0.1, col = color))) +
    theme_jjf() +
    labs(title = title, x = xlab, y = ylab)
  
  return(result)
  
}
