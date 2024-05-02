library(ggplot2)

# Analyses descriptives

univarie = function(data, variable, plot = FALSE) {
  tab <- table(data[[variable]])
  print(tab)
  sry <- summary(data[[variable]])
  print(sry)
  is_numeric <- is.numeric(data[[variable]])
  
  if(plot) {
    if (is_numeric) {
      hist(data[[variable]], main = paste("Histogram of", variable), xlab = variable, col = "lightblue")
    } else {
      barplot(tab, main = paste("Barplot of", variable), xlab = variable, col = "lightblue")
    }
  }
}


bivarie <- function(data, x, y, plot = TRUE) {
  tab <- table(data[[x]], data[[y]])
  print(tab)
  
  plots <- list()

  if (plot) {
    # Stacked Bar chart
    plots$stacked <- tryCatch({
      ggplot(data, aes(x = !!rlang::sym(x), fill = !!rlang::sym(y))) + 
        geom_bar(position = "stack") +
        scale_fill_brewer(palette = "Set2") +
        labs(y = y, x = x, title = paste("Stacked Bar chart of", x, "and", y))
    }, error = function(e) NULL)

    # Grouped Bar chart
    plots$grouped <- tryCatch({
      ggplot(data, aes(x = !!rlang::sym(x), fill = !!rlang::sym(y))) + 
        geom_bar(position = "dodge") +
        scale_fill_brewer(palette = "Set2") +
        labs(y = y, x = x, title = paste("Grouped Bar chart of", x, "and", y))
    }, error = function(e) NULL)

    # Scatter plot
    plots$scatter <- tryCatch({
      ggplot(data, aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
        geom_point(color = "cornflowerblue", size = 2, alpha = 0.8) + 
        labs(x = x, y = y, title = paste("Scatter plot", x, "vs", y)) +
        geom_smooth(method = "lm", color = "steelblue") +
        geom_smooth(method = "lm", formula = !!rlang::sym(y) ~ poly(!!rlang::sym(x)), color = "indianred3")
    }, error = function(e) NULL)

    # Line plot
    plots$line <- tryCatch({
      ggplot(data, aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
        geom_line(size = 1.5, color = "lightgrey") +
        geom_point(size = 3, color = "steelblue") +
        labs(y = y, x = x, title = paste("Line plot", x, "vs", y))
    }, error = function(e) NULL)

    # Bar chart for x
    plots$bar_x <- tryCatch({
      ggplot(data, aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
        geom_bar(stat = "identity", fill = "cornflowerblue") +
        labs(title = paste("Bar chart", x, "vs", y), x = x, y = y)
    }, error = function(e) NULL)

    # Bar chart for y
    plots$bar_y <- tryCatch({
      ggplot(data, aes(x = !!rlang::sym(y), y = !!rlang::sym(x))) +
        geom_bar(stat = "identity", fill = "cornflowerblue") +
        labs(title = paste("Bar chart", y, "vs", x), x = y, y = x)
    }, error = function(e) NULL)
  }

  plots
}



