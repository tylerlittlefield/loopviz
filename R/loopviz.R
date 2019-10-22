initialize_plot <- function() {
  plot(
    x = c(-1, 1),
    y = c(-1, 1),
    type = "n",
    asp = 1,
    bty = "n",
    xaxt = "n",
    yaxt = "n",
    ann = FALSE
  )
}

plot_circles <- function(i, x) {
  for (i in 1:x[["circles"]]) {
    radius <- x$radii[i]
    theta <- seq(0, 2 * pi, length = 200)
    x_position <- ifelse(x$radii[i] == 1, 0, 1 - radius)
    lines(x = radius * cos(theta) + x_position, y = radius * sin(theta))
  }
}

parse_title <- function(i) {
  switch (as.character(length(i)),
    "0" = title("0 Nested Loops"),
    "1" = title("0 Nested Loops"),
    "2" = title(paste(length(i) - 1, "Nested Loop")),
    title(paste(length(i) - 1, "Nested Loops"))
  )
}

initialize_params <- function(i) {
  list(
    iterations = unlist(i),
    circle = seq_along(i),
    circles = length(i),
    radii = c(1, 0.5 ^ seq_along(i))[1:length(i)]
  )
}

animate <- function(x, delay = 0.1, loop = FALSE) {
  path <- list()
  for (i in 1:x[["circles"]]) {
    radius <- x$radii[i]
    theta <- seq(0, 2 * pi, length = 200)
    x_position <- ifelse(x$radii[i] == 1, 0, 1 - radius)
    if (x$circle[i] == 1) {
      path[[i]] <- list(
        x = rep(rev(radius * cos(theta) + x_position), 1),
        y = rep(rev(radius * sin(theta)), 1)
      )
    } else {
      path[[i]] <- list(
        x = rep(rev(radius * cos(theta) + x_position), x[["iterations"]][i]),
        y = rep(rev(radius * sin(theta)), x[["iterations"]][i])
      )
    }
  }

  path <- do.call("rbind", lapply(path, as.data.frame))
  path <- path[seq(from = 1, to = nrow(path), by = nrow(path) / 200), ]

  grDevices::png("frame%03d.png")
  graphics::par(ask = FALSE)
  for(i in 1:nrow(path)) {
    initialize_plot()
    lines(x = path$x, y = path$y)
    points(x = path$x[i], y = path$y[i])
    parse_title(x[["circle"]])
  }
  dev.off()
  png_files <- sprintf("frame%03d.png", 1:nrow(path))
  gif_file <- gifski::gifski(png_files, delay = delay, loop = loop)
  unlink(png_files)
  utils::browseURL(gif_file)
}

#' Visualize Nested Loops
#' @description Simply pass a vector of integers. The first integer represents
#' the number of iterations for the top level loop. The second integer
#' represents the number of iterations for the first nested loop and so on. As
#' an example, \code{loopviz(1, 2)} is a loop that iterates once and contains a
#' nested loop which iterates twice.
#'
#' @param ... A vector of integers
#' @export
loopviz <- function(..., animate = FALSE, delay = NULL, loop = TRUE) {
  i <- list(...)
  x <- initialize_params(i)

  if (animate) {
    if (is.null(delay)) {
      delay <- 0.1
      message("No value given to delay, defaulting to 0.1")
    }
    animate(x, delay, loop)
  } else {
    initialize_plot()
    plot_circles(i, x)
    parse_title(i)
  }
}
