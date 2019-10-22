# i <- list(1, 1)
#
# x <- list(
#   iterations = unlist(i),
#   circles = length(i),
#   radii = c(1, 0.5 ^ seq_along(i))[1:length(i)]
# )
#
# plot(
#   x = c(-1, 1),
#   y = c(-1, 1),
#   type = "n",
#   asp = 1,
#   # bty = "n",
#   # xaxt = "n",
#   # yaxt = "n",
#   ann = FALSE
# )
#
# for (i in 1:x[["circles"]]) {
#   radius <- x$radii[i]
#   theta <- seq(0, 2 * pi, length = 200)
#   x_position <- ifelse(x$radii[i] == 1, 0, radius)
#   lines(x = radius * cos(theta) + x_position, y = radius * sin(theta))
# }
#
# radius1 <- x$radii[1]
# radius2 <- x$radii[2]
# theta <- seq(0, 2 * pi, length = 200)
# x_position1 <- ifelse(x$radii[1] == 1, 0, radius1)
# x_position2 <- ifelse(x$radii[2] == 1, 0, radius2)
# lines(x = radius1 * cos(theta) + x_position1, y = radius1 * sin(theta))
# lines(x = radius2 * cos(theta) + x_position2, y = radius2 * sin(theta))
# # points(x = radius1 * cos(theta)[1:50] + x_position1, y = radius1 * sin(theta)[1:50])
# # points(x = radius2 * cos(theta) + x_position2, y = radius2 * sin(theta))
# # points(x = radius1 * cos(theta)[51:length(theta)] + x_position1, y = radius1 * sin(theta)[51:length(theta)])
#
# test <- data.frame(
#   x = c(
#     rev(radius1 * cos(theta)[1:50] + x_position1),
#     rev(radius2 * cos(theta) + x_position2),
#     rev(radius1 * cos(theta)[51:length(theta)] + x_position1)
#   ),
#   y = c(
#     rev(radius1 * sin(theta)[1:50]),
#     rev(radius2 * sin(theta)),
#     rev(radius1 * sin(theta)[51:length(theta)])
#   )
# )
#
# test <- data.frame(
#   x = test$x[c(TRUE, FALSE)],
#   y = test$y[c(TRUE, FALSE)]
# )
#
# library(gifski)
#
# png("frame%03d.png")
# par(ask = FALSE)
# for(i in 1:200) {
#   plot(
#     x = c(-1, 1),
#     y = c(-1, 1),
#     type = "n",
#     asp = 1,
#     # bty = "n",
#     # xaxt = "n",
#     # yaxt = "n",
#     ann = FALSE
#   )
#   lines(x = radius1 * cos(theta) + x_position1, y = radius1 * sin(theta))
#   lines(x = radius2 * cos(theta) + x_position2, y = radius2 * sin(theta))
#   points(x = test$x[i], y = test$y[i])
# }
# dev.off()
# png_files <- sprintf("frame%03d.png", 1:200)
# gif_file <- gifski(png_files, delay = 0.01)
# unlink(png_files)
# utils::browseURL(gif_file)
