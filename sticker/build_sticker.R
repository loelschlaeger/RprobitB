### define font
library("showtext")
font_add_google("Martel", "my_font")
showtext_auto()

### build plot
C <- 3
s <- c(0.2, 0.3, 0.5)
mu <- c(0.6, 1.5, 1.9)
sd <- c(0.35, 0.5, 0.3)
x <- seq(-0.5, 3.5, 0.001)
mixture <- matrix(0, nrow = length(x), ncol = C)
for(c in 1:C) {
  mixture[,c] <- s[c] * dnorm(x, mean = mu[c], sd = sd[c])
}
mixture <- cbind(x, mixture, rowSums(mixture))
colnames(mixture) <- c("x", "C1", "C2", "C3", "joint")
library("ggplot2")
mixtures_color <- "#c06c84"
total_mixture_color <- "#6c5b7b"
p <- ggplot2::ggplot(as.data.frame(mixture)) +
  ggplot2::theme_void() +
  ggplot2::geom_line(aes(x = x, y = C1), color = mixtures_color) +
  ggplot2::geom_line(aes(x = x, y = C2), color = mixtures_color) +
  ggplot2::geom_line(aes(x = x, y = C3), color = mixtures_color) +
  ggplot2::geom_line(
    aes(x = x, y = joint), color = total_mixture_color, size = 0.6, linetype = "dashed"
  )

### build sticker
library("hexSticker")
sticker_file <- sticker(
  ### image
  subplot = p,
  s_x = 1,
  s_y = 0.9,
  s_width = 1.7,
  s_height = 0.65,
  ### package name
  package = "RprobitB",
  p_x = 1,
  p_y = 1.4,
  p_color = "#355c7d",
  p_family = "my_font",
  p_fontface = "plain",
  p_size = 20,
  ### sticker
  h_size = 1.2,
  h_fill = "#f67280",
  h_color = "#355c7d",
  spotlight = FALSE,
  l_x = 0.9,
  l_y = 1.4,
  l_width = 2,
  l_height = 1,
  l_alpha = 0.8,
  white_around_sticker = FALSE,
  ### URL
  url = "loelschlaeger.de/RprobitB",
  u_x = 1,
  u_y = 0.1,
  u_color = "#355c7d",
  u_family = "my_font",
  u_size = 5.4,
  u_angle = 30,
  ### save file
  filename = "sticker/RprobitB_sticker.png",
  asp = 1,
  dpi = 300
)
plot(sticker_file)


