### define font
library("showtext")
font_add_google("Martel", "my_font")
showtext_auto()

### define colors
background_color = "#f67280"
text_color = "#355c7d"
frame_color = text_color
mixtures_color = "#c06c84"
total_mixture_color = "#6c5b7b"

### build plot
C <- 4
s <- c(0.15,0.2,0.3,0.35)
mu_est_p <- c(0.5,1.2,1.8,2)
sd_est_p <- c(0.35,0.6,0.5,0.3)
x <- seq(-0.5,3.5,0.001)
est_mixture <- matrix(0, nrow = length(x), ncol = C)
for(c in 1:C) est_mixture[,c] <- s[c] * dnorm(x, mean = mu_est_p[c], sd = sd_est_p[c])
ymax <- max(rowSums(est_mixture))
par(las = 1)
plot(0, xlim = c(min(x),max(x)), ylim = c(0,ymax),
     type = "n", main = "", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty="n")
col <- rep(mixtures_color,4)
for(c in 1:C) lines(x,est_mixture[,c],col=col[c],lty=1,lwd=1.5)
lines(x,rowSums(est_mixture),lty=5,lwd=2,col=total_mixture_color)
library("ggplot2")
p <- ggplot2::ggplot() +
  ggplot2::theme_minimal()

### build sticker
library("hexSticker")
sticker_file <- sticker(
  ### image
  subplot = p,
  s_x = 1,
  s_y = 0.8,
  s_width = 1.7,
  s_height = 0.8,
  ### package name
  package = "RprobitB",
  p_x = 1,
  p_y = 1.35,
  p_color = text_color,
  p_family = "my_font",
  p_fontface = "plain",
  p_size = 25,
  ### sticker
  h_size = 1.2,
  h_fill = background_color,
  h_color = frame_color,
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
  u_color = text_color,
  u_family = "my_font",
  u_size = 5.4,
  u_angle = 30,
  ### save file
  filename = "sticker/RprobitB_sticker.png",
  asp = 1,
  dpi = 300
)
plot(sticker_file)


