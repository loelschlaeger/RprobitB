### install package 'hexSticker'
#remotes::install_github("GuangchuangYu/hexSticker")

### colors
background_color = "#f67280"
text_color = "#355c7d"
frame_color = text_color
mixtures_color = "#c06c84"
total_mixture_color = "#6c5b7b"

### produce sticker
s = hexSticker::sticker(subplot = ~{

  ### model parameters
  C = 4
  s = c(0.15,0.2,0.3,0.35)

  ### estimated parameter
  mu_est_p = c(0.5,1.2,1.8,2)
  sd_est_p = c(0.35,0.6,0.5,0.3)

  ### specify x-range
  x = seq(-0.5,3.5,0.001)

  ### compute density of estimated mixing components
  est_mixture = matrix(0,nrow=length(x),ncol=C)
  for(c in 1:C) est_mixture[,c] = s[c] * dnorm(x,mean=mu_est_p[c],sd=sd_est_p[c])

  ### make empty plot
  ymax = max(rowSums(est_mixture))
  par(las = 1)
  plot(0, xlim = c(min(x),max(x)), ylim = c(0,ymax),
       type = "n", main = "", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty="n")

  ### select colors for mixing components
  col = rep(mixtures_color,4)

  ### plot estimated mixing components
  for(c in 1:C) lines(x,est_mixture[,c],col=col[c],lty=1,lwd=1.5)

  ### plot estimated mixture
  lines(x,rowSums(est_mixture),lty=5,lwd=2,col=total_mixture_color)

  },
s_x=.73,
s_y = .7, #s_y=.85,
s_width=2.5,
s_height = 1.45, #s_height=2,
package="RprobitB",
p_color=text_color,
p_size=8,
p_y = 1.45,
h_fill=background_color,
h_color=frame_color,
url="modelling heterogeneous choices",
u_color=text_color,
u_size=1.45,
filename="sticker/sticker.pdf")

### restart R session (necessary for some unknown reason)
.rs.restartR()

### open sticker file
system2("open", args = "sticker/sticker.pdf", wait = FALSE)
