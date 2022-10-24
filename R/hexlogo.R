library(hexSticker)
library(sysfonts)
library(showtextdb)
library(showtext) ## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Fira Sans", "sans-serif") ## Source font: https://fonts.google.com/specimen/Fira+Sans?query=fira#standard-styles
showtext_auto()

imgPath <- "cloud.png"

sticker(imgPath, package = "WeatherMaringa", p_size=15, s_x=1, s_y=.75, s_width=.6,
        h_fill="#db0a13",h_color="#fcde02", filename = "hexlogo.png")
