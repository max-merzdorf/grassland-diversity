library(magick)

a <- image_read("./images/graphs/planetHist_Green_band.png")
b <- image_read("./images/graphs/planetHist_Red_band.png")
c <- image_read("./images/graphs/planetHist_RE_band.png")
d <- image_read("./images/graphs/planetHist_NIR_band.png")
e <- image_read("./images/graphs/planet_hist_legend.png")

i1 <- image_append(c(a,c), stack=TRUE)
i2 <- image_append(c(b,d), stack=TRUE)
big <- image_append(c(i1,i2))
small <- e

# pad
h_big    <- image_info(big)$height
h_small  <- image_info(small)$height

# Calculate padding needed to center the smaller image
pad_total <- h_big - h_small
pad   <- pad_total / 2

# Pad the small image so its total height equals the big image's height
# Use color = "none" for transparent or "white" for white padding
small_centered <- small |>
  image_border(color = "none", geometry = paste0("0x", pad/2)) |>
  image_border(color = "none", geometry = paste0("0x", pad/2))

# Append horizontally (side by side) without resizing
montage <- image_append(c(big, small_centered))
montage

image_write(montage, "./images/graphs/planetHists_big.png")
