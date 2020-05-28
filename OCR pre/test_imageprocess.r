# install.packages("pdftools")
# install.packages("pdftables")
# install.packages("magick")
# install.packages("tesseract")
setwd("E:/Data Plus/data/raw/From FPC/Production of Electric Energy")

library(pdftools)
library(qpdf)

# select the page of pdf files we want to convert
files <- pdf_subset("1941.REA annual statistical report.pdf", page= 31, output= "page_old.pdf")

# convert pdf to png
bitmap <- pdf_render_page(files, page = 1, dpi = 600)
png::writePNG(bitmap, "page.png")

# processing png to improve quality
library(tesseract)
library(magick)
input <- image_read("page.png")

image_info(input)

text <- input %>%
  image_convert(type = 'grayscale') %>% # convert the picture to white-black style
  image_normalize() %>% # increases contrast by normalizing the pixel values to span the full range of colors
  image_enhance() %>% # minimize the background noise
  image_contrast() %>% # enhances intensity differences in image
  image_morphology() %>% # morphology can adjust pixel values based on the maximum/minimum of its neighbourhood. I think it will help remove the ink around text, but it doesn't provide much help.
  image_write(path = "page_new.pdf", format = 'pdf', density = '300x300')
