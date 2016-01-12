#' A function for drawling barcharts with images.
#'
#' @import ggplot2
#' @importFrom grid rasterGrob
#' @importFrom png readPNG
#'
#' @param df
#' @param x
#' @param y
#' @param image
#'
#' @export
rasterbar <- function(df, x, y, image, reorder=TRUE, shrinktop=TRUE) {

  included_images = c("1WTC","BoA","Chrysler","ESB","NYTimes")
  if (image %in% included_images) {
      if (image == "1WTC")     img <- readPNG(system.file("pics/1WTC.png", package = "ggRasterBar"))
      if (image == "BoA")      img <- readPNG(system.file("pics/BoA.png", package = "ggRasterBar"))
      if (image == "Chrysler") img <- readPNG(system.file("pics/chrysler.png", package = "ggRasterBar"))
      if (image == "ESB")      img <- readPNG(system.file("pics/ESB.png", package = "ggRasterBar"))
      if (image == "NYTimes")  img <- readPNG(system.file("pics/NYTimes.png", package = "ggRasterBar"))
  } else {
      try(img <- readPNG(image))
  }

  #add baseplot with no info
  plt <- ggplot(df, aes_string(x=x,y=y)) + geom_point(alpha=0)

  #check for reordering
  if (reorder == TRUE) df <- df[order(df[[ y ]], decreasing = TRUE),]


  maxy <- max(df[[y]])
  for (i in 1:nrow(df)){
      print("row")
      print(df[i,])

      dims <- dim(img) #627 104 4
      ylen <- dims[1]
      xlen <- dims[2]

      # caluclate image cropping ingo
      xmin = i - 0.5
      xmax = i + 0.5
      yval_row = df[[y]][[i]]
      yval_normal <- (yval_row/maxy) * ylen
      ymin = 0
      ymax = yval_normal

      # the image can be shrunk from the top or the bottom
      if (shrinktop) {
          img2 <- img[(ylen-yval_normal):ylen, 1:xlen, 1:4]
      } else {
          img2 <- img[1:yval_normal, 1:xlen, 1:4]
      }

      g <- rasterGrob(img2, interpolate=TRUE)
      plt <- plt + annotation_custom(g,xmin=xmin, xmax=xmax, ymin=ymin, ymax=yval_row)
      print(paste(xmin,xmax, ymin,ymax, yval_row))
  }

  return(plt)
}

