```{r, echo=FALSE, out.width="150%"}
library(png)
library(grid)
library(gridExtra)
img1 <-  rasterGrob(as.raster(readPNG("../fig/02-OSC_Flyer 15x15_Englisch.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("../fig/02-OSC_Flyer 15x15_Englisch-2.png")), interpolate = FALSE)
grid.arrange(img1, img2, ncol = 2)
```