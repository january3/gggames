library(ggplot2)
library(grid)

## GeomFafik <- ggproto("GeomFafik", 
##                Geom, 
##                required_aes=c("xmin", "ymin", "xmax", "ymax"),
##                default_aes=aes(shape=19, colour="black"),
##                draw_key=draw_key_blank(),
##                draw_panel=function(data, panel_params, coord) {
##                   
##                   coords <- coord$transform(data, panel_params)
##
##                   w <- coords$xmax - coords$xmin
##                   h <- coords$ymax - coords$ymin
##                   x <- coords$xmin + w/2
##                   y <- coords$ymin + h/2
##                   grob1 <- grid::rectGrob(x, y, width=w, height=h,
##                                           gp=gpar(col=coord$colour))
##                   grob2 <- grid::pointsGrob(x=x, y=y,
##                                             gp=gpar(col=coord$colour))
##                    gTree("fafik_grob", children=gList(grob1, grob2))
##                })
##
##
## geom_fafik <- function(mapping = NULL, data = NULL, stat = "identity",
##                               position = "identity", na.rm = FALSE, show.legend = NA, 
##                               inherit.aes = TRUE, ...) {
##   layer(
##     geom = GeomFafik, mapping = mapping,  data = data, stat = stat, 
##     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
##     params = list(na.rm = na.rm, ...)
##   )
## }


## ------------------------------------------------------------ ##
## Pie widget                                                   ##
## ------------------------------------------------------------ ##

## draw one pie slice: polygon approximating a slice
.pie.slice <- function(x, y, w, h, start, stop, frac, res, col) {
  
  # commodity function converting polar to cartesian
  .a2xy <- function(a, r=1, adj=0) {
    t <- pi * (0.5  - 2 * a) - pi * adj/180
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) )
  }

  ni <- max(3, floor(res * frac)) # number of points on the polygon
  po <- .a2xy(seq.int(start, stop, length.out=ni))

  grid::polygonGrob(c(x, po$x), c(y, po$y), gp=grid::gpar(fill=col))
}


## add labels to the pie slices
.pie.labels <- function(x, y, w, h, v, labels, label.pos="outside") {

   adj <- 0
   v <- c(0, cumsum(v)/sum(v))
   dv <- diff(v)
   nv <- length(dv)

   lp <- v[1:(length(v) - 1)] + dv/2

   r <- 1
   t <- pi * (0.5 - 2 * lp) - pi * adj/180
   xp <- x + r * w * cos(t) / 2
   yp <- y + r * h * sin(t) / 2

   hjust <- ifelse(xp > x, 0, 1)
   vjust <- ifelse(yp > y, 0, 1)
   textGrob(labels, xp, yp, gp=gpar(col="black"), hjust=hjust, vjust=vjust)
}


## call .pie.slice for each value to be shown
.pie.whole <- function(x, y, w, h, v, fill) {

     v <- c(0, cumsum(v)/sum(v))
     dv <- diff(v)
     nv <- length(dv)

     res <- 50
     
     grobss <- map(1:nv, ~ .pie.slice(x, y, w/2, h/2, v[.], v[.+1], dv[.], res, fill[.]))
}


## draw a pie widget using data$value
.pie_widget_draw <- function(data, panel_params, coord, wgdata) {
     
     ct <- coord$transform(data, panel_params)

     x <- ct$x[1] 
     y <- ct$y[1]
     w <- ct$width[1]
     h <- ct$height[1]

     grobs <- .pie.whole(x=ct$x[1], y=ct$y[1], w=ct$width[1], h=ct$height[1], v=data$value, fill=ct$fill)
     groblab <- .pie.labels(x, y, w, h, data$value, data$labels)

     #grob1 <- grid::rectGrob(x, y, width=w, height=h, gp=gpar(col=coord$colour))
     #grob2 <- grid::pointsGrob(x=x, y=y, gp=gpar(col=coord$colour))
     
     grobs <- c(grobs, list(groblab)) #, grob1, grob2))

     class(grobs) <- "gList"
     gTree("fafik_grob", children=grobs)
     #groblab
     #textGrob("test", x[1], y[1])
}


GeomPieWidget <- ggproto("GeomPieWidget",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, fill="black", colour="black", labels=NULL),
  draw_key=draw_key_rect,
  draw_group=.pie_widget_draw,
  extra_params=c("na.rm")
)


geom_pie_widget <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = TRUE, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomPieWidget, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}






## ------------------------------------------------------------ ##
## Burst widget: pie-like, yet better                           ##
## ------------------------------------------------------------ ##


.burst_widget_draw <- function(data, panel_params, coord, wgdata) {

  ct <- coord$transform(data, panel_params)

  x <- ct$x[1] 
  y <- ct$y[1]
  w <- ct$width[1]
  h <- ct$height[1]

  # plot areas, so sqrt 
  v <- sqrt(data$value) 
  v <- v / max(v) / 2 
  n <- length(v) 
  nn <- 100 
   
  # commodity function for turning polar into cartesian 
  a2xy <- function(a, r = 0.5, adj=0) { 
    t <- pi * (0.5  - 2 * a) - pi * adj/180 
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) ) 
  }

  # positions of the pie fragments
  rad <- seq.int(0, 1, length.out=n+1)

  grobs <- map(1:n, ~ {
    ni <- max(3, floor(nn * 1/n))
    po <- a2xy(seq.int(rad[.], rad[.+1], length.out=ni), r=v[.])
    grid::polygonGrob(c(x, po$x), c(y, po$y), 
			gp=grid::gpar(fill=ct$fill))
  })

     class(grobs) <- "gList"
     gTree("bar_widget_grob", children=grobs)


# if(!is.null(labels)) {
#   angle <- seq.int(1/n/2, 1 - 1/n/2, length.out=n)
#   lp <- a2xy(angle, v/2)
#   params <- c(list(x=lp$x, y=lp$y, labels=labels), label.params)
#   do.call(text, params)
# }

}



GeomBurstWidget <- ggproto("GeomBurstWidget",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, fill="black", colour="black", labels=NULL),
  draw_key=draw_key_rect,
  draw_group=.burst_widget_draw,
  extra_params=c("na.rm")
)


geom_burst_widget <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = TRUE, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomBurstWidget, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}






## ------------------------------------------------------------ ##
## Planets widget: display bubles arranged in a circle          ##
## ------------------------------------------------------------ ##


## draw a pie widget using data$value
.planet_widget_draw <- function(data, panel_params, coord, wgdata) {
     
  ct <- coord$transform(data, panel_params)
  print(ct)

  v <- sqrt(data$value)
  dv <- v/sum(v)
  v <- c(0, cumsum(v)/sum(v))
  nv <- length(dv)
  vmax <- max(dv)

  x <- ct$x[1] 
  y <- ct$y[1]
  w <- ct$width[1] / 2
  h <- ct$height[1] / 2
  nn <- 100             # "resolution"

  adj <- 0

  # commodity function for converting polar to cartesian
  a2xy <- function(a, r=0.5) {
    t <- pi * (0.5  - 2 * a) - pi * adj/180
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) )
  }

  pos <- a2xy(v)

  grobs <- pointsGrob(pos$x, pos$y, pch=ct$shape, 
    gp=gpar(col=alpha(ct$fill, ct$alpha), fill=alpha(ct$fill, ct$alpha),
    fontsize=v * ct$psize[1] * .pt))

  grobs
}

GeomPlanetWidget <- ggproto("GeomPlanetWidget",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, psize=10, fill=NA, colour="black", labels=NULL, alpha=NA),
  draw_key=draw_key_rect,
  draw_group=.planet_widget_draw,
  extra_params=c("na.rm", "psize")
)


geom_planet_widget <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = TRUE, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom     = GeomPlanetWidget, 
    mapping  = mapping,  
    data     = data, 
    stat     = stat, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params      = list(na.rm = na.rm, ...)
  )
}









## ------------------------------------------------------------ ##
## Mini barplot                                                 ##
## ------------------------------------------------------------ ##

## calculate the mini barplot
.bar_widget_bars <- function(x, y, w, h, v, fill) {

     nv <- length(v)
     v <- h * v / max(v)  # scale the values

     dx <- w / nv         # width of a bar

     # xx and yy are the mid positions of the rectangles
     xx <- x - w/2 + seq(dx/2, w - dx/2, length.out=nv)
     yy <- y - h/2 + v/2 

     ## widths and heights of the rectangles
     ww <- rep(dx, nv)
     hh <- v

     list(rectGrob(xx, yy, ww, hh, gp=gpar(fill=fill)))
}


## draw a mini barplot
.bar_widget_draw_group <- function(data, panel_params, coord, wgdata) {

     ct <- coord$transform(data, panel_params)

     grobs <- .bar_widget_bars(x=ct$x[1], y=ct$y[1], w=ct$width[1], h=ct$height[1], v=data$value, fill=ct$fill)

     class(grobs) <- "gList"
     gTree("bar_widget_grob", children=grobs)
}

## the widget for the mini barplot
GeomBarWidget <- ggproto("GeomPieWidget",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, colour="black", fill=NULL, labels=NULL),
  draw_key=draw_key_rect,
  draw_group=.bar_widget_draw_group,
  extra_params=c("na.rm")
)

geom_bar_widget <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = TRUE, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomBarWidget, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



