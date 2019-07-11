library(ggplot2)
library(grid)

## wrapper around widgets:
## return a function suitable for drawing the group for the given widget
.widget_wrap <- function(widget) {

  tocall <- switch(widget, 
    burst=.burst_widget_draw,
    planet=.planet_widget_draw,
    pie=.pie_widget_draw,
    bar=.bar_widget_draw,
    NULL
    )

  ## the function to be returned
  f <- function(data, panel_params, coord) {
    ct <- coord$transform(data, panel_params)

    x <- ct$x[1] 
    y <- ct$y[1]
    w <- ct$width[1]
    h <- ct$height[1]

    grobs <- tocall(x, y, w, h, data$value, ct)

    if(is(grobs, "list")) {
      class(grobs) <- "gList"
      grobs <- gTree("widget_grobs", children=grobs)
    }

    return(grobs)
  }

  return(f)
}



## ------------------------------------------------------------ ##
## Pie widget                                                   ##
## ------------------------------------------------------------ ##

## draw one pie slice: polygon approximating a slice
.pie.slice <- function(x, y, w, h, start, stop, frac, res, col, adj=0) {
  
  # commodity function converting polar to cartesian
  .a2xy <- function(a, r=1, adj=0) {
    t <- pi * (0.5  - 2 * a) - pi * adj/180
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) )
  }

  ni <- max(3, floor(res * frac)) # number of points on the polygon
  po <- .a2xy(seq.int(start, stop, length.out=ni), adj=adj)

  grid::polygonGrob(c(x, po$x), c(y, po$y), gp=grid::gpar(fill=col))
}


## add labels to the pie slices
.labels.circular <- function(x, y, w, h, v, labels, adj=0, label.pos="outside") {

   v <- c(0, cumsum(v)/sum(v))
   dv <- diff(v)
   nv <- length(dv)

   ## fractional label position
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
.pie_widget_draw <- function(x, y, w, h, v, ct) {

     fill <- ct$fill
     adj  <- ct$padj[1]
     print(ct)

     v <- c(0, cumsum(v)/sum(v))
     dv <- diff(v)
     nv <- length(dv)

     res <- 50
     
     grobs <- map(1:nv, ~ .pie.slice(x, y, w/2, h/2, v[.], v[.+1], dv[.], res, fill[.], adj=adj))
     groblab <- .labels.circular(x, y, w, h, ct$value, ct$labels, adj=adj)
     c(grobs, list(groblab))
}


GeomWidgetPie <- ggproto("GeomWidgetPie",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, fill="black", colour="black", labels=NULL, padj=0),
  draw_key=draw_key_rect,
  setup_data=function(data, params) { 
    data$padj <- params$padj
    data
  },
  draw_group=.widget_wrap("pie"),
  extra_params=c("na.rm", "padj")
)


geom_widget_pie <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = TRUE, 
                              inherit.aes = TRUE, padj=0, ...) {
  layer(
    geom = GeomWidgetPie, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, padj=padj, ...)
  )
}






## ------------------------------------------------------------ ##
## Burst widget: pie-like, yet better                           ##
## ------------------------------------------------------------ ##


.burst_widget_draw <- function(x, y, w, h, v, ct) {

  # plot areas, so sqrt 
  v <- sqrt(v)
  v <- v / max(v) / 2 
  n <- length(v) 
  nn <- 100 
   
  # commodity fuction for turning polar into cartesian 
  a2xy <- function(a, r = 0.5, adj=0) { 
    t <- pi * (0.5  - 2 * a) - pi * adj/180 
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) ) 
  }

  # positions of the pie fragments
  rad <- seq.int(0, 1, length.out=n+1)

  grobs <- purrr::map(1:n, ~ {
    ni <- max(3, floor(nn * 1/n))
    po <- a2xy(seq.int(rad[.], rad[.+1], length.out=ni), r=v[.])
    grid::polygonGrob(c(x, po$x), c(y, po$y), 
			gp=grid::gpar(fill=ct$fill[.]))
  })

  return(grobs)


# if(!is.null(labels)) {
#   angle <- seq.int(1/n/2, 1 - 1/n/2, length.out=n)
#   lp <- a2xy(angle, v/2)
#   params <- c(list(x=lp$x, y=lp$y, labels=labels), label.params)
#   do.call(text, params)
# }

}



GeomWidgetBurst <- ggproto("GeomWidgetBurst",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  #default_aes=aes(shape=19, fill="black", colour="black", labels=NULL),
  default_aes=aes(shape=19, colour="black", alpha=NA, fill=NA, labels=NULL),
  draw_key=draw_key_rect,
  draw_group=.widget_wrap("burst"),
  extra_params=c("na.rm")
)


geom_widget_burst <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = TRUE, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomWidgetBurst, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


## ------------------------------------------------------------ ##
## Stacked widget: horizontal or vertical stacked bar           ##
## ------------------------------------------------------------ ##






## ------------------------------------------------------------ ##
## Planets widget: display bubles arranged in a circle          ##
## ------------------------------------------------------------ ##


## draw a planet widget 
.planet_widget_draw <- function(x, y, w, h, v, ct) {
     
  v <- sqrt(v)
  dv <- v/sum(v)
  v <- c(0, cumsum(v)/sum(v))
  nv <- length(dv)
  vmax <- max(dv)

  w <- w / 2
  h <- h / 2
  nn <- 100             # "resolution"

  adj <- 0

  # commodity function for converting polar to cartesian
  a2xy <- function(a, r=0.5) {
    t <- pi * (0.5  - 2 * a) - pi * adj/180
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) )
  }

  pos <- a2xy(v)

  pointsGrob(pos$x, pos$y, pch=ct$shape, 
    gp=gpar(col=alpha(ct$fill, ct$alpha), fill=alpha(ct$fill, ct$alpha),
    fontsize=v * ct$size[1] * .pt))
}


GeomWidgetPlanet <- ggproto("GeomWidgetPlanet",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  #default_aes=aes(shape=19, psize=10, fill=NA, colour="black", labels=NULL, alpha=NA),
  default_aes=aes(shape=19, size=10, colour="black", alpha=NA, fill=NA, labels=NULL),
  draw_key=draw_key_rect,
  draw_group=.widget_wrap("planet"),
  extra_params=c("na.rm")
)


#' @rdname geom_widget
#' @export
geom_widget_planet <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = TRUE, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom     = GeomWidgetPlanet, 
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

#' @param x,y coordinates of the center of the area of the widget
#' @param w,h width and height of the widget
#' @param v numeric vector of values
#' @labels text to show next to widget features
#' @plot whether to plot immediately (FALSE: just return a list of grobs)
#' @export
widget_bar <- function(x, y, w, h, v, 
                       fill="grey", col="black", labels=NULL, plot=TRUE) {

     nv <- length(v)
     v <- h * v / max(v)  # scale the values

     dx <- w / nv         # width of a bar

     # xx and yy are the mid positions of the rectangles
     xx <- x - w/2 + seq(dx/2, w - dx/2, length.out=nv)
     yy <- y - h/2 + v/2 

     ## widths and heights of the rectangles
     ww <- rep(dx, nv)
     hh <- v

     list(rectGrob(xx, yy, ww, hh, 
      gp=gpar(col=col, fill=fill)))
}


## wrapper around widget_bar()
.bar_widget_draw <- function(x, y, w, h, v, ct) {
  fill <- alpha(ct$fill, ct$alpha)
  col  <- alpha(ct$colour, ct$alpha)
  widget_bar(x, y, w, h, v, fill, col, plot=FALSE)
}


## the widget for the mini barplot
GeomWidgetBar <- ggproto("GeomWidgetBar",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, colour="black", alpha=NA, fill=NA, labels=NULL),
  draw_key=draw_key_rect,
  draw_group=.widget_wrap("bar"),
  extra_params=c("na.rm")
)

#' @rdname geom_widget
#' @export
geom_widget_bar <- function(mapping = NULL, data = NULL, 
                              show.legend = TRUE, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomWidgetBar, mapping = mapping,  data = data, stat = "identity", 
    position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = FALSE, ...)
  )
}


#' A collection of plotting widgets
#'
#' A collections of widgets (mini plots) which can be placed anywhere on the figure
#'
#'
#' @mapping result of the `aes()` function
#' @param data a data frame
#' @name geom_widget
NULL
