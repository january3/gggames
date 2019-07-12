library(ggplot2)
library(grid)

draw_key_widget <- function(data, params, size) {

  draw_key_rect(data, params, size=10)

}

## wrapper around widgets:
## return a function suitable for drawing the group for the given widget
.widget_wrap <- function(widget) {

  tocall <- switch(widget, 
    burst=.burst_widget_draw,
    planet=.planet_widget_draw,
    pie=.pie_widget_draw,
    bar=.bar_widget_draw,
    stacked=.stacked_widget_draw,
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


## add labels to the pie slices
.labels.circular <- function(x, y, w, h, labels, lp=NULL, adj=0, label.pos="outside") {

#  v <- c(0, cumsum(v)/sum(v))
#  dv <- diff(v)
#  nv <- length(dv)
#
#  ## fractional label position
#  lp <- v[1:(length(v) - 1)] + dv/2
   n <- length(labels)

   if(is.null(lp)) {

     dd <- 1/n/2
     lp <- seq(dd, 1-dd, length.out=n)
   }

   print(lp)

   r <- 1
   t <- pi * (0.5 - 2 * lp) - pi * adj/180
   xp <- x + r * w * cos(t) / 2
   yp <- y + r * h * sin(t) / 2

   hjust <- ifelse(xp > x, 0, 1)
   vjust <- ifelse(yp > y, 0, 1)
   textGrob(labels, xp, yp, gp=gpar(col="black"), hjust=hjust, vjust=vjust)
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

  # label positions
  lp <- v[1:(length(v) - 1)] + dv/2
  groblab <- .labels.circular(x, y, w, h, ct$labels, lp=lp, adj=adj)
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


geom_widget_pie <- function(mapping = NULL, data = NULL, 
                              show.legend = TRUE, 
                              inherit.aes = TRUE, padj=0, ...) {
  layer(
    geom = GeomWidgetPie, 
    mapping = mapping,  
    data = data, 
    stat = "identity", 
    position = "identity", 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = FALSE, padj=padj, ...)
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

  adj=ct$padj[1]
  print(adj)
   
  # commodity fuction for turning polar into cartesian 
  a2xy <- function(a, r = 0.5, adj=0) { 
    t <- pi * (0.5  - 2 * a) - pi * adj/180 
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) ) 
  }

  # positions of the pie fragments
  rad <- seq.int(0, 1, length.out=n+1)

  grobs <- purrr::map(1:n, ~ {
    ni <- max(3, floor(nn * 1/n))
    po <- a2xy(seq.int(rad[.], rad[.+1], length.out=ni), r=v[.], adj=adj)
    grid::polygonGrob(c(x, po$x), c(y, po$y), 
			gp=grid::gpar(fill=ct$fill[.]))
  })

  groblab <- .labels.circular(x, y, w, h, ct$labels, adj=adj)

  return(c(grobs, list(groblab)))
}



GeomWidgetBurst <- ggproto("GeomWidgetBurst",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, colour="black", alpha=NA, fill=NA, labels=NULL),
  setup_data=function(data, params) { 
    data$padj <- params$padj
    data
  },
  draw_key=draw_key_rect,
  draw_group=.widget_wrap("burst"),
  extra_params=c("na.rm", "padj")
)


geom_widget_burst <- function(mapping = NULL, data = NULL, 
                              show.legend = TRUE, 
                              inherit.aes = TRUE, padj=0, ...) {
  layer(
    geom = GeomWidgetBurst, 
    mapping = mapping,  
    data = data, 
    stat = "identity", 
    position = "identity", 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = FALSE, padj=padj, ...)
  )
}



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
  draw_key=draw_key_widget,
  draw_group=.widget_wrap("planet"),
  extra_params=c("na.rm")
)


#' @rdname geom_widget
#' @export
geom_widget_planet <- function(mapping = NULL, data = NULL, 
                              show.legend = TRUE, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom     = GeomWidgetPlanet, 
    mapping  = mapping,  
    data     = data, 
    stat     = "identity", 
    position = "identity", 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params      = list(na.rm = FALSE, ...)
  )
}





## ------------------------------------------------------------ ##
## Stacked widget: horizontal or vertical stacked bar           ##
## ------------------------------------------------------------ ##

#' @param x,y coordinates of the center of the area of the widget
#' @param w,h width and height of the widget
#' @param v numeric vector of values
#' @labels text to show next to widget features
#' @plot whether to plot immediately (FALSE: just return a list of grobs)
#' @export
stacked_bar <- function(x, y, w, h, v, 
                       fill="grey", col="black", labels=NULL, plot=TRUE,
                       horizontal=TRUE) {

     nv <- length(v)
     v <- v / max(v)  # scale the values

     if(horizontal) {
       xx <- (x - w/2) + w * c(0, v[-nv])
       yy <- rep(y - h/2, nv)
       ww <- w * v
       hh <- h
     } else {
       xx <- rep(x - w/2, nv)
       yy <- (y - h/2) + h * c(0, v[-nv])
       ww <- w
       hh <- h * v
     }

     bbox <- rectGrob(x, y, w, h, gp=gpar(col="black"))
     list(rectGrob(xx, yy, ww, hh, 
      gp=gpar(col=col, fill=fill), hjust=0, vjust=0), bbox)
}


## wrapper around widget_bar()
.stacked_widget_draw <- function(x, y, w, h, v, ct) {
  fill <- alpha(ct$fill, ct$alpha)
  col  <- alpha(ct$colour, ct$alpha)
  horizontal <- ct$horizontal[1]
  stacked_bar(x, y, w, h, v, fill, col, plot=FALSE, horizontal=horizontal)
}


## the widget for the mini barplot
GeomWidgetStacked <- ggproto("GeomWidgetStacked",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, colour="black", alpha=NA, fill=NA, labels=NULL),
  draw_key=draw_key_rect,
  setup_data=function(data, params) { 
    data$horizontal <- params$horizontal
    data
  },
  draw_group=.widget_wrap("stacked"),
  extra_params=c("na.rm", "horizontal")
)

#' @rdname geom_widget
#' @export
geom_widget_stacked <- function(mapping = NULL, data = NULL, 
                              show.legend = TRUE, 
                              inherit.aes = TRUE, horizontal=TRUE, ...) {
  layer(
    geom = GeomWidgetStacked, mapping = mapping,  data = data, stat = "identity", 
    position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = FALSE, horizontal=horizontal, ...)
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
