library(ggplot2)
library(grid)


testfun1 <- function(...) {
  print(list(...)$dupa)
}

testfun2 <- function() {
  testfun1(dupa=5)
}

draw_panel_func <- function(data, panel_params, coord, ...) {
  showpoints <- list(...)$showpoints
  if(showpoints) {
    coords <- coord$transform(data, panel_params)
    grid::pointsGrob(coords$x, coords$y)
  } else {
    zeroGrob()
  }
}


## definition of the new geom. setup_data inserts the parameter 
## into data, therefore making it accessible for .draw_panel_func
GeomSimplePoint <- ggproto("GeomSimplePoint", Geom,
  required_aes = c("x", "y"),
  default_aes = aes(shape = 19, colour = "black"),
  draw_key = draw_key_point,
  extra_params = c("na.rm", "showpoints"),
  draw_panel = draw_panel_func
)

geom_simple_point <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomSimplePoint, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
} 
























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
    ring=.ring_widget_draw,
    bar=.bar_widget_draw,
    stacked=.stacked_widget_draw,
    NULL
    )

  ## the function to be returned
  f <- function(data, panel_params, coord, wargs) {

    ct <- coord$transform(data, panel_params)

    x <- ct$x[1] 
    y <- ct$y[1]
    w <- ct$width[1]
    h <- ct$height[1]

    print(wargs)
    grobs <- tocall(x, y, w, h, data$value, ct, wargs)

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

   r <- 1.1
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
## Ring widget                                                   ##
## ------------------------------------------------------------ ##

widget_ring <- function(x, y, w, h, v, 
                       fill="grey", col="black", labels=NULL, plot=TRUE,
                       adj=0, rinner=0.5) {

  if(length(fill) == 1) fill <- rep(fill, length(v))
  if(length(col) == 1) col <- rep(col, length(v))

  ri <- rinner

  v <- c(0, cumsum(v)/sum(v))
  dv <- diff(v)
  nv <- length(dv)

  res <- 50
  
  grobs <- purrr::map(1:nv, ~ .ring.slice(x, y, w/2, h/2, v[.], v[.+1], dv[.], res, fill[.], adj=adj, ri=ri))

  # label positions
  lp <- v[1:(length(v) - 1)] + dv/2
  if(!is.null(labels)) {
    groblab <- .labels.circular(x, y, w, h, labels, lp=lp, adj=adj)
    grobs <- c(grobs, list(groblab))
  }
  grobs
}


## draw one pie slice: polygon approximating a slice
.ring.slice <- function(x, y, w, h, start, stop, frac, res, col, adj=0, ri=0.5) {
  
  # commodity function converting polar to cartesian
  .a2xy <- function(a, r=1, adj=0) {
    t <- pi * (0.5  - 2 * a) - pi * adj/180
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) )
  }

  ni <- max(3, floor(res * frac)) # number of points on the polygon
  po1 <- .a2xy(seq.int(start, stop, length.out=ni), adj=adj)
  po2 <- .a2xy(rev(seq.int(start, stop, length.out=ni)), adj=adj, r=ri)

  grid::polygonGrob(c(po1$x, po2$x), c(po1$y, po2$y), gp=grid::gpar(fill=col))
}

## call .pie.slice for each value to be shown
.ring_widget_draw <- function(x, y, w, h, v, ct, wargs) {

  widget_ring(x, y, w, h, v, 
    fill=ct$fill, col=ct$fill, labels=ct$labels, plot=FALSE,
    adj=wargs$padj, rinner=wargs$rinner)
}


GeomWidgetRing <- ggproto("GeomWidgetRing",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, fill="black", colour="black", labels=NULL, padj=0),
  draw_key=draw_key_rect,
  draw_group=.widget_wrap("ring"),
  extra_params=c("na.rm", "wargs")
)


geom_widget_ring <- function(mapping = NULL, data = NULL, 
                              show.legend = TRUE, 
                              inherit.aes = TRUE, padj=0, rinner=0.5, ...) {

  wargs=list(padj=padj, rinner=rinner)
  layer(
    geom = GeomWidgetRing, 
    mapping = mapping,  
    data = data, 
    stat = "identity", 
    position = "identity", 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = FALSE, wargs=wargs,...)
  )
}





## ------------------------------------------------------------ ##
## Burst widget: pie-like, yet better                           ##
## ------------------------------------------------------------ ##

#' @importFrom purrr map
widget_burst <- function(x, y, w, h, v, 
                       fill="grey", col="black", labels=NULL, plot=TRUE,
                       adj=0) {

  if(length(fill) == 1) fill <- rep(fill, length(v))
  if(length(col) == 1) col <- rep(col, length(v))

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
    po <- a2xy(seq.int(rad[.], rad[.+1], length.out=ni), r=v[.], adj=adj)
    grid::polygonGrob(c(x, po$x), c(y, po$y), 
			gp=grid::gpar(fill=fill[.], col=col[.]))
  })

  groblab <- .labels.circular(x, y, w, h, labels, adj=adj)

  return(c(grobs, list(groblab)))
}


.burst_widget_draw <- function(x, y, w, h, v, ct, wargs) {

  widget_burst(x, y, w, h, v, fill=ct$fill, col=ct$fill, 
    labels=ct$labels, adj=wargs$padj)

}


GeomWidgetBurst <- ggproto("GeomWidgetBurst",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, colour="black", alpha=NA, fill=NA, labels=NULL),
  draw_key=draw_key_rect,
  draw_group=.widget_wrap("burst"),
  extra_params=c("na.rm", "wargs")
)


geom_widget_burst <- function(mapping = NULL, data = NULL, 
                              show.legend = TRUE, 
                              inherit.aes = TRUE, padj=0, ...) {
  wargs <- list(padj=padj)
  layer(
    geom = GeomWidgetBurst, 
    mapping = mapping,  
    data = data, 
    stat = "identity", 
    position = "identity", 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = FALSE, wargs=wargs, ...)
  )
}



## ------------------------------------------------------------ ##
## Planets widget: display bubles arranged in a circle          ##
## ------------------------------------------------------------ ##

widget_planet <- function(x, y, w, h, v, 
                       fill="grey", col="black", labels=NULL, plot=TRUE,
                       pch=19, size=18, adj=0) {

  v <- sqrt(v)
  dv <- v/sum(v)
  v <- c(0, cumsum(v)/sum(v))
  nv <- length(dv)
  vmax <- max(dv)

  w <- w / 2
  h <- h / 2
  nn <- 100             # "resolution"

  # commodity function for converting polar to cartesian
  a2xy <- function(a, r=0.5) {
    t <- pi * (0.5  - 2 * a) - pi * adj/180
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) )
  }

  pos <- a2xy(v)

  pointsGrob(pos$x, pos$y, pch=pch,
    gp=gpar(col=col, fill=fill, fontsize=v * size))

}


## draw a planet widget 
.planet_widget_draw <- function(x, y, w, h, v, ct, wargs) {

  widget_planet(x, y, w, h, v,
    fill=alpha(ct$fill, ct$alpha),
    col=alpha(ct$fill, ct$alpha),
    size= ct$size[1] * .pt, adj=wargs$padj)

}


GeomWidgetPlanet <- ggproto("GeomWidgetPlanet",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  #default_aes=aes(shape=19, psize=10, fill=NA, colour="black", labels=NULL, alpha=NA),
  default_aes=aes(shape=19, size=10, colour="black", alpha=NA, fill=NA, labels=NULL),
  draw_key=draw_key_widget,
  draw_group=.widget_wrap("planet"),
  extra_params=c("na.rm", "wargs")
)


#' @rdname geom_widget
#' @export
geom_widget_planet <- function(mapping = NULL, data = NULL, 
                              show.legend = TRUE, 
                              inherit.aes = TRUE, padj=0, ...) {
  wargs <- list(padj=padj)
  layer(
    geom     = GeomWidgetPlanet, 
    mapping  = mapping,  
    data     = data, 
    stat     = "identity", 
    position = "identity", 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params      = list(na.rm = FALSE, wargs=wargs, ...)
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
widget_stacked_bar <- function(x, y, w, h, v, 
                       fill="grey", col="black", labels=NULL, plot=TRUE,
                       horizontal=TRUE) {

  v <- c(0, cumsum(v)/sum(v))
  dv <- diff(v)
  nv <- length(dv)

  if(horizontal) {
    xx <- (x - w/2) + w * v[-length(v)]
    yy <- rep(y - h/2, nv)
    ww <- w * dv
    hh <- h
  } else {
    xx <- rep(x - w/2, nv)
    yy <- (y - h/2) + h * v[-length(v)]
    ww <- w
    hh <- h * dv
  }

  bbox <- rectGrob(x, y, w, h, gp=gpar(col="black"))
  
  grobs <- list(rectGrob(xx, yy, ww, hh, 
   gp=gpar(col=col, fill=fill), hjust=0, vjust=0), bbox)

  if(!is.null(labels)) {
    print(labels)

    if(horizontal) {
       labels <- paste0(labels, "  ")
       xx <- (x - w/2) + w * (v[-length(v)] + v[-1]) / 2
       gr <- textGrob(labels, xx, yy, gp=gpar(col="black"), hjust=1, rot=45)
     } else {
       labels <- paste0(labels, " ")
       yy <- (y - h/2) + h * (v[-length(v)] + v[-1]) / 2
       gr <- textGrob(labels, xx, yy, gp=gpar(col="black"), hjust=1)
     }
     grobs <- c(grobs, list(gr))

  }

  return(grobs)
}


## wrapper around widget_bar()
.stacked_widget_draw <- function(x, y, w, h, v, ct, wargs) {
  horizontal <- wargs$horizontal
  fill <- alpha(ct$fill, ct$alpha)
  col  <- alpha(ct$colour, ct$alpha)
  widget_stacked_bar(x, y, w, h, v, fill, col, plot=FALSE, horizontal=horizontal, labels=ct$labels)
}


## the widget for the mini barplot
GeomWidgetStacked <- ggproto("GeomWidgetStacked",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, colour="black", alpha=NA, fill=NA, labels=NULL),
  draw_key=draw_key_rect,
  draw_group=.widget_wrap("stacked"),
  extra_params=c("na.rm", "wargs")
)

#' @rdname geom_widget
#' @export
geom_widget_stacked <- function(mapping = NULL, data = NULL, 
                              show.legend = TRUE, 
                              inherit.aes = TRUE, horizontal=TRUE, ...) {
  wargs <- list(horizontal=horizontal)
  layer(
    geom = GeomWidgetStacked, mapping = mapping,  data = data, stat = "identity", 
    position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = FALSE, wargs=wargs,  ...)
  )
}








## ------------------------------------------------------------ ##
## Mini barplot                                                 ##
## ------------------------------------------------------------ ##

#' @param x,y coordinates of the center of the area of the widget
#' @param w,h width and height of the widget
#' @param v numeric vector of values
#' @param labels text to show next to widget features
#' @param plot whether to plot immediately (FALSE: just return a list of grobs)
#' @param horizontal if TRUE, the bar plot will be horizontal
#' @export
widget_bar <- function(x, y, w, h, v, 
                       fill="grey", col="black", labels=NULL, plot=TRUE,
                       horizontal=FALSE) {

  print(horizontal)
  nv <- length(v)
  v <- h * v / max(v)  # scale the values


  # xx and yy are the mid positions of the rectangles
  if(horizontal) {
    dy <- h / nv
    xx <- x - w/2 + v/2
    yy <- y - h/2 + seq(dy/2, w - dy/2, length.out=nv)
    
    ww <- v
    hh <- rep(dy, nv)

  } else {

    dx <- w / nv         # width of a bar
    xx <- x - w/2 + seq(dx/2, w - dx/2, length.out=nv)
    yy <- y - h/2 + v/2 

    ## widths and heights of the rectangles
    ww <- rep(dx, nv)
    hh <- v

  }

  grobs <- list(rectGrob(xx, yy, ww, hh, gp=gpar(col=col, fill=fill)))

  if(!is.null(labels)) {
    grobtxt <- textGrob(paste0(labels, "  "), xx, y - h/2, vjust=1, rot=45, hjust=1)
    grobs <- c(grobs, list(grobtxt))
  }

  grobs
}


## wrapper around widget_bar()
.bar_widget_draw <- function(x, y, w, h, v, ct, wargs) {
  horizontal <- wargs$horizontal
  fill <- alpha(ct$fill, ct$alpha)
  col  <- alpha(ct$colour, ct$alpha)
  widget_bar(x, y, w, h, v, fill, col, plot=FALSE, labels=ct$labels, horizontal=horizontal)
}


## the widget for the mini barplot
GeomWidgetBar <- ggproto("GeomWidgetBar",
  Geom,
  required_aes=c("x", "y", "width", "height", "group", "value"),
  default_aes=aes(shape=19, colour="black", alpha=NA, fill=NA, labels=NULL),
  draw_key=draw_key_rect,
  draw_group=.widget_wrap("bar"),
  extra_params=c("na.rm", "wargs")
)

#' @rdname geom_widget
#' @export
geom_widget_bar <- function(mapping = NULL, data = NULL, 
                              show.legend = TRUE, 
                              inherit.aes = TRUE, horizontal=FALSE, ...) {

  # widget additional arguments
  wargs <- list(horizontal=horizontal)
  layer(
    geom = GeomWidgetBar, mapping = mapping,  data = data, stat = "identity", 
    position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = FALSE, wargs=wargs, ...)
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
