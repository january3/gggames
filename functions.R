GeomFafik <- ggproto("GeomFafik", 
               Geom, 
               required_aes=c("xmin", "ymin", "xmax", "ymax"),
               default_aes=aes(shape=19, colour="black"),
               draw_key=draw_key_blank(),
               draw_panel=function(data, panel_params, coord) {
                  
                  coords <- coord$transform(data, panel_params)

                  w <- coords$xmax - coords$xmin
                  h <- coords$ymax - coords$ymin
                  x <- coords$xmin + w/2
                  y <- coords$ymin + h/2
                  grob1 <- grid::rectGrob(x, y, width=w, height=h,
                                          gp=gpar(col=coord$colour))
                  grob2 <- grid::pointsGrob(x=x, y=y,
                                            gp=gpar(col=coord$colour))
                   gTree("fafik_grob", children=gList(grob1, grob2))
               })


geom_fafik <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomFafik, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



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


.pie.whole <- function(x, y, w, h, v, fill) {

     v <- c(0, cumsum(v)/sum(v))
     dv <- diff(v)
     nv <- length(dv)

     res <- 50
     
     grobss <- map(1:nv, ~ .pie.slice(x, y, w/2, h/2, v[.], v[.+1], dv[.], res, fill[.]))

}


.pie_widget_draw <- function(data, panel_params, coord, wgdata) {
     
     coords <- coord$transform(data, panel_params)
     #print(data)

     w <- (coords$xmax - coords$xmin)[1]
     h <- (coords$ymax - coords$ymin)[1]
     x <- (coords$xmin + w/2)[1]
     y <- (coords$ymin + h/2)[1]

     grobss <- .pie.whole(x, y, w, h, data$value, data$fill)
     groblab <- .pie.labels(x, y, w, h, data$value, data$labels)

     grob1 <- grid::rectGrob(x, y, width=w, height=h, gp=gpar(col=coord$colour))
     grob2 <- grid::pointsGrob(x=x, y=y, gp=gpar(col=coord$colour))
     
     grobss <- c(grobss, list(groblab)) #, grob1, grob2))

     class(grobss) <- "gList"
     gTree("fafik_grob", children=grobss)
     #groblab
     #textGrob("test", x[1], y[1])
}


GeomPieWidget2 <- ggproto("GeomPieWidget",
  Geom,
  required_aes=c("xmin", "ymin", "xmax", "ymax", "group", "value"),
  default_aes=aes(shape=19, fill="black", colour="black", labels=NULL),
  draw_key=draw_key_blank(),
  draw_group=.pie_widget_draw,
  extra_params=c("na.rm")
)


geom_pie_widget2 <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = FALSE, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomPieWidget2, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

.bar_widget_bars <- function(x, y, w, h, v, fill) {

     nv <- length(v)
     v <- h * v / max(v)

     dx <- w / nv
     xx <- x - w/2 + seq(dx/2, w - dx/2, length.out=nv)
     yy <- y - h/2 + v/2 
     ww <- rep(dx, nv)
     hh <- v


     list(rectGrob(xx, yy, ww, hh, gp=gpar(fill=fill)))

}


.bar_widget_draw <- function(data, panel_params, coord, wgdata) {

     coords <- coord$transform(data, panel_params)
     print(coords)

     w <- (coords$xmax - coords$xmin)[1]
     h <- (coords$ymax - coords$ymin)[1]
     x <- (coords$xmin + w/2)[1]
     y <- (coords$ymin + h/2)[1]


     grob1 <- grid::rectGrob(x, y, width=w, height=h, gp=gpar(col=coord$colour))
     grob2 <- grid::pointsGrob(x=x, y=y, gp=gpar(col=coord$colour))
     
     grobss <- .bar_widget_bars(x=x, y=y, w=w, h=h, v=data$value, fill=coords$fill)
     #grobss <- c(grobss, list(grob1, grob2))

     class(grobss) <- "gList"
     gTree("fafik_grob", children=grobss)
 

}


GeomBarWidget <- ggproto("GeomPieWidget",
  Geom,
  required_aes=c("xmin", "ymin", "xmax", "ymax", "group", "value"),
  default_aes=aes(shape=19, colour="black", fill=NULL, labels=NULL),
  draw_key=draw_key_blank(),
  draw_group=.bar_widget_draw,
  extra_params=c("na.rm")
)



geom_bar_widget <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = FALSE, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomBarWidget, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomTestText <- ggproto("GeomTestText",
  Geom,
  required_aes=c("x", "y"),
  draw_key=draw_key_blank(),
  draw_panel=function( ) {

  }
)


## the variants for additional data

GeomPieWidget <- ggproto("GeomPieWidget",
  Geom,
  required_aes=c("xmin", "ymin", "xmax", "ymax"),
  default_aes=aes(shape=19, colour="black"),
  draw_key=draw_key_blank(),
  draw_panel=.pie_widget_draw,
  extra_params=c("na.rm", "check_overlap", "wgdata")
)


geom_pie_widget <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = FALSE, 
                              inherit.aes = TRUE, check_overlap=FALSE, wgdata=NULL, ...) {
  layer(
    geom = GeomPieWidget, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, wgdata=wgdata,
    check_overlap=check_overlap, ...)
  )
}



