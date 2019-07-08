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

  grid::polygonGrob(c(x, po$x), c(y, po$y), gp=gpar(fill=col))
}

.pie_widget_draw <- function(data, panel_params, coord, wgdata) {
     
     coords <- coord$transform(data, panel_params)
     print(str(panel_params))
     #print(str(coord))
     print(str(data))
     #print(str(wgdata))

     v <- data$value
     print(v)
     v <- c(0, cumsum(v)/sum(v))
     print(v)
     dv <- diff(v)
     nv <- length(dv)

     res <- 100
     
     w <- (coords$xmax - coords$xmin)[1]
     h <- (coords$ymax - coords$ymin)[1]
     x <- (coords$xmin + w/2)[1]
     y <- (coords$ymin + h/2)[1]
     grobss <- map(1:nv, ~ .pie.slice(x, y, w/2, h/2, v[.], v[.+1], dv[.], res, data$fill[.]))
     grob1 <- grid::rectGrob(x, y, width=w, height=h, gp=gpar(col=coord$colour))
     grob2 <- grid::pointsGrob(x=x, y=y, gp=gpar(col=coord$colour))
     
     grobss <- c(grobss, list(grob1, grob2))

     class(grobss) <- "gList"
     gTree("fafik_grob", children=grobss)
}

GeomPieWidget2 <- ggproto("GeomPieWidget",
  Geom,
  required_aes=c("xmin", "ymin", "xmax", "ymax", "group", "value"),
  default_aes=aes(shape=19, colour="black"),
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



