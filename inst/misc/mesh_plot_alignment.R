
dsn <- "/vsicurl/https://esa-worldcover.s3.amazonaws.com/v100/2020/ESA_WorldCover_10m_2020_v100_Map_AWS.vrt"
info <- vapour::vapour_raster_info(dsn)
#dput(info[c("dimension", "extent", "geotransform")])

src_dm <-  c(4320000L, 1728000L)
src_ex <-  c(-180, 180, -60, 84)
#src_gt <- c(-180, 8.33333333333e-05, 0, 84, 0, -8.33333333333e-05)
src_gt <- affinity::extent_dim_to_gt(src_ex, src_dm)
usr_ex <- c(-123.2232, -123.2228,  42.3032, 42.3036)

d <- elevation(src_ex, dimension = c(1024, 512), )
mesh_raster <- function(dimension = c(2, 2), extent = NULL, ...) {
  if (is.null(extent)) {
    extent <- c(0, dimension[1L], 0, dimension[2])
  }
  ##
  qm <- textures::quad(dimension, ...)
  qm$vb[1, ] <- scales::rescale(qm$vb[1, ], extent[1:2])
  qm$vb[2, ] <- scales::rescale(qm$vb[2, ], extent[3:4])
  qm
}

mr_plot <- function (x, col = NULL, add = FALSE, zlim = NULL, border = "black", ..., axes = TRUE)
{
  # if (!is.null(coords)) {
  #   warning("argument 'coords' is only used for 'mesh_plot(Raster)', ignoring")
  # }
  # if (!is.null(x$material$texture)) {
  #   x <- texture_mesh3d(x)
  # }
  # if (!is.null(crs)) {
  #   xy <- try(reproj::reproj(t(x$vb[1:2, ]), crs)[, 1:2],
  #             silent = TRUE)
  #   if (!inherits(xy, "try-error"))
  #     x$vb[1:2, ] <- t(xy)
  # }
  # plot_points <- FALSE
  # if (!is.null(x$ip)) {
  #   id <- x$ip
  #   plot_points <- TRUE
  # }
  # if (!is.null(x$is)) {
  #   id <- x$is
  # }
  if (!is.null(x$ib)) {
    id <- x$ib
  }
  if (!is.null(x$it)) {
    id <- x$it
  }
  xx <- x$vb[1L, id]
  yy <- x$vb[2L, id]
  ID <- rep(seq_len(ncol(id)), each = nrow(id))
  if (is.null(col)) {
    if (is.null(x$material$color)) {
      cols <- viridis::viridis(100)[scales::rescale(x$vb[3L,
                                                         id[1L, ]], c(1, 100))]
    }
    else {
      cols <- x$material$color
    }
  }
  else {
    cols <- col
  }
  xx <- list(x = xx, y = yy, id = ID, col = cols)
  if (!add) {
    graphics::plot.new()
    graphics::plot.window(xlim = range(xx$x, finite = TRUE),
                          ylim = range(xx$y, finite = TRUE), asp = 1)
  }
  vps <- gridBase::baseViewports()
  grid::pushViewport(vps$inner, vps$figure, vps$plot)

    grid::grid.polygon(xx$x, xx$y, xx$id, gp = grid::gpar(col = border,
                                                          fill = xx$col, ...), default.units = "native")

  grid::popViewport(3)
  if (axes) axis(1);axis(2)
  invisible(NULL)
}
ax <- align_extent(par('usr'), dimension = info$dimension, extent = info$extent)

bg <- mesh_raster(extent_dimension(ax, info$dimension, info$extent), ax)

e <- whatarelief::elevation()
ex <- c(130, 150, -50, -30)
im <- whatarelief::streetmap(extent = ex, dimension = dev.size("px")*.7)
mr <- mesh_raster(dim(im)[2:1], extent = ex, ydown = TRUE)
mr$meshColor <- "faces"
mr$material$color <- im ## palr::d_pal(e)
mr_plot(mr, border = NA)
maps::map(add = T)




mr$vb[1:2, ] <- t(reproj::reproj_xy(t(mr$vb[1:2, ]), "+proj=laea"))

#vcrop(usr_ex, src_dm, src_ex)
ex_plot <- function(x, ...) {
  rect(x[1], x[3], x[2], x[4], ...)
}

share <- c(-123.22327, -123.22273,   42.30314,   42.30369)
par(mfrow = c(2, 2))
for (snap in c("out", "near", "in")) {
  al_ex <- align_extent(usr_ex, src_dm, src_ex, snap = snap)
  al_dm <- extent_dimension(al_ex, src_dm, src_ex)

  mr <- mesh_raster(al_dm, al_ex)
  print(al_dm)
  print(al_ex)
  print(x_res(al_dm, al_ex))
  print(y_res(al_dm, al_ex))
  print(ncol(mr$ib))
  plot(NA, xlim = share[1:2], ylim = share[3:4], xlab = "", ylab = "", asp = 1/cos(42 * pi/180), main = snap)
  mr_plot(bg, add = TRUE, border = "grey")
  mr_plot(mr, add = TRUE)
  ex_plot(usr_ex, lty = 2)
}




# xmin, xmax, ymin, ymax (gdal_translate -projwin is xmin ymax xmax ymin)
ex <- usr_ex
#            dfULX - adfGeoTransform[0]) / adfGeoTransform[1]
srcwin0 <- (ex[1] - info$geotransform[1]) / info$geotransform[2]
# (psOptions->dfULY - adfGeoTransform[3]) / adfGeoTransform[5];
srcwin1 <- (ex[4] - info$geotransform[4]) / info$geotransform[6]
#psOptions->adfSrcWin[2] = (psOptions->dfLRX - psOptions->dfULX) / adfGeoTransform[1];
srcwin2 <- (ex[2] - ex[1])/info$geotransform[2]
#psOptions->adfSrcWin[3] = (psOptions->dfLRY - psOptions->dfULY) / adfGeoTransform[5];
srcwin3 <- (ex[3] - ex[4])/info$geotransform[6]

# psOptions->adfSrcWin[0] = floor(psOptions->adfSrcWin[0] + 0.001);
# psOptions->adfSrcWin[1] = floor(psOptions->adfSrcWin[1] + 0.001);
# psOptions->adfSrcWin[2] = floor(psOptions->adfSrcWin[2] + 0.5);
# psOptions->adfSrcWin[3] = floor(psOptions->adfSrcWin[3] + 0.5);

srcwin <- c(floor(srcwin0 + 0.001),
            floor(srcwin1 + 0.001),
            floor(srcwin2 + 0.5),
            floor(srcwin3 + 0.5))

xmin <- x_from_col(src_dm, src_ex, srcwin[1] + 1) - x_res(src_dm, src_ex)/2
ymax <- y_from_row(src_dm, src_ex, srcwin[2] + 1) + y_res(src_dm, src_ex)/2
xmax <- xmin + srcwin[3] * x_res(src_dm, src_ex)
ymin <- ymax + srcwin[4] * -y_res(src_dm, src_ex)

#abline(v = c(xmin, xmax), h = c(ymin, ymax))
plot(NA, xlim = share[1:2], ylim = share[3:4], xlab = "", ylab = "", asp = 1/cos(42 * pi/180), main = "gdal -projwin")
mr_plot(bg, add = TRUE, border = "grey")

mr_plot(mesh_raster(srcwin[3:4], c(xmin, xmax, ymin, ymax)), add = TRUE)
ex_plot(usr_ex, lty = 2)
