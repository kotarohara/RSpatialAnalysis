require(maptools)
require(spdep)
require(rgdal)
require(GISTools)

nc.sids <- readShapePoly(
  system.file('etc/shapes/sids.shp', package='spdep')[1], 
  ID="FIPSNO", 
  proj4string = CRS("+proj=longlat +ellps=clrk66"))

nc.sids.p <- spTransform(nc.sids, CRS("+init=epsg:2264 +units=mi"))

plot(nc.sids.p)
add.scale <- function () {
  lines(c(480, 480, 530, 530), c(25, 20, 20, 25))
  text(505, 10, "50 Miles")
  
}

nc.lw <- nb2listw(poly2nb(nc.sids.p))
sids79 <- 1000 * nc.sids.p$SID79 / nc.sids.p$BIR79
nc.lI <- localmoran(sids79, nc.lw)
sids.shade <- auto.shading(c(nc.lI[,1], -nc.lI[,1]), cols=brewer.pal(5, "PRGn"))
choropleth(nc.sids.p, nc.lI[,1], shading=sids.shade)
choro.legend(120.3, 54.9, sids.shade, fmt="%6.2f")
title("Sudden INfant Death Syndrome (Local Moran's I)", cex.main=2)
add.scale()

pval.shade <- shading(c(0.01, 0.05, 0.1), cols=rev(brewer.pal(4, 'PuRd')))
choropleth(nc.sids.p, nc.lI[,5], shading=pval.shade)
choro.legend(120.3, 54.9, pval.shade, fmt="%6.2f")
title("Sudden Infant Death Syndrome (Local p-value)", cex.main=2)
add.scale()

# 8.4
pval.shade <- shading(c(0.01, 0.05, 0.1), cols=rev(brewer.pal(4, 'PuRd')))
choropleth(nc.sids.p,
           p.adjust(nc.lI[,5], method='bonferroni'),
           shading=pval.shade)
choro.legend(120.3, 54.9, pval.shade, fmt="%6.2f")
title("Sudden Infant Death Syndrome (Bonferroni Adjusted p-value)", cex.main=2)
add.scale()