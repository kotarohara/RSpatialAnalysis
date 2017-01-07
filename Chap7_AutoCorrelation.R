# Auto correlation of sidewalks
# Depends on gdal and proj. Run:
# sudo apt-get install libgdal1-dev libproj-dev
require(spdep)
require(SpatialEpi)
require(GISTools)
require(rgdal)

data(pennLC)

penn.state.latlong <- pennLC$spatial.polygon

penn.state.utm <- spTransform(penn.state.latlong, CRS("+init=epsg:3724 +units=km")) 

smk <- pennLC$smoking$smoking * 100

shades <- auto.shading(smk, n=6, cols=brewer.pal(5, 'Blues'))
choropleth(penn.state.utm, smk, shades)
choro.legend(538.5336, 4394, shades, title='Smoking Uptake (% of popn.)')

par(mfrow=c(3,2), mar=c(1,1,1,1)/2)
real.data.i <- sample(1:6, 1)

for (i in 1:6) {
  if (i == real.data.i) {
    choropleth(penn.state.utm, smk, shades)
  } else {
    choropleth(penn.state.utm, sample(smk), shades)
  }
  
  par(mfrow=c(1,1))
}

penn.state.nb <- poly2nb(penn.state.utm)
penn.state.nb

plot(penn.state.utm, border='lightgray')
plot(penn.state.nb, coordinates(penn.state.utm), add=TRUE, col='red')

penn.state.nb2 <-poly2nb(penn.state.utm, queen=FALSE)
plot(penn.state.utm, border='lightgrey')
plot(penn.state.nb, coordinates(penn.state.utm), add=TRUE, col='blue', lwd=2)
plot(penn.state.nb2, coordinates(penn.state.utm), add=TRUE, col='yellow')

penn.state.lw <- nb2listw(penn.state.nb2)
penn.state.lw

smk.lagged.means <- lag.listw(penn.state.lw, smk)
choropleth(penn.state.utm, smk.lagged.means, shades)

plot(smk, smk.lagged.means, asp=1, xlim=range(smk), ylim=range(smk))

abline(a=0, b=1)
abline(v=mean(smk), lty=2)
abline(h=mean(smk.lagged.means), lty=2)

# 7.6
sar.res <- spautolm(smk~1,listw=penn.state.lw)
sar.res

sar.res$lambda + c(-2, 2) * sar.res$lambda.se
