library(spData)
library(sf)
library(terra)
library(tmap)
library(viridis)
library(tidyterra)
library(ggplot2)
library(gstat)
library(leaflet)

d <- properties
d$vble <- d$prpsqm

# criando o contorno
map <- st_union(depmunic) %>% st_sf()
qtm(map)

# criando um raster cobrindo o mapa
grid <- terra::rast(map, nrows = 100, ncols = 100)

# obtendo as coordenadas das celulas
xy <- terra::xyFromCell(grid, 1:ncell(grid)) %>% as.data.frame()

# convertendo as coordenadas em sf
coop <- st_as_sf(xy, coords = c("x", "y"), crs = st_crs(map))
qtm(coop)

# filtrando apenas os pontos que estao dentro do contorno
coop <- st_filter(coop, map)
qtm(coop)

# criando o modelo de idw
model <- gstat(formula = vble ~ 1, locations = d, nmax = 3,
             set = list(idp = 1))

# criando um objeto com a previsao nos pontos
resp <- predict(model, coop)

# transfere os valores previstos para a camada raster
pred <- terra::rasterize(resp, grid, field = "var1.pred", fun = "mean")
tm_shape(pred) + tm_raster(alpha = 0.6, palette = "viridis")

# plota a camada criada com ggplot2
ggplot() + geom_spatraster(data = pred)

# plota a camada criada em um mapa leaflet
leaflet() %>% addTiles() %>% addRasterImage(pred)
