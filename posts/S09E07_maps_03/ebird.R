install.packages("auk")
system.file("extdata/ebd-sample.txt", package = "auk")

install.packages("rebird")
#Ebird API key: 2vq67ivm8j0r

library(rebird)
x <- ebirdgeo(
  species = species_code("spinus tristis"),
  lat = 39.99,
  lng = -82.99,
  back = 30,
  dist = 50
  )

x <- ebirdgeo(
  species = species_code("haliaeetus leucocephalus"),
  lat = 39.99,
  lng = -82.99,
  back = 30,
  dist = 50
)
write_tsv(x, "posts/S09E07_maps_03/bald_eagle.tsv")

View(x)

mapview(
  st_as_sf(x, coords = c("lng", "lat"), crs = 4326),
  zcol = "howMany",
  label = "obsDt"
)
