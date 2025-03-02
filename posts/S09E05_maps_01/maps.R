# Load packages
library(ggmap)
library(sf)
library(tidyverse)
library(ggrepel)

# Set theme
theme_set(theme_minimal())

# Read the shapefile
nc_file <- system.file("shape/nc.shp", package = "sf")
nc <- st_read(nc_file)

ggplot(data = nc) +
  geom_sf()

ggplot(data = nc) +
  geom_sf(aes(fill = AREA)) +
  scale_fill_viridis_c()

ggplot(data = nc) +
  geom_sf_interactive(aes(fill = AREA, tooltip = AREA)) +
  scale_fill_viridis_c()

ggplot(data = nc) +
  geom_sf(aes(fill = BIR74)) +
  scale_fill_viridis_c()

nc_cities <- us.cities |>
  filter(country.etc == "NC") |>
  mutate(name = sub(" NC", "", name))

ggplot() +
  geom_sf(data = nc, aes(fill = BIR74 / 1000)) +
  geom_point(
    data = nc_cities |> filter(pop > 250000),
    aes(x = long, y = lat),
    ) +
  geom_label(
    data = nc_cities |> filter(pop > 250000),
    aes(x = long, y = lat - 0.2, label = name),
    fill = alpha("grey80", 0.7)
    ) +
  scale_fill_viridis_c(name = "Number\nof births\n(thousands)") +
  theme_void() +
  theme(legend.position = "top")
  
ggplot() +
  geom_sf(data = nc, fill = "grey95") +
  geom_point(data = nc_cities,
             aes(x = long, y = lat, size = pop),
             fill = "skyblue", shape = 21) +
  geom_text_repel(data = nc_cities,
                  aes(x = long, y = lat, label = name)) +
  scale_fill_viridis_c(name = "Number\nof births\n(thousands)") +
  scale_size_continuous(name = "Population size") +
  theme_void() +
  theme(legend.position = "top")

# STATEWISE --------------------------------------------------------------------
states <- map_data("state")

ggplot(states, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group)) +
  coord_sf()

capitals <- subset(us.cities, capital == 2)
capitals_notAKHI <- capitals[!(capitals$country.etc %in% "AK" | capitals$country.etc %in% "HI"), ] #exclude Alaska and Hawaii
capitals_notAKHI$city <- sub(' [^ ]*$','',capitals_notAKHI$name) # split out city for the label

ggplot() +
  geom_polygon(
    data = states,
    aes(x = long, y = lat, group = group),
    fill = "grey80", color = "grey20", size = 0.1,
    ) +
  geom_label_repel(
    data = capitals_notAKHI,
    aes(x = long, y = lat, label = city),
    size = 2) +
  geom_point(
    data = capitals_notAKHI,
    aes(x = long, y = lat),
    fill = "blue",
    shape = 21
    ) +
  coord_sf() +
  theme_void()

library(usdata)
state_stats

state_stats$state[!tolower(state_stats$state) %in% states$region]

states2 <- tibble(states) |>
  rename(state = region) |>
  left_join(
    state_stats |> mutate(state = tolower(state)),
    by = "state"
    )

ggplot() +
  geom_polygon(
    data = states2,
    aes(x = long, y = lat, group = group, fill = income / 1000),
    color = "grey20", size = 0.1,
  ) +
  coord_sf() +
  scale_fill_viridis_c() +
  theme_void() +
  theme(legend.position = "top")

# OHIO -------------------------------------------------------------------------
# ============================================================
county_df <- map_data("county") |>
  rename(state = region, county = subregion)
head(county)

county_mapdata <- county |>
  rename(county = name) |> 
  mutate(county = tolower(sub(" County", "", county)),
         state = tolower(state)) |>
  right_join(county_df, by = c("county", "state"))
ohio <- county_mapdata |> filter(state == "ohio")

ggplot(ohio) +
  aes(x = long, y = lat, group = group, fill = pop2017) +
  geom_polygon(colour = alpha("white", 0.1)) + 
  theme_void()


# ============================================================
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group))

# County map
county_df <- map_data("county") #projection = "albers", parameters = c(39, 45))
county_df <- county_df |> left_join(unemp, by = c("subregion" = "county"))
ggplot(county_df) +
  aes(long, lat, group = group, fill = rate) +
  geom_polygon(colour = alpha("white", 0.1)) + 
  theme_void()

data.frame(state.abb, state.name)


states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
counties <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))

counties |> filter(grepl("ohio,", ID)) |>
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = ID)) +
  coord_sf()

### Interactive maps with _ggiraph_

if (! require(ggiraph)) install.packages("ggiraph")
library(ggiraph)

p <- state_map |>
  filter(state %in% c("Ohio", "Indiana", "Michigan")) |> 
  ggplot() +
  geom_polygon_interactive(
    aes(x = long, y = lat, group = group, fill = state, tooltip = state),
    color = "grey40"
  ) +
  coord_sf()

girafe(ggobj = p)

