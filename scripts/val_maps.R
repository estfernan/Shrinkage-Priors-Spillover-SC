# """
# This script generates Figures S7, which presents exploratory data analysis plots
# using a map of the available ZIP3 regions.
# """

library(dplyr)
library(ggplot2)
library(gridExtra)
library(latex2exp)
library(scales)
library(sf)
library(tidyr)

source("pkg/ops.R")
source("pkg/utilities.R")

load("data/sample_information.RData")

map <- readRDS("data/zip3_map.RDS")

if (!exists("beverage_sales"))
{
  beverage_sales <- readRDS("data/sample_sales.RDS")
  warning("FileNotFound: R Object 'beverage_sales' not found, loading sample data.")
}

X  <- scale(baseline_covariates)
dC <- weighted_distance(X, d, kappa_d = 1, exclude_treated = FALSE)

wgt_dist <- as_tibble(dC, rownames = "zip3") %>%
  rename(dist = value) %>%
  mutate(dist = ifelse(zip3 == "191", NA, dist))

volume_change <- beverage_sales %>%
  as_tibble(rownames = "date") %>%
  pivot_longer(cols = !date, names_to = "zip3", values_to = "volume") %>%
  summarise(
    pre_volume = mean(volume[as.Date(date) <= dates[T0]], na.rm = TRUE),
    post_volume = mean(volume[as.Date(date) > dates[T0]], na.rm = TRUE),
    .by = zip3
  ) %>%
  transmute(zip3, change = post_volume - pre_volume) %>%
  left_join(wgt_dist, by = "zip3") %>%
  left_join(map, by = "zip3") %>%
  st_as_sf()

# figure S7, panel (a): change in average relative volume sales
p1 <- ggplot(data = volume_change, mapping = aes(fill = change)) +
  geom_sf() +
  coord_sf() +
  scale_fill_distiller(
    name = "Change in Average Relative Volume Sales",
    palette = "RdYlBu",
    direction = -1,
    labels = comma
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(nrow = 1, title.position = "top", title.hjust = 0.5))

# figure S7, panel (b): covariate similarity, weighted distances
p2 <- ggplot(data = volume_change, mapping = aes(fill = dist)) +
  geom_sf() +
  coord_sf() +
  scale_fill_distiller(
    name = TeX("Covariate Similarity: $d_{i1}^C$, $\\kappa_d=1$"),
    palette = "YlOrBr",
    direction = 1,
    labels = comma
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(nrow = 1, title.position = "top", title.hjust = 0.5))

save_plot("sfigs/sfig07.pdf", plot = arrangeGrob(p1, p2, ncol = 2), width = 10, height = 10)
