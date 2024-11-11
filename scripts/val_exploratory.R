# """
# This script generates Figures S6, which presents exploratory data analysis plots.
# """

library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

source("pkg/utilities.R")

load("data/sample_information.RData")

if (!exists("beverage_sales"))
{
  beverage_sales <- readRDS("data/sample_sales.RDS")
  warning("FileNotFound: R Object 'beverage_sales' not found, loading sample data.")
}

longtab <- as.data.frame(beverage_sales, row.names = FALSE) %>%
  mutate(date = dates, .before = 1) %>%
  pivot_longer(
    cols = !date,
    names_to = "zip3",
    values_to = "beverage_sales"
  ) %>%
  mutate(
    group = ifelse(zip3 == "191", "treated", "control"),
    location = ifelse(d[zip3] > 0, "non-bordering", "bordering"),
    category = ifelse(group == "treated", group, location),
    .after = zip3
  )

grouptab <- longtab %>%
  summarise(
    beverage_sales = mean(beverage_sales, na.rm = TRUE),
    .by = c(date, category)
  )

# figure S4, panel (a): trajectory of relative volume sales
p1 <- ggplot(longtab, mapping = aes(x = date, y = beverage_sales, color = group, group = zip3)) +
  geom_vline(xintercept = dates[T0], linetype = "dashed") +
  geom_line() +
  scale_color_manual(
    name = "",
    breaks = c("control", "treated"),
    labels = c("Control Unit, Three-Digit ZIP Code Area", "Treated Unit, Philadelphia"),
    values = c("#d9d9d9", "#8da0cb")
  ) +
  scale_x_date() +
  scale_y_continuous(limits = c(0, NA), labels = comma) +
  labs(
    x = TeX("Aggregated Four-Week Sales Reporting Period, $t$"),
    y = TeX("Relative Volume Sales in Ounces, $Y_{it}$")
  ) +
  theme_bw(base_size = 12) +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_text(margin = margin(12, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 12, 0, 0)),
    legend.position = "right"
  )

# figure S4, panel (b): trajectory of relative volume sales, grouped by location
p2 <- ggplot(grouptab, mapping = aes(x = date, y = beverage_sales, color = category)) +
  geom_vline(xintercept = dates[T0], linetype = "dashed") +
  geom_line() +
  scale_color_brewer(
    name = "",
    breaks = c("bordering", "non-bordering", "treated"),
    labels = c("Border 3-Digit ZIP Code Areas", "Non-Border 3-Digit ZIP Code Areas", "Philadelphia"),
    palette = "Set2"
  ) +
  scale_x_date() +
  scale_y_continuous(limits = c(0, NA), labels = comma) +
  labs(
    x = TeX("Aggregated Four-Week Sales Reporting Period, $t$"),
    y = TeX("Average Relative Volume Sales in Ounces")
  ) +
  theme_bw(base_size = 12) +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_text(margin = margin(12, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 12, 0, 0)),
    legend.position = "right"
  )

save_plot("sfigs/sfig06.pdf", plot = arrangeGrob(p1, p2, ncol = 1), width = 12, height = 10)
