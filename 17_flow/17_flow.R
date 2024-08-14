# #30DayMapChallenge | November 2023 - Day 17 | theme: flow
# Data source: UNHCR CORE Map services, Refugee Data Finder

# load packages -----------------------------------------------------------

library(tidyverse)
library(unhcrthemes)
library(refugees)
library(sf)
library(esri2sf)
library(ragg)

# setup -------------------------------------------------------------------

poly_url <- "https://gis.unhcr.org/arcgis/rest/services/core_v2/wrl_polbnd_int_15m_a_unhcr/FeatureServer/0"
line_url <- "https://gis.unhcr.org/arcgis/rest/services/core_v2/wrl_polbnd_int_15m_l_unhcr/FeatureServer/0"


# load data ---------------------------------------------------------------

# load polygons and lines using esri2sf
poly_sf <- esri2sf::esri2sf(poly_url, replaceDomainInfo = FALSE)
line_sf <- esri2sf::esri2sf(line_url, replaceDomainInfo = FALSE)

# load flows from Ukraine in 2022
ukr_flows_2022 <- refugees::flows |>
    filter(
        year == 2022,
        coo_iso == "UKR"
    ) |>
    select(-year, -returned_refugees, -coo, -coa)

# load country coordinates
country_coords <- read_csv("17_flow/wrl_polbnd_int_1m_p_unhcr.csv")

# load country region
country_region <- refugees::countries |>
    select(iso_code, unhcr_region)

# wrangle -----------------------------------------------------------------

# remove antarctica
poly_sf <- poly_sf |>
    filter(iso3 != "ATA")

# include line type for cartography
line_sf <- line_sf |>
    mutate(
        line_type = case_when(
            cartography == "International boundary line" ~ "solid",
            cartography == "Dashed boundary line" ~ "dashed",
            cartography == "Boundary of former Palestinian mandate" ~ "dotdash",
            TRUE ~ "dotted"
        ),
        line_type = factor(line_type, levels = c("solid", "dashed", "dotted", "dotdash"))
    )

# pivot_longer pop categories
# group_by coo and coa
# summarize total pop
# create id for each country pair
# pivot_longer to get orgasy and iso3
# join to get coordinates
# create sf object
# group_by id
# summarize mean pop_num
# create line from mutlipoint
flow_sf <- ukr_flows_2022 |>
    pivot_longer(
        cols = 5:7,
        names_to = "pop_type",
        values_to = "pop_num"
    ) |>
    group_by(coo_iso, coa_iso) |>
    summarize(
        pop_num = sum(pop_num, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
        id = paste0(coo_iso, "-", coa_iso)
    ) |>
    pivot_longer(
        cols = 1:2,
        names_to = "orgasy",
        values_to = "iso3"
    ) |>
    left_join(country_coords, by = "iso3") |>
    mutate(
        lon = if_else(iso3 == "RUS", 42, lon),
        lat = if_else(iso3 == "RUS", 53, lat)
    ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    group_by(id) |>
    summarize(
        pop_num = mean(pop_num, na.rm = TRUE)
    ) |>
    mutate(
        coa_iso = str_sub(id, 5, 7),
    ) |>
    left_join(country_region, by = c("coa_iso" = "iso_code")) |>
    st_cast("LINESTRING") |>
    st_segmentize(dfMaxLength = 20000) |>
    mutate(highlight = pop_num >= 750000) |>
    arrange(pop_num)


# plot --------------------------------------------------------------------

# camcorder
camcorder::gg_record(
    dir = here::here("temp-plot"),
    # scale = 2,
    device = agg_png,
    width = 1080 * 2 / 300,
    height = 1080 * 2 / 300,
    units = "in"
)

# colors
map_colors <- c(
    polygons = "#eae6e4", lines = "#a8aaad",
    water = "#e1eef9"
)

# text
plot_title <- "Refugee and Asylum-seekerd flows from Ukraine in 2022"

# plot_subtitle <- "The descriptions below present a broad, simplified view of refugee accommodations globally, with actual situations varying by context."

plot_caption <- "<em>The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the United Nations.</em><br><strong>Note:</strong> Only flows greater than 5,000 people are represented on the map. <strong>Source:</strong> UNHCR Refugee Data Finder"


# plot

# plot <-
ggplot() +
    geom_sf(
        data = poly_sf,
        fill = if_else(poly_sf$iso3 == "UKR", map_colors["polygons"], "white"),
        color = NA
    ) +
    geom_sf(
        data = line_sf,
        aes(linetype = line_type),
        color = map_colors["lines"],
        linewidth = .2,
        show.legend = FALSE
    ) +
    geom_sf(
        data = filter(flow_sf, pop_num >= 5000),
        aes(
            linewidth = pop_num,
            color = highlight
        ),
        # color = "#0072bc",
        alpha = .2
    ) +
    scale_linewidth(
        range = c(.1, 20),
        breaks = c(25000, 250000, 750000),
        labels = c("25k", "250k", "750k")
    ) +
    scale_color_manual(
        values = c("TRUE" = "#FF8490", "FALSE" = "#0072bc")
    ) +
    # scale_color_unhcr_c(
    #     palette = "pal_red",
    # ) +
    # scale_alpha_manual(
    #     values = c(.1, .6),
    #     guide = FALSE
    # ) +
    coord_sf(
        crs = "+proj=moll",
        default_crs = sf::st_crs(4326),
        xlim = c(-10, 44),
        ylim = c(35, 70)
    ) +
    labs(
        title = plot_title,
        caption = plot_caption,
        linewidth = "Number of people",
        color = NULL
    ) +
    theme_unhcr(void = TRUE, legend_title = TRUE) +
    theme(
        panel.background = element_rect(
            fill = alpha(map_colors["water"], .5),
            color = NA
        ),
        legend.position = c(0, .94),
        legend.direction = "horizontal",
        legend.background = element_rect(
            fill = alpha("white", .8),
            color = NA
        ),
        legend.key.height = unit(1, "cm"),
    ) +
    guides(
        linewidth = guide_legend(title.position = "top", title.hjust = 0)
    )



# save --------------------------------------------------------------------
# stop recording
camcorder::gg_stop_recording()

path <- here::here("17_flow", "/")

# base plot
ggsave(paste0(path, "plot.png"),
    plot = plot, device = agg_png, dpi = 300,
    width = 2400 / 300, height = 2000 / 300, units = "in"
)

# load logo and chart
chart <- magick::image_read(paste0(path, "plot.png"))
logo <- magick::image_read_svg("https://raw.githubusercontent.com/vidonne/unhcrdesign/master/inst/resources/logo/unhcr_logo_blue.svg",
    height = 100
)

# put chart and logo together
plot_wlogo <- chart |>
    magick::image_composite(logo,
        gravity = "SouthEast",
        offset = "+80+40"
    )

magick::image_write(plot_wlogo, path = paste0(path, "17_flow.png"), format = "png")
