# #30DayMapChallenge | November 2023 - Day 01 | theme: points
# Data source: UNHCR CORE Map services

# load packages -----------------------------------------------------------

library(tidyverse)
library(unhcrthemes)
library(sf)
library(httr)
library(esri2sf)
library(ragg)

# setup -------------------------------------------------------------------

poly_url <- "https://gis.unhcr.org/arcgis/rest/services/core_v2/wrl_polbnd_int_15m_a_unhcr/FeatureServer/0"
line_url <- "https://gis.unhcr.org/arcgis/rest/services/core_v2/wrl_polbnd_int_15m_l_unhcr/FeatureServer/0"
# loc_url <- "https://gis.unhcr.org/arcgis/rest/services/core_v2/wrl_prp_p_unhcr_PoC/FeatureServer/0"


# load data ---------------------------------------------------------------

# load polygons and lines using esri2sf
poly_sf <- esri2sf::esri2sf(poly_url, replaceDomainInfo = FALSE)
line_sf <- esri2sf::esri2sf(line_url, replaceDomainInfo = FALSE)
# loc_sf <- esri2sf::esri2sf(loc_url, replaceDomainInfo = FALSE)

# load location points using httr as esri2sf throws an error
loc_url <- httr::parse_url("https://gis.unhcr.org/arcgis/rest/services")
loc_url$path <- paste(loc_url$path,
    "core_v2/wrl_prp_p_unhcr_PoC/FeatureServer/0/query",
    sep = "/"
)
loc_url$query <- list(
    where = "1=1",
    outFields = "*",
    returnGeometry = "true",
    f = "geojson"
)
loc_request <- httr::build_url(loc_url)

loc_sf <- st_read(loc_request)


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

# filter pop_type == 52 (refugee)
# filter out loc_subtype == 42 > Dispersed?
# case_when on loc_subtype to create new factor levels
# loc_subtype classification:
# 37 (Centre) = Centre
# 38 (Planned settlement) = Settlement (Formal or Informal)
# 39 (Unplanned settlement) = Settlement (Formal or Informal)
# 40 (Spontaneous location) = Spontaneous Location
# 41 (Arrangement (hosted or rental)) = Individual accommodation in communities
# 42 (Dispersed) = Deleted see above
# 43 (Accomodation) = Individual accommodation in communities
loc_ref <- loc_sf |>
    filter(
        pop_type == 52,
        loc_subtype != 42
    ) |>
    mutate(
        loc_subtype = case_when(
            loc_subtype == 37 ~ "centre",
            loc_subtype == 38 | loc_subtype == 39 ~ "settlement",
            loc_subtype == 40 ~ "spontaneous",
            TRUE ~ "accommodation"
        ),
        loc_subtype = factor(loc_subtype,
            levels = c("settlement", "centre", "accommodation", "spontaneous")
        )
    )


# plot --------------------------------------------------------------------

# camcorder
camcorder::gg_record(
    dir = here::here("temp-plot"),
    # scale = 2,
    device = agg_png,
    width = 1200 * 2 / 300,
    height = 1000 * 2 / 300,
    units = "in"
)

# colors
map_colors <- c(polygons = "#eae6e4", lines = "#a8aaad")

loc_colors <- c(
    "settlement" = unname(unhcrthemes::unhcr_pal(n = 1, name = "pal_blue")),
    "centre" = unname(unhcrthemes::unhcr_pal(n = 1, name = "pal_navy")),
    "accommodation" = unname(unhcrthemes::unhcr_pal(n = 1, name = "pal_green")),
    "spontaneous" = unname(unhcrthemes::unhcr_pal(n = 1, name = "pal_red"))
)

# text
plot_title <- "Refugee locations in the world"

plot_subtitle <- "The descriptions below present a broad, simplified view of refugee accommodations globally, with actual situations varying by context."

plot_caption <- "<em>The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the United Nations.</em> <strong>Source:</strong> UNHCR Core GIS Data (as of 16 Nov. 2023)"

loc_label <- c(
    "settlement" = paste0(
        "<span style=\"margin-bottom:5px; font-size: 10pt; color:",
        loc_colors["settlement"],
        ";\">**Settlement (Formal and Informal)**</span><br>",
        "Formal settlements are officially designated areas for asylum seekers, refugees, or IDPs, while informal settlements are spontaneous, self-identified settlements without official allocation or negotiations."
    ),
    "centre" = paste0(
        "<span style=\"font-size: 10pt; color:",
        loc_colors["centre"],
        ";\">**Centre**</span><br>",
        "A centre is often used at the beginning of a new emergency to temporarily host high influxes of refugees, pending transfer to a suitable formal settlement or accommodation."
    ),
    "accommodation" = paste0(
        "<span style=\"font-size: 10pt; color:",
        loc_colors["accommodation"],
        ";\">**Individual accommodation in communities**</span><br>",
        "People living in individual housing or with host families in cities, towns, villages."
    ),
    "spontaneous" = paste0(
        "<span style=\"font-size: 10pt; color:",
        loc_colors["spontaneous"],
        ";\">**Spontaneous location**</span><br>",
        "When people cross the border and settle in small groups along the border, often these people are moved to a transit centre, informal or formal settlement."
    )
)

# plot

plot <-
    ggplot() +
    geom_sf(
        data = poly_sf,
        fill = map_colors["polygons"],
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
        data = loc_ref,
        aes(
            color = loc_subtype,
        ),
        shape = 1,
        alpha = .5,
        size = 1,
        stroke = .2,
        show.legend = FALSE
    ) +
    scale_linetype_identity() +
    scale_color_manual(values = loc_colors) +
    coord_sf(
        crs = "+proj=robin",
        default_crs = sf::st_crs(4326),
        xlim = c(-130, 158)
    ) +
    labs(
        title = plot_title,
        subtitle = plot_subtitle,
        caption = plot_caption
    ) +
    facet_wrap(~loc_subtype,
        labeller = labeller(loc_subtype = loc_label)
    ) +
    theme_unhcr(
        void = TRUE,
        plot_title_margin = 5,
        subtitle_size = 10
    ) +
    theme(
        strip.text = ggtext::element_textbox_simple(
            margin = margin(b = 15, unit = "pt"),
            color = "#1a1a1a",
            face = "plain",
            size = 8.5,
            hjust = 0,
            vjust = 1,
            lineheight = 1.15,
            # minheight = unit(50, "pt"),
        ),
        plot.caption = ggtext::element_textbox_simple(
            maxwidth = unit(.75, "npc"),
            hjust = 0,
            vjust = 0,
            margin = margin(t = 12, unit = "pt"),
        ),
        panel.spacing.x = unit(16, "pt")
    )


# save --------------------------------------------------------------------
# stop recording
camcorder::gg_stop_recording()

path <- here::here("01_points", "/")

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

magick::image_write(plot_wlogo, path = paste0(path, "01_points.png"), format = "png")
