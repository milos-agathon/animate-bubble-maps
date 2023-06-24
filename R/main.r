###################################################
#                 Let's animate bubble maps with R
#                 Milos Popovic
#                 2023/06/24
###################################################
# libraries we need
libs <- c(
    "tidyverse", "giscoR",
    "sf", "ggmap",
    "gganimate", "classInt"
)

# install missing libraries
installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

# load libraries

invisible(lapply(libs, library, character.only = T))

# 1. DATA
#-----------
url <- "https://ucdp.uu.se/downloads/ged/ged231-csv.zip"
file_name <- "ucdp-ged.zip"

download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
)

unzip(file_name)

ucdp_ged <- read.csv(
    "GEDEvent_v23_1.csv",
    sep = ","
)

names(ucdp_ged)

# Syria dataframe
unique(ucdp_ged$country)
ucdp_ged_syria <- ucdp_ged |>
    dplyr::filter(
        country == "Syria"
    ) |>
    dplyr::select(
        26, 30:31, 39,
        43
    ) |>
    dplyr::mutate(
        time = as.Date(
            date_start
        )
    ) |>
    dplyr::filter(
        time >= as.Date(
            "2011-03-15"
        )
    ) |>
    dplyr::mutate(
        month_year = as.factor(format(
            as.Date(time), "%Y-%m"
        ))
    )


summary(ucdp_ged_syria)
levels(ucdp_ged_syria$month_year)

# 2. AGGREGATE MONTHLY
#---------------------

ucdp_ged_syria_monthly <- ucdp_ged_syria |>
    dplyr::group_by(
        where_coordinates,
        latitude,
        longitude,
        month_year
    ) |>
    dplyr::summarise(
        sum_violence = sum(
            deaths_civilians
        )
    ) |>
    dplyr::mutate(
        sum_violence = replace(
            sum_violence,
            sum_violence == 0,
            NA
        )
    )

head(ucdp_ged_syria_monthly)
summary(ucdp_ged_syria_monthly$sum_violence)

# 3. BREAKS
#----------

breaks <- classInt::classIntervals(
    ucdp_ged_syria_monthly$sum_violence,
    n = 6,
    style = "fisher"
)$brks

vmin <- min(
    ucdp_ged_syria_monthly$sum_violence,
    na.rm = T
)

vmax <- max(
    ucdp_ged_syria_monthly$sum_violence,
    na.rm = T
)

# 4. SYRIA LAYER
#---------------
# load national map of Syria
syria_bbox <- giscoR::gisco_get_countries(
    epsg = "4326",
    resolution = "1",
    country = "SY",
    cache = T,
    update_cache = T
) |>
    sf::st_bbox() # get bounding box


syria_coords <- c(
    syria_bbox[["xmin"]], 
    syria_bbox[["ymin"]],
    syria_bbox[["xmax"]],
    syria_bbox[["ymax"]]
)

syria <- ggmap::get_stamenmap(
    syria_coords,
    zoom = 8,
    maptype = "toner-2011"
)

# ggmap(syria)

# 5. STATIC VIOLENCE MAP
#-----------------------
get_syria_violence_monthly_map <- function() {
    map_monthly <-
        ggmap(syria) +
        geom_point(
            data = ucdp_ged_syria_monthly,
            aes(
                x = longitude,
                y = latitude, size = sum_violence
            ),
            color = "#FF0899",
            alpha = 1
        ) +
        scale_size(
            breaks = round(breaks, 0),
            range = c(1, 12),
            limits = c(vmin, vmax),
            name = "Civilian deaths"
        ) +
        guides(
            size = guide_legend(
                direction = "vertical",
                title.position = "top",
                label.position = "right",
                title.hjust = .5,
                label.hjust = 0,
                ncol = 1,
                byrow = T
            )
        ) +
        theme_void() +
        theme(
            legend.position = "right",
            legend.text = element_text(
                size = 11, color = "grey10"
            ),
            legend.title = element_text(
                size = 12, color = "grey10"
            ),
            plot.title = element_text(
                size = 20, color = "grey10",
                hjust = .5, vjust = 5
            ),
            plot.subtitle = element_text(
                size = 40, color = "#FF0899",
                hjust = .5, vjust = 3
            ),
            plot.caption = element_text(
                size = 10, color = "grey10",
                hjust = .5, vjust = -10
            ),
            plot.margin = unit(
                c(t = 0, r = 3, b = 0, l = 0),
                "lines"
            )
        ) +
        labs(
            x = "",
            y = "",
            title = "Civilian deaths in Syrian Civil war",
            caption = "UCDP GED v. 20.1",
            subtitle = "{as.factor(closest_state)}"
        )
    return(map_monthly)
}

map_monthly <- get_syria_violence_monthly_map()
print(map_monthly)

# 6. ANIMATE - MONTHLY
#---------------------
timelapse_map_monthly <- map_monthly +
    gganimate::transition_states(
        month_year,
        transition_length = 3,
        state_length = 1
    ) +
    enter_fade() +
    exit_fade() +
    ease_aes("linear", interval = .2)

animated_map_monthly <- gganimate::animate(
    timelapse_map_monthly,
    nframes = 150,
    duration = 20,
    start_pause = 3,
    end_pause = 30,
    res = 300,
    units = "in",
    width = 7,
    height = 7,
    fps = 15,
    renderer = gifski_renderer(loop = T)
)
