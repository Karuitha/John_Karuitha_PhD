## Load required packages ----
if (!require(pacman)) {
        install.packages("pacman")
}

pacman::p_load(
        tidyverse, tidymodels, ggthemes, janitor,
        rvest, countrycode, readxl, maps, mapproj
)

## load data ----
europenet <- read_xlsx("Average Internet Speeds Across Europe.xlsx") %>%
        clean_names() %>%
        mutate(country_en = countrycode(
                sourcevar = country,
                origin = "iso2c",
                destination = "country.name"
        )) %>%
        relocate(country_en, .after = country) %>%
        mutate(country_en = case_when(
                country == "EL" ~ "Greece",
                country == "BIH" ~ "Bosnia and Herzegovina",
                country == "MDA" ~ "Moldova",
                country == "UK" ~ "United Kingdom",
                country == "XKO" ~ "Kosovo",
                TRUE ~ country_en
        ))


full_data <- map_data("world") %>%
        inner_join(europenet, by = c("region" = "country_en"))


## Create a base map ----
full_data %>%
        ggplot(aes(
                x = long, y = lat, map_id = region,
                fill = average_download_speed
        )) +
        geom_map(
                map = full_data,
                color = "gray80",
                linewidth = 0.2
        ) +
        labs(x = "", y = "", title = "Average Internet Download Speeds") +
        coord_map("ortho", orientation = c(55, 16, 0)) +
        theme_minimal() +
        theme(
                legend.title = element_blank(),
                legend.position = "bottom",
                legend.text = element_text(size = 5, face = "italic"),
                axis.text = element_blank()
        ) +
        scale_fill_gradient_tableau(palette = "Blue-Green Sequential")

