library(ggplot2)
library(tidyverse)
library(ggtext)
library(rlang)
library(patchwork)
library(cowplot)
# library(gridExtra)

element_textbox()

chic <- readr::read_csv("D:\\root\\Application\\R-4.2.2\\tests\\Rtest.csv")
tibble::glimpse(chic) # 读取数据
head(chic, 20)

g <- ggplot(chic, aes(x = date, y = temp)) +
    geom_point(color = "firebrick") +
    labs(
        x = "Year", y = "Temperature (°F)",
        title = "Temperatures in Chicago",
        subtitle = "Daily temperatures in °F from 1997 to 2001"
    ) +
    theme(
        plot.title = element_text(family = "myFont", hjust = .5, size = 25),
        plot.subtitle = element_text(family = "myFont", hjust = .5, size = 15)
    )
plot(g)

ggplot(
    chic,
    aes(x = date, y = temp)
) +
    geom_point(color = season) +
    labs(x = "Year", y = "Temperature (°F)")

ggplot(chic, aes(x = date, y = o3)) +
    geom_line(aes(color = "line")) +
    geom_point(aes(color = "points")) +
    labs(x = "Year", y = "Ozone") +
    scale_color_discrete("Type:")

ggplot(chic, aes(x = date, y = temp)) +
    geom_point(color = "firebrick") +
    labs(x = "Year", y = "Temperature (°F)") +
    scale_y_continuous(
        breaks = seq(0, 100, 10),
        minor_breaks = seq(0, 100, 2.5)
    ) +
    scale_x_continuous(
        breaks = seq(0, 20, 3),
        minor_breaks = seq(0, 20, 1)
    )

g + facet_wrap(~season, nrow = 4, scales = "free_x") +
    theme(
        strip.background = element_blank(),
        strip.text = element_textbox_highlight(
            family = "Playfair", size = 12, face = "bold",
            fill = "white", box.color = "chartreuse4", color = "chartreuse4",
            halign = .5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
            padding = margin(5, 0, 3, 0), margin = margin(0, 1, 3, 1),
            hi.labels = c("1997", "1998", "1999", "2000"),
            hi.fill = "chartreuse4", hi.box.col = "black", hi.col = "white"
        )
    )
element_textbox_highlight <- function(..., hi.labels = NULL, hi.fill = NULL,
                                      hi.col = NULL, hi.box.col = NULL, hi.family = NULL) {
    structure(
        c(
            element_textbox(...),
            list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col, hi.family = hi.family) # nolint
        ),
        class = c("element_textbox_highlight", "element_textbox", "element_text", "element") # nolint
    )
}


element_grob.element_textbox_highlight <- function(element, label = "", ...) { # nolint
    if (label %in% element$hi.labels) {
        element$fill <- element$hi.fill %||% element$fill
        element$colour <- element$hi.col %||% element$colour
        element$box.colour <- element$hi.box.col %||% element$box.colour
        element$family <- element$hi.family %||% element$family
    }
    NextMethod()
}

ggplot(chic, aes(x = date, y = temp)) +
    geom_point(
        shape = 21, size = 2, stroke = 1,
        color = "#3cc08f", fill = "#c08f3c"
    ) +
    labs(x = "Year", y = "Temperature (°F)")

g + theme_bw(base_family = "Playfair")

ggplot(sample, aes(x = date, y = temp, color = season)) +
    geom_point() +
    geom_label(aes(label = season), hjust = .5, vjust = .5) +
    labs(x = "Year", y = "Temperature (°F)") +
    xlim(as.Date(c("1997-01-01", "2000-12-31"))) +
    ylim(c(0, 90)) +
    theme(legend.position = "none")

p <- chic %>%
    mutate(season = fct_reorder(season, temp)) %>% # Reorder data
    ggplot(aes(x = season, y = temp, fill = temp, color = temp)) +
    geom_violin(width = 2.1, size = 0.2) +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) + # nolint
    theme_ipsum() +
    theme(
        legend.position = "none"
    ) +
    coord_flip() + # This switch X and Y axis and allows to get the horizontal version # nolint
    xlab("Temp") +
    ylab("season")
