#!/usr/bin/env Rscript
library("argparse")
library("dplyr")
library("forcats")
library("ggplot2")
library("ggsci")
library("readr")
library("scales")
library("tidyr")

parser <- ArgumentParser()

parser$add_argument("-i", "--csv", type = "character", help = "csv file for plot")
parser$add_argument("-o", "--out", type = "character", help = "out file for plot")

args <- parser$parse_args()

csv <- args$csv
out <- args$out

if (is.null(csv) || is.null(out)) {
    parser$print_help()
    quit()
}

data <- read_csv(csv)

data

data <- data %>%
    mutate(
        server = case_when(
            ccc == "MICA_CCC_NONE" ~ "Primary",
            ccc == "MICA_CCC_COPYCAT" & server == "primary" ~ "Primary-Log",
            ccc == "MICA_CCC_COPYCAT" & server == "backup" ~ "CopyCat",
            ccc == "MICA_CCC_KUAFU" & server == "primary" ~ "Primary-Log",
            ccc == "MICA_CCC_KUAFU" & server == "backup" ~ "KuaFu"
        ),
        ccc = NULL,
        server = fct_relevel(server, c(
            "Primary", "Primary-Log",
            "CopyCat", "KuaFu"
        ))
    ) %>% filter(n_districts == 4)

# data

primary <- data %>%
    filter(server == "Primary") %>%
    mutate(n_threads = n_clients)

# primary

backup <- data %>%
    filter(server %in% c("CopyCat", "KuaFu")) %>%
    mutate(n_threads = n_workers)

# backup

data <- bind_rows(primary, backup) %>%
    group_by(server, n_warehouses, n_threads) %>%
    summarize(
        min_throughput_tps = min(throughput_tps),
        med_throughput_tps = median(throughput_tps),
        max_throughput_tps = max(throughput_tps)
    ) %>%
    select(
        server,
        n_warehouses,
        n_threads,
        min_throughput_tps,
        med_throughput_tps,
        max_throughput_tps
    )

data

one_warehouse <- filter(data, n_warehouses == 1)

one_warehouse %>%
    group_by(server, n_warehouses) %>%
    mutate(
        max_med = max(med_throughput_tps)
    ) %>%
    ungroup() %>%
    filter(
        med_throughput_tps == max_med
    ) %>%
    select(server, n_threads, med_throughput_tps)

barwidth <- 0.9
errorwidth <- 0.5

p <- ggplot(
    one_warehouse,
    aes(
        x = n_threads,
        y = med_throughput_tps,
        ymin = min_throughput_tps,
        ymax = max_throughput_tps,
        color = server
    )
) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    # geom_col(
    #     position = position_dodge(width = barwidth),
    #     color = "black", size = 1
    # ) +
    geom_errorbar(
        # position = position_dodge(width = barwidth),
        width = errorwidth, size = 1
    ) +
    scale_x_continuous(
        limits = c(0, 20.5),
        # breaks = c(1, 2, 4, 8, 16, 20),
        # labels = label_number_si(),
        expand = c(0, 0),
    ) +
    # scale_y_continuous(
    #     limits = c(0, 1010000),
    #     breaks = pretty_breaks(n = 5),
    #     labels = comma,
    #     expand = c(0, 0),
    # ) +
    scale_color_brewer(type = "div", palette = "Paired") +
    labs(
        x = "Thread Count",
        y = "Throughput (transactions/s)",
        color = ""
    ) +
    theme_classic(
        base_size = 28,
        base_family = "serif"
    ) +
    theme(
        axis.text = element_text(size = 28, color = "black"),
        axis.title = element_text(size = 32, color = "black"),
        legend.text = element_text(size = 28, color = "black"),
        legend.title = element_text(size = 32, color = "black"),
        legend.position = "top",
        legend.justification = "left",
        legend.margin = margin(0, 0, -15, -25),
        panel.grid.major.x = element_line(color = "grey", linetype = 2),
        panel.grid.major.y = element_line(color = "grey", linetype = 2),
        panel.spacing = unit(0, "mm"),
        plot.margin = margin(5, 5, 10, 5),
    )

width <- 10 # inches
height <- (9 / 16) * width

ggsave(out, plot = p, height = height, width = width, units = "in")