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
    )

data

n_clients <- data %>%
    filter(server == "Primary") %>%
    group_by(n_warehouses, n_clients) %>%
    summarize(
        med_throughput_tps = median(throughput_tps)
    ) %>%
    group_by(n_warehouses) %>%
    mutate(
        max_med_throughput_tps = max(med_throughput_tps)
    ) %>%
    filter(med_throughput_tps == max_med_throughput_tps) %>%
    select(n_warehouses, n_clients)

n_clients

primary <- data %>%
    filter(server == "Primary") %>%
    semi_join(n_clients)

primary

n_workers <- data %>%
    filter(server %in% c("CopyCat", "KuaFu")) %>%
    group_by(server, n_warehouses, n_workers) %>%
    summarize(
        med_throughput_tps = median(throughput_tps)
    ) %>%
    group_by(server, n_warehouses) %>%
    mutate(
        max_med_throughput_tps = max(med_throughput_tps)
    ) %>%
    filter(med_throughput_tps == max_med_throughput_tps) %>%
    select(server, n_warehouses, n_workers)

n_workers

n_workers_restricted <- n_workers %>%
    left_join(n_clients, by = "n_warehouses") %>%
    mutate(
        n_workers = min(n_workers, n_clients)
    ) %>%
    select(server, n_warehouses, n_workers)

n_workers_restricted

backup <- data %>%
    filter(server %in% c("CopyCat", "KuaFu")) %>%
    semi_join(n_workers_restricted)

backup

data <- bind_rows(primary, backup) %>%
    mutate(
        n_warehouses = factor(n_warehouses),
    ) %>%
    group_by(server, n_warehouses) %>%
    summarize(
        min_throughput_tps = min(throughput_tps),
        med_throughput_tps = median(throughput_tps),
        max_throughput_tps = max(throughput_tps)
    )

data

one_warehouse <- filter(data, n_warehouses == 1)

barwidth <- 0.9
errorwidth <- 0.5

p <- ggplot(
    one_warehouse,
    aes(
        x = n_warehouses,
        y = med_throughput_tps,
        ymin = min_throughput_tps,
        ymax = max_throughput_tps,
        fill = server
    )
) +
    # geom_line(size = 1) +
    # geom_point(size = 2) +
    geom_col(
        position = position_dodge(width = barwidth),
        color = "black", size = 1
    ) +
    geom_errorbar(
        position = position_dodge(width = barwidth),
        width = errorwidth, size = 1
    ) +
    # scale_x_discrete(
    #     limits = c(0, 20.5),
    #     # breaks = c(1, 2, 4, 8, 16, 20),
    #     # labels = label_number_si(),
    #     expand = c(0, 0),
    # ) +
    scale_y_continuous(
        limits = c(0, 1200000),
        breaks = pretty_breaks(n = 5),
        labels = comma,
        expand = c(0, 0),
    ) +
    scale_fill_brewer(type = "div", palette = "Paired") +
    labs(
        x = "Warehouse Count",
        y = "Throughput (transactions/s)",
        fill = ""
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