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

parser$add_argument("-i", "--csv",
    type = "character",
    help = "csv file for plot"
)
parser$add_argument("-o", "--out",
    type = "character",
    help = "out file for plot"
)

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
            impl == "MICA_CCC_NONE" ~ "Primary",
            impl == "MICA_CCC_COPYCAT" & server == "primary" ~ "Primary-Log",
            impl == "MICA_CCC_COPYCAT" & server == "backup" ~ "CopyCat",
            impl == "MICA_CCC_KUAFU" & server == "primary" ~ "Primary-Log",
            impl == "MICA_CCC_KUAFU" & server == "backup" ~ "KuaFu"
        ),
        impl = NULL,
        n_inserts = factor(n_inserts),
        n_clients = factor(n_clients)
    ) %>%
    filter(server == "Primary") %>%
    group_by(server, n_inserts, n_clients) %>%
    summarize(
        min_throughput_tps = min(throughput_tps),
        med_throughput_tps = median(throughput_tps),
        max_throughput_tps = max(throughput_tps)
    )


data

barwidth <- 0.9
errorwidth <- 0.5

p <- ggplot(
    data,
    aes(
        x = n_inserts,
        y = med_throughput_tps,
        ymin = min_throughput_tps,
        ymax = max_throughput_tps,
        fill = n_clients
    )
) +
    geom_col(
        position = position_dodge(width = barwidth),
        color = "black", size = 1
    ) +
    geom_errorbar(
        position = position_dodge(width = barwidth),
        width = errorwidth, size = 1
    ) +
    scale_y_continuous(
        limits = c(0, 3050000),
        breaks = pretty_breaks(n = 4),
        labels = label_number_si(),
        expand = c(0, 0),
    ) +
    scale_fill_brewer(type = "div", palette = "Paired") +
    labs(
        x = "Inserts Per Transaction",
        y = "Throughput (transactions/s)",
        fill = "Threads"
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
        legend.margin = margin(0, 0, -15, 0),
        panel.grid.major.x = element_line(color = "grey", linetype = 2),
        panel.grid.major.y = element_line(color = "grey", linetype = 2),
        panel.spacing = unit(0, "mm"),
        plot.margin = margin(5, 5, 10, 5),
    )

width <- 10 # inches
height <- (3 / 4) * width

ggsave(out, plot = p, height = height, width = width, units = "in")