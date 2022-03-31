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
            impl == "MICA_CCC_COPYCAT" & server == "backup" ~ "C5",
            impl == "MICA_CCC_KUAFU" & server == "primary" ~ "Primary-Log",
            impl == "MICA_CCC_KUAFU" & server == "backup" ~ "KuaFu"
        ),
        impl = NULL,
        server = fct_relevel(server, c(
            "Primary", "Primary-Log",
            "C5", "KuaFu"
        ))
    )

# data

n_clients <- data %>%
    filter(server == "Primary") %>%
    group_by(n_inserts, n_clients) %>%
    summarize(
        med_throughput_tps = median(throughput_tps)
    ) %>%
    group_by(n_inserts) %>%
    mutate(
        max_med_throughput_tps = max(med_throughput_tps)
    ) %>%
    filter(med_throughput_tps == max_med_throughput_tps) %>%
    select(n_inserts, n_clients)

# n_clients

primary <- data %>%
    filter(server == "Primary") %>%
    semi_join(n_clients)

# primary

primary_med_throughput <- primary %>%
    group_by(server, n_inserts) %>%
    summarize(
        primary_med_throughput_tps = median(throughput_tps)
    ) %>%
    ungroup() %>%
    select(n_inserts, primary_med_throughput_tps)

# primary_med_throughput

n_workers <- data %>%
    filter(server %in% c("C5", "KuaFu")) %>%
    group_by(server, n_inserts, n_workers) %>%
    summarize(
        med_throughput_tps = median(throughput_tps)
    ) %>%
    group_by(server, n_inserts) %>%
    mutate(
        max_med_throughput_tps = max(med_throughput_tps)
    ) %>%
    filter(med_throughput_tps == max_med_throughput_tps) %>%
    select(server, n_inserts, n_workers)

# n_workers

n_workers_restricted <- n_workers %>%
  left_join(n_clients, by = "n_inserts") %>%
  mutate(
      n_workers = min(n_workers, n_clients)
  ) %>%
  select(server, n_inserts, n_workers)

# n_workers_restricted

backup <- data %>%
    filter(server %in% c("C5", "KuaFu")) %>%
    # semi_join(n_workers)
    semi_join(n_workers_restricted)

# backup

data <- bind_rows(primary, backup) %>%
    left_join(primary_med_throughput, by = "n_inserts") %>%
    mutate(
        throughput_tps = case_when(
            server == "KuaFu" ~ throughput_tps / (n_inserts + 1),
            server == "C5" ~ throughput_tps / (n_inserts + 1),
            TRUE ~ throughput_tps
        ),
        n_inserts = factor(n_inserts),
    ) %>%
    group_by(server, n_inserts) %>%
    summarize(
        min_throughput_tps = min(throughput_tps) / primary_med_throughput_tps,
        med_throughput_tps = median(throughput_tps) / primary_med_throughput_tps,
        max_throughput_tps = max(throughput_tps) / primary_med_throughput_tps
    )

data %>%
    filter(n_inserts == 128)

barwidth <- 0.9
errorwidth <- 0.5

p <- ggplot(
    data,
    aes(
        x = n_inserts,
        y = med_throughput_tps,
        ymin = min_throughput_tps,
        ymax = max_throughput_tps,
        fill = server
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
        limits = c(0, 2.60),
        breaks = pretty_breaks(n = 6),
        # labels = label_number_si(),
        expand = c(0, 0),
    ) +
    scale_fill_brewer(type = "div", palette = "Paired") +
    labs(
        x = "Inserts Per Transaction",
        y = "Relative Throughput",
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