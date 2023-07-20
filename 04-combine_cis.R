library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(boot)

x_tbl <- readRDS("bootstrap/x_tbl.rds")

newdata <- tibble(
    group = gl(2, nrow(x_tbl), labels = c("NA", "ASD")),
    linear = rep(x_tbl$linear, 2),
    quadradic = rep(x_tbl$quadradic, 2),
    cubic = rep(x_tbl$cubic, 2)
)
models <- readRDS("bootstrap/base_models.rds")
y_pred <- map(models, function(m, x) {
    y <- predict(m, x)
    tibble(
        value = gl(3, 29, labels = c("NA", "ASD", "diff")),
        step = rep(1:29, 3),
        vocab_size = rep(seq(20, 580, by = 20), 3),
        t0 = c(y, y[1:29] - y[30:58])
    )
}, x = newdata) %>%
    list_rbind(names_to = "metric") %>%
    mutate(metric = as.factor(metric))

labels <- c("NA", "ASD", "diff")
metrics <- c("aspl", "clust", "indegree")
conds <- expand_grid(metrics, labels)

df <- map2(conds$metrics, conds$labels, function(metric, label, x) {
    val_id <- c("NA" = 0, "ASD" = 1, diff = 2)[label]
    fname <- sprintf("boot-10000_bs-bca_conf-bonf_value-%d_metric-%s.rds", val_id, metric)
    p <- file.path("bootstrap", fname)
    lst <- readRDS(p)
    mat <- matrix(unlist(lst), nrow = 2)
    df <- bind_cols(x, tibble(metric = metric, value = label, step = 1:29, vocab_size = seq(20, 580, by = 20), cil = mat[1,], ciu = mat[2,]))
    return(df)
}, x = x_tbl) %>%
    list_rbind() %>%
    mutate(across(c("metric", "value"), as.factor)) %>%
    left_join(y_pred)


df$metric <- factor(df$metric, levels = c("indegree", "aspl", "clust"))
df$value <- factor(df$value, levels = c("ASD", "NA", "diff"))
df$sig <- sign(df$cil) == sign(df$ciu)
df$val_sig_bw <- factor(
    if_else(df$sig, as.character(df$value), paste(as.character(df$value), "ns", sep = "_")),
    levels = c("ASD_ns", "NA_ns", "diff_ns","ASD", "NA", "diff")
)
p_bw <- ggplot(df, aes(x = vocab_size, y = t0, shape = val_sig_bw, fill = val_sig_bw)) +
    scale_shape_manual(values = c(21, 22, 23, 21, 22, 23)) +
    scale_fill_manual(values = c("white", "white", "white", "grey", "grey", "grey")) +
    geom_linerange(aes(ymin = cil, ymax = ciu)) +
    geom_point(size = 2) +
    geom_abline(intercept = 0, slope = 0) +
    facet_grid(~metric) +
    xlab("vocabulary size") +
    ylab("RAN-standardized value") +
    theme_bw(base_size = 18)

ggsave("na-asd_netstats_bs10000_conf-bonf_bw.png", plot = p_bw, width = 12, height = 6, dpi = 300)
ggsave("na-asd_netstats_bs10000_conf-bonf_bw.pdf", plot = p_bw, width = 12, height = 6, dpi = 300)

df$val_sig <- factor(
    if_else(df$sig, as.character(df$value), "ns"),
    levels = c("ns", "ASD", "NA", "diff")
)
df$val_notsig <- factor(
    if_else(!df$sig, as.character(df$value), "sig"),
    levels = c("sig", "ASD", "NA", "diff")
)
p_col <- ggplot(df, aes(x = vocab_size, y = t0, fill = val_sig, color = val_notsig)) +
    scale_fill_manual(values = c("white", "#1b9e77", "#d95f02", "#7570b3")) +
    scale_color_manual(values = c("black", "#1b9e77", "#d95f02", "#7570b3")) +
    geom_linerange(aes(ymin = cil, ymax = ciu), color = "black") +
    geom_point(shape = 21, size = 2) +
    geom_abline(intercept = 0, slope = 0) +
    facet_grid(~metric) +
    xlab("vocabulary size") +
    ylab("RAN-standardized value") +
    theme_bw(base_size = 18)

ggsave("na-asd_netstats_bs10000_conf-bonf_col.png", plot = p_col, width = 12, height = 6, dpi = 300)
ggsave("na-asd_netstats_bs10000_conf-bonf_col.pdf", plot = p_col, width = 12, height = 6, dpi = 300)
