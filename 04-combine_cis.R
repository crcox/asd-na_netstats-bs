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


df$value <- factor(df$value, levels = c("ASD", "NA", "diff"))
df$sig <- sign(df$cil) == sign(df$ciu)
ggplot(df, aes(x = vocab_size, y = t0, shape = value, fill = sig)) +
    scale_shape_manual(values = c(21, 22, 23)) +
    scale_fill_manual(values = c("white", "grey")) +
    geom_pointrange(aes(ymin = cil, ymax = ciu)) +
    geom_point(data = df %>% filter(sig)) +
    geom_abline(intercept = 0, slope = 0) +
    facet_grid(~metric) +
    xlab("vocabulary size") +
    ylab("RAN-standardized value") +
    theme_bw(base_size = 18)

ggsave("na-asd_netstats_bs10000_conf-bonf_bw.png", width = 12, height = 6, dpi = 300)
