# ------------------------------------------------------------------------------
# classification further losses

# FIG: overview losses (hinge, squared scores, 0-1, squared hinge, exponential)
# ------------------------------------------------------------------------------

library(tidyr)
library(ggplot2)
library(viridis)
library(grid)

# DATA -------------------------------------------------------------------------

x <- seq(-4, 4, length.out = 800)

hinge <- ifelse(x < 1, 1 - x, 0)
hinge_sq <- ifelse(x < 1, (1 - x)^2, 0)
sq_scores <- (1 - x)^2
exp <- exp(-x)
zero_one <- as.numeric(x < 0)

# Define original colors (extracted from viridis::viridis(5, end = 0.9))
base_colors <- viridis::viridis(5, end = 0.9)
color_map <- c(
  "hinge"     = base_colors[2],
  "hinge_sq"  = base_colors[1],
  "sq_scores" = base_colors[3],
  "zero_one"  = base_colors[5],
  "exp"       = base_colors[4]  # new color for exponential loss
)

# Create long-form data frame including exponential loss
df_exp <- gather(
  data.frame(x, hinge, hinge_sq, sq_scores, zero_one, exp),
  key = "loss",
  value = "value",
  -x)

# Set factor levels to guarantee consistent legend order
df_exp$loss <- factor(df_exp$loss, levels = c("hinge", "hinge_sq", "sq_scores", "zero_one", "exp"))

# Assign dashed linetype only to 'sq_scores'
df_exp$aux <- as.factor(ifelse(df_exp$loss == "sq_scores", 1, 0))

# Define linetype mapping as a named vector (keys are loss names)
linetype_map <- c(
  "hinge"     = "solid",
  "hinge_sq"  = "solid",
  "sq_scores" = "dashed",
  "zero_one"  = "solid",
  "exp"       = "solid"
)

# Define label mapping for the legend
label_map <- c(
  "hinge"     = "Hinge",
  "hinge_sq"  = "Squared hinge",
  "sq_scores" = "Squared (scores)",
  "zero_one"  = "0-1",
  "exp"       = "Exponential"
)

# Helper function to create and save a plot from a given data subset
create_plot <- function(data, file_name) {
  p <- ggplot(data, aes(x = x, y = value, color = loss, linetype = aux)) +
    geom_line(size = 1.1) +
    scale_color_manual(
      name = "Loss",
      values = color_map,
      labels = label_map
    ) +
    guides(
      color = guide_legend(
        ncol = 1,
        override.aes = list(
          linetype = unname(linetype_map[levels(data$loss)]),
          size = 1.1
        )
      ),
      linetype = "none"
    ) +
    ylim(c(0, 4)) +
    scale_x_continuous(breaks = seq(-4, 4)) +
    xlab(expression(yf(x))) +
    ylab(expression(L(y, f(x)))) +
    theme_minimal() +
    theme(
      text = element_text(size = 20),
      legend.position = "right",
      legend.key.width = unit(3, "lines")
    )
  ggsave(file_name, p, width = 10, height = 4)
  return(p)
}

# Plot 1: Only 0-1 loss and hinge loss
df_subset1 <- droplevels(subset(df_exp, loss %in% c("zero_one", "hinge")))
df_subset1$loss <- factor(df_subset1$loss, levels = c("hinge", "zero_one"))
p_subset1 <- create_plot(df_subset1, "../figure/overview_classif_subset1.png")

# Plot 2: Only 0-1 loss, hinge, and squared hinge
df_subset2 <- droplevels(subset(df_exp, loss %in% c("zero_one", "hinge", "hinge_sq")))
df_subset2$loss <- factor(df_subset2$loss, levels = c("hinge", "hinge_sq", "zero_one"))
p_subset2 <- create_plot(df_subset2, "../figure/overview_classif_subset2.png")

# Plot 3: 0-1 loss, hinge, squared hinge, and squared (scores)
df_subset3 <- droplevels(subset(df_exp, loss %in% c("zero_one", "hinge", "hinge_sq", "sq_scores")))
df_subset3$loss <- factor(df_subset3$loss, levels = c("hinge", "hinge_sq", "sq_scores", "zero_one"))
p_subset3 <- create_plot(df_subset3, "../figure/overview_classif_subset3.png")

# Plot 4: All losses (hinge, squared hinge, squared (scores), 0-1, and exponential)
p_all <- create_plot(df_exp, "../figure/overview_classif_all.png")

