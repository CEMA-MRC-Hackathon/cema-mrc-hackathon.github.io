
# 1. Read data and create plot -----------
data <- read.csv("_manuscript_scripts/data-raw/participants.csv")
data <- data %>% filter(role != "Admin")

# Create a grid for the plot
grid_data <- expand.grid(x = 1:10, y = 1:10)
grid_data <- grid_data[seq_len(nrow(data)),]
grid_data$role <- sort(data$role)

# Define unique roles and assign colours
unique_roles <- unique(grid_data$role)
role_colors <- RColorBrewer::brewer.pal(length(unique_roles), "Set3")
names(role_colors) <- unique_roles

# Plot the data
gg1 <- ggplot(grid_data, aes(x = x, y = y, fill = role)) +
  geom_point(size = 7, color = "black", shape = 21) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  MetBrewer::scale_fill_met_d(palette_name = "Egypt", name = "Role") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
gg1

gg2 <- data %>% ggplot(aes(x = gender, fill = ethnicity)) +
  geom_bar() +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  MetBrewer::scale_fill_met_d(palette_name = "Hokusai2", name = "Gender") +
  xlab("Gender") + ylab("Total") +
  theme(axis.line = element_line()) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0),
                                           add = c(0, 1)),
                     labels = scales::number_format(accuracy = 1))
gg2

# 2. Save figures to plots directory -----------
roles_gg <- cowplot::plot_grid(
  gg2 + theme(legend.position = "top") +
    guides(fill = guide_legend(nrow = 4, title.position="top")),
  gg1 + theme(legend.position = "top") +
    guides(fill = guide_legend(nrow = 2, title.position="top")),
  ncol = 2,
  rel_widths = c(0.6, 0.5), scale = 0.97, labels = "AUTO"
)
fig_save("roles", roles_gg, width = 10, height = 6, res = 600, plot_dir = "_manuscript_scripts/plots")

