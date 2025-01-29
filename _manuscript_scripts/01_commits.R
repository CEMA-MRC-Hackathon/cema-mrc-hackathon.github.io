# 1. Package loading and functions ------------

# Load required libraries
library(gh)
library(ggplot2)
library(dplyr)
library(tidyr)

# Function to fetch commits for a repository
get_commits <- function(repo) {
  message("Fetching commits for: ", repo)
  commits <- gh("/repos/{owner}/{repo}/commits", owner = owner, repo = repo, .limit = Inf)
  data.frame(
    repo = repo,
    sha = sapply(commits, function(x) x$sha),
    author = sapply(commits, function(x) ifelse(is.null(x$author$login), x$commit$committer$name, x$author$login)),
    date = as.POSIXct(sapply(commits, function(x) x$commit$author$date), format = "%Y-%m-%dT%H:%M:%SZ", tz = "Africa/Nairobi"),
    message = sapply(commits, function(x) x$commit$message),
    stringsAsFactors = FALSE
  )
}

## function to save fig
fig_save <- function(name,
                     fig,
                     width = 6,
                     height = 6,
                     plot_dir = file.path(here::here(), "analysis/plots"),
                     pdf_plot = TRUE,
                     font_family = "Helvetica",
                     res = 300,
                     ...) {

  if(!is.null(font_family)) {
    fig <- fig + ggplot2::theme(text = ggplot2::element_text(family = font_family))
  }

  dir.create(plot_dir, showWarnings = FALSE)
  fig_path <- function(name) {paste0(plot_dir, "/", name)}

  ragg::agg_png(fig_path(paste0(name,".png")),
                width = width,
                height = height,
                units = "in",
                res = res,
                ...)
  print(fig)
  dev.off()

  if(pdf_plot) {
    pdf(file = fig_path(paste0(name,".pdf")), width = width, height = height)
    print(fig)
    dev.off()
  }

}


# 2. Sourcing Commit Data ------------

# Organisation name
owner <- "CEMA-MRC-Hackathon"

# List of repositories
repos <- c(
  "animal-surveillance",
  "malaria-vaccine",
  "data-quality-validation",
  "human-resources",
  "cold-chain-capacity",
  "measles-outbreaks-threshold"
)


# Loop through repositories and collect commits
all_commits <- do.call(rbind, lapply(repos, get_commits))

# correct where people some how made commits without usernames
all_commits <- all_commits %>%
  mutate(author = case_when(
  author == "John Ojal" ~ "ojal",
  author == "Evans Omondi" ~ "Komondi",
  .default = as.character(author)
))


# 3. Plot 1: Cumulative commits over time by repository ---------

plot1_data <- all_commits %>%
  group_by(repo, date) %>%
  summarise(commits = n(), .groups = "drop") %>%
  arrange(repo, date) %>%
  group_by(repo) %>%
  mutate(cumulative_commits = cumsum(commits))

gg1 <- plot1_data %>%
  filter(date < "2025-01-25") %>%
  ungroup() %>%
  tidyr::complete(repo, date) %>%
  arrange(repo, date) %>%
  group_by(repo) %>%
  fill(cumulative_commits, .direction = "downup") %>%
ggplot(aes(x = date + lubridate::hours(3), y = cumulative_commits, colour = repo)) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Cumulative Commits",
    colour = "Repository"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  MetBrewer::scale_color_met_d("Klimt") +
  scale_x_datetime(date_breaks = "1 day", limits = as.POSIXct(c("2025-01-20","2025-01-25")), date_labels = "%b %d") +
  theme(axis.line = element_line(), panel.grid.major.x = element_line(colour = "darkgrey", linewidth = 0.5, linetype = "dashed")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0),
                                           add = c(0, 1)),
                     labels = scales::number_format(accuracy = 1))
gg1

# 4. Plot 2: Cumulative commits by user over time, coloured by repo ------

plot2_data <- all_commits %>%
  group_by(author, repo, date) %>%
  summarise(commits = n(), .groups = "drop") %>%
  arrange(author, date) %>%
  group_by(author, repo) %>%
  mutate(cumulative_commits = cumsum(commits))

gg2 <- plot2_data %>%
  filter(date < "2025-01-25") %>%
  ungroup() %>%
  tidyr::complete(author, date) %>%
  arrange(author, date) %>%
  group_by(author) %>%
  fill(repo, .direction = "downup") %>%
  fill(cumulative_commits, .direction = "down") %>%
  mutate(cumulative_commits = replace_na(cumulative_commits, 0)) %>%
ggplot(aes(x = date + lubridate::hours(3), y = cumulative_commits, colour = repo, group = interaction(author, repo))) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Cumulative Commits by Participant",
    colour = "Repository"
  ) +
  lemon::facet_rep_wrap(.~repo, scales = "free_y", ncol = 3)+
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  MetBrewer::scale_color_met_d("Klimt") +
  scale_x_datetime(date_breaks = "1 day", limits = as.POSIXct(c("2025-01-20","2025-01-25")), date_labels = "%b %d") +
  theme(axis.line = element_line(), panel.grid.major.x = element_line(colour = "darkgrey", linewidth = 0.5, linetype = "dashed")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0),
                                           add = c(0, 1)),
                                           labels = scales::number_format(accuracy = 1))
gg2

# 5. Save figure to plots director
commit_gg <- cowplot::plot_grid(
    gg1 + theme(legend.position = c(0.25,0.85), legend.background = element_rect()),
    gg2 + theme(legend.position = "none"),
    ncol = 2,
    rel_widths = c(0.4, 0.6), scale = 0.98, labels = "AUTO"
  )

fig_save("commits", commit_gg, width = 18, height = 7, res = 600, plot_dir = "_manuscript_scripts/plots")


# 5. Save figure to plots director
commit_gg2 <- cowplot::plot_grid(
  gg1 + theme(legend.position = c(0.15,0.65), legend.background = element_rect()),
  gg2 + theme(legend.position = "none", axis.text.x = element_text(size = 10)),
  ncol = 1,
  rel_heights = c(0.4, 0.6), scale = 0.98, labels = "AUTO"
)

fig_save("commits2", commit_gg2, width = 10, height = 9, res = 600, plot_dir = "_manuscript_scripts/plots")


