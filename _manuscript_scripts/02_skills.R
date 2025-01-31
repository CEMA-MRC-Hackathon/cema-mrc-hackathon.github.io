# 1. Package loading and functions ------------

# Load required libraries
library(tidyverse)
library(readxl)
library(dplyr)
install.packages("ggridges", repos = "https://cloud.r-project.org")
library(ggridges)
library(viridis)
library(tidyr)


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


# 2. Read and Format Feedback ------------
Daily_Feedback <- googlesheets4::read_sheet("1WuL47QXHLukyd_QgdGEh2nL7ezBfqyQZzslZFmevUU8")
Daily_Feedback <- Daily_Feedback %>% filter(Timestamp > as.Date("2025-01-22"))

# Convert wide to long
long_df <- pivot_longer(
  data = Daily_Feedback,
  cols = c(10:14),  # Specify columns to pivot
  names_to = "Question",
  values_to = "Score"
)

# tidy roles
long_df <- long_df %>%
  rename(roles = `Please provide your type of engagement in the hackathon.`) %>%
  filter(roles != "Health Economist, University , academia") %>%
  filter(!is.na(roles))

long_df <- long_df %>%
  mutate(roles = replace(roles, roles == "Hacker - Software Engineering focus", "Hacker - Software Focus")) %>%
  mutate(roles = replace(roles, roles == "Hacker - Modelling focus", "Hacker - Health Focus")) %>%
  mutate(Question = gsub(" Skills", "", Question))

# 3. Create and save figures to plots directory -----------
skills_gg <- long_df %>% group_by(roles, Question, Score) %>%
  summarise(n = n()) %>%
  group_by(roles, Question) %>%
  mutate(n = n/sum(n)) %>%
  ungroup() %>%
  mutate(Score = case_when(
    Score == 1 ~ "Strongly Disagree",
    Score == 2 ~ "Disagree",
    Score == 3 ~ "Neutral",
    Score == 4 ~ "Agree",
    Score == 5 ~ "Strongly Agree",
  )) %>%
  mutate(Score = factor(Score, levels = (c("Strongly Agree", "Agree", "Neutral","Disagree","Strongly Disagree")))) %>%
  ggplot() +
  geom_bar(aes(x = Question, y=n, fill=Score), position="stack", stat="identity", alpha = 0.9, color = "darkgrey", lwd = 0.25)+
  coord_flip() +
  xlab("To what extent do you agree your experience during the hackathon \ncontributed significantly to your development of each skill?\n")+
  ylab("Precentage")+
  scale_fill_manual(name = "Score:",
                    values = rev(c("Strongly Agree" = "#3e6488",
                                   "Agree" = "#829db2",
                                   "Neutral"= "#c8cdd1",
                                   "Disagree" = "#edad89",
                                   "Strongly Disagree"= "#e36c32")),
                    guide = guide_legend(reverse = TRUE)) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(legend.position="top", panel.grid.minor.x = element_blank(),
        panel.spacing.x = unit(1.5, "lines"),
        plot.margin = margin(5, 20, 5, 5), ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1),
                     expand = expand_scale(mult = c(0.02, 0),
                                           add = c(0, 0))) +
  facet_wrap(~roles)
skills_gg

fig_save("skills", skills_gg, width = 12, height = 8, res = 600, plot_dir = "_manuscript_scripts/plots")

