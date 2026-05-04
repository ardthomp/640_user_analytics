# Legacy HUMC trend analysis -----------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(tidytext)
library(textstem)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "text_helpers.R"))

standardize_campus_name <- function(x) {
  x_clean <- str_squish(as.character(x))
  x_lower <- str_to_lower(x_clean)
  
  case_when(
    is.na(x_clean) | x_clean == "" | x_lower == "na" ~ "Unknown/Not specified",
    str_detect(x_lower, "hackensack") | x_lower == "humc" ~ "Hackensack University Medical Center",
    str_detect(x_lower, "\\bjfk\\b") ~ "JFK University Medical Center",
    str_detect(x_lower, "palisades") ~ "Palisades Medical Center",
    str_detect(x_lower, "carrier") ~ "Carrier Clinic",
    str_detect(x_lower, "network") ~ "Network",
    TRUE ~ x_clean
  )
}

# Load data ----------------------------------------------------------------

humc <- read_csv(here("data", "processed", "humc.csv"), show_col_types = FALSE)

# Set up output -------------------------------------------------------------

paths <- make_output_paths("humc")

figures_dir <- paths$figures_dir
csv_dir <- paths$csv_dir

# Prepare data ------------------------------------------------------------------

legacy_dat <- humc %>%
  dplyr::mutate(
    year_month = as.Date(month),
    campus_affiliation_clean = standardize_campus_name(campus_affiliation)
  )

# Top lemmas across all legacy HUMC literature search records, 2013–2025 ----

phrases_tbl <- read_phrases(phrases_path)
custom_map <- read_custom_merges(custom_merge_path)
lex_map <- read_lex(lex_path)

legacy_text <- legacy_dat %>%
  mutate(
    research_topic_clean = clean_topic_for_normalization(topic),
    research_topic_clean = collapse_phrases(research_topic_clean, phrases_tbl),
    research_topic_clean = na_if(research_topic_clean, "")
  )

tidy_lemmas_legacy <- legacy_text %>%
  filter(!is.na(research_topic_clean)) %>%
  dplyr::select(
    request_id,
    year,
    year_month,
    campus_affiliation_clean,
    research_topic_clean
  ) %>%
  tidytext::unnest_tokens(word, research_topic_clean) %>%
  mutate(
    word = str_replace_all(word, "[’`]", "'"),
    word = str_replace_all(word, "\\.", ""),
    word = str_replace_all(word, "[^a-z0-9'_-]", "")
  ) %>%
  filter(word != "", str_detect(word, "[a-z0-9]")) %>%
  left_join(lex_map, by = c("word" = "token")) %>%
  mutate(
    lemma0 = coalesce(lemma_from_lex, word),
    lemma1 = if_else(
      is.na(lemma_from_lex),
      textstem::lemmatize_words(word),
      lemma0
    )
  ) %>%
  left_join(custom_map, by = c("lemma1" = "token")) %>%
  mutate(lemma = coalesce(lemma_custom, lemma1)) %>%
  anti_join(tidytext::stop_words, by = c("lemma" = "word")) %>%
  filter(!str_detect(lemma, "^\\d+$"))

top_lemmas_legacy <- tidy_lemmas_legacy %>%
  count(lemma, sort = TRUE, name = "n") %>%
  mutate(prop = n / sum(n))

readr::write_csv(
  top_lemmas_legacy,
  file.path(csv_dir, "top_lemmas_legacy_2013_2025.csv")
)

requests_legacy_4mo <- legacy_dat %>%
  filter(!is.na(campus_affiliation_clean), !is.na(year_month)) %>%
  mutate(
    year = year(year_month),
    month_num = month(year_month),
    period_4mo = case_when(
      month_num %in% 1:4 ~ "Jan–Apr",
      month_num %in% 5:8 ~ "May–Aug",
      month_num %in% 9:12 ~ "Sep–Dec"
    ),
    period_label = paste(year, period_4mo)
  ) %>%
  count(period_label, campus_affiliation_clean, name = "n_requests")

# Plot: Total Literature Search Requests by Year -----------------------------------

legacy_total_4mo <- legacy_dat %>%
  filter(!is.na(year_month)) %>%
  mutate(
    year = year(year_month),
    month_num = month(year_month),
    period_start = case_when(
      month_num %in% 1:4  ~ make_date(year, 1, 1),
      month_num %in% 5:8  ~ make_date(year, 5, 1),
      month_num %in% 9:12 ~ make_date(year, 9, 1)
    )
  ) %>%
  count(period_start, name = "n_requests")

p_legacy_total <- ggplot(legacy_total_4mo, aes(period_start, n_requests)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Total Literature Search Requests (4-Month Periods)",
    subtitle = "Legacy HUMC Logs, 2013–2025",
    x = "Year",
    y = "Number of Requests"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(figures_dir, "legacy_total_requests_4mo.png"),
  plot = p_legacy_total,
  width = 8,
  height = 5,
  dpi = 300
)

# Plot: Total Literature Search Request by Campus  -------------------------------------------------------------------------

p_legacy_4mo <- ggplot(
  requests_legacy_4mo,
  aes(
    x = period_label,
    y = n_requests,
    color = campus_affiliation_clean,
    group = campus_affiliation_clean
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Literature Search Requests by Campus (4-Month Periods)",
    subtitle = "Legacy HUMC Logs, 2013–2025",
    x = "Period",
    y = "Number of Requests",
    color = "Campus"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(figures_dir, "legacy_requests_by_campus_4mo.png"),
  p_legacy_4mo,
  width = 12,
  height = 6,
  dpi = 300
)

# Plot: Bar Plot Total Legacy Requests by Year

legacy_requests_by_year <- legacy_dat %>%
  filter(!is.na(year)) %>%
  count(year, name = "n_requests")

p_legacy_year <- ggplot(
  legacy_requests_by_year,
  aes(x = factor(year), y = n_requests)
) +
  geom_col() +
  geom_text(aes(label = n_requests), vjust = -0.3, size = 3) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Total Literature Search Requests by Year",
    subtitle = "Legacy HUMC Logs, 2013–2025",
    x = "Year",
    y = "Number of Requests"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(figures_dir, "legacy_requests_by_year.png"),
  plot = p_legacy_year,
  width = 10,
  height = 6,
  dpi = 300
)

# Plot: Top Lemmas ---------------------------------------------------------

p_top_lemmas_legacy <- top_lemmas_legacy %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(lemma, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Lemmas in Literature Search Requests",
    subtitle = "Legacy HUMC logs, 2013–2025",
    x = "Lemma",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave(
  file.path(figures_dir, "top_lemmas_legacy_2013_2025.png"),
  p_top_lemmas_legacy,
  width = 8,
  height = 6,
  dpi = 300
)

# Top Lemmas Over Time ------------------------------------------

lemmas_by_time <- tidy_lemmas_legacy %>%
  filter(!is.na(year_month)) %>%
  count(year_month, lemma, sort = TRUE)

write_csv(
  lemmas_by_time,
  file.path(csv_dir, "lemmas_by_time.csv")
)

top_terms <- tidy_lemmas_legacy %>%
  count(lemma, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(lemma)
top_terms <- tidy_lemmas_legacy %>%
  count(lemma, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(lemma)

lemmas_time_plot <- lemmas_by_time %>%
  filter(lemma %in% top_terms) %>%
  ggplot(aes(x = year_month, y = n, color = lemma)) +
  geom_line() +
  labs(
    title = "Top Search Topics Over Time",
    subtitle = "Legacy HUMC logs, 2013–2025",
    x = "Year",
    y = "Number of Mentions",
    color = "Lemma"
  ) +
  theme_minimal()

ggsave(
  file.path(figures_dir, "lemmas_over_time.png"),
  lemmas_time_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Export Topics

research_topics <- legacy_dat %>%
  filter(!is.na(topic), topic != "") %>%
  mutate(topic = str_squish(topic)) %>%
  distinct(topic) %>%
  arrange(topic)

write_csv(
  research_topics,
  file.path(csv_dir, "research_topics_legacy_raw.csv")
)