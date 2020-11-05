library(tidyverse)
library(readxl)

budget_sheet <- file.path(data_dir, 'r_and_d', 'federal_rd_by_function.xlsx')

rd_budget <- read_excel(budget_sheet,1, skip=2) %>% t()
n <- rd_budget[1,]
rd_budget <- rd_budget[-1, !is.na(n)]
colnames(rd_budget) <- discard(n, ~is.na(.x))
rd_budget <- rd_budget %>% as.data.frame(stringsAsFactors = FALSE) %>% rownames_to_column('year') %>%
  as_tibble() %>%
  mutate(year = str_replace(year, "[^0-9]+$", "")) %>%
  filter(str_detect(year, '^[0-9]+$')) %>% mutate(year = as.integer(year))

rd_budget <- rd_budget %>% select(-starts_with('Source'), -starts_with('Note'),
                                  -starts_with("*Beginning in "),
                                  -starts_with("AAAS |"))

rd_budget <- rd_budget %>% mutate_at(vars(-year), as.numeric) %>%
  filter(!is.na(`TOTAL R&D`))
rd_budget <- rd_budget %>% gather(key = func, value = budget, -year) %>%
  select(year, func, budget) %>%
  filter(! func %in% c('TOTAL R&D', "Nondef. R&D") ) %>%
  mutate(budget = as.numeric(budget))

y_last <- rd_budget %>% top_n(1, year) %>% transmute(func = func, bgt = budget) %>%
  arrange(desc(bgt)) %>%
  mutate(func = ordered(func, levels = func))

rd_budget$func <- ordered(rd_budget$func, levels = levels(y_last$func))

rd_budget_nd<- rd_budget %>% filter(! func %in% c('Defense R&D', 'Natural Resources'))

plot_funding <- function(budget) {
  y_max <- budget %>% dplyr::group_by(year) %>% dplyr::summarize(b = sum(budget)) %>%
    ungroup() %>% select(b) %>% max()
  p <- ggplot(budget, aes(x = year, y = budget, fill = func, order = desc(func))) +
    geom_bar(stat='identity') +
    scale_fill_brewer(palette = 'Accent', name = "Budget Function") +
    scale_x_continuous(breaks = seq(1950,2020,5), expand=c(0,0.1)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 1.02 * y_max)) +
    labs(x = "Year", y = "$ Billion", title = str_c("Non-Defense R&D ", min(budget$year), "-", max(budget$year))) +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1., vjust = 1.1))
  p
}
