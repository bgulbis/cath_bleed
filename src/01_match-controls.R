library(tidyverse)
library(readxl)
library(MatchIt)
library(themebg)

data_cath <- read_excel("data/raw/data_from-cath-report.xlsx",
                        sheet = "Inclusions") %>%
    select(last_name = `Patient Last Name`,
           first_name = `Patient First Name`,
           fin = `Other ID (2045)`,
           procedure_date = `Date of Procedure (5300)`,
           bleed = `Obs Bld`,
           dialysis = `Currently on Dialysis (4065)`,
           presentation = `CAD Presentation (5000)`,
           iabp = `IABP (5330)`,
           access_site = `Arterial Access Site (5350)`,
           gpi = `GP IIb/IIIa (any)`) %>%
    filter(!is.na(last_name)) %>%
    mutate(group = if_else(!is.na(bleed), "case", "control"),
           case = !is.na(bleed)) %>%
    mutate_at(c("dialysis", "iabp", "gpi"), funs(. == "Yes")) %>%
    mutate_at(c("presentation", "access_site", "group"), factor) %>%
    select(-bleed)

set.seed(77123)
m_data <- matchit(case ~ dialysis + presentation + iabp + access_site + gpi,
              data = data_cath, ratio = 3)

data_match <- match.data(m_data)

# check distribution of groups

data_match %>%
    add_count(group) %>%
    add_count(group, presentation) %>%
    distinct(group, presentation, n, nn) %>%
    group_by(group, presentation) %>%
    mutate(pct = nn / n * 100) %>%
    ggplot(aes(x = presentation, y = pct)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ group) +
    theme_bg()

data_match %>%
    add_count(group) %>%
    add_count(group, access_site) %>%
    distinct(group, access_site, n, nn) %>%
    group_by(group, access_site) %>%
    mutate(pct = nn / n * 100) %>%
    ggplot(aes(x = access_site, y = pct)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ group) + 
    theme_bg()

data_match %>%
    add_count(group) %>%
    group_by(group, n) %>%
    summarize_at(c("dialysis", "iabp", "gpi"), sum, na.rm = TRUE) %>%
    mutate_at(c("dialysis", "iabp", "gpi"), funs(. / n * 100))

write.csv(data_match, "data/external/matched_groups.csv", row.names = FALSE)
