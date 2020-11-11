#!/usr/bin/env Rscript

suppressPackageStartupMessages({
    library(ggplot2)
    library(dplyr)
    library(magrittr)
    library(scales)
})

PresidentialPolling <- president_polls_3 %>%
    filter(answer %in% c("Trump", "Biden")) %>%
    mutate(date = as.Date(start_date, format = "%m/%d/%y")) %>%
    group_by(state) %>%
    mutate(mean.biden.share = mean(pct[answer == "Biden"])) %>%
    filter(state != "")

# order by mean biden share
PresidentialPolling$state <- with(PresidentialPolling, reorder(state, mean.biden.share))

g <- ggplot(data = PresidentialPolling, aes(x = date, y = I(pct/100), colour = factor(answer))) +
    geom_point() +
    ylim(0.25, 0.75) +
    geom_smooth(aes(group = answer)) + 
    facet_wrap(~state, ncol = 5) +
    theme_bw() +
    scale_colour_manual(values = c("Blue", "Red")) +
    xlab("Polling start date") +
    ylab("Pct Support") +
    theme(legend.position = "top") + 
    scale_y_continuous(labels = scales::percent, breaks = seq(0,1, 0.25))

pdf("all_50.pdf", width = 12, height = 12)
print(g)
dev.off()


g2 <- ggplot(data = PresidentialPolling %>% filter(mean.biden.share < 55 & mean.biden.share > 45),
            aes(x = date, y = I(pct/100), colour = factor(answer))) +
    geom_point() +
    ylim(0.45, 0.55) + 
    geom_smooth(aes(group = answer)) + 
    facet_wrap(~state, ncol = 5) +
    theme_bw() +
    scale_colour_manual(values = c("Blue", "Red")) +
    xlab("Polling start date") +
    ylab("Pct Support") +
    theme(legend.position = "top") + 
    scale_y_continuous(labels = scales::percent, breaks = seq(0,1, 0.25))

pdf("tossups.pdf", width = 12, height = 12)
print(g2)
dev.off()

library(survey)
library(srvyr)
polls = PresidentialPolling %>%
    mutate(polldate_end = as.Date(end_date, format = "%m/%d/%y"), 
           days_to_elec = as.Date("2020-11-03") - polldate_end) %>%
    filter(answer %in% c("Biden", "Trump")) %>%
    select(answer, pct, days_to_elec, state, pollster, sample_size) %>%
    group_by(answer) %>%
    mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = answer, values_from = pct) %>%
    select(-row, -pollster) %>%
    filter(state %in% state.name) %>%
    na.omit() %>%
    mutate(poll_margin = Trump-Biden) %>%
    as_survey_design(ids = 1, weights=sample_size) %>%
    group_by(state, days_to_elec) %>%
    summarize(avg_pollmarg = survey_mean(poll_margin))

polls_sum = polls %>%
    ungroup() %>%
    mutate(wts = scale(1/(as.numeric(days_to_elec)+2), center=F)[,1]) %>%
    as_survey_design(ids = 1, weights=wts) %>%
    group_by(state) %>%
    summarize(avg_pollmarg_wtd = survey_mean(avg_pollmarg))
View(polls_sum)


