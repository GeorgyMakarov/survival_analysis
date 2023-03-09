library(ggplot2)
library(patchwork)
source('data_generator.R')

dt %>% 
  group_by(def_event) %>% 
  summarise(count = n(),
            amt   = sum(sales_amt)) %>% 
  mutate(wt = round(amt * 100 / sum(amt), 1)) %>% 
  mutate(nms = ifelse(test = def_event == 0, 
                      yes  = paste0('Censored ', wt[def_event == 0] , '%'), 
                      no   = paste0('Occured ', wt[def_event == 1] , '%'))) %>% 
  ggplot(aes(x = nms, y = count, fill = as.factor(nms))) +
  geom_bar(stat = 'identity', width = 0.5) +
  scale_fill_manual(values = c('#a3a0a9', '#284387')) +
  labs(
    x = '',
    y = ''
  ) +
  theme(
    panel.background   = element_rect(fill = 'white'),
    panel.grid.major.x = element_blank(),
    axis.ticks.length  = unit(0, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y        = element_blank(),
    legend.position    = 'none',
    plot.title         = element_text(face = 'bold', size = 14)
  )


# Probability that a randomly selected bad debt is from i-th business unit 
raw_dt %>% 
  group_by(b_unit) %>% 
  summarise(count = n(),
            good  = n() - sum(def_event),
            bad   = sum(def_event)) %>% 
  mutate(
    ph     = count / sum(count),
    peh    = bad / count,
    ph_peh = ph * peh,
    phe    = round(ph_peh / sum(ph_peh) * 100, 0),
    b_unit = ifelse(grepl(' ', b_unit), paste0(b_unit, 'U'), b_unit)
  ) %>% 
  select(b_unit, phe) %>% 
  ggplot(aes(x = b_unit, y = phe, fill = as.factor(b_unit))) +
  geom_bar(stat = 'identity', width = 0.5) +
  scale_fill_manual(values = c('#a3a0a9', '#a3a0a9', '#284387', '#a3a0a9', '#a3a0a9')) +
  labs(
    x = '',
    y = ''
  ) +
  theme(
    panel.background   = element_rect(fill = 'white'),
    panel.grid.major.x = element_blank(),
    axis.ticks.length  = unit(0, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y        = element_blank(),
    legend.position    = 'none',
    plot.title         = element_text(face = 'bold', size = 14)
  ) -> p1


# Probability that a randomly selected bad debt is from AEM or OEM
raw_dt %>% 
  group_by(b_segm) %>% 
  summarise(count = n(),
            good  = n() - sum(def_event),
            bad   = sum(def_event)) %>% 
  mutate(
    ph     = count / sum(count),
    peh    = bad / count,
    ph_peh = ph * peh,
    phe    = round(ph_peh / sum(ph_peh) * 100, 0)
  ) %>% 
  select(b_segm, phe) %>% 
  ggplot(aes(x = b_segm, y = phe, fill = as.factor(b_segm))) +
  geom_bar(stat = 'identity', width = 0.5) +
  scale_fill_manual(values = c('#a3a0a9', '#284387')) +
  labs(
    x = '',
    y = ''
  ) +
  theme(
    panel.background   = element_rect(fill = 'white'),
    panel.grid.major.x = element_blank(),
    axis.ticks.length  = unit(0, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y        = element_blank(),
    legend.position    = 'none',
    plot.title         = element_text(face = 'bold', size = 14)
  ) -> p2


# Probability that a randomly selected bad debt is from i-th business area
raw_dt %>% 
  group_by(b_area) %>% 
  summarise(count = n(),
            good  = n() - sum(def_event),
            bad   = sum(def_event)) %>% 
  mutate(
    ph     = count / sum(count),
    peh    = bad / count,
    ph_peh = ph * peh,
    phe    = round(ph_peh / sum(ph_peh) * 100, 0)
  ) %>% 
  select(b_area, phe) %>% 
  ggplot(aes(x = b_area, y = phe, fill = as.factor(b_area))) +
  geom_bar(stat = 'identity', width = 0.5) +
  scale_fill_manual(values = c('#a3a0a9', '#a3a0a9', '#284387', '#a3a0a9')) +
  labs(
    x = '',
    y = ''
  ) +
  theme(
    panel.background   = element_rect(fill = 'white'),
    panel.grid.major.x = element_blank(),
    axis.ticks.length  = unit(0, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y        = element_blank(),
    legend.position    = 'none',
    plot.title         = element_text(face = 'bold', size = 14)
  ) -> p3


# Probability that a random bad debt is from small, medium or large customer
raw_dt %>% 
  group_by(cust_size) %>% 
  summarise(count = n(),
            good  = n() - sum(def_event),
            bad   = sum(def_event)) %>% 
  mutate(
    ph     = count / sum(count),
    peh    = bad / count,
    ph_peh = ph * peh,
    phe    = round(ph_peh / sum(ph_peh) * 100, 0),
    cust_size = c('Large', 'Medium', 'Small')
  ) %>% 
  select(cust_size, phe) %>% 
  ggplot(aes(x = cust_size, y = phe, fill = as.factor(cust_size))) +
  geom_bar(stat = 'identity', width = 0.5) +
  scale_fill_manual(values = c('#284387', '#a3a0a9', '#a3a0a9')) +
  labs(
    x = '',
    y = ''
  ) +
  theme(
    panel.background   = element_rect(fill = 'white'),
    panel.grid.major.x = element_blank(),
    axis.ticks.length  = unit(0, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y        = element_blank(),
    legend.position    = 'none',
    plot.title         = element_text(face = 'bold', size = 14)
  ) -> p4

(p1 | p2) / (p3 | p4)
rm(p1, p2, p3, p4)


# Density plots for overdue time per censored and default events
raw_dt %>%
  mutate(def_event = ifelse(def_event == 0, 'Censored', 'Occured')) %>% 
  ggplot(aes(x = overdue_t, group = def_event, fill = as.factor(def_event))) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  scale_fill_manual(values = c('#284387', '#a3a0a9')) +
  labs(
    x = 'Overdue Days',
    y = ''
  ) +
  theme(
    panel.background   = element_rect(fill = 'white'),
    panel.grid.major.x = element_blank(),
    axis.ticks.length  = unit(0, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y        = element_blank(),
    legend.title       = element_blank(),
    legend.position    = 'top',
    plot.title         = element_text(face = 'bold', size = 14)
  ) -> p1


# Density plots for accounts receivable balance at the time of default
raw_dt %>%
  mutate(def_event = ifelse(def_event == 0, 'Censored', 'Occured')) %>% 
  ggplot(aes(x = ar_balance, group = def_event, fill = as.factor(def_event))) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  scale_fill_manual(values = c('#284387', '#a3a0a9')) +
  labs(
    x = 'Accounts Receivable, tUSD',
    y = ''
  ) +
  theme(
    panel.background   = element_rect(fill = 'white'),
    panel.grid.major.x = element_blank(),
    axis.ticks.length  = unit(0, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y        = element_blank(),
    legend.title       = element_blank(),
    legend.position    = 'top',
    plot.title         = element_text(face = 'bold', size = 14)
  ) -> p2


# Density plots for sales amount
raw_dt %>%
  mutate(def_event = ifelse(def_event == 0, 'Censored', 'Occured')) %>% 
  ggplot(aes(x = sales_amt, group = def_event, fill = as.factor(def_event))) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  scale_fill_manual(values = c('#284387', '#a3a0a9')) +
  labs(
    x = 'Invoice Amount, tUSD',
    y = ''
  ) +
  theme(
    panel.background   = element_rect(fill = 'white'),
    panel.grid.major.x = element_blank(),
    axis.ticks.length  = unit(0, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y        = element_blank(),
    legend.title       = element_blank(),
    legend.position    = 'top',
    plot.title         = element_text(face = 'bold', size = 14)
  ) -> p3

# Density plots for contract terms
raw_dt %>%
  mutate(def_event = ifelse(def_event == 0, 'Censored', 'Occured')) %>% 
  ggplot(aes(x = contr_term, group = def_event, fill = as.factor(def_event))) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  scale_fill_manual(values = c('#284387', '#a3a0a9')) +
  labs(
    x = 'Contract payment term, days',
    y = ''
  ) +
  theme(
    panel.background   = element_rect(fill = 'white'),
    panel.grid.major.x = element_blank(),
    axis.ticks.length  = unit(0, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y        = element_blank(),
    legend.title       = element_blank(),
    legend.position    = 'top',
    plot.title         = element_text(face = 'bold', size = 14)
  ) -> p4

(p1 | p2) / (p3 | p4)
rm(p1, p2, p3, p4)


# Probability that random bad debt is from ascending or descending gdp
raw_dt %>% 
  group_by(gdp) %>% 
  summarise(count = n(),
            good  = n() - sum(def_event),
            bad   = sum(def_event)) %>% 
  mutate(
    ph     = count / sum(count),
    peh    = bad / count,
    ph_peh = ph * peh,
    phe    = round(ph_peh / sum(ph_peh) * 100, 0),
    gdp    = c('grow', 'fall')
  ) %>% 
  select(gdp, phe) %>% 
  ggplot(aes(x = gdp, y = phe, fill = as.factor(gdp))) +
  geom_bar(stat = 'identity', width = 0.5) +
  scale_fill_manual(values = c('#a3a0a9', '#284387')) +
  labs(
    x = 'GDP trend',
    y = ''
  ) +
  theme(
    panel.background   = element_rect(fill = 'white'),
    panel.grid.major.x = element_blank(),
    axis.ticks.length  = unit(0, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y        = element_blank(),
    legend.position    = 'none',
    plot.title         = element_text(face = 'bold', size = 14)
  ) -> p1


# Probability that bad debt has bank guarantee
raw_dt %>% 
  group_by(bank_guar) %>% 
  summarise(count = n(),
            good  = n() - sum(def_event),
            bad   = sum(def_event)) %>% 
  mutate(
    ph     = count / sum(count),
    peh    = bad / count,
    ph_peh = ph * peh,
    phe    = round(ph_peh / sum(ph_peh) * 100, 0),
    bank_guar = c('no', 'yes')
  ) %>% 
  select(bank_guar, phe) %>% 
  ggplot(aes(x = bank_guar, y = phe, fill = as.factor(bank_guar))) +
  geom_bar(stat = 'identity', width = 0.5) +
  scale_fill_manual(values = c('#284387', '#a3a0a9')) +
  labs(
    x = 'Bank Guarantee',
    y = ''
  ) +
  theme(
    panel.background   = element_rect(fill = 'white'),
    panel.grid.major.x = element_blank(),
    axis.ticks.length  = unit(0, "mm"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.text.y        = element_blank(),
    legend.position    = 'none',
    plot.title         = element_text(face = 'bold', size = 14)
  ) -> p2

p1 | p2
