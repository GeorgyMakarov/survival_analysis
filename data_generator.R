library(dplyr)
library(data.table)

# TODO: Compute number of parameters using keras for LSTM to check the number
# of observations. The number of observations must be 10 times the number of
# parameters of the network.

# TODO: replace time absolute with weights of the total number of observations

car_park <- list(
  
  'time' = {
    set.seed(123)
    c(rnorm(300,  1.5 * 52, 11),
      rnorm(2100, 2.5 * 52, 12),
      rnorm(550,  3.0 * 52, 13),
      rnorm(50,   3.0 * 52, 20)) %>% 
      round(., 0)
  },
  
  'make' = {
    set.seed(123)
    sample(c('A', 'B', 'C'), 3000, T, c(0.31, 0.45, 0.24))
  },
  
  'gen' = {
    set.seed(123)
    sample(c('fst', 'snd', 'trd'), 3000, T, c(0.11, 0.70, 0.19))
  },
  
  'soft' = {
    set.seed(123)
    sample(c('2019.01', '2020.02', '2020.03'), 3000, T, c(0.15, 0.10, 0.75))
  },
  
  'miles' = {
    set.seed(123)
    rnorm(3000, 95, 20)
  },
  
  'ct' = {
    set.seed(123)
    runif(3000, 60, 120)
  },
  
  'ks' = {
    set.seed(123)
    rnorm(3000, 6.0, 1.1)
  },
  
  'ox' = {
    set.seed(123)
    rnorm(3000, 7.0, 1.0)
  },
  
  'et' = {
    set.seed(123)
    runif(3000, 300, 1000)
  }
) %>% as.data.table(.)

max_values <- 
  car_park[, .SD, .SDcols = is.numeric] %>% 
  purrr::map_dbl(., max, na.rm = T)     %>% 
  round(., 4)

coefficients <- round(1 / max_values, 4)

makers <- 
  list(
    'make' = c('A', 'B', 'C'),
    'good' = c(490, 295, 198),
    'bad'  = c(10, 5, 2)
  ) %>% 
  as.data.table(.) %>% 
  .[, total := good + bad] %>% 
  .[, px    := total / sum(total)] %>% 
  .[, pyx   := round(bad / total, 4)] %>% 
  .[, pyxpx := px * pyx] %>% 
  .[, pxy   := pyxpx / sum(pyxpx)] %>% 
  .[, pxy] %>% 
  stats::setNames(., c('A', 'B', 'C'))

gens <- 
  list(
    'gen'  = c('fst', 'snd', 'trd'),
    'good' = c(147, 590, 246),
    'bad'  = c(3, 10, 4)
  ) %>% 
  as.data.table(.) %>% 
  .[, total := good + bad] %>% 
  .[, px    := total / sum(total)] %>% 
  .[, pyx   := round(bad / total, 4)] %>% 
  .[, pyxpx := px * pyx] %>% 
  .[, pxy   := pyxpx / sum(pyxpx)] %>% 
  .[, pxy] %>% 
  stats::setNames(., c('fst', 'snd', 'trd'))

softs <- 
  list(
    'gen'  = c('2019.01', '2020.02', '2020.03'),
    'good' = c(196, 689, 98),
    'bad'  = c(4, 11, 2)
  ) %>% 
  as.data.table(.) %>% 
  .[, total := good + bad] %>% 
  .[, px    := total / sum(total)] %>% 
  .[, pyx   := round(bad / total, 4)] %>% 
  .[, pyxpx := px * pyx] %>% 
  .[, pxy   := pyxpx / sum(pyxpx)] %>% 
  .[, pxy] %>% 
  stats::setNames(., c('2019.01', '2020.02', '2020.03'))
  

fail_probs <- 
  car_park %>% 
  .[, .SD, .SDcols = is.numeric] %>% 
  .[, Map("*", .SD, coefficients)] %>% 
  .[, prod_col := Reduce(`*`, .SD) * 300.0] %>% 
  .[, .SD, .SDcols = c('prod_col')] %>% 
  .[, `:=` (make = makers[match(car_park$make, names(makers))],
            gen  = gens[match(car_park$gen, names(gens))],
            soft = softs[match(car_park$soft, names(softs))])] %>% 
  .[, prod_col := Reduce(`*`, .SD)] %>% 
  .[, prod_col := prod_col + {set.seed(123); rnorm(3000, 0, 0.03)}] %>% 
  .[, broken := ifelse(prod_col > 0.5, 1, 0)] %>% 
  .[, 'broken',] %>% 
  unlist(., use.names = F)

car_park[, event := fail_probs]
setcolorder(car_park, 
            c('event', colnames(car_park)[!colnames(car_park) == 'event']))

write.csv()

