library(dplyr)
library(data.table)
library(lubridate)

produce_probs <- function(categories, good, bad, out_name){
  res <- 
    data.frame(
      cats = categories,
      good = good,
      bad  = bad
    ) %>% 
    mutate(total = good + bad) %>% 
    mutate(ph = total / sum(total),
           peh = bad / total) %>% 
    mutate(ph_peh = ph * peh) %>% 
    mutate(prob_bad = round(ph * ph_peh / sum(ph * ph_peh), 3)) %>% 
    mutate(peh       = good / total,
           ph_peh    = ph * peh,
           prob_good = round(ph_peh / sum(ph_peh), 4)) %>% 
    select(cats, prob_good, prob_bad)
  
  colnames(res) <- c(out_name, 'good', 'bad')
  return(res)
}


my_random_walk <- function(n, seed, sc){
  set.seed(seed)
  x1 <- rnorm(n)
  return(cumsum(x1) %>% scales::rescale(., sc))
}

my_sample <- function(seed, n, from, prob){
  set.seed(seed)
  res <- sample(from, n, T, prob)
  return(res)
}

my_sigmoid <- function(x){
  return(
    1 / (1 + exp(x))
  )
}

my_runif <- function(seed, n){
  set.seed(seed)
  res <- runif(n, 0, round(1 / n, 4))
  return(res)
}

# List of factors:

# default event
 
# Business Unit
bu <- produce_probs(c('East E', 'West E', 'China', 'India', 'US'),
                    c(19152, 8798, 31878, 20899, 14759),
                    c(1008, 561, 322, 2322, 301),
                    'bu')
bu$bu <- as.character(bu$bu)

# Business segment
bs <- produce_probs(c('AEM', 'OEM'),
                    c(43121, 56879),
                    c(2437, 2078),
                    'bs')
bs$bs <- as.character(bs$bs)

# Business Area
buar <- produce_probs(c('North', 'East', 'South', 'West'),
                      c(27950, 31878, 20899, 14759),
                      c(1569, 322, 2322, 301),
                      'buar')
buar$buar <- as.character(buar$buar)

# Inter company trade
isic <- produce_probs(c('0', '1'),
                      c(98752, 1248),
                      c(4510, 4),
                      'isic')
isic$isic <- as.numeric(as.character(isic$isic))

# Has bank guarantee
bgar <- produce_probs(c('0', '1'),
                      c(85322, 14678),
                      c(1581, 2934),
                      'bgar')
bgar$bgar <- as.numeric(as.character(bgar$bgar))

# Customer size n employees
csize <- produce_probs(c('small', 'middle', 'large'),
                       c(11217, 46782, 42001),
                       c(914, 1239, 2362),
                       'csize')
csize$csize <- as.character(csize$csize)

# Balance
set.seed(123)
x1 <- 
  round(rbeta(1e5, 10, 2) * 100, -1) %>% 
  data.frame(balance = .) %>% 
  group_by(balance) %>% 
  summarise(count = n()) %>% 
  mutate(bad = c(0, 1, 2, 4, 329, 533, 1180, 1881, 584))

balance <- produce_probs(as.character(x1$balance),
                         x1$count,
                         x1$bad,
                         'balance')
balance$balance <- as.numeric(as.character(balance$balance))

# Contract term
cterm <- produce_probs(c('30', '45', '60', '75', '90'),
                       c(10210, 20217, 45134, 20621, 3818),
                       c(12, 81, 3329, 1088, 4),
                       'cterm')
cterm$cterm <- as.numeric(as.character(cterm$cterm))

# Sales amount
set.seed(123)
x1 <- 
  round(rbeta(1e5, 5, 2) * 80, 0) %>% 
  data.frame(sales = .) %>% 
  group_by(sales)       %>% 
  summarise(count = n())

set.seed(123)
x2 <- 
  round(rnorm(4514, 50, 10), 0) %>% 
  data.frame(sales = .) %>% 
  group_by(sales) %>% 
  summarise(count = n())

sales <- merge(x1, x2, by = 'sales', all.x = T)
sales[is.na(sales)] <- 0
sum(is.na(sales))

sales <- 
  sales %>% 
  mutate(good = count.x - count.y,
         bad  = count.y) %>% 
  select(sales, good, bad)

sales <- produce_probs(as.character(sales$sales),
                       sales$good,
                       sales$bad,
                       'sales')
sales$sales <- as.numeric(as.character(sales$sales))

# Duration
set.seed(123)
x1 <- 
  round(rbeta(1e5, 2, 6) * 90, 0) %>% 
  data.frame(durt = .) %>% 
  group_by(durt) %>% 
  summarise(count = n())

x2 <- 
  round(rbeta(4514, 2, 5) * 90, 0) %>% 
  data.frame(durt = .) %>% 
  group_by(durt) %>% 
  summarise(count = n())

durt <- merge(x1, x2, by = 'durt', all.x = T)
durt[is.na(durt)] <- 0

durt <- 
  durt %>% 
  mutate(good = count.x - count.y, bad  = count.y) %>% 
  select(durt, good, bad)

durt <- produce_probs(as.character(durt$durt),
                      durt$good,
                      durt$bad,
                      'durt')
durt$durt <- as.numeric(as.character(durt$durt))

# Random walk GDP
gdp <- list('East E' = list(c(3344, 3364), 123), 
            'West E' = list(c(9268, 9278), 220), 
            'China'  = list(c(12207, 12267), 300), 
            'India'  = list(c(2640, 2660), 225), 
            'US'     = list(c(19475, 19495), 227))
gdp <- lapply(setNames(names(gdp), names(gdp)),
              function(i){my_random_walk(365, gdp[[i]][[2]], gdp[[i]][[1]])})


# Oil prices
oil_price <- my_random_walk(365, 223, c(77, 90))

rm(x1, x2)

# Pick non-default events
n_events    <- 100000 - 4514
good_seed   <- 125
good_events <- data.frame(
  def_event  = 0,
  overdue_t  = my_sample(good_seed, n_events, durt$durt, durt$good),
  b_unit     = my_sample(good_seed, n_events, bu$bu, bu$good),
  b_segm     = my_sample(good_seed, n_events, bs$bs, bs$good),
  b_area     = my_sample(good_seed, n_events, buar$buar, buar$good),
  inter_co   = my_sample(good_seed, n_events, isic$isic, isic$good),
  bank_guar  = my_sample(good_seed, n_events, bgar$bgar, bgar$good),
  cust_size  = my_sample(good_seed, n_events, csize$csize, csize$good),
  ar_balance = my_sample(good_seed, n_events, balance$balance, balance$good),
  contr_term = my_sample(good_seed, n_events, cterm$cterm, cterm$good),
  sales_amt  = my_sample(good_seed, n_events, sales$sales, sales$good)
)

n_events   <- 4514
bad_seed   <- 126
bad_events <- data.frame(
  def_event  = 1,
  overdue_t  = my_sample(good_seed, n_events, durt$durt, durt$bad),
  b_unit     = my_sample(good_seed, n_events, bu$bu, bu$bad),
  b_segm     = my_sample(good_seed, n_events, bs$bs, bs$bad),
  b_area     = my_sample(good_seed, n_events, buar$buar, buar$bad),
  inter_co   = my_sample(good_seed, n_events, isic$isic, isic$bad),
  bank_guar  = my_sample(good_seed, n_events, bgar$bgar, bgar$bad),
  cust_size  = my_sample(good_seed, n_events, csize$csize, csize$bad),
  ar_balance = my_sample(good_seed, n_events, balance$balance, balance$bad),
  contr_term = my_sample(good_seed, n_events, cterm$cterm, cterm$bad),
  sales_amt  = my_sample(good_seed, n_events, sales$sales, sales$bad)
)

dt <- rbind(good_events, bad_events)

exceptions <- c('dt', 'gdp', 'oil_price', 'my_runif', 'my_sample')
rm(list = setdiff(ls(), exceptions))


# Find GDP regression to default -----------------------------------------------

dt <- as.data.table(dt)
nrow(dt[def_event == 0, ])

# Find by region linear regression to fit into sigmoid function for better
# classification
gdp_dt <- 
  lapply(gdp, function(i){as.data.table(data.frame(value = i))}) %>% 
  rbindlist(., use.names = T, idcol = 'region')

by_region <- 
  dt %>% 
  group_by(b_unit, def_event) %>% 
  summarise(count = n()) %>% 
  as.data.table(.) %>% 
  dcast(., b_unit ~ def_event, value.var = 'count') %>% 
  setnames(., old = colnames(.), new = c('region', 'good', 'bad')) %>% 
  .[, total := good + bad]
  
gdp_dt <- merge(gdp_dt, by_region, by = 'region', all.x = T)

tmp_dt <- 
  gdp_dt %>% 
  .[, .(mean_bad = mean(bad)), by = region] %>% 
  .[, desc_gdp := c(0.41, 0.33, 0.21, 0.56, 0.34)] %>% 
  .[, ascn_gdp := 1 - desc_gdp]

bad_gdp <- 
  lapply(
    setNames(tmp_dt[['region']], tmp_dt[['region']]),
    function(i){
      as.data.table(
        data.frame(
          value = my_sample(seed = nchar(i), 
                            n    = tmp_dt[region == i, mean_bad], 
                            from = c('descending', 'ascending'),
                            prob = c(tmp_dt[region == i, desc_gdp],
                                     tmp_dt[region == i, ascn_gdp]))
        )
      )
    }
  ) %>% 
  rbindlist(., use.names = T, idcol = 'b_unit')

bad_gdp
bad_gdp$b_unit <- as.character(bad_gdp$b_unit)
bad_gdp$value  <- as.character(bad_gdp$value)
rm(gdp_dt, gdp, by_region, tmp_dt)


# Add GDP growing or decreasing to main table
tmp_dt     <- copy(dt)
tmp_dt$gdp <- 'none'

for (i in unique(dt$b_unit)){
  tmp_dt$gdp[tmp_dt$def_event == 1 & tmp_dt$b_unit == i] <- 
    bad_gdp[b_unit == i, value]
}

tmp_dt$gdp[tmp_dt$def_event == 0] <- 
  my_sample(seed = 125,
            n    = length(tmp_dt[def_event == 0, gdp]),
            from = c('descending', 'ascending'),
            prob = c(0.52, 0.48))

rm(bad_gdp, dt, i, oil_price, my_runif)

set.seed(123)
idx_shuffle <- sample(x = seq(nrow(tmp_dt)), size = nrow(tmp_dt))
dt          <- tmp_dt[idx_shuffle, ]
rm(tmp_dt, idx_shuffle, my_sample)

raw_dt <- copy(dt)

# One hot encoding
dt[, `:=`(is_china = ifelse(b_unit == 'China',    1, 0),
          is_easte = ifelse(b_unit == 'East E',   1, 0),
          is_india = ifelse(b_unit == 'India',    1, 0),
          is_usa   = ifelse(b_unit == 'US',       1, 0),
          is_aem   = ifelse(b_segm == 'AEM',      1, 0),
          e_buar   = ifelse(b_area == 'East',     1, 0),
          n_buar   = ifelse(b_area == 'North',    1, 0),
          s_buar   = ifelse(b_area == 'South',    1, 0),
          c_large  = ifelse(cust_size == 'large', 1, 0),
          c_small  = ifelse(cust_size == 'small', 1, 0),
          a_gdp    = ifelse(gdp == 'ascending',   1, 0))]
dr <- c('b_unit', 'b_segm', 'b_area', 'cust_size', 'gdp')
dt <- dt[, !dr, with = FALSE]
rm(dr)
