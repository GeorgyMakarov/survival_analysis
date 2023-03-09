source('split_data.R')

# Use test data non-default accounts receivable to compute test error using the
# loss probability computed from historical training data.
ar_report <- 
  test_dt[, .SD, .SDcols = c('def_event', 'overdue_t', 'sales_amt')] %>% 
  dcast(., overdue_t ~ def_event, sum, value.var = 'sales_amt') %>% 
  setnames(., old = colnames(.), new = c('overdue_t', 'good', 'bad'))

# 1. Prepare financial statement for the end of the month with AR balances
iter_vector <- setNames(c(20, 30, 40), c(2, 3, 4))
ar_report$dso <- 1
for (i in seq_along(iter_vector)){
  ar_report$dso[ar_report$overdue_t > iter_vector[[i]]] <- as.numeric(
    names(iter_vector)[[i]]
  )
}

ar_report <- 
  ar_report %>% 
  group_by(dso) %>% 
  summarise(ar     = sum(good),
            ac_bad = sum(bad))

# 2. Prepare historical data about payments and accounts receivable ageing
ar_prev <- train_dt
iter_vector <- setNames(c(20, 30, 40), c(2, 3, 4))
ar_prev$dso <- 1
for (i in seq_along(iter_vector)){
  ar_prev$dso[ar_prev$overdue_t > iter_vector[[i]]] <- as.numeric(
    names(iter_vector)[[i]]
  )
}

## This table goes to paper
ageing <- 
  ar_prev %>% 
  group_by(dso) %>% 
  summarise(sales = sum(sales_amt),
            paid  = sum(sales_amt[def_event == 0])) %>% 
  mutate(acc_sales = cumsum(sales),
         acc_paid  = cumsum(paid),
         diff_pay  = acc_sales - acc_paid) %>% 
  mutate(total_pay = ifelse(dso < 4, acc_sales, acc_paid)) %>% 
  select(dso, sales, total_pay) %>% 
  mutate(ageing = sum(sales) - total_pay)

# 3. Compute loss probabilities
ecl_comp <- 
  ageing %>% 
  mutate(loss   = ageing[dso == 4],
         ageing = c(sum(sales), ageing[dso < 4])) %>% 
  mutate(loss_prob = loss / ageing) %>% 
  select(dso, loss, ageing, loss_prob)

# 4. Adjust for macroeconomic factors -- goes to paper
adj_loss <-  192717 * (1 + (0.05/12))
adj_ecl  <- 
  ecl_comp %>% 
  mutate(loss      = adj_loss,
         loss_prob = loss / ageing)

# 5. Compute ECL for reporting date
provision_m <- 
  ar_report %>% 
  mutate(ecl_prob  = adj_ecl$loss_prob,
         provision = round(ar * ecl_prob, 0))

mae_pm <- sum(abs(provision_m$ac_bad - provision_m$provision)) / nrow(provision_m)
err_pm <- round(mae_pm / sum(provision_m$ac_bad), 2)

rm(list = setdiff(ls(), c('ar_report', 'provision_m', 'mae_pm', 'err_pm')))
