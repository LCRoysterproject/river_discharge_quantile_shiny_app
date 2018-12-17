dis_quant <- dis_noleap %>%
  mutate(md = strftime(dates, format = "%m-%d")) %>% #<- this is to create a day and month in a date time format
  group_by(md) %>% #<- grouping by day
  # by summarising after group_by, this means taking all val that has same md, and calculate 0th percentile of
  # them using 'quantile(val, 0)', and assign the value to 'quan0'.
  # Likewise for 25th, 50th, 75th and 100th percentile, so the table after this point has six columns
  # md, quan0, quan25, quan50, quan75, quan100
  summarise(quan0 = quantile(val, 0),
            quan25 = quantile(val, 0.25),
            quan50 = quantile(val, 0.50),
            quan75 = quantile(val, 0.75),
            quan100 = quantile(val, 1)) %>%
  # The table is in 'wide' format, which is not suitable for ggplot so we'll need to convert it to
  # 'long' format: three columns md, quantile, val. The number of row of the table is expected to be 365*5
  # What 'gather' does is take all the column (except md because we have '-md' argument), put all the value in those
  # columns to 'val' and their corresponding column name to 'quantile' column
  gather("quantile", "val", -md)

# The dis_quant now is three columns but the entries for 'quantile' column are 'quan0', 'quan25' etc
# (those were the column names). So we remove the 'quan' from all these entries.
# After removal, the entries of this column are still characters, if we do plotting now, ggplot automatically
# convert them to factor, and order them by alphabetical order, so it will be "0", "100", "25", "50", "75".
# Not only the ordering is not right but ggplot will put the ribbon for 0 percentile first, then cover it with
# ribbon for 100th percentile. Since 100th percentile always > 0th, the 0th percentile ribbon will never show
# up in final plot. So it's important to reorder the level so that the biggest value get plotted first.
dis_quant$quantile <- str_remove(dis_quant$quantile, "quan") %>%
  factor(levels = c("100", "75", "50", "25", "0"))
