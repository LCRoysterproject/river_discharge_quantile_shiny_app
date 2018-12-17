# library(tidyhydat)
library(tidyverse)
library(lubridate)
library(waterData)

#station to analyze
station = '02323500'   
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
# dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1950-01-01")

# importDVs is slow when it needed to pull so much data. So here's some code to dynamically update
# and store the data (TODO: write a wrapper function for it)
# Also using RDS format instead of CSV here. RDS is an R data file format that
# stores also datatype so dis$dates already is Date object when you read the file..
dis <- read_rds("data/dis1.rds")
if (max(dis$dates) < (Sys.Date() - 5)) {
  sdate <- max(dis$dates) + 1
  newdis <- importDVs(staid = station, code = '00060', stat = '00003', sdate= as.character(sdate))
  dis <- bind_rows(dis, newdis) %>%
    distinct() # In case there's repeated rows due to code failure
  write_rds(dis, "data/dis1.rds")
}

# Since dis$dates is already Date object, no action requires for dates column

# Remove leap days
dis_noleap <- dis %>%
  filter(!(month(dates) == 2 & day(dates) == 29))

# Calculate 0%, 25%, 50%, 75% and 100% quantile for each day
# dplyr and tidyr involve here
dis_quant <- dis_noleap %>%
  mutate(md = strftime(dates, format = "%m-%d")) %>%
  group_by(md) %>%
  summarise(quan0 = quantile(val, 0),
            quan25 = quantile(val, 0.25),
            quan50 = quantile(val, 0.50),
            quan75 = quantile(val, 0.75),
            quan100 = quantile(val, 1)) %>%
  gather("quantile", "val", -md)

# Remove the "quan" and set the quantile column to factor of 5 levels
# Note that the levels are on descending order because this force ggplot
# to plot quantile 100, then overlay quantile 75 on top of it, so on and
# so forth, i.e. quantile 100 won't cover other quantiles up.
dis_quant$quantile <- str_remove(dis_quant$quantile, "quan") %>%
  factor(levels = c("100", "75", "50", "25", "0"))

# Year of interest, keep this at the current year
yoi = 2018

# Add year to dis_quant's date for plotting purpose
dis_quant1 <- dis_quant %>%
  mutate(dates = paste(yoi, md, sep="-") %>% as.Date)

dis_yoi <- dis_noleap %>%
  filter(year(dates) == yoi)


# To create plots for quarterly reports

library(scales)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

quant2018<-ggplot(dis_yoi, aes(x=dates, y=val)) +
  xlab("Month")+
  ylab("River Discharge (ft^3)") +
  labs(title= "2018",fill= "Quantile") +
  geom_ribbon(data = dis_quant1, aes(x=dates, ymax=val, ymin=0, fill=quantile)) +
  geom_line(size=1.1) +
  geom_text()
    scale_fill_manual(values=cbPalette) +
  scale_x_date(labels = date_format("%b"))+
    theme_minimal() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

require(cowplot)
quantile_plot<-plot_grid(quant2013,quant2014, quant2015, quant2016, quant2017, quant2018, ncol=2, labels=c("a","b","c","d","e","f")) 

ggsave("quantile2018_2018.png", width= 10, height=10, dpi=300)
