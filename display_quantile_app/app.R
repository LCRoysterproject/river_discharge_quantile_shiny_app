library("tidyverse")
library("lubridate")
library("waterData")
library("shiny")

# station and site
station = '02323500'   
stinfo  = siteInfo(station)

# Load data and dynamically check if data needs to be updated
dis <- read_rds("data/dis1.rds")
if (max(dis$dates) < (Sys.Date() - 5)) {
  sdate <- max(dis$dates) + 1
  newdis <- importDVs(staid = station, code = '00060', stat = '00003', sdate= as.character(sdate))
  dis <- bind_rows(dis, newdis) %>%
    distinct() # In case there's repeated rows due to code failure
  write_rds(dis, "data/dis1.rds")
}

# Data carpentries and create quantile data table
dis_noleap <- dis %>%
  filter(!(month(dates) == 2 & day(dates) == 29)) %>% 
  na.omit()#<- rempving the leap day year for all years that have it

dis_quant <- dis_noleap %>%
  mutate(md = strftime(dates, format = "%m-%d")) %>%
  group_by(md) %>%
  summarise(quan0 = quantile(val, 0),
            quan25 = quantile(val, 0.25),
            quan50 = quantile(val, 0.50),
            quan75 = quantile(val, 0.75),
            quan100 = quantile(val, 1)) %>%
  gather("quantile", "val", -md) %>% 
  na.omit()
dis_quant$quantile <- str_remove(dis_quant$quantile, "quan") %>%
  factor(levels = c("100", "75", "50", "25", "0"))

#### UI ####
ui <- fluidPage(
   
   titlePanel("Suwannee River Discharge Quantiles"),
   
   sidebarLayout(
      sidebarPanel(
        
        h4("Percentile Description"),
        helpText("A percentile is a value on a scale of one hundred that indicates the percent of a distribution that is equal to or below it. For example, on the map of daily streamflow conditions a river discharge at the 90th percentile is equal to or greater than 90 percent of the discharge values recorded on this day of the year during all years that measurements have been made.  In general,a percentile greater than 75 is considered above normal, a percentile between 25 and 75 is considered normal, and a percentile less than 25 is considered below normal"),
        
         sliderInput("yoi",
                     "Year:",
                     min = 1950, sep = "",
                     max = year(Sys.Date()),
                     value = year(Sys.Date()),
                     step = 1)
      ),
      
      mainPanel(
          width = 7,
         plotOutput("quantPlot", height = "600px")
      )
   )
)

#### SERVER ####
server <- function(input, output) {
   
   output$quantPlot <- renderPlot({
     dis_quant1 <- dis_quant %>%
       mutate(dates = paste(input$yoi, md, sep="-") %>% as.Date)
     
     dis_yoi <- dis_noleap %>%
       filter(year(dates) == input$yoi)
     
     cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
     
     
     ggplot(dis_yoi, aes(x=dates, y=val)) +
       ggtitle(input$yoi) +
       ylab("River Discharge (ft^3)")+
       xlab ("Date") +
       guides(fill=guide_legend(title="Quantiles")) +
       geom_ribbon(data = dis_quant1, aes(x=dates, ymax=val, ymin=0, fill=quantile)) +
       geom_line(size=1.2) +
       scale_fill_manual(values=cbPalette) +
       theme_minimal() +
       theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

