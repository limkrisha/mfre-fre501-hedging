rm(list = ls()) 
graphics.off()

# load packages and data
pacman::p_load(here, dplyr, ggplot2)
data <- read.csv(here("Data", "corn_ethanol_data.csv"), header=TRUE, sep=",", stringsAsFactors = FALSE) 

data$Date <- as.Date(data$Week, format = c("%m/%d/%Y")) 
head(data)

# plot the three spot price data series
plot_spot_corn <- ggplot(data, aes(x = Date, y = CornSpot)) +  
  geom_line() + 
  labs(title = "Weekly Corn Spot Price, Iowa", y= "$/bu", x = "Date") + 
  theme(plot.title = element_text(size=10)) 
plot_spot_corn
  
plot_spot_DDG <- ggplot(data, aes(x = Date, y = DDG)) +  
    geom_line() + 
    labs(title = "Weekly DDG Spot Price, Iowa", y= "$/Ton", x = "Date") + 
    theme(plot.title = element_text(size=10)) 
plot_spot_DDG
  
plot_spot_ethanol <- ggplot(data, aes(x = Date, y = Ethanol)) +  
    geom_line() + 
    labs(title = "Weekly Ethanol Spot Price, Iowa", y= "$/Gallon", x = "Date") + 
    theme(plot.title = element_text(size=10)) 
plot_spot_ethanol

# uncomment to overwrite files
# ggsave(plot_spot_corn, file = here("Images", "CornSpot.png"), dpi = 150)
# ggsave(plot_spot_DDG, file = here("Images", "DDG.png"), dpi = 150)
# ggsave(plot_spot_ethanol, file = here("Images", "Ethanol.png"), dpi = 150)

# construct an ethanol revenue and cost data series (standardized to $/bu) and then plot.
data <- mutate(data, 
               Revenue = 0.0085*DDG + 2.8*Ethanol,
               Cost = CornSpot)

plot_crush <- ggplot(data, aes(x = Date)) + 
  geom_line(aes(y = Revenue, color = "Revenue")) + 
  geom_line(aes(y = Cost, color = "Cost")) + 
  labs(title = "Revenue and Cost in Ethanol Productions", y = "$/bu", x = "Date") +
  theme(plot.title = element_text(size=10)) +
  scale_color_manual(name = "Variable", values = c("Revenue" = "blue", "Cost" = "red"))
plot_crush

# ggsave(plot_crush, file = here("Images", "Revenue_Cost.png"), dpi = 150)

# optimal hedge ratio (beta when regression log of spot price on log of futures price)
data <- data %>%
  mutate(log_spot = log(CornSpot),
         log_fut = log(CornFut/100))

hedge_ratio <- lm(log_spot ~ log_fut, data = data)
summary(hedge_ratio)

