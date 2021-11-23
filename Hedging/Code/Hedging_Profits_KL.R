rm(list = ls()) 
graphics.off()

# load packages and data
pacman::p_load(here, dplyr, ggplot2, kableExtra, magick, webshot)
profits <- readRDS(file = here("Code", "compare_KL.rds"))
head(profits)

# Create histograms
crush_hedged <- ggplot(profits, aes(x=crushH)) +  xlab("Deviation from Target Crush Margin, Hedged ($/bu)") +
  geom_histogram(color="black", fill="white", bins = 30)
crush_hedged

crush_nohedged <- ggplot(profits, aes(x=crushNH)) +  xlab("Deviation from Target Crush Margin, No Hedge ($/bu)") +
  geom_histogram(color="black", fill="white", bins = 30)
crush_nohedged

basis_change <- ggplot(profits, aes(x=BasisChange)) +  xlab("Change in Basis over Hedging Period ($/bu)") +
  geom_histogram(color="black", fill="white", bins = 30)
basis_change

long_corn <- ggplot(profits, aes(x=long_corn)) +  xlab("Profits, Long in Corn ($/bu)") +
  geom_histogram(color="black", fill="white", bins = 30)
long_corn

short_hedge <- ggplot(profits, aes(x=short_hedge)) +  xlab("Profits, Short Hedge ($/bu)") +
  geom_histogram(color="black", fill="white", bins = 30)
short_hedge

# save images - uncomment to run and overwrite files 
# ggsave(crush_hedged, file = here("Images", "hist_crush_hedged.png"), dpi = 150)
# ggsave(crush_nohedged, file = here("Images", "hist_crush_nohedged.png"), dpi = 150)
# ggsave(basis_change, file = here("Images", "hist_basischange.png"), dpi = 150)
# ggsave(long_corn, file = here("Images", "hist_long_corn.png"), dpi = 150)
# ggsave(short_hedge, file = here("Images", "hist_short_hedge.png"), dpi = 150)

crushH_percentile <- quantile(profits$crushH, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))

crushNH_percentile <- quantile(profits$crushNH, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))

basischange_percentile <- quantile(profits$BasisChange, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))

longhedge_percentile <- quantile(profits$long_hedge, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))

shorthedge_percentile <- quantile(profits$short_hedge, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))

crushH_percentile
crushNH_percentile
basischange_percentile
longhedge_percentile
shorthedge_percentile

perc <- rbind(crushH_percentile, crushNH_percentile, basischange_percentile, longhedge_percentile, shorthedge_percentile)

# rename rows
rownames(perc) <- c("Crush Hedged", "Crush No Hedged", "Basis Change", "Long Hedge", "Short Hedge")
perc

# create table
# for saving the table
webshot::install_phantomjs(force = TRUE)

perc %>%
  kbl() %>%
  kable_styling() %>%
  save_kable(here("Images", "summary.png"))
  
# if above does not work, just use 
perc %>%
  kbl() %>%
  kable_styling() 
