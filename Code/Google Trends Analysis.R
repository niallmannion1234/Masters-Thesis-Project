setwd("D:\\1 Masters\\Dataset\\trainingandtestdata")

# Scrape and analyst data from Google trends
vaccine_line <- gtrends(c("vaccine", "vaccination", "swine", "covid-19"), geo = "US", time = "all")
plot.gtrends.silent <- function(x, ...) {df <- x$interest_over_time
df$date <- as.Date(df$date) 
df$hits <- if(typeof(df$hits) == 'character'){ 
  as.numeric(gsub('<','',df$hits)) } else { df$hits} 
df$legend <- paste(df$keyword, " (", df$geo, ")", sep = "") 
p <- ggplot(df, aes_string(x = "date", y = "hits", color = "legend")) + 
  geom_line(size = 2) + 
  xlab("Date") + 
  ylab("Search hits") + 
  ggtitle("") + 
  theme_bw() 
invisible(p)}
Project_Theme = theme(
  axis.title.x = element_text(size = 19),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 22),
  legend.text = element_text(color = "black", size = 15),
  plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))
my_plot <- plot.gtrends.silent(vaccine_line) 
my_plot + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + Project_Theme +
  ggtitle("Google Search Interest (US)")

# side effects interest 
other_line <- gtrends(c("immunization", "mainstream media",
                        "pharmaceutical industry", "vaccine"), geo = "US", time = "all")
other_plot <- plot.gtrends.silent(other_line) 
other_plot + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + Project_Theme +
  ggtitle("Google Search Interest in Vaccine Related Topics Over Time (US)")

# media and pharmaceutical companies 
side_effects_line <- gtrends(c("vaccine side effects", "thimerosal", "vaccines autism",
                               "vaccine autism link"), geo = "US", time = "all")
side_effects_plot <- plot.gtrends.silent(side_effects_line) 
side_effects_plot + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + Project_Theme +
  ggtitle("Google Search Interest in Vaccine Side Effects Over Time (US)")

# Interest in vaccines by state
my_theme <- function() {theme_bw() + theme(panel.background = element_blank(),
                                           plot.background = element_rect(fill = "seashell"), panel.border = element_blank(),
                                           strip.background = element_blank(), plot.margin = unit(c(.5, .5, .5, .5), "cm"), 
                                           panel.spacing = unit(3, "lines"), panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(), legend.background = element_blank(),
                                           legend.key = element_blank(), legend.title = element_blank())}
my_theme2 <- function() {
  my_theme() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())}
gtrends_vaccine <- gtrends(c("vaccine"), time  = "today 12-m", geo = c("US"))
gtrends_vaccine <- gtrends_vaccine$interest_by_region
statesMap    <- map_data("state")
gtrends_vaccine$region  <- sapply(gtrends_vaccine$location,tolower)
final_gtrends   <- merge(statesMap ,gtrends_vaccine,by="region")
regionLabels <- aggregate(cbind(long, lat) ~ region, data=final_gtrends, 
                          FUN=function(x) mean(range(x)))
final_gtrends %>% ggplot() + geom_polygon(aes(x=long, y=lat,
                                              group=group, fill = hits)) + 
  guides(fill = "colorbar") + geom_text_repel(data = regionLabels,
                                              aes(long, lat, label = region), size = 5) + scale_fill_distiller(palette =
                                                                                                                 "Reds") + my_theme2() + theme(legend.position = "none") + theme(plot.title =
                                                                                                                                                                                   element_text(hjust = 0.5)) + coord_fixed(1.3) + 
  ggtitle("Google search interest for 'vaccine' in the US over the last 12 months")

setwd("D:\\1 Masters\\Dataset\\Final Datasets")
gtrends_iot <- vaccine_line$interest_over_time
write.csv(gtrends_iot, 'gtrends.csv') 
