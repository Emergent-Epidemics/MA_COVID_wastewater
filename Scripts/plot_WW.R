#Plot WW data for MA
#SV Scarpino
#Jan. 2023

###########
#libraries#
###########
library(ggplot2)
library(wesanderson)

###############
#Global Params#
###############


######
#Data#
######
dat <- read.csv("../Data/covid-19-weekly-wastewater-report-raw-data-1-5-2023.csv")

dat$Sample.collection.date <- as.POSIXct(strptime(dat$Sample.collection.date, format = "%m/%d/%y"))

######
#Plot#
######
time_series <- by(data = dat$X7.day.average.of.SARS.CoV.2.concentration, INDICES = as.character(dat$Sample.collection.date), FUN = median, na.rm = TRUE)

dates <- strptime(names(time_series), format = "%Y-%m-%d")
x <- as.numeric(time_series)


#state-level-plot
use_plot_state <- which(dates > as.POSIXct(strptime("2022-12-01", format = "%Y-%m-%d")))
plot(dates[use_plot_state], x[use_plot_state]/min(x[use_plot_state]), type = "l", lwd = 3, bty = "n", xlab = "2022-23", ylab = "Normalized 7-day avg. SARS-CoV-2 concentration", main = "SARS-CoV-2 WW signal in MA")

#site-level-plot
use_plot_site <- which(dat$Sample.collection.date > as.POSIXct(strptime("2022-09-01", format = "%Y-%m-%d")))
dat.plot <- dat[use_plot, ]

cols <- wes_palette(name = "Zissou1", n = length(unique(dat.plot$Name.of.Sampling.Location)), type = "continuous")

dat.plot$X7.day.average.of.SARS.CoV.2.concentration <- dat.plot$X7.day.average.of.SARS.CoV.2.concentration/median(dat.plot$X7.day.average.of.SARS.CoV.2.concentration, na.rm = TRUE)

ggplot(dat.plot, aes(x = Sample.collection.date, y = X7.day.average.of.SARS.CoV.2.concentration, color = Name.of.Sampling.Location)) + geom_line(alpha = 0.5, size = 1) + scale_color_manual(values = cols, name = "Sampling location") + xlab("Date (2022-23)") + ylab("Wastewater COVID-19 signal (7-day avg. smoothed normalized") + theme(legend.position = "right", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffff75", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 14), axis.title = element_text(colour = "black", size = 15), panel.grid.minor = element_line(colour = "#00000050",linetype = 3), panel.grid.major = element_line(colour = "#00000060", linetype = 3)) + geom_smooth(inherit.aes = FALSE, aes(x = Sample.collection.date, y = X7.day.average.of.SARS.CoV.2.concentration), color = "#4d4d4d", size = 1.25)

