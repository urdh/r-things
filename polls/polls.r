# Load libraries
library(data.table)
library(devtools)
library(repmis)
library(lubridate)
library(zoo)
library(scales)
library(ggplot2)
library(reshape2)
library(plyr)
library(proto)

# Helper function for rolling mean in ggplot2
StatRollApplyR <- proto(ggplot2:::Stat, {
	required_aes <- c("x", "y")
	default_geom <- function(.) GeomLine
	objname <- "rollapplyr"
	calculate_groups <- function(., data, scales, ...) {
		.super$calculate_groups(., data, scales, ...)
	}
	calculate <- function(., data, scales, width, FUN, fill=NA, ...) {
		require(zoo)
		filtered <- rollapplyr(data$y, width, FUN, fill=fill, ...)
		result <- data.frame(x=data$x, y=filtered)
		return(result)
	}
})
stat_rollapplyr <- StatRollApplyR$new

# Load data
data_url <- "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls <- repmis::source_data(data_url, sep = ",", dec = ".", header = TRUE)
parties <- c("M", "FP", "C", "KD", "S", "V", "MP", "SD", "FI", "Uncertain")
# Convert dates to proper format
polls$PublDate <- as.Date(polls$PublDate)
polls$collectPeriodTo <- as.Date(polls$collectPeriodTo)
polls$collectPeriodFrom <- as.Date(polls$collectPeriodFrom)
# Remove NA polls
polls$PublDate[is.na(polls$PublDate)] <- polls$collectPeriodTo[is.na(polls$PublDate)]
polls$collectPeriodTo[is.na(polls$collectPeriodTo)] <- polls$PublDate[is.na(polls$collectPeriodTo)]
polls$collectPeriodFrom[is.na(polls$collectPeriodFrom)] <- polls$PublDate[is.na(polls$collectPeriodFrom)]
polls <- subset(polls, !is.na(PublDate) & !is.na(n))
# Expand polls so result covers entire measurement period
polls$nrow <- 1:nrow(polls)
polls.dt <- data.table(polls)
polls.dt <- polls.dt[, list(PollDate=seq(collectPeriodFrom,collectPeriodTo,by="day")), by="nrow"]
polls.dt <- merge(polls.dt, data.table(polls), by=c("nrow"))
# Produce a weighted mean of polls for each day
polls.dt[, `:=`(parties, Map('*', polls.dt[, parties, with=F], polls.dt[, "n", with=F])), with=F]
polls.dt <- polls.dt[, lapply(.SD, sum), by = "PollDate", .SDcols = c(parties, "n")]
polls.dt[, `:=`(parties, Map('/', polls.dt[, parties, with=F], polls.dt[, "n", with=F])), with=F]
# Create a continuous time series using the data
polls.zoo <- zoo(polls.dt, polls.dt$PollDate)
polls.zoo <- merge(polls.zoo, zoo(,seq(start(polls.zoo), end(polls.zoo), by = 1)), all = TRUE)
polls.zoo <- subset(polls.zoo, select = parties)

# Transform back to data frame and reorder parties
derivData <- fortify(polls.zoo, NA, melt = TRUE)
derivData$Value <- as.numeric(levels(derivData$Value))[derivData$Value]
derivData$Series <- factor(derivData$Series,
                           levels = levels(derivData$Series)[c(6,5,7,3,4,2,1,8,9,10)])
derivData$Series <- revalue(derivData$Series, c("Uncertain" = "Osäkra"))
# Do the same for non-continous non-averaged data
pollData <- melt(polls, id.vars = "PublDate", parties,
								 variable.name = "Series", value.name = "Value")
pollData$Index <- pollData$PublDate
pollData$Series <- factor(pollData$Series,
													levels = levels(pollData$Series)[c(6,5,7,3,4,2,1,8,9,10)])
pollData$Series <- revalue(pollData$Series, c("Uncertain" = "Osäkra"))
# Define the color scheme for the plot (colors from http://sv.wikipedia.org/wiki/Mall:Partifärg)
colors <- c("#b70410", "#f9232b", "#79cf49", "#00993c", "#211974",
						"#5cb7e9", "#0049d8", "#dedd37", "#e2328d", "#cccccc")

# Create data for election results
elections <- data.frame(
	PublDate = c(as.Date("2002-09-15"), as.Date("2006-09-17"),
							 as.Date("2010-09-19"), as.Date("2014-09-14")),
	M = c(15.26, 26.23, 30.06, 23.33),
	FP = c(13.39, 7.54, 7.06, 5.42),
	C = c(6.19, 7.88, 6.56, 6.11),
	KD = c(9.15, 6.59, 5.60, 4.57),
	S = c(39.85, 34.99, 30.66, 31.01),
	V = c(8.39, 5.85, 5.60, 5.72),
	MP = c(4.65, 5.24, 7.34, 6.89),
	SD = c(1.44, 2.93, 5.70, 12.86),
	FI = c(NA, 0.68, 0.40, 3.12),
	Uncertain = c(NA, NA, NA, NA)
)
electionData <- melt(elections, id.vars = "PublDate", parties,
								variable.name = "Series", value.name = "Value")
electionData$Index <- electionData$PublDate
electionData$Series <- factor(electionData$Series,
															levels = levels(electionData$Series)[c(6,5,7,3,4,2,1,8,9,10)])
electionData$Series <- revalue(electionData$Series, c("Uncertain" = "Osäkra"))
electionData <- subset(electionData, Index > as.Date("2003-01-01"))

# Actual plot
do_plot <- function () {
	p <- ggplot(derivData, aes(x = Index, y = Value, color = Series, group = Series))
	p + geom_point(data = pollData, alpha = 0.125) +
			geom_point(data = electionData, shape = 18) +
	    stat_rollapplyr(width = 84, FUN = mean, na.rm = TRUE) +
	    geom_hline(yintercept = 4, colour = "#333333", linetype = "dashed") +
	    scale_colour_manual(name = "Parti", values = colors) +
	    labs(x = "Datum", y = "Stöd (%)") +
	    scale_x_date(breaks = date_breaks("1 year"),
	                 minor_breaks = date_breaks("1 month"),
	                 labels = date_format("%Y")) +
	    scale_y_continuous(breaks = 0:10*5, minor_breaks = 0:50, limits = c(0, 50))
}

# Outputs
png("polls.png", width = 1920, height = 1080)
print(do_plot())
dev.off()

pdf("polls.pdf", width = 11.692, height = 8.267)
print(do_plot())
dev.off()

#library(tikzDevice)
#tikz("polls.tikz", width = 11.692, height = 8.267)
#print(do_plot())
#dev.off()

#library(SVGAnnotation)
#svg("polls.svg", width = 11.692, height = 8.267)
#print(do_plot())
#dev.off()
#radioShowHide("polls.svg", labels = c("V","S","MP","C","KD","FP","M","SD","FI","Uncertain"))
