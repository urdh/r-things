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
	default_geom <- function(.) GeomSmooth
	objname <- "rollapplyr"
	calculate_groups <- function(., data, scales, ...) {
		.super$calculate_groups(., data, scales, ...)
	}
	calculate <- function(., data, scales, width, FUN, lowlimit = 0.0, na.fill=NA, ...) {
		require(zoo)
		# TODO make this a parameter
		ignored_group <- (data$group[1] >= 10)
		with_ribbon <- (!is.null(data$ymax) && !is.null(data$ymin)) && !ignored_group;

		filtered <- rollapplyr(data$y, width, FUN, fill=na.fill, ..., align="center")
		filtered[filtered < lowlimit] <- NA

		if(with_ribbon) {
			minvals <- rollapplyr(data$ymin, floor(width/2), min, fill=c("extend"), ..., align="center")
			maxvals <- rollapplyr(data$ymax, floor(width/2), max, fill=c("extend"), ..., align="center")
			minvals[minvals == Inf] <- NA
			maxvals[maxvals == -Inf] <- NA
			minvals <- rollapplyr(minvals, width, mean, fill=na.fill, ..., align="center")
			maxvals <- rollapplyr(maxvals, width, mean, fill=na.fill, ..., align="center")
			minvals[filtered < lowlimit] <- NA
			maxvals[filtered < lowlimit] <- NA
			return(data.frame(x=data$x, y=filtered, ymin=minvals, ymax=maxvals))
		} else {
			return(data.frame(x=data$x, y=filtered))
		}
	}
})
stat_rollapplyr <- StatRollApplyR$new

# Helper map for converting columns to party names
partyNameMap <- c("M" = "Moderaterna",
								  "FP" = "Liberalerna",
									"C" = "Centerpartiet",
									"KD" = "Kristdemokraterna",
									"S" = "Socialdemokraterna",
									"V" = "Vänsterpartiet",
									"MP" = "Miljöpartiet",
									"SD" = "Sverigedemokraterna",
									"FI" = "Feministiskt initiativ",
									"Uncertain" = "Osäkra",
									"RightBlock" = "Alliansen",
									"LeftBlock" = "Rödgröna")

# Load data
data_url <- "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls <- repmis::source_data(data_url, sep = ",", header = TRUE)
parties <- c("M", "FP", "C", "KD", "S", "V", "MP", "SD", "FI", "Uncertain",
						 "RightBlock", "LeftBlock")
# Calculate block/coalition values
polls$RightBlock <- polls$M  + polls$FP + polls$C  + polls$KD
polls$LeftBlock  <- polls$S  + polls$MP + polls$V
# Convert dates to proper format
polls$PublDate <- as.Date(polls$PublDate)
polls$collectPeriodTo <- as.Date(polls$collectPeriodTo)
polls$collectPeriodFrom <- as.Date(polls$collectPeriodFrom)
# Remove NA polls
polls$PublDate[is.na(polls$PublDate)] <- polls$collectPeriodTo[is.na(polls$PublDate)]
polls$collectPeriodTo[is.na(polls$collectPeriodTo)] <- polls$PublDate[is.na(polls$collectPeriodTo)]
polls <- subset(polls, !is.na(collectPeriodFrom) & !is.na(PublDate) & !is.na(n))
# Calculate some kind of "interview-per-day" value
polls$perDay <- polls$n / as.numeric(polls$collectPeriodTo - polls$collectPeriodFrom + 1)
# Expand polls so result covers entire measurement period
polls$nrow <- 1:nrow(polls)
polls.dt <- data.table(polls)
polls.dt <- polls.dt[, list(PollDate=seq(collectPeriodFrom,collectPeriodTo,by="day")), by="nrow"]
polls.dt <- merge(polls.dt, data.table(polls), by=c("nrow"))
polls.dt.min <- copy(polls.dt)
polls.dt.max <- copy(polls.dt)
# Produce a weighted mean of polls for each day
polls.dt[, `:=`(parties, Map('*', polls.dt[, parties, with=F], polls.dt[, "perDay", with=F])), with=F]
polls.dt <- polls.dt[, lapply(.SD, sum, na.rm = TRUE), by = "PollDate", .SDcols = c(parties, "perDay")]
polls.dt[, `:=`(parties, Map('/', polls.dt[, parties, with=F], polls.dt[, "perDay", with=F])), with=F]
# Create a continuous time series using the data
polls.zoo <- zoo(polls.dt, polls.dt$PollDate)
polls.zoo <- merge(polls.zoo, zoo(,seq(start(polls.zoo), end(polls.zoo), by = 1)), all = TRUE)
polls.zoo <- subset(polls.zoo, select = parties)

# Do stuff with the copy too
polls.dt.min <- polls.dt.min[, lapply(.SD, min, na.rm = TRUE), by = "PollDate", .SDcols = c(parties)]
polls.zoo.min <- zoo(polls.dt.min, polls.dt.min$PollDate)
polls.zoo.min <- merge(polls.zoo.min, zoo(,seq(start(polls.zoo.min), end(polls.zoo.min), by = 1)), all = TRUE)
polls.zoo.min <- subset(polls.zoo.min, select = parties)
polls.dt.max <- polls.dt.max[, lapply(.SD, max, na.rm = TRUE), by = "PollDate", .SDcols = c(parties)]
polls.zoo.max <- zoo(polls.dt.max, polls.dt.max$PollDate)
polls.zoo.max <- merge(polls.zoo.max, zoo(,seq(start(polls.zoo.max), end(polls.zoo.max), by = 1)), all = TRUE)
polls.zoo.max <- subset(polls.zoo.max, select = parties)

# Transform back to data frame and reorder parties
derivData <- fortify(polls.zoo, NA, melt = TRUE)
derivData$Value <- as.numeric(levels(derivData$Value))[derivData$Value]
derivData$Series <- factor(derivData$Series,
                           levels = levels(derivData$Series)[c(6,5,7,3,4,2,1,8,9,10,11,12)])
derivData$Series <- revalue(derivData$Series, partyNameMap)
# Min/max as well
pollDataMin <- fortify(polls.zoo.min, NA, melt = TRUE)
pollDataMax <- fortify(polls.zoo.max, NA, melt = TRUE)
derivData$MinValue <- as.numeric(levels(pollDataMin$Value))[pollDataMin$Value]
derivData$MaxValue <- as.numeric(levels(pollDataMax$Value))[pollDataMax$Value]
derivData$MinValue[derivData$MinValue == Inf] = NA
derivData$MaxValue[derivData$MaxValue == -Inf] = NA

# Do the same for non-continous non-averaged data
pollData <- melt(polls, id.vars = "PublDate", parties,
								 variable.name = "Series", value.name = "Value")
pollData$Index <- pollData$PublDate
pollData$Series <- factor(pollData$Series,
													levels = levels(pollData$Series)[c(6,5,7,3,4,2,1,8,9,10,11,12)])
pollData$Value[pollData$Series == "RightBlock"] <- NA
pollData$Value[pollData$Series == "LeftBlock"] <- NA
pollData$MinValue <- pollData$Value
pollData$MaxValue <- pollData$Value
pollData$Series <- revalue(pollData$Series, partyNameMap)
# Define the color scheme for the plot (colors from http://sv.wikipedia.org/wiki/Mall:Partifärg)
colors <- c("#b70410", "#f9232b", "#79cf49", "#00993c", "#211974",
						"#5cb7e9", "#0049d8", "#dedd37", "#e2328d", "#cccccc",
						"#0049d8", "#f9232b") # blocks take colors from (M) and (S) resp.
# Line type and alpha to separate parties from blocks/coalitions
lineType <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2)
alphas <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5)

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
	Uncertain = c(NA, NA, NA, NA),
	RightBlock = c(NA, NA, NA, NA),
	LeftBlock = c(NA, NA, NA, NA)
)
electionData <- melt(elections, id.vars = "PublDate", parties,
								variable.name = "Series", value.name = "Value")
electionData$Index <- electionData$PublDate
electionData$MinValue <- electionData$Value
electionData$MaxValue <- electionData$Value
electionData$Series <- factor(electionData$Series,
													levels = levels(electionData$Series)[c(6,5,7,3,4,2,1,8,9,10,11,12)])
electionData$Series <- revalue(electionData$Series, partyNameMap)
electionData <- subset(electionData, Index > as.Date("2003-01-01"))

# Actual plot
get_plot <- function () {
	p <- ggplot(derivData, aes(x = Index, y = Value, ymin = MinValue, ymax = MaxValue,
														 color = Series, group = Series, linetype = Series, alpha = Series))
	return(
		p + geom_point(data = pollData, alpha = 0.125) +
				geom_point(data = electionData, shape = 18) +
		    stat_rollapplyr(width = 84, FUN = median, lowlimit = 1.0, na.rm = TRUE, alpha = 0.15) +
		    geom_hline(yintercept = 4, colour = "#333333", linetype = "dashed") +
		    scale_colour_manual(name = "Parti", values = colors) +
				scale_linetype_manual(name = "Parti", values = lineType) +
				scale_alpha_manual(name = "Parti", values = alphas) +
		    labs(x = "Datum", y = "Stöd (%)") +
		    scale_x_date(breaks = date_breaks("1 year"),
		                 minor_breaks = date_breaks("1 month"),
		                 labels = date_format("%Y")) +
		    scale_y_continuous(breaks = 0:12*5, minor_breaks = 0:60, limits = c(0, 60))
	)
}
plot <- get_plot()

# Outputs
png("polls.png", width = 1920, height = 1080)
print(plot)
dev.off()

pdf("polls.pdf", width = 11.692, height = 8.267)
print(plot)
dev.off()
