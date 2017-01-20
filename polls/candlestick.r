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

preprocess <- function(polldata, party) {
  # Extract the party value we are interested in
  polldata[["Value"]] <- polldata[, c(party)]
  # Convert dates to a suitable format
  polldata[["PublDate"]]      <- as.Date(polldata[["PublDate"]])
  polldata[["PublYearMonth"]] <- floor_date(polldata[["PublDate"]], "month")
  # Remove polls with missing data
  nopubdate <- is.na(polldata[["PublDate"]])
  noenddate <- is.na(polldata[["collectPeriodTo"]])
  polldata[["PublDate"]][nopubdate] <- polldata[["collectPeriodTo"]][nopubdate]
  polldata[["collectPeriodTo"]][noenddate] <- polldata[["PublDate"]][noenddate]
  polldata <-
    subset(polldata, !(is.na(collectPeriodFrom) | is.na(PublDate) | is.na(n))) # nolint
  # Done!
  return(polldata)
}

get_plot <- function(data, title) {
  # Colors from Avanza; up is blue and down is red
  colors <- c("#74B6D3", "#E8926F")

  # nolint start
  p <- ggplot(
    polls_dt,
    aes(
      x = PublYearMonth, ymin = ValueMin, ymax = ValueMax,
      lower = ValueOpen, upper = ValueClose, middle = ValueMiddle,
      group = PublYearMonth, color = ValueUpTrend, fill = ValueUpTrend
    )
  )
  # nolint end

  # Plot data
  p <- p + geom_boxplot(stat = "identity")

  # Styling
  p <- p + scale_colour_manual(guide = FALSE, values = colors) +
           scale_fill_manual(  guide = FALSE, values = colors)

  # Labels and scales
  p <- p + labs(x = "Datum", y = "Stöd (%)", title = title)
  p <- p + scale_x_date(
    breaks = date_breaks("1 year"),
    minor_breaks = date_breaks("1 month"),
    labels = date_format("%Y")
  )
  p <- p + scale_y_continuous(
    breaks = 0:12 * 5,
    minor_breaks = 0:60,
    limits = c(0, 60)
  )

  # Done!
  return(p)
}

# What party are we interested in?
PARTY <- "S"

# Helper map for converting columns to party names
partynames <- c(
  "M"          = "Moderaterna",
  "L"         = "Liberalerna",
  "C"          = "Centerpartiet",
  "KD"         = "Kristdemokraterna",
  "S"          = "Socialdemokraterna",
  "V"          = "Vänsterpartiet",
  "MP"         = "Miljöpartiet",
  "SD"         = "Sverigedemokraterna",
  "FI"         = "Feministiskt initiativ",
  "Uncertain"  = "Osäkra"
)

# Load data
data_url  <- "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
poll_data <- preprocess(repmis::source_data(data_url, sep = ",", header = TRUE), PARTY)

# Get max/min values etc.
polls_dt  <- data.table(poll_data)
polls_dt[, ValueMax := lapply(.SD, max, na.rm = TRUE),
  by = "PublYearMonth", .SDcols = c("Value")]
polls_dt[, ValueMin := lapply(.SD, min, na.rm = TRUE),
  by = "PublYearMonth", .SDcols = c("Value")]
polls_dt[, ValueOpen := lapply(.SD, tail, n = 1, na.rm = TRUE),
  by = "PublYearMonth", .SDcols = c("Value")]
polls_dt[, ValueClose := lapply(.SD, head, n = 1, na.rm = TRUE),
  by = "PublYearMonth", .SDcols = c("Value")]
polls_dt[, ValueMiddle := lapply(.SD, mean, na.rm = TRUE),
  by = "PublYearMonth", .SDcols = c("Value")]
polls_dt[, ValueUpTrend := ValueOpen > ValueClose,
  by = "PublYearMonth"]

# Actual plot
plot <- get_plot(polls_dt, partynames[PARTY])

# Outputs
ggsave("candlestick.png", plot, width = 25.6, height = 14.4, units = "in", dpi = 75)
ggsave("candlestick.pdf", plot, width = 420,  height = 297,  units = "mm")
