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

StatRollApply <- # nolint
  ggproto("RollApply", Stat,
    required_aes = c("x", "y"),
    compute_group = function(data, scales, FUN = median, width = 84,
                             rm.below = NULL, rm.above = NULL, ...) {
      require(zoo)
      filtered <-
        rollapplyr(data$y, width, FUN, fill = NA, ..., align = "center")
      if (!is.null(rm.below))
        filtered[filtered < rm.below] <- NA
      if (!is.null(rm.above))
        filtered[filtered > rm.above] <- NA
      return(data.table(x = data$x, y = filtered))
    }
  )

StatRollApplyRibbon <- # nolint
  ggproto("RollApplyRibbon", Stat,
    required_aes = c("ymin", "ymax"),
    compute_group = function(data, scales, FUN = mean, width = 84, ...) {
      require(zoo)
      minima <-
        rollapplyr(data$ymin, floor(width / 2), min,
                   fill = c("extend"), ..., align = "center")
      minima[minima ==  Inf] <- NA
      data$ymin <- rollapplyr(minima, width, mean,
                              fill = NA, ..., align = "center")

      maxima <-
        rollapplyr(data$ymax, floor(width / 2), max,
                   fill = c("extend"), ..., align = "center")
      maxima[maxima == -Inf] <- NA
      data$ymax <- rollapplyr(maxima, width, mean,
                              fill = NA, ..., align = "center")
      return(data.table(x = data$x, ymin = data$ymin, ymax = data$ymax))
    }
  )

stat_rolling_median <-
  function(mapping = NULL, data = NULL, width = 84,
           geom = "smooth", position = "identity",
           show.legend = NA, inherit.aes = TRUE,
           rm.below = NULL, rm.above = NULL, ...) {
    layer(stat = StatRollApply, # nolint
      data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(FUN = median, width = width,
                    rm.below = rm.below, rm.above = rm.above, ...)
    )
  }

stat_rolling_mean_ribbon <-
  function(mapping = NULL, data = NULL, width = 84,
           geom = "ribbon", position = "identity",
           show.legend = NA, inherit.aes = TRUE, ...) {
    layer(stat = StatRollApplyRibbon, # nolint
      data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(FUN = mean, width = width, ...)
    )
  }

preprocess <- function(polldata) {
  # Calculate coalition values
  polldata[["RightBlock"]] <- rowSums(polldata[, c("M", "L", "C", "KD")])
  polldata[["LeftBlock"]]  <- rowSums(polldata[, c("S", "V", "MP")])
  # Convert dates to a suitable format
  polldata[["PublDate"]]          <- as.Date(polldata[["PublDate"]])
  polldata[["collectPeriodTo"]]   <- as.Date(polldata[["collectPeriodTo"]])
  polldata[["collectPeriodFrom"]] <- as.Date(polldata[["collectPeriodFrom"]])
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

expand_polls <- function(poll_data) {
  # Expand polls so the result of each covers its entire measurement period
  poll_data[["rownum"]] <- 1:nrow(poll_data)
  polls_dt <- data.table(poll_data)
  polls_dt <- polls_dt[,
    list(PollDate = seq(collectPeriodFrom, collectPeriodTo, by = "day")), # nolint
    by = "rownum"]
  polls_dt <- merge(polls_dt, data.table(poll_data), by = c("rownum"))
  return(polls_dt)
}

transform_polls <- function(poll_data, FUN, .SDcols) {
  return(
    poll_data[, lapply(.SD, FUN, na.rm = TRUE),
              by = "PollDate", .SDcols = .SDcols]
  )
}

make_continuous <- function(poll_data, select) {
  poll_zoo <- zoo(poll_data, poll_data[["PollDate"]])
  poll_zoo <- merge(poll_zoo,
                    zoo(, seq(start(poll_zoo), end(poll_zoo), by = 1)),
                    all = TRUE)
  poll_zoo <- subset(poll_zoo, select = select)
  return(fortify(poll_zoo, NA, melt = TRUE))
}

numeric_levels <- function(column) {
  return(as.numeric(levels(column))[column])
}
reorder_levels <- function(column, new_order) {
  return(
    factor(column, levels = levels(column)[new_order])
  )
}


get_plot <- function(derived, real, election) {
  # Define the color/line/alpha scheme for the plot
  # Ccolors from http://sv.wikipedia.org/wiki/Mall:Partifärg
  # Blocks take colors from (M) and (S) resp., uncertains are grey
  colors <- c(
    "#b70410", "#f9232b", "#79cf49",
    "#00993c", "#211974", "#5cb7e9",
    "#0049d8", "#dedd37", "#e2328d",
    "#cccccc", "#0049d8", "#f9232b"
  )
  linetypes <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,   2  )
  alphas    <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5)

  # nolint start
  p <- ggplot(
    derived,
    aes(
      x = Index, y = Value, ymin = MinValue, ymax = MaxValue,
      color = Series, group = Series, linetype = Series, alpha = Series
    )
  )
  # nolint end

  # Rolling median of the derived data
  p <- p + stat_rolling_median(rm.below = 1.0, na.rm = TRUE, alpha = 0.15)
  # Ribbon for derived data
  p <- p + stat_rolling_mean_ribbon(
    data = subset(derived, !(Series %in% c("Osäkra", "Alliansen", "Rödgröna"))),
    width = 84, na.rm = TRUE, alpha = 0.05, fill = "#333333", colour = NA
  )

  # Actual polls
  p <- p + geom_point(
    data = subset(real, !(Series %in% c("Alliansen", "Rödgröna"))),
    alpha = 0.125
  )

  # Election results
  p <- p + geom_point(data = election, shape = 18)

  # Lower limit for getting into Riksdagen
  p <- p + geom_hline(yintercept = 4, colour = "#333333", linetype = "dashed")

  # Styling
  p <- p + scale_colour_manual(  name = "Parti", values = colors   ) +
           scale_linetype_manual(name = "Parti", values = linetypes) +
           scale_alpha_manual(   name = "Parti", values = alphas   )

  # Labels and scales
  p <- p + labs(x = "Datum", y = "Stöd (%)")
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
  "Uncertain"  = "Osäkra",
  "RightBlock" = "Alliansen",
  "LeftBlock"  = "Rödgröna"
)
partyorder <- c(6, 5, 7, 3, 4, 2, 1, 8, 9:12)
parties <- c(
  "M", "L", "C", "KD", "S", "V", "MP", "SD", "FI",
  "Uncertain", "RightBlock", "LeftBlock"
)

# Load data
data_url  <- "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
poll_data <- preprocess(repmis::source_data(data_url, sep = ",", header = TRUE))

# Calculate a weighting value based on estimated interviews per day
poll_durations <-
  poll_data[["collectPeriodTo"]] - poll_data[["collectPeriodFrom"]] + 1
poll_data[["weight"]] <- poll_data[["n"]] / as.numeric(poll_durations)

# Expand the polls and get min/max series for later use
polls_dt     <- expand_polls(poll_data)
polls_dt_min <- transform_polls(copy(polls_dt), min, c(parties))
polls_dt_max <- transform_polls(copy(polls_dt), max, c(parties))

# Produce a weighted mean of polls for each day
polls_dt[, `:=`(parties, Map("*",
                             polls_dt[, parties, with = F],
                             polls_dt[, "weight", with = F])), with = F]
polls_dt <- transform_polls(polls_dt, sum, c(parties, "weight"))
polls_dt[, `:=`(parties, Map("/",
                             polls_dt[, parties, with = F],
                             polls_dt[, "weight", with = F])), with = F]

# Create a continuous time series
derived_data  <- make_continuous(polls_dt, select = parties)
poll_data_min <- make_continuous(polls_dt_min, select = parties)
poll_data_max <- make_continuous(polls_dt_max, select = parties)

# Transform back to data frame and reorder parties
derived_data[["Value"]]    <- numeric_levels(derived_data[["Value"]])
derived_data[["MinValue"]] <- numeric_levels(poll_data_min[["Value"]])
derived_data[["MaxValue"]] <- numeric_levels(poll_data_max[["Value"]])

# Reorder and translate party names
derived_data[["Series"]] <- reorder_levels(derived_data[["Series"]], partyorder)
derived_data[["Series"]] <- revalue(derived_data[["Series"]], partynames)

# Remove nonsensical min/max values
derived_data[["MinValue"]][derived_data[["MinValue"]] ==  Inf] <- NA
derived_data[["MaxValue"]][derived_data[["MaxValue"]] == -Inf] <- NA

# Do the same for non-continous non-averaged data
poll_data <- melt(poll_data, id.vars = "PublDate", parties,
                 variable.name = "Series", value.name = "Value")
poll_data[["Index"]]  <- poll_data[["PublDate"]]
poll_data[["Series"]] <- reorder_levels(poll_data[["Series"]], partyorder)
poll_data[["Series"]] <- revalue(poll_data[["Series"]], partynames)

poll_data[["MinValue"]] <- poll_data[["Value"]]
poll_data[["MaxValue"]] <- poll_data[["Value"]]

# Create data for election results
elections <- data.frame(
  PublDate = c(as.Date("2002-09-15"), as.Date("2006-09-17"),
               as.Date("2010-09-19"), as.Date("2014-09-14")),
  M  = c(15.26, 26.23, 30.06, 23.33),
  L = c(13.39, 7.54,  7.06,  5.42 ),
  C  = c(6.19,  7.88,  6.56,  6.11 ),
  KD = c(9.15,  6.59,  5.60,  4.57 ),
  S  = c(39.85, 34.99, 30.66, 31.01),
  V  = c(8.39,  5.85,  5.60,  5.72 ),
  MP = c(4.65,  5.24,  7.34,  6.89 ),
  SD = c(1.44,  2.93,  5.70,  12.86),
  FI = c(NA,    0.68,  0.40,  3.12 ),
  Uncertain  = c(NA, NA, NA, NA),
  RightBlock = c(NA, NA, NA, NA),
  LeftBlock  = c(NA, NA, NA, NA)
)
election_data <- melt(elections, id.vars = "PublDate", parties,
                variable.name = "Series", value.name = "Value")
election_data[["Index"]]  <- election_data[["PublDate"]]
election_data[["Series"]] <- revalue(election_data[["Series"]], partynames)
election_data[["Series"]] <-
  reorder_levels(election_data[["Series"]], partyorder)

election_data[["MinValue"]] <- election_data[["Value"]]
election_data[["MaxValue"]] <- election_data[["Value"]]
election_data <- subset(election_data, Index > as.Date("2003-01-01"))

# Actual plot
plot <- get_plot(derived_data, poll_data, election_data)

# Outputs
ggsave("polls.png", plot, width = 25.6, height = 14.4, units = "in", dpi = 75)
ggsave("polls.pdf", plot, width = 420,  height = 297,  units = "mm")
