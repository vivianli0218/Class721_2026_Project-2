library(dplyr)

aq_2018 <- read.csv(
  "data/AQ_2018.csv",
  check.names = FALSE
)

head(aq_2018)
str(aq_2018)

#rename columns to short
names(aq_2018) <- c(
  "date",
  "co",
  "units_co",
  "pm25",
  "units_pm25",
  "o3",
  "units_o3"
)

names(aq_2018)

# count exact duplicate rows
sum(duplicated(aq_2018))

# remove exact duplicate rows
aq_2018 <- aq_2018[!duplicated(aq_2018), ]

# check the remaining number of rows
nrow(aq_2018)

# convert dates from month/day/year to Date format
aq_2018$date <- as.Date(
  aq_2018$date,
  format = "%m/%d/%Y"
)

# check the conversion
class(aq_2018$date)
head(aq_2018$date)
sum(is.na(aq_2018$date))


# names of the three concentration columns
pollutants <- c("co", "pm25", "o3")

# count negative readings for each pollutant
for (p in pollutants) {
  print(p)
  print(sum(aq_2018[[p]] < 0, na.rm = TRUE))
}

# replace negative readings with zero 
for (p in pollutants) {
  negative <- !is.na(aq_2018[[p]]) & aq_2018[[p]] < 0
  aq_2018[[p]][negative] <- 0
}

# count missing concentrations in each pollutant column
colSums(is.na(aq_2018[pollutants]))


# calculate one daily mean for each pollutant
aq_2018_daily <- aq_2018 |>
  group_by(date) |>
  summarise(
    across(
      all_of(pollutants),
      ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

head(aq_2018_daily)
nrow(aq_2018_daily)
anyDuplicated(aq_2018_daily$date)


o3_units <- "ppm"

# convert ozone only when ppb is requested
if (o3_units == "ppb") {
  aq_2018_daily$o3 <- aq_2018_daily$o3 * 1000
}

# compare ozone values in the two units
o3_ppb <- aq_2018_daily$o3 * 1000

head(aq_2018_daily$o3)
head(o3_ppb)


# o3_units: ozone output unit, "ppm" (default) or "ppb".
process_aq <- function(file_path, o3_units = "ppm") {
  aq <- read.csv(file_path, check.names = FALSE)
  
  names(aq) <- c(
    "date", "co", "units_co", "pm25",
    "units_pm25", "o3", "units_o3"
  )
  
  aq <- aq[!duplicated(aq), ]
  
  aq$date <- as.Date(aq$date, format = "%m/%d/%Y")
  
  pollutants <- c("co", "pm25", "o3")
  
  for (p in pollutants) {
    negative <- !is.na(aq[[p]]) & aq[[p]] < 0
    aq[[p]][negative] <- 0
  }
  
  # calculate daily means separately for each pollutant
  aq <- aq |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      dplyr::across(
        all_of(pollutants),
        ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  # check the requested ozone unit
  o3_units <- match.arg(o3_units, c("ppm", "ppb"))
  
  # Convert the daily ozone concentration when requested
  if (o3_units == "ppb") {
    aq$o3 <- aq$o3 * 1000
  }
  
  return(aq)
}

# process each year with ozone in ppm
aq_2018_clean <- process_aq("data/AQ_2018.csv")
aq_2019_clean <- process_aq("data/AQ_2019.csv")
aq_2020_clean <- process_aq("data/AQ_2020.csv")

# Combine the three years
aq_all <- dplyr::bind_rows(
  aq_2018_clean,
  aq_2019_clean,
  aq_2020_clean
)


