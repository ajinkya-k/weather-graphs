library(tidyverse)

# 
ghc_msn_raw <- read_csv(
  "noaa_access/USW00014837.csv.gz",
  col_names = c(
      "id", "yearmoda", "element", "value",
      "mflag", "qflag", "sflag", "obs_time"
  ),
  col_types = "cccncccc"
)


ghc_msn <- ghc_msn_raw  %>%
  select(
    -ends_with("flag")
  ) %>%
  mutate(
    date = ymd(parse_date_time(yearmoda, "%y%m%d")),
    year = year(date),
    month = month(date),
    day = mday(date),
    day_of_year = yday(date)
  )  %>%
  arrange(date) %>%
  pivot_wider(
    id_cols = c(year, month, day, date, day_of_year),
    names_from = element,
    values_from = value
  ) %>%
  mutate(
    TMIN = (TMIN / 10) * 9/5 + 32,
    TMAX = (TMAX / 10) * 9/5 + 32,
    TAVG = (TAVG / 10) * 9/5 + 32
  )

head(ghc_msn)

ghn_msn_no_na <- ghc_msn  %>% filter(!is.na(TMAX))


write.csv(ghn_msn_no_na, file = "data/DailyTemp_Madison.csv")
