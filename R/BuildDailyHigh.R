library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)

ghcn <- read_csv("data/DailyTemp_Madison.csv")

year.to.plot <- max(ghcn$year)-1
last.date <- max(ghcn[ghcn$year == year.to.plot, ]$date)
first.date <- min(ghcn$date)

this.year <- ghcn %>%
  filter(year == year.to.plot)

daily.summary.stats <- ghcn %>%
  filter(year < year.to.plot) %>%
  select(day_of_year, PRCP, TMAX, TMIN, TAVG) %>%
  pivot_longer(cols = -day_of_year) %>%
  group_by(day_of_year, name) %>%
  summarise(max = max(value, na.rm = T),
            min = min(value, na.rm = T),
            x5 = quantile(value, 0.05, na.rm = T),
            x20 = quantile(value, 0.2, na.rm = T),
            x40 = quantile(value, 0.4, na.rm = T),
            x60 = quantile(value, 0.6, na.rm = T),
            x80 = quantile(value, 0.8, na.rm = T),
            x95 = quantile(value, 0.95, na.rm = T)) %>%
  ungroup()

# month breaks
month.breaks <- ghcn %>%
  filter(year == 2019) %>%
  group_by(month) %>%
  slice_min(order_by = day_of_year, n = 1) %>%
  ungroup() %>%
  select(month, day_of_year) %>%
  mutate(month_name = month.abb)

record.status.this.year <- this.year %>%
  select(day_of_year, PRCP, TMAX, TMIN, TAVG) %>%
  pivot_longer(cols = -day_of_year, values_to = "this_year") %>%
  inner_join(daily.summary.stats %>% select(-starts_with("x"))) %>%
  mutate(record_status = case_when(
    this_year > max ~ "max",
    this_year < min ~ "min",
    TRUE ~ "none"
  )) %>%
  filter(record_status != "none")

col_pal <- RColorBrewer::brewer.pal(5, "YlGnBu")[2:5]


t_var = "TAVG"
max.graph <- daily.summary.stats %>%
  filter(name == t_var) %>%
  ggplot(aes(x = day_of_year)) +
  # draw vertical lines for the months
  geom_vline(xintercept = c(month.breaks$day_of_year, 365),
             linetype = "dotted", lwd = 0.2) +
  # ribbon between the lowest and 5th, 95th and max percentiles
  geom_ribbon(aes(ymin = min, ymax = max),
              fill = col_pal[1]) +
  # ribbon between the 5th and 20th, 80th to 95th percentiles
  geom_ribbon(aes(ymin = x5, ymax = x95),
              fill =col_pal[2]) +
  # ribbon between the 20th and 40th, 60th and 80th percentiles
  geom_ribbon(aes(ymin = x20, ymax = x80),
              fill = col_pal[3]) +
  # ribbon between the 40th and 60th percentiles
  geom_ribbon(aes(ymin = x40, ymax = x60),
              fill = col_pal[4]) +
  # y-axis breaks
  geom_hline(yintercept = seq(-20, 100, 10),
             color = "white", lwd = 0.1) +
  # line for this year's values
  geom_line(data = this.year,
            aes(y = get(t_var)), lwd = 0.8) +
  # points for maximum records set this year
  geom_point(data = filter(record.status.this.year, 
                           name == t_var,
                           record_status == "max"),
             aes(y = this_year), shape = 21, size = 2, color = "black", fill = "white") +
  # points for minimum records set this year
  geom_point(data = filter(record.status.this.year,
                           name == t_var,
                           record_status == "min"),
             aes(y = this_year), shape = 21, size = 2, color = "black", fill = "white") +
  scale_y_continuous(breaks = seq(-40, 100, 10),
                     labels = scales::unit_format(suffix = "°"),
                     expand = expansion(0.01),
                     name = NULL,
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand = expansion(0),
                     breaks = month.breaks$day_of_year + 15,
                     labels = month.breaks$month_name,
                     name = NULL) +
  labs(title = "Daily High Temperature at Madison",
       subtitle = paste("The line shows daily highs for",
                        paste0(lubridate::year(last.date), "."),
                        "The ribbons cover the",
                        "historical range. The last date shown is", 
                        format(last.date, "%b %d, %Y.")),
       caption = paste("Records begin on", format(first.date, "%b %d %Y"), 
                       ". This graph was last updated on", format(Sys.Date(), "%B %d, %Y."))) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "linen",
                                       colour = "linen"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 16),
        axis.ticks = element_blank())

max.graph

legend.df <- daily.summary.stats %>%
  filter(day_of_year %in% 165:201,
         name == "TMAX") %>%
  mutate(max = max - 60,
         min = min - 60,
         x5 = x5 - 60,
         x20 = x20 - 60,
         x40 = x40 - 60,
         x60 = x60 - 60,
         x80 = x80 - 60,
         x95 = x95 - 60)

legend.line.df <- tibble(
  day_of_year = 165:201,
  temp = case_when(
    day_of_year == 165 ~ legend.df$x40[legend.df$day_of_year == 165],
    day_of_year == 168 ~ legend.df$x40[legend.df$day_of_year == 165] + 3,
    day_of_year == 172 ~ legend.df$x40[legend.df$day_of_year == 165] - 4,
    day_of_year == 177 ~ legend.df$min[legend.df$day_of_year == 177] - 1,
    day_of_year == 180 ~ legend.df$x20[legend.df$day_of_year == 180] - 1,
    day_of_year == 182 ~ legend.df$x60[legend.df$day_of_year == 182] + 1,
    day_of_year == 185 ~ legend.df$x60[legend.df$day_of_year == 185] - 6,
    day_of_year == 189 ~ legend.df$max[legend.df$day_of_year == 189] + 1,
    day_of_year == 194 ~ legend.df$x60[legend.df$day_of_year == 194],
    day_of_year == 198 ~ legend.df$x40[legend.df$day_of_year == 198],
    day_of_year == 201 ~ legend.df$x60[legend.df$day_of_year == 201],
    TRUE ~ NA_real_
  )
) %>%
  filter(!is.na(temp))

legend.labels <- legend.df %>%
  pivot_longer(cols = c(max, min, starts_with("x")),
               names_to = "levels") %>%
  mutate(label = case_when(
    levels == "max" ~ "max",
    levels == "min" ~ "min",
    levels == "x95" ~ "95th percentile of past years",
    TRUE ~ paste0(str_sub(levels, 2, -1), "th")
  )) %>%
  mutate(filter_day = ifelse(
    levels %in% c("max", "x80", "x40", "x5"),
    min(day_of_year),
    max(day_of_year)
  )) %>%
  filter(day_of_year == filter_day)

##  Add legend
max.graph2 <- max.graph +
  # ribbon between the lowest and 5th percentiles
  geom_ribbon(data = legend.df,
              aes(ymin = min, ymax = max),
              fill = col_pal[1]) +
  # ribbon between the 5th and 20th percentiles
  geom_ribbon(data = legend.df,
              aes(ymin = x5, ymax = x95),
              fill = col_pal[2]) +
  # ribbon between the 20th and 40th percentiles
  geom_ribbon(data = legend.df,
              aes(ymin = x20, ymax = x80),
              fill = col_pal[3]) +
  # ribbon between the 40th and 60th percentiles
  geom_ribbon(data = legend.df,
              aes(ymin = x40, ymax = x60),
              fill = col_pal[4]) +
  geom_line(data = legend.line.df, aes(y = temp), lwd = 0.5) +
  geom_point(aes(x = 177, y = legend.line.df$temp[legend.line.df$day_of_year == 177]),
             shape = 21, color = "black", fill = "white") +
  geom_point(aes(x = 189, y = legend.line.df$temp[legend.line.df$day_of_year == 189]),
             shape = 21, color = "black", fill = "white") +
  geom_text(aes(x = 180, y = legend.line.df$temp[legend.line.df$day_of_year == 177] - 2,
                label = "all-time record low set this year"),
            hjust = 0, size = 3) +
  geom_text(aes(x = 192, y = legend.line.df$temp[legend.line.df$day_of_year == 189] + 2,
                label = "all-time record high set this year"),
            hjust = 0, size = 3) +
  ggrepel::geom_text_repel(data = filter(legend.labels,
                                         filter_day == max(filter_day)),
                           aes(y = value, label = label),
                           min.segment.length = 0, size = 3,
                           direction = "y", hjust = 0, nudge_x = 5) +
  ggrepel::geom_text_repel(data = filter(legend.labels,
                                         filter_day == min(filter_day)),
                           aes(y = value, label = label),
                           min.segment.length = 0, size = 3,
                           direction = "y", hjust = 1, nudge_x = -5)

ggsave("graphs/DailyHighTemp_Madison.png", plot = max.graph2,
       width = 12, height = 4)


