library(lubridate)


############ function for trust level trends from 2017 to 2024 #################

plot_trust_trends <- function(data, trust_col, date_col, metric_col, title = "Trust-Level Trends from 2017 to 2024") {
  
  trust_col_sym <- sym(trust_col)
  date_col_sym <- sym(date_col)
  metric_col_sym <- sym(metric_col)
  
  # Assign trend_data directly to the global environment
  trend_data <<- data %>%
    select(!!trust_col_sym, !!date_col_sym, !!metric_col_sym) %>%
    arrange(!!date_col_sym)
  
  trend_plot <- ggplot(trend_data, aes(x = !!date_col_sym, y = !!metric_col_sym, group = !!trust_col_sym, color = !!trust_col_sym)) +
    geom_line() +
    labs(title = title, x = "Date", y = "Metric") +
    scale_y_continuous(labels = label_number()) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(filename = "trend_plot.jpeg", plot = trend_plot, width = 10, height = 6, dpi = 300)
  
  return(list(plot = trend_plot, data = trend_data))
}


plot_trust_trends(rtt_data2, "trust_code", "date", "completed_pathways_for_admitted_patients")


########## Gini index ###############

Gini = function (x, corr = FALSE, na.rm = TRUE) 
{
  if (!na.rm && any(is.na(x))) 
    return(NA_real_)
  x <- as.numeric(na.omit(x))
  l <- length(x)
  x <- sort(x)
  gi <- sum(x * 1L:l)
  gi <- 2 * gi/sum(x) - (l + 1L)
  if (corr) 
    gi/(l - 1L)
  else gi/l
}

gini_by_trust_specialty <- elective_workforce %>%
  group_by(Workforce) %>%
  summarize(gini = Gini(total_fte, na.rm = TRUE))


########### Relative value to 2019/20 ###########

library(dplyr)

divide_march_2019_and_2024 <- function(data, date_col, value_col, group_cols) {
  data[[date_col]] <- as.Date(data[[date_col]])
  
  data_2019 <- data %>%
    filter(format(!!sym(date_col), "%Y-%m") == "2019-03") %>%
    mutate(day = format(!!sym(date_col), "%d")) %>%
    select(all_of(group_cols), day, value_2019 = !!sym(value_col))
  
  data_2024 <- data %>%
    filter(format(!!sym(date_col), "%Y-%m") == "2024-03") %>%
    mutate(day = format(!!sym(date_col), "%d")) %>%
    select(all_of(group_cols), day, value_2024 = !!sym(value_col))
  
  result <- data_2024 %>%
    inner_join(data_2019, by = c(group_cols, "day"),relationship="many-to-many") %>%
    mutate(result = value_2024 / value_2019) %>%

  return(result)
}

rtt54<-divide_march_2019_and_2024 (rtt_data2,"date","incomplete_pathways",c("trust_code","Combined"))
