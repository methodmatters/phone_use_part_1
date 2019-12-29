# load the libraries we'll need
library(ggplot2)
library(RColorBrewer)
library(plyr); library(dplyr)
library(tidyr)
library(readr)

# directory where the data file is located
in_dir = 'D:\\Directory\\'
# read the data
full_data <- read_csv(paste0(in_dir, 'phone_use.csv'))

# [1] 548 unique days
length(unique(full_data$oneday))
# first date
full_data$oneday[1]
# last date
full_data$oneday[length(full_data$oneday)]

# what do the data look like?
head(full_data)



# Hexadecimal color specification 
color_palette <- brewer.pal(n = 3, name = "Dark2")

# set up the x-axis labels
x_axis_labels = seq(from = 0, to = 23, by = 1)

# plot all types of phone usage throughout the course of the day
full_data %>% filter(!is.na(call_type)) %>% # remove observations for which no usage was recorded
  # set up the plot basics
  ggplot(aes(x = hour, y = n, fill = call_type)) + 
  # we want a bar chart
  geom_bar(stat = 'identity') +
  # set the labels
  labs(x = "Hour of Day", y = "Total", title = 'Phone Usage Across the Day' ) +
  # we want continuous tick marks for every hour of the day
  # the code below uses the x_axis_labels we set up above to achieve this
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  # set the legend title and specify the colors we define above
  # with the color palette (from R Color Brewer)
  scale_fill_manual(name = "Usage Type", values=color_palette)

# make separate facets for each type of usage
full_data %>% filter(!is.na(call_type)) %>% 
  ggplot(aes(x = hour, y = n, fill = call_type)) + 
  geom_bar(stat = 'identity') +
  labs(x = "Hour of Day", y = "Total", title = 'Phone Usage by Type Across the Day' )  + 
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  scale_fill_manual(name = "Usage Type", values=color_palette) + 
  facet_grid(call_type ~ .)

# analyze patterns by week / weekend 
full_data %>% filter(!is.na(call_type)) %>% #head()
  ggplot(aes(x = hour, y = n, fill = call_type)) + 
  geom_bar(stat = 'identity') + 
  labs(x = "Hour of Day", y = "Total", title = 'Phone Usage by Type Across the Day' )  + 
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  scale_fill_manual(name = "Usage Type", values=color_palette) + 
  facet_grid(week_weekend ~ .)

# week/weekend plots by usage type

# data by week/weekend
full_data %>% filter(!is.na(call_type) & call_type == 'Data') %>% 
  ggplot(aes(x = hour, y = n, fill = call_type)) + 
  geom_bar(stat = 'identity') + 
  labs(x = "Hour of Day", y = "Total", title = 'Data Usage Across the Day: Weekdays vs. Weekends' )  + 
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  scale_fill_manual(name = "Usage Type", values=color_palette[1]) + 
  facet_grid(week_weekend ~ .)

# phone calls by week/weekend
full_data %>% filter(!is.na(call_type) & call_type == 'Phone Call') %>% 
  ggplot(aes(x = hour, y = n, fill = call_type)) + 
  geom_bar(stat = 'identity') + 
  labs(x = "Hour of Day", y = "Total", title = 'Phone Calls Across the Day: Weekdays vs. Weekends' )  + 
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  scale_fill_manual(name = "Usage Type", values=color_palette[2]) + 
  facet_grid(week_weekend ~ .)

# text messages by week/weekend 
full_data %>% filter(!is.na(call_type) & call_type == 'Text Message') %>% #head()
  ggplot(aes(x = hour, y = n, fill = call_type)) + 
  geom_bar(stat = 'identity') + 
  labs(x = "Hour of Day", y = "Total", title = 'Text Messages Across the Day: Weekdays vs. Weekends' )  + 
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  scale_fill_manual(name = "Usage Type", values=color_palette[3]) + 
  facet_grid(week_weekend ~ .)
