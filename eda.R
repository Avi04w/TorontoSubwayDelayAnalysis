library(opendatatoronto)
library(dplyr)

package <- show_package("996cfe8d-fb35-40ce-b569-698d51fc683b")
resources <- list_package_resources("996cfe8d-fb35-40ce-b569-698d51fc683b")

subway_code_data <- resources |> 
  filter(id == "3900e649-f31e-4b79-9f20-4731bbfd94f7") |> 
  get_resource()

subway_delay_data <- resources |> 
  filter(id == "2ee1a65c-da06-4ad1-bdfb-b1a57701e46a") |>
  get_resource()


subway_code_data <- subway_code_data |> 
  select("SUB RMENU CODE", "CODE DESCRIPTION...3")
colnames(subway_code_data) <- c("Code", "Code Description")

subway_data <- merge(subway_delay_data, subway_code_data, by = "Code")

library(lubridate)

colnames(subway_data)[colnames(subway_data) == "Min Delay"] <- "Min_Delay"

subway_data <- subway_data |> 
  filter(`Min_Delay` != "None")

colnames(subway_data)[colnames(subway_data) == "_id"] <- "ID"

subway_data$`Min_Delay` <- as.integer(subway_data$`Min_Delay`)

subway_data <- subway_data |> 
  filter(`Min_Delay` != 360) |> 
  filter(Min_Delay > 0)

subway_data$Date <- as.Date(subway_data$Date, format = "%Y-%m-%d")
subway_data$Time <- paste0(subway_data$Time, ":00")
subway_data$Time <- hms::as_hms(subway_data$Time)

stations <- subway_data |> 
  group_by(Station) |> 
  summarise(
    n = n()) %>% 
  filter(n > 100)

subway_data <- subway_data |> 
  filter(Station %in% stations$Station)

subway_data <- subway_data |> 
  filter(Line %in% c("YU", "BD"))

subway_data$Hour <- hour(subway_data$Time)
subway_data <- subway_data |> 
  mutate(Time_Category = case_when(
    Hour %in% c(7, 8, 9, 15, 16, 17, 18) ~ "Peak",
    TRUE ~ "Non-Peak"
  ))

library(knitr)

station_summary <- subway_data |> 
  group_by(Station) |> 
  summarise(
    `Total Delays` = n(),
    `Average Delay Time` = mean(`Min_Delay`)
  ) |> 
  arrange(desc(`Total Delays`))

kable(head(station_summary, 10), caption = "Top 10 Stations with Most Delays")


library(ggplot2)

station_delays <- subway_data |> 
  group_by(Station, Line) |> 
  summarise(Total_Delays = n(), .groups = "drop") |> 
  arrange(desc(Total_Delays)) |> 
  head(20)

p1 <- ggplot(station_delays, aes(x = reorder(Station, Total_Delays), y = Total_Delays, fill = Line)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Total Number of Delays per Subway Station",
    x = "Subway Station",
    y = "Number of Delays",
    fill = "Subway Line",
    caption = "Figure 1: Data From City of Toronto Open Data Portal"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("YU" = "#d9230f", "BD" = "steelblue")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.background = element_rect(fill = "#fcfcfc", color = NA),
        panel.background = element_rect(fill = "#fcfcfc", color = NA),
        legend.background = element_rect(fill = "#fcfcfc", color = NA),
        legend.box.background = element_rect(fill = "#fcfcfc", color = NA),
        strip.background = element_rect(fill = "#fcfcfc", color = NA))

delay_causes <- subway_data |> 
  group_by(`Code Description`) |> 
  summarise(
    Count = n(), 
    `Average Delay Time` = mean(Min_Delay)) |> 
  arrange(desc(Count)) |> 
  head(10)

kable(delay_causes, caption = "Top 10 Most Frequent Delay Causes")

library(treemapify)

p2 <- ggplot(delay_causes, aes(area = Count, fill = Count, label = paste(`Code Description`, "\n", "Count:", Count, "Delays", "\n", "Average Delay:", round(`Average Delay Time`, 2), "mins"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  scale_fill_gradient(low = "indianred1", high = "indianred4") +
  labs(title = "Top 10 Reasons of Delay",
       caption = "Figure 2: Data From City of Toronto Open Data Portal") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.background = element_rect(fill = "#fcfcfc", color = NA),
        panel.background = element_rect(fill = "#fcfcfc", color = NA),
        legend.background = element_rect(fill = "#fcfcfc", color = NA),
        legend.box.background = element_rect(fill = "#fcfcfc", color = NA),
        strip.background = element_rect(fill = "#fcfcfc", color = NA))

delay_summary <- subway_data |> 
  group_by(Code, `Code Description`) |> 
  summarise(Total_Delay_Time = sum(Min_Delay, na.rm = TRUE)) |> 
  arrange(desc(Total_Delay_Time)) |> 
  head(10)

p3 <- ggplot(delay_summary, aes(x = reorder(Code, Total_Delay_Time), y = Total_Delay_Time, fill = Total_Delay_Time)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Delay Causes by Total Delay Time",
    x = "Delay Code",
    y = "Total Delay Time (Minutes)",
    fill = "Total Delay (min)",
    caption = "Figure 3: Data From City of Toronto Open Data Portal"
  ) +
  theme_minimal() +
  scale_fill_gradient(low = "indianred1", high = "indianred4") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.background = element_rect(fill = "#fcfcfc", color = NA),
        panel.background = element_rect(fill = "#fcfcfc", color = NA),
        legend.background = element_rect(fill = "#fcfcfc", color = NA),
        legend.box.background = element_rect(fill = "#fcfcfc", color = NA),
        strip.background = element_rect(fill = "#fcfcfc", color = NA))

# Display table
delay_summary |> 
  select(Code, `Code Description`) |> 
  kable(caption = "Delay Code Definitions")

peak_summary <- subway_data |> 
  group_by(Time_Category) |> 
  summarise(
    `Total Delays` = n(),
    `Average Delay Time (mins)` = round(mean(Min_Delay, na.rm = TRUE), 2)
  )

peak_summary <- peak_summary |> 
  mutate(`Average Number of Delays per Hour` = ifelse(Time_Category == "Peak", 
                                                      `Total Delays` / 8, 
                                                      `Total Delays` / 14))

kable(peak_summary, caption = "Delays During Peak vs Non-Peak Hours")

p4 <- ggplot(subway_data, aes(x = Hour, fill = Time_Category)) +
  geom_bar() +
  labs(
    title = "Number of Delays by Hour of the Day",
    x = "Hour of the Day",
    y = "Total Delays",
    fill = "Time Category",
    caption = "Figure 4: Data From City of Toronto Open Data Portal"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Peak" = "#d9230f", "Non-Peak" = "steelblue")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.background = element_rect(fill = "#fcfcfc", color = NA),
        panel.background = element_rect(fill = "#fcfcfc", color = NA),
        legend.background = element_rect(fill = "#fcfcfc", color = NA),
        legend.box.background = element_rect(fill = "#fcfcfc", color = NA),
        strip.background = element_rect(fill = "#fcfcfc", color = NA))