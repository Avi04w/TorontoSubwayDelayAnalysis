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


library(httr)
library(jsonlite)

# Set Toronto's coordinates
latitude <- 43.65107
longitude <- -79.347015

# Set start and end dates
start_date <- "2024-01-01"
end_date <- "2024-12-31"

# Make GET request
response <- GET(
  url = "https://archive-api.open-meteo.com/v1/archive?",
  query = list(
    latitude = latitude,
    longitude = longitude,
    start_date = start_date,
    end_date = end_date,
    hourly = "temperature_2m,precipitation"
  )
)

# Check if successful
if (status_code(response) == 200) {
  # Parse content
  data <- content(response, as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE)
  
  # Extract and organize hourly data
  hourly_weather <- data$hourly  %>% 
    as.data.frame()
  
} else {
  print(response)
  print(paste("Failed to retrieve data. Status code:", status_code(response)))
}

hourly_weather <- hourly_weather %>%
  mutate(
    Date = substr(time, 1, 10),
    Time = substr(time, 12, 16)
  ) %>%
  select(Date, Time, everything(), -time) # reorder columns and remove original 'time' column

hourly_weather$Date <- as.Date(hourly_weather$Date, format = "%Y-%m-%d")
hourly_weather <- hourly_weather |> 
  mutate(Hour = strtoi(substr(Time, 1, 2)))

full_data <- subway_data |> 
  left_join(hourly_weather, by = c("Date", "Hour"))

full_data <- full_data |> 
  mutate(Incident = ifelse(grepl("Passenger|Customer|Assault|Polce|Unsanitary|Bomb|Patron|Emergency Alarm|Graffiti|Robbery|Suspicious Package|Unauthorized|Person", `Code Description`, ignore.case = TRUE), "Passenger Related", "Non-Passenger Related"))

library(caret)
set.seed(123)

regression_data <- full_data |> 
  select(Min_Delay, Hour, temperature_2m, precipitation, Station, Line, Day, Incident, Time_Category)

regression_data <- regression_data |> 
  mutate(
    Station = as.factor(Station),
    Line = as.factor(Line),
    Day = as.factor(Day),
    Incident = as.factor(Incident),
    Time_Category = as.factor(Time_Category)
  )

regression_data <- na.omit(regression_data)

train_index <- createDataPartition(regression_data$Min_Delay, p = 0.8, list = FALSE)

train_data <- regression_data[train_index, ]
test_data  <- regression_data[-train_index, ]

# Fit linear regression model
lm_model <- train(
  Min_Delay ~ Hour + temperature_2m + precipitation + Station + Line + Day + Incident + Time_Category,
  data = train_data,
  method = "lm",
  trControl = trainControl(method = "none")
)

test_data <- test_data |> 
  filter(Min_Delay < 100)

lm_preds <- predict(lm_model, newdata = test_data)

results_df <- data.frame(
  Actual = test_data$Min_Delay,
  Predicted = lm_preds
)

# Evaluate performance
lm_rmse <- RMSE(lm_preds, test_data$Min_Delay)
lm_mae  <- MAE(lm_preds, test_data$Min_Delay)
lm_r2   <- R2(lm_preds, test_data$Min_Delay)

library(mgcv)

gam_model <- gam(`Min_Delay` ~ 
                   s(Hour) + 
                   s(precipitation) + 
                   s(temperature_2m) + 
                   Station + 
                   Line + 
                   Time_Category + 
                   Day + 
                   Incident,
                 data = train_data, 
                 family = gaussian())

# Predict
gam_preds <- predict(gam_model, newdata = test_data)

gam_results_df <- data.frame(
  Actual = test_data$Min_Delay,
  Predicted = gam_preds
)

new_gam <- gam(`Min_Delay` ~ 
                 s(precipitation) + 
                 s(Hour) + 
                 Station + 
                 Incident,
               data = train_data, 
               family = gaussian())

# Predict
gam_preds <- predict(new_gam, newdata = test_data)

gam_results_df <- data.frame(
  Actual = test_data$Min_Delay,
  Predicted = gam_preds
)

a <- AIC(gam_model, new_gam)

# Evaluate performance
gam_rmse <- RMSE(gam_preds, test_data$Min_Delay)
gam_mae  <- MAE(gam_preds, test_data$Min_Delay)
gam_r2   <- R2(gam_preds, test_data$Min_Delay)

library(xgboost)

factor_vars <- c("Station", "Line", "Day", "Incident", "Time_Category")
for (var in factor_vars) {
  train_data[[var]] <- as.factor(train_data[[var]])
  test_data[[var]]  <- factor(test_data[[var]], levels = levels(train_data[[var]]))
}

# Fit Random Forest
rf_model <- train(
  Min_Delay ~ Hour + precipitation + Station + Line + Day + Incident + Time_Category,
  data = train_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  importance = TRUE
)

# Predict
rf_preds <- predict(rf_model, newdata = test_data)

# Evaluate
rf_rmse <- RMSE(rf_preds, test_data$Min_Delay)
rf_mae  <- MAE(rf_preds, test_data$Min_Delay)
rf_r2   <- R2(rf_preds, test_data$Min_Delay)

# Fit XGBoost
xgb_model <- train(
  Min_Delay ~ Hour + precipitation + Station + Line + Day + Incident + Time_Category,
  data = train_data,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5),
  verbosity = 0
)

# Predict
xgb_preds <- predict(xgb_model, newdata = test_data)

# Evaluate
xgb_rmse <- RMSE(xgb_preds, test_data$Min_Delay)
xgb_mae  <- MAE(xgb_preds, test_data$Min_Delay)
xgb_r2   <- R2(xgb_preds, test_data$Min_Delay)

comparison_df <- data.frame(
  Actual = test_data$Min_Delay,
  Linear = predict(lm_model, newdata = test_data),
  GAM = gam_preds,
  RF = rf_preds,
  XGB = xgb_preds
)

library(tidyr)
long_df <- pivot_longer(comparison_df, cols = -Actual, names_to = "Model", values_to = "Predicted")

p5 <- ggplot(long_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#da251d") +
  facet_wrap(~ Model, scales = "free") +
  labs(
    title = "Predicted vs Actual Delay Duration by Model",
    x = "Actual Delay Duration (minutes)",
    y = "Predicted Delay Duration (minutes)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 14),
    text = element_text(size = 12)
  )


residuals_df <- data.frame(
  Model = c(rep("Linear", nrow(test_data)),
            rep("GAM", nrow(test_data)),
            rep("Random Forest", nrow(test_data)),
            rep("XGBoost", nrow(test_data))),
  Residual = c(
    test_data$Min_Delay - predict(lm_model, newdata = test_data),
    test_data$Min_Delay - gam_preds,
    test_data$Min_Delay - rf_preds,
    test_data$Min_Delay - xgb_preds
  )
)

performance_metrics <- data.frame(
  Model = c("Linear", "GAM", "Random Forest", "XGBoost"),
  R2 = c(
    R2(predict(lm_model, newdata = test_data), test_data$Min_Delay),
    R2(gam_preds, test_data$Min_Delay),
    R2(rf_preds, test_data$Min_Delay),
    R2(xgb_preds, test_data$Min_Delay)
  ),
  RMSE = c(
    RMSE(predict(lm_model, newdata = test_data), test_data$Min_Delay),
    RMSE(gam_preds, test_data$Min_Delay),
    RMSE(rf_preds, test_data$Min_Delay),
    RMSE(xgb_preds, test_data$Min_Delay)
  ),
  MAE = c(
    MAE(predict(lm_model, newdata = test_data), test_data$Min_Delay),
    MAE(gam_preds, test_data$Min_Delay),
    MAE(rf_preds, test_data$Min_Delay),
    MAE(xgb_preds, test_data$Min_Delay)
  )
)

performance_metrics[ , 2:4] <- round(performance_metrics[ , 2:4], 10)

kable(performance_metrics, digits = 4, caption = "Model Performance Summary (R², RMSE, MAE)")

# Random Forest variable importance
rf_importance <- varImp(rf_model)

rf_imp_df <- rf_importance$importance
rf_imp_df$Variable <- rownames(rf_imp_df)
rf_imp_df <- rf_imp_df[order(-rf_imp_df$Overall), ][1:10, ]

p6 <- ggplot(rf_imp_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "#da251d") +
  coord_flip() +
  labs(
    title = "Top 10 Variable Importance - Random Forest",
    x = "Variable",
    y = "Importance Score"
  ) +
  theme_minimal()

# XGBoost variable importance
xgb_importance <- varImp(xgb_model)

xgb_imp_df <- xgb_importance$importance
xgb_imp_df$Variable <- rownames(xgb_imp_df)
xgb_imp_df <- xgb_imp_df[order(-xgb_imp_df$Overall), ][1:10, ]

p7 <- ggplot(xgb_imp_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "#da251d") +
  coord_flip() +
  labs(
    title = "Top 10 Variable Importance - XGBoost",
    x = "Variable",
    y = "Importance Score"
  ) +
  theme_minimal()
