library(tidyverse)
library(plotly)
library(widgetframe)
library(tidytext)

sb_locs <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/starbucks-locations.csv")

sb_nutr <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/starbucks-menu-nutrition.csv")

usa_pop <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/us_state_pop.csv")

usa_states<-read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/states.csv")


head(sb_locs)
head(sb_nutr)
head(usa_pop)
head(usa_states)

sb_usa <- sb_locs |> filter(Country == "US")

sb_locs_state <- sb_usa |>
  group_by(`State/Province`) |>
  rename("Abbreviation" = `State/Province`) |>
  summarize(store_count = n())

# need state abbreviations
usa_pop_abbr <- usa_pop |> 
  rename("State" = "state") |> 
  full_join(usa_states, by = "State")

sb_locs_state <- usa_pop_abbr |> 
  full_join(sb_locs_state, by = "Abbreviation")

# Drop NA cols (it is USA territories such as Guam and Puerto Rico)
sb_locs_state <- sb_locs_state |> drop_na()

summary(sb_locs_state)

p1 <- ggplot(sb_locs_state, aes(x = population, y = store_count)) +
  geom_point(aes(text = paste("State:", Abbreviation)), color = "blue", alpha = 0.6) +
  geom_smooth(formula = 'y ~ x', method = "lm", color = "red", se = FALSE) +
  labs(title = "Starbucks Stores vs. State Population",
       x = "State Population",
       y = "Number of Starbucks Stores") +
  theme_minimal()

ggplotly(p1)


p2 <- ggplot(sb_nutr, aes(x = Calories, fill = Category)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("red", "steelblue")) +
  labs(title = "Caloric Distribution of Starbucks Menu Items",
       x = "Calories",
       y = "Count",
       fill = "Category") +
  theme_minimal()

ggplotly(p2)


top_words <- sb_nutr |>
  unnest_tokens(word, Item) |>
  count(word, sort = TRUE) |>
  anti_join(stop_words, by = "word") |>
  slice_max(n, n = 20)

p3 <- ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Top 20 Words in Starbucks Menu Items",
       x = "Word",
       y = "Count") +
  theme_minimal()

ggplotly(p3)

library(plotly)

plot_ly(data = sb_nutr, 
        x = ~Calories, 
        y = ~`Carb. (g)`, 
        type = 'scatter', 
        mode = 'markers', 
        color = ~Category,
        colors = c("Food" = "red", "Drinks" = "steelblue"),
        marker = list(size = 8)) |> 
  layout(title = "Calories vs Carbs in Starbucks Menu Items",
         xaxis = list(title = "Calories"),
         yaxis = list(title = "Carbs (g)"),
         legend = list(title = list(text = "Category")))


top_words <- top_words |> 
  slice_max(n, n=10)

top_words_pattern <- paste0("(?i)", paste(top_words$word, collapse = "|"))

sb_nutr_filtered <- sb_nutr |>
  filter(str_detect(Item, top_words_pattern)) |> 
  mutate(highlighted_word = str_extract(Item, top_words_pattern))

plot_ly(data = sb_nutr_filtered, 
        x = ~Calories, 
        y = ~`Carb. (g)`, 
        type = 'scatter', 
        mode = 'markers', 
        color = ~Category,
        colors = c("Food" = "red", "Drinks" = "steelblue"),
        text = ~paste("Item:", Item, "<br>Word:", highlighted_word), 
        hoverinfo = "text",
        marker = list(size = 8)) |> 
  layout(title = "Calories vs Carbs for Items with Top 10 Words",
         xaxis = list(title = "Calories"),
         yaxis = list(title = "Carbs (g)"),
         hovermode = "compare",
         legend = list(title = list(text = "Category")))

sb_nutr_long <- sb_nutr_filtered |>
  pivot_longer(cols = c(Calories, `Fat (g)`, `Carb. (g)`, `Protein (g)`, `Fiber (g)`), names_to = "Nutrient", values_to = "Value")

plot_list <- list()

colours <- c("Calories" = "red", 
             "Fat (g)" = "blue",
             "Carb. (g)" = "green", 
             "Protein (g)" = "purple", 
             "Fiber (g)" = "orange")

nutrients <- names(colours)

for (nutrient in nutrients) {
  p <- plot_ly(data = sb_nutr_long |> filter(Nutrient == nutrient), 
               x = ~highlighted_word, 
               y = ~Value, 
               type = "box",
               name = nutrient,
               marker = list(color = colours[[nutrient]])) |> 
    layout(title = paste("Distribution of", nutrient, "by Top 10 Words"),
           xaxis = list(title = "Top 10 Frequent Words in Item Name"),
           yaxis = list(title = paste(nutrient, "Value")),
           boxmode = "group")
  
  plot_list <- append(plot_list, list(p))
}

subplot(plot_list, nrows = length(nutrients), shareX = TRUE, titleX = TRUE)

plot_ly(data = sb_nutr_filtered, 
        x = ~Calories, 
        y = ~`Carb. (g)`, 
        z = ~`Protein (g)`, 
        color = ~Category,  
        colors = c("steelblue", "red"),
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 6, opacity = 0.8),
        text = ~paste("Item:", Item, "<br>Word:", highlighted_word), 
        hoverinfo = "text") %>%
  layout(title = "3D Scatterplot: Calories, Carbs, and Protein",
         scene = list(
           xaxis = list(title = "Calories"),
           yaxis = list(title = "Carbs"),
           zaxis = list(title = "Protein")
         ))

# Set up mapping details
set_map_details <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('steelblue')
)

# Make sure both maps are on the same color scale
shadeLimit <- 125

# Create hover text
sb_locs_state$hover <- with(sb_locs_state, paste("Number of Starbucks: ", store_count, '<br>', "State: ", Abbreviation, '<br>', "Population: ", population))

# Create the map
map1 <- plot_geo(sb_locs_state, locationmode = 'USA-states') |> 
  add_trace(
    z = ~store_count,
    locations = ~Abbreviation,
    text = ~hover,
    hoverinfo = "text",
    colorscale = 'Reds',
    zmin = 0, zmax = max(sb_locs_state$store_count, na.rm = TRUE)
  ) |> 
  layout(
    title = "Number of Starbucks Stores per State",
    geo = set_map_details
  )

map1


map2 <- plot_geo(sb_locs_state, locationmode = 'USA-states') |> 
  add_trace(
    z = ~population,
    locations = ~Abbreviation,
    text = ~hover,
    hoverinfo = "text",
    colorscale = "Reds",
    zmin = 0, zmax = max(sb_locs_state$population, na.rm = TRUE)
  ) |> 
  layout(
    title = "Population per State",
    geo = set_map_details
  )

map2

subplot(map1, map2)
