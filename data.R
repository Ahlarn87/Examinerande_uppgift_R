library(tidyverse)

# laddar in data
insurance_data_raw <- read.csv("data/insurance_costs.csv")

# Skapar en överblick av datan
glimpse(insurance_data_raw)
summary(insurance_data_raw)

#Kollar saknade värden.
colSums(is.na(insurance_data_raw))

# Kollar unika värden i aktuella kolumner för att se eventuella fel, t.ex. olika stavningar
unique_values_raw <- tibble(
  column = c(
    "sex", "region", "smoker", "chronic_condition", "exercise_level", "plan_type"
  )
) %>%
  mutate(
    value = map(column, \(col) unique(insurance_data_raw[[col]])),
    values = map_chr(value, \(vals) paste(sort(vals), collapse = ", "))) %>%
  select(column, values)

unique_values_raw

# Datastädning, rensar text och gör värden konsekventa samt ändrar datatyper
insurance_data_clean <- insurance_data_raw %>%
  mutate(
    region = str_trim(region),
    region = str_to_lower(region),
    smoker = str_trim(smoker),
    smoker = str_to_lower(smoker),
    smoker = if_else(smoker=="yes", TRUE, FALSE, missing = NA),
    chronic_condition = if_else(chronic_condition=="yes", TRUE, FALSE, missing = NA),
    exercise_level = str_trim(exercise_level),
    exercise_level = na_if(exercise_level, ""),
    plan_type = str_trim(plan_type),
    plan_type = str_to_lower(plan_type),
    region = as.factor(region),
    annual_checkups = as.integer(annual_checkups),
    exercise_level = as.factor(exercise_level),
    plan_type = as.factor(plan_type),
    sex = as.factor(sex)
  )

glimpse(insurance_data_clean)

# Unikq värden efter datastädning
unique_values_clean <- tibble(
  column = c(
    "sex", "region", "smoker", "chronic_condition", "exercise_level", "plan_type"
  )
) %>%
  mutate(
    value = map(column, \(col) unique(insurance_data_clean[[col]])),
    values = map_chr(value, \(vals) paste(sort(vals), collapse = ", "))) %>%
  select(column, values)


colSums(is.na(insurance_data_clean))

#Hantera saknade värden i den rensade datan
insurance_data_ready <- insurance_data_clean %>%
  mutate(
    # Sätter saknade värden för bmi till medianen för att undvika outliers, mer stabilt
    bmi = coalesce(bmi, round(median(bmi, na.rm = TRUE), digits = 1)),
    # Sätter saknade värden för annual_checkups till medianen för att undvika outliers, mer stabilt
    annual_checkups = coalesce(annual_checkups, round(median(annual_checkups, na.rm = TRUE), digits = 0)),
    # Sätter saknade värden för exercise_level till den vanligaste kategorin, medium
    exercise_level = fct_na_value_to_level(exercise_level, level = "medium")
  )

summary(insurance_data_ready)


# Skapar nya variabler
insurance_data_ready <- insurance_data_ready %>%
  mutate(
    # Ny variabel som rangordnar bmi
    bmi_category = case_when(
      bmi < 18.5 ~ "underweight",
      bmi < 25 ~ "normal",
      bmi < 30 ~ "overweight",
      TRUE ~ "obese"
    ) %>% factor(levels = c("underweight", "normal", "overweight", "obese")),
    # Ny variabel som delar upp ålder i åldersspann
    age_group = cut(age, breaks = c(0, 25, 35, 45, 55, 110),
                    labels = c("18-24", "25-34", "35-44", "45-55", "55+"),
                    right = FALSE),
    has_children = children > 0,
    # Ny variabel som kombinerar tidigare accidents/claims
    prior_events = prior_claims + prior_accidents
  )

summary(insurance_data_ready)
glimpse(insurance_data_ready)

# värden för charges (medel, median)
summary(insurance_data_ready$charges)

# Skapar ett diagram för att se fördelningen av försäkringskostnaden
insurance_cost_viz <- ggplot(insurance_data_ready, aes(x=charges)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(
    title = "Fördelning av försäkringskostnad",
    subtitle = "Liten högerfördelning samt en lång svans mot höga kostnader",
    x = "Försäkringskostnad (USD)",
    y = "Antal personer",
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# Skapar ett diagram för att se skillnaden mellan rökare/ickerökare
cost_vs_smokers_viz <- ggplot(insurance_data_ready, aes(x=smoker, y=charges)) +
  geom_violin(aes(fill=smoker), alpha=0.75, color="black") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.6) +
  labs(
    title = "Fördelning av försäkringskostnader mellan rökare och icke-rökare",
    subtitle = "Tydlig ökning i kostnad för rökare, högre median och större spridning",
    x = NULL,
    y = "Försäkringskostnad"
  ) +
  theme_minimal(base_size = 11) +
  scale_x_discrete(labels = c("FALSE" = "Icke-rökare", "TRUE" = "Rökare")) +
  scale_fill_manual(values = c("FALSE" = "seagreen", "TRUE" = "indianred")) +
  theme(
    legend.position = "none",
    plot.title = element_text(face="bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(size = 10, colour = "grey40", hjust = 0.5)
  )

# Skapar ett diagram som visar kostnad efter ålder, BMI och rökstatus
age_vs_cost_viz <- ggplot(insurance_data_ready, aes(x = age, y = charges, color=smoker)) +
  geom_point(alpha = 0.6, size=1.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth=1.2) +
  facet_wrap(~ bmi_category, nrow = 2, scales = "free_y") +
  labs(
    title = "Samband mellan ålder och försäkringskostnad uppdelat på BMI och rökstatus",
    subtitle = "Vi ser en stigande kostnad för ålder i alla grupper förutom personer med lågt BMI",
    x = "Ålder (år)",
    y = "Försäkringskostnad (USD)",
    color = "Rökare"
  ) +
  theme_minimal(base_size = 11) +
  scale_color_manual(
    values = c("FALSE" = "seagreen", "TRUE" = "indianred"),
    labels = c("Icke-rökare", "Rökare")
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 10),
    legend.title = element_blank()
  )

# Skapar ett diagram som visar kostnad efter tidigare händelser
stat_sum_prior_events_viz <- ggplot(insurance_data_ready, aes(x = prior_events, y = charges, fill = prior_events)) +
  stat_summary(fun = mean, geom = "col", width = 0.7, alpha = 0.7) +
  labs(
    title = "Genomsnittlig försäkringskostnad beroende på antal tidigare händelser",
    subtitle = "Ju fler tidigare skador/olyckor, desto högre genomsnittlig kostnad",
    x = "Tidigare händelser (claims + accidents)",
    y = "Genomsnittlig kostnad (USD)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0.5),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Skapar ett diagram som visar genomsnittlig kostnad för träningsnivå
stat_sum_exercise_viz <- ggplot(insurance_data_ready, aes(x = exercise_level, y = charges, fill = exercise_level)) +
  stat_summary(fun = mean, geom = "col", width = 0.7, alpha = 0.7) +
  labs(
    title = "Genomsnittlig försäkringskostnad beroende på träningsnivå",
    subtitle = "Ju lägre träningsnivå, desto högre genomsnittlig kostnad",
    x = "Träningsnivå",
    y = "Genomsnittlig kostnad (USD)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0.5),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Skapar en tabell som visar värden bland olika kategorier
table <- insurance_data_ready %>%
  group_by(smoker, bmi_category) %>%
  summarise(
    Antal = n(),
    Medelvärde = round(mean(charges), 0),
    Median = round(median(charges), 0),
    Std.av = round(sd(charges), 0),
    Min = min(charges),
    Max = max(charges),
    .groups = "drop"
  ) %>%
  mutate(smoker = ifelse(smoker == "TRUE", "Rökare", "Icke-rökare")) %>%
  arrange(smoker, bmi_category)





# Regression med modell på alla variabler, för att kunna göra ett urval av de som påverkas mest
model_1 <- lm(charges ~ 
                sex + region + smoker + age + bmi+ has_children+ chronic_condition+ exercise_level+ plan_type+ prior_events+ annual_checkups, data = insurance_data_ready
              )
summary(model_1)

# Väljer ut de starkaste variablerna p<0.05
model_data <- insurance_data_ready %>%
  select(
    charges,
    smoker,
    age,
    bmi,
    chronic_condition,
    exercise_level,
    plan_type,
    prior_events
  )

# Modell med de 7 starkaste variablerna
model_7 <- lm(charges ~ smoker + chronic_condition + prior_events + bmi + age + exercise_level + plan_type, data = model_data)
summary(model_7)
confint(model_7)

# Modell med de 5 starkaste variablerna
model_5 <- lm(charges ~ smoker + chronic_condition + prior_events + bmi + age, data = model_data)
summary(model_5)

# Modell med de 3 starkaste variablerna
model_3 <- lm(charges ~ smoker + chronic_condition + prior_events, data = model_data)
summary(model_3)

# Skapar en sammanställning och jämförelse av modellerna
model_comparison <- tibble(
  model = c(
    "Model 1: all variables",
    "Model 7: 7 variables",
    "Model 5: 5 variables",
    "Model 3: 3 variables"
  ),
  r_squared = c(
    summary(model_1)$r.squared,
    summary(model_7)$r.squared,
    summary(model_5)$r.squared,
    summary(model_3)$r.squared
  ),
  adjusted_r_squared = c(
    summary(model_1)$adj.r.squared,
    summary(model_7)$adj.r.squared,
    summary(model_5)$adj.r.squared,
    summary(model_3)$adj.r.squared
  ),
  residual_se = c(
    summary(model_1)$sigma,
    summary(model_7)$sigma,
    summary(model_5)$sigma,
    summary(model_3)$sigma
  )
)

# Tar fram data för alla tre modeller för att se residuals
model_3_diagnostics <- model_data %>% 
  mutate(
    fitted_value = fitted(model_3),
    residual = resid(model_3),
    model = "Model 3"
  )

model_5_diagnostics <- model_data %>% 
  mutate(
    fitted_value = fitted(model_5),
    residual = resid(model_5),
    model = "Model 5"
  )

model_7_diagnostics <- model_data %>% 
  mutate(
    fitted_value = fitted(model_7),
    residual = resid(model_7),
    model = "Model 7"
  )

# Skapar diagram för att se residualer vid predikterat pris
model_comparison_viz <- bind_rows(model_3_diagnostics, model_5_diagnostics, model_7_diagnostics) %>% 
  ggplot(aes(x = fitted_value, y = residual)) +
  geom_point(alpha = 0.5, size = 1.6, color = "seagreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "indianred", linewidth = 0.8) +
  facet_wrap(~model, scales = "free_x") +
  labs(
    title = "Residualer i tre olika modeller",
    subtitle = "Jämförelse av hur väl modellerna presterar",
    x = "Predikterat pris (USD)",
    y = "Residual"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )

# Väljer Modell 7 efter jämförelse
chosen_model_diagnostics <- model_data %>% 
  mutate(
    fitted_value = fitted(model_7),
    residual = resid(model_7)
  )

chosen_model_diagnostics %>% 
  select(charges, fitted_value, residual) %>% 
  slice_head(n = 10)

# Skapar diagram som visar faktiskt pris mot predikterat pris för vald modell
chosen_model_viz <- ggplot(chosen_model_diagnostics, aes(x = fitted_value, y = charges)) +
  geom_point(alpha = 0.5, size = 1.8, color = "seagreen") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red",  linewidth = 1) +
  labs(
    title = "Faktiskt pris mot predikterat pris",
    subtitle = "Modell 7 - Hur väl stämmer prediktionerna med verkligheten?",
    x = "Predikterat pris (USD)",
    y = "Faktiskt pris (USD)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0.5),
    panel.grid.minor = element_blank()
  )