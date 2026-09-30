# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(stargazer)


#Load stock data
ev_data <- read_excel ("C:/Users/lised/OneDrive/Bureau/chinese eco.xlsx")
ev_data <- ev_data %>%
  filter(year>2015)
# Converts all columns except Year and Country to numeric, ensuring data consistency for analysis.
ev_data <- ev_data %>%
  mutate(across(-c(year, country), as.numeric)) 
# Replace "-" with NA in the dataset
ev_data[ev_data == " "] <- NA
# Check for missing values
#Provides a statistical summary of ev_data, including counts of missing values, min, max, mean, and quartiles.
summary(ev_data)



ev_data$pays_code <- ifelse(ev_data$country == 'China', 1, 0)




# graph 1

ggplot(ev_data, aes(x = year)) +
  geom_line(aes(y = `EV share (%)`, color = country), size = 1) +
  geom_line(aes(y = `EV subsidise` / 1000, color = country), linetype = "dashed") +
  scale_y_continuous(
    name = "EV Share (%)",
    sec.axis = sec_axis(~.*1000, name = "Subsidy per Vehicle (USD)")
  ) +
  labs(title = "EV Share and Subsidies (2015–2023)", x = "Year") +
  theme_minimal()

ggplot(ev_data, aes(x = year)) +
  geom_line(aes(y = `EV share (%)`, color = country), size = 1) +
  geom_line(aes(y = (`EV charging points`/ 10000) , color = country), linetype = "dashed") +
  scale_y_continuous(
    name = "EV Share (%)",
    sec.axis = sec_axis(~.*1000, name = "Charging point")
  ) +
  labs(title = "EV Share and Charging point (2015–2023)", x = "Year") +
  theme_minimal()

#graph 2

charger_data <- ev_data %>%
  select(year, country, `EV charging points (Publicly available fast)`, `EV charging points (Publicly available low)`) %>%
  pivot_longer(cols = c(`EV charging points (Publicly available fast)`, `EV charging points (Publicly available low)`), names_to = "type", values_to = "count")

ggplot(charger_data, aes(x = year, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~country) +
  labs(title = "Charger Types by Country", y = "Number of Chargers", x = "Year") +
  theme_minimal()

# graph 3

ev_data <- ev_data %>%
  mutate(chargers_per_100k = `EV charging points`/ `population` * 100000)

ggplot(ev_data, aes(x = chargers_per_100k, y = `EV share (%)`, color = country)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "EV Share vs. Charging Infrastructure", x = "Chargers per 100k People", y = "EV Share (%)") +
  theme_minimal()

#table 1

ev_data %>%
  group_by(country) %>%
  summarise(
    EV_Share_Mean = mean(`EV share (%)`),
    Subsidy_Mean = mean(`EV subsidise`),
    Chargers_per_100k = mean(chargers_per_100k),
    GDP_per_Capita = mean(`GDP per  capita (current US$)`)
  )

# regression model
model <- lm(`EV share (%)` ~ `EV subsidise` + `GDP per  capita (current US$)` + chargers_per_100k , data = ev_data)
summary (model) 

tidy(model) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Regression Coefficients", x = "Variable", y = "Estimate") +
  theme_minimal()

ev_data$predicted <- predict(model)


ggplot(ev_data, aes(x = `EV share (%)`, y = predicted, color = country)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Actual vs. Predicted EV Share", x = "Actual EV Share (%)", y = "Predicted EV Share (%)") +
  theme_minimal()


stargazer(model, type = "html", 
                            out = "C:/Users/lised/OneDrive/Bureau/ev_regression_table.html", 
                            title = "Regression Results: EV Share",
                            dep.var.labels = "EV Share (%)",
                            covariate.labels = c("EV Subsidy", "GDP per Capita", "Chargers per 100k"),
                            digits = 4)
