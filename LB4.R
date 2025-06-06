library(tidyverse)
library(lubridate)
library(np) 
movies_initial <- read_csv("D:\\Навчання\\3 Курс\\2 Семестр\\AD\\TMDB_movie_dataset_v11.csv")

# Переклад назв місяців
month_translation_num <- c(
  "Січ", "Лют", "Бер", "Кві", "Тра", "Чер", 
  "Лип", "Сер", "Вер", "Жов", "Лис", "Гру"
)

# ID, де потрібно замінити значення на NA
revenue_to_na <- c(1407985, 1270893, 1224207, 1326885, 1294302, 1236552)
budget_to_na <- c(1381066, 1235037, 1224207, 1057999, 1022208, 
                  1201764, 1399448, 1426913, 1449031, 1320160, 
                  1453767, 1453985, 1414861, 1398923, 1365277,
                  1417006, 1441191, 1450893, 1450893, 1301115, 1228885,
                  1272552, 1229118, 1294480, 622311, 1369796, 1108211)
runtime_to_na <- c(206026, 368247, 717019, 206026, 392372, 454409, 500980,
                   544686, 732330, 66871, 633832, 671214, 531640, 535892,
                   523167, 631038, 698754, 685310, 651033, 125120, 298752)

movies_clean <- movies_initial %>%
  
  # Numeric columns
  mutate(
    vote_average = if_else(vote_average <= 0, NA_real_, vote_average),
    vote_count = if_else(vote_count < 0, NA_integer_, vote_count),
    revenue = if_else(revenue < 0, NA_real_, revenue),
    runtime = if_else(runtime <= 0, NA_real_, runtime),
    budget = if_else(budget < 10, NA_real_, budget)
  ) %>%
  
  # Categorical variables
  mutate(  
    status = factor(status),
    original_language = factor(original_language)
  ) %>%
  
  # Text columns
  mutate(
    production_companies = na_if(str_trim(production_companies), ""),
    keywords = na_if(str_trim(keywords), ""),
    genres = na_if(str_trim(genres), "")
  ) %>%
  
  # Empty titles to NA
  mutate(
    title = na_if(str_trim(title), ""),
    original_title = na_if(str_trim(original_title), "")
  ) %>%

  # Extracting date parts
  mutate(
    release_year = year(release_date),
    release_month = month(release_date),
    release_day = day(release_date),
    release_month_ua = month_translation_num[month(release_date)]
  ) %>%
  
  # Season label
  mutate(
    season = case_when(
      release_month_ua %in% c("Січ", "Лют", "Бер") ~ "Зима",
      release_month_ua %in% c("Кві", "Тра", "Чер") ~ "Весна",
      release_month_ua %in% c("Лип", "Сер", "Вер") ~ "Літо",
      release_month_ua %in% c("Жов", "Лис", "Гру") ~ "Осінь",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Manual error correction
  mutate(
    revenue = if_else(release_date > Sys.Date(), NA_real_, revenue),
    revenue = if_else(id %in% revenue_to_na, NA_real_, revenue),
    budget  = if_else(id %in% budget_to_na, NA_real_, budget),
    runtime = if_else(id %in% runtime_to_na, NA_real_, runtime)
  ) %>%

  # Drop unnecessary columns
  select(-backdrop_path, -homepage, -popularity, -poster_path, -imdb_id)

movies_model_data <- movies_clean %>%
  filter(!is.na(vote_average), !is.na(runtime), !is.na(season), 
         !is.na(vote_count), !is.na(budget), !is.na(release_year)) %>%
  mutate(
    log_vote_count = log1p(vote_count),  # log(1 + x) уникнення лог(0)
    log_budget = log1p(budget),
    runtime2 = runtime^2,
    runtime3 = runtime^3,
    decade = factor(floor(release_year / 10) * 10)
  )

#частково лінійна регресія з непараметричною оцінкою runtime
# Формуємо дані
df <- movies_model_data %>%
  select(vote_average, runtime, season, vote_count, budget)

df_small <- df %>% slice_sample(n = 100000)

# Ядрова регресія vote_average ~ runtime
m_y <- npreg(vote_average ~ runtime, data = df_small)

# Ядрова регресія всіх X ~ runtime
m_x1 <- npreg(vote_count ~ runtime, data = df_small)
m_x2 <- npreg(budget ~ runtime, data = df_small)

# Фактор season у даммі-перетворенні (без першої базової)
season_dummies <- model.matrix(~ season, data = df_small)[, -1]

# Обчислюємо залишки
res_y <- residuals(m_y)
res_x1 <- residuals(m_x1)
res_x2 <- residuals(m_x2)

# Побудова моделі на залишках без decade
X_resid <- cbind(res_x1, res_x2, season_dummies)
colnames(X_resid)[1:2] <- c("log_vote_count_resid", "log_budget_resid")

partially_linear_model <- lm(res_y ~ X_resid)
summary(partially_linear_model)

set.seed(123)

# Дані для графіку
runtime_grid <- data.frame(runtime = seq(min(df_small$runtime), max(df_small$runtime), length.out = 100))

# Прогноз з уже навченого m_y (без повторного виклику npreg)
fit <- predict(m_y, newdata = runtime_grid)

# Створення датафрейму з оціненими значеннями
plot_data <- data.frame(
  runtime = runtime_grid$runtime,
  fit = fit
)

# Побудова графіку
ggplot(plot_data, aes(x = runtime, y = fit)) +
  geom_line(color = "blue", size = 1.2) +
  labs(
    title = "Частково лінійна регресія: vote_average ~ runtime + season, vote_count, budget",
    x = "Runtime (хв)",
    y = "Оцінене значення vote_average"
  ) +
  theme_minimal()

df_small <- df %>% slice_sample(n = 10000)

# Ядрова регресія vote_average ~ runtime
m_y <- npreg(vote_average ~ runtime, data = df_small)

# Ядрова регресія всіх X ~ runtime
m_x1 <- npreg(vote_count ~ runtime, data = df_small)
m_x2 <- npreg(budget ~ runtime, data = df_small)

# Фактор season у даммі-перетворенні (без першої базової)
season_dummies <- model.matrix(~ season, data = df_small)[, -1]

# Обчислюємо залишки
res_y <- residuals(m_y)
res_x1 <- residuals(m_x1)
res_x2 <- residuals(m_x2)

# Побудова моделі на залишках без decade
X_resid <- cbind(res_x1, res_x2, season_dummies)
colnames(X_resid)[1:2] <- c("log_vote_count_resid", "log_budget_resid")

partially_linear_model <- lm(res_y ~ X_resid)
summary(partially_linear_model)

set.seed(123)

# Дані для графіку
runtime_grid <- data.frame(runtime = seq(min(df_small$runtime), max(df_small$runtime), length.out = 100))

# Прогноз з уже навченого m_y (без повторного виклику npreg)
fit <- predict(m_y, newdata = runtime_grid)

# Створення датафрейму з оціненими значення
plot_data <- data.frame(
  runtime = runtime_grid$runtime,
  fit = fit
)

# Бутстреп функція
bootstrap_npreg <- function(data, grid, B = 100) {
  fits <- matrix(NA, nrow = nrow(grid), ncol = B)
  
  for (b in 1:B) {
    sample_data <- data %>% slice_sample(n = nrow(data), replace = TRUE)
    model_b <- npreg(vote_average ~ runtime, data = sample_data)
    fits[, b] <- predict(model_b, newdata = grid)
  }
  
  return(fits)
}

set.seed(123)

runtime_grid <- data.frame(runtime = seq(min(df_small$runtime), max(df_small$runtime), length.out = 100))

m_y <- npreg(vote_average ~ runtime, data = df_small)
fit <- predict(m_y, newdata = runtime_grid)

# Бутстреп оцінки
B <- 100
bootstrap_fits <- bootstrap_npreg(df_small, runtime_grid, B = B)

lower <- apply(bootstrap_fits, 1, quantile, probs = 0.025)
upper <- apply(bootstrap_fits, 1, quantile, probs = 0.975)

# Формуємо фінальний датафрейм
plot_data <- data.frame(
  runtime = runtime_grid$runtime,
  fit = fit,
  lower = lower,
  upper = upper
)

# Побудова графіку з тінню довірчого інтервалу
ggplot(plot_data, aes(x = runtime, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1.2) +
  labs(
    title = 
    x = 
    y = 
  ) +
  theme_minimal()