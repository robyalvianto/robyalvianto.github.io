library(here)
library(tidyverse)
library(DBI)
library(RMySQL)
library(lubridate)
library(tidymodels)
library(tidyclust)
library(corrplot)
library(lmtest)
library(car)
library(broom)
library(gridExtra)
library(sandwich)
library(forecast)
library(factoextra)

conn <-dbConnect(
  MySQL(),
  username = 'root',
  host = '127.0.0.1',
  root = 3306,
  password = 'robyalvianto1146@.',
  dbname = 'cm_1'
)

dbListTables(conn)
dbListFields(conn, 'payment')
dbGetQuery(conn, 'DESC payment')

exec.summary <-dbGetQuery(
  conn = conn,
  statement = 'SELECT 
                  SUM(p.amount) AS \'Total Revenue\',
                  AVG(p.amount) AS \'Average per Transactions\',
                  COUNT(r.rental_id) AS \'Total Rental\'
               FROM payment AS p
               INNER JOIN rental AS r ON p.rental_id = r.rental_id'
)

trend <-dbGetQuery(
  conn = conn,
  statement = 'SELECT
                  r.rental_date AS \'Rental Date\',
                  COUNT(r.rental_id) AS \'Total Rental\',
                  SUM(p.amount) AS \'Total Revenue\'
               FROM rental AS r
               INNER JOIN payment AS p ON r.rental_id = p.rental_id
               GROUP BY r.rental_date'
)

category.film <-dbGetQuery(
  conn = conn,
  statement = 'WITH comb AS(
                  SELECT 
                      c.name AS category,
                      i.inventory_id AS inventory_id
                  FROM category AS c
                  INNER JOIN film_category AS fc ON c.category_id = fc.category_id
                  INNER JOIN film AS f ON fc.film_id = f.film_id
                  INNER JOIN inventory AS i ON f.film_id = i.film_id
               )
               SELECT 
                  c.category AS Category,
                  r.rental_id AS \'Rental ID\',
                  p.amount AS Amount
               FROM comb AS c
               INNER JOIN rental AS r ON c.inventory_id = r.inventory_id
               INNER JOIN payment AS p ON r.rental_id = p.rental_id'
)

film.actor <-dbGetQuery(
  conn = conn,
  statement = 'WITH film_actor AS(
                  SELECT
                      f.title AS title,
                      CONCAT(a.first_name, \' \', a.last_name) AS actor,
                      i.inventory_id AS inventory_id
                  FROM film AS f
                  INNER JOIN film_actor AS fc ON f.film_id = fc.film_id
                  INNER JOIN actor AS a ON fc.actor_id = a.actor_id
                  INNER JOIN inventory AS i ON f.film_id = i.film_id
               )
               SELECT
                  fa.title AS Title,
                  fa.actor AS Actor,
                  r.rental_id AS \'Rental ID\',
                  p.amount AS Amount
               FROM film_actor AS fa
               INNER JOIN rental AS r ON fa.inventory_id = r.inventory_id
               INNER JOIN payment AS p ON r.rental_id = p.rental_id'
)

dbExecute(
  conn = conn,
  statement = 'SET @time_ref = \'2006-03-01 00:00:00\''
)

dbExecute(
  conn = conn,
  statement = 'CREATE TABLE IF NOT EXISTS time_ref_metadata(
                  create_id VARCHAR(11) PRIMARY KEY,
                  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
                  method VARCHAR(10) COLLATE utf8mb4_general_ci,
                  time_ref DATETIME,
                  timezone VARCHAR(5) COLLATE utf8mb4_general_ci,
                  script_path VARCHAR(100) COLLATE utf8mb4_general_ci NOT NULL DEFAULT \'Pending\',
                  script_commit VARCHAR(30) COLLATE utf8mb4_general_ci NOT NULL DEFAULT \'Pending\',
                  notes VARCHAR(50) COLLATE utf8mb4_general_ci
               ) ENGINE = innodb CHAR SET utf8mb4'
)

dbGetQuery(
  conn = conn,
  statement = 'DESC time_ref_metadata'
)

dbBegin(conn)

dbExecute(
  conn = conn,
  statement = 'INSERT INTO time_ref_metadata(create_id, method, time_ref, timezone, notes)
               VALUES(
                  \'CID20060301\',
                  \'FIXED\',
                  CAST(@time_ref AS DATETIME),
                  \'UTC+7\',
                  \'REFERENCE DATE FOR REFERENCE THE CURRENT TIME\' 
               )'
)

dbGetQuery(
  conn = conn,
  statement = 'SELECT * FROM time_ref_metadata'
)

dbCommit(conn)

segment <-dbGetQuery(
  conn = conn,
  statement = 'SELECT
                  c.customer_id AS \'Customer ID\',
                  CONCAT(c.first_name, \' \', c.last_name) AS \'Customer Name\',
                  c.active AS Status,
                  MAX(r.rental_date) AS \'Last Rental Date\',
                  DATEDIFF(CAST(@time_ref AS DATETIME), MAX(r.rental_date)) AS \'Recency Date\',
                  COUNT(DISTINCT(r.rental_id)) AS Frequency,
                  SUM(p.amount) AS Monetary
               FROM customer AS c
               INNER JOIN rental AS r ON c.customer_id = r.customer_id
               INNER JOIN payment AS p ON c.customer_id = p.customer_id
               GROUP BY c.customer_id'
)

store <-dbGetQuery(
  conn = conn,
  statement = 'WITH loc AS(
                  SELECT 
                      s.store_id AS store_id,
                      a.address AS address,
                      c.city AS city,
                      co.country AS country
                  FROM store AS s
                  INNER JOIN address AS a ON s.address_id = a.address_id
                  INNER JOIN city AS c ON a.city_id = c.city_id
                  INNER JOIN country AS co ON c.country_id = co.country_id
               )
               SELECT
                  l.store_id AS \'Store ID\',
                  l.address AS Address,
                  l.city AS City,
                  l.country AS Country,
                  COUNT(p.payment_id) AS \'Transaction Frequency\',
                  SUM(p.amount) AS \'Total Transaction\'
               FROM loc AS l
               INNER JOIN customer AS c ON l.store_id = c.store_id
               INNER JOIN payment AS p ON c.customer_id = p.customer_id
               GROUP BY l.store_id'
)

View(trend)
View(category.film)
View(film.actor)
View(segment)

glimpse(exec.summary)
glimpse(trend)
glimpse(category.film)
glimpse(film.actor)
glimpse(segment)
glimpse(store)

trend %>%
  summarise(total.null.trend = sum(is.na(trend)))

category.film %>%
  summarise(total.null.category.film = sum(is.na(category.film)))

film.actor %>%
  summarise(total.null.film.actor = sum(is.na(film.actor)))

segment %>%
  summarise(total.null.segment = sum(is.na(segment)))

trend %>%
  summarise(total.duplicated.trend = sum(duplicated(trend)))

category.film %>%
  summarise(total.duplicated.category.film = sum(duplicated(category.film)))

film.actor %>%
  summarise(total.duplicated.film.actor = sum(duplicated(film.actor)))
  
segment %>%
  summarise(total.duplicated.segment = sum(duplicated(segment)))

truncated.daily.trend <-trend %>%
  mutate(
    `Rental Date` = ymd_hms(`Rental Date`),
    `Rental Date` = floor_date(`Rental Date`, unit = 'day'),
    `Rental Date` = ymd(`Rental Date`)
  ) %>%
  group_by(`Rental Date`) %>%
  summarise(
    `Total Rental` = sum(`Total Rental`),
    `Total Revenue` = sum(`Total Revenue`)
  )

grid.arrange(
  ggplot(
    data = truncated.daily.trend %>%
      select(`Rental Date`, `Total Rental`),
    aes(x = `Rental Date`, y = `Total Rental`)
  ) + geom_line(
    linewidth = 1,
    color = '#ffffff'
  ) + geom_point(
    size = 1,
    colour = '#ffffff'
  ) + theme (
    panel.background = element_rect(fill = '#232F72'),
    panel.grid = element_line(color = 'gray'),
    plot.background = element_rect(fill = '#232F72'),
    axis.title.x = element_text(color = '#ffffff'),  
    axis.title.y = element_text(color = '#ffffff'),
    axis.text.x = element_text(color = '#ffffff'),
    axis.text.y = element_text(color = '#ffffff')
  ),
  
  ggplot(
    data = truncated.daily.trend %>%
      select(`Rental Date`, `Total Revenue`),
    aes(x = `Rental Date`, y = `Total Revenue`)
  ) + geom_line(
    linewidth = 1,
    color = '#ffffff'
  ) + geom_point(
    size = 1,
    color = '#ffffff'
  ) + theme(
    panel.background = element_rect(fill = '#232F72'),
    panel.grid = element_line(color = 'gray'),
    plot.background = element_rect(fill = '#232F72'),
    axis.title.x = element_text(color = '#ffffff'),  
    axis.title.y = element_text(color = '#ffffff'),
    axis.text.x = element_text(color = '#ffffff'),
    axis.text.y = element_text(color = '#ffffff')
  ),
  
  ncol = 2
)

popular.category <-category.film %>%
  group_by(Category) %>%
  count(Category) %>%
  ungroup() %>%
  arrange(desc(n))

revenue.category <-category.film %>%
  group_by(Category) %>%
  summarise(Revenue = sum(Amount)) %>%
  ungroup() %>%
  arrange(desc(Revenue)) 

grid.arrange(
  ggplot(
    data = popular.category,
    aes(x = fct_reorder(Category, n), y = n)
  ) + geom_col(
    width = 0.5,
    color = '#000000',
    fill = '#ffffff',
    linewidth = 0.6
  ) + coord_flip() + labs(
    title = 'Category Interest',
    x = 'Films',
    y = 'Total'
  ) + geom_text(
    aes(label = paste0(n)),
    size = 5,
    color = '#ffffff',
    position = position_stack(vjust = 1.03)
  ) + theme(
    plot.background = element_rect(fill = '#232F72'),
    panel.background = element_rect(fill = '#232F72'),
    panel.grid = element_blank(),
    plot.title = element_text(size = 22, hjust = 0.5, color = '#ffffff', margin = margin(t = 15, b = 15)),
    axis.title.x = element_text(color = '#ffffff', size = 12),
    axis.title.y = element_text(color = '#ffffff', size = 12),
    axis.text.x = element_text(color = '#ffffff', size = 10),
    axis.text.y = element_text(color = '#ffffff', size = 10, margin = margin(r = -33)),
    
  ),
  
  ggplot(
    data = revenue.category,
    aes(x = fct_reorder(Category, Revenue), y = Revenue)
  ) + geom_col(
    width = 0.5,
    color = '#000000',
    fill = '#ffffff',
    linewidth = 0.6
  ) + coord_flip() + labs(
    title = 'Best Revenue Category',
    x = 'Category',
    y = 'Revenue'
  ) + geom_text(
    aes(label = paste0(Revenue)),
    size = 5,
    color = '#ffffff',
    position = position_stack(vjust = 1.05)
  ) + theme(
    plot.background = element_rect(fill = '#232F72'),
    panel.background = element_rect(fill = '#232F72'),
    panel.grid = element_blank(),
    plot.title = element_text(size = 22, hjust = 0.5, color = '#ffffff', margin = margin(t = 15, b = 15)),
    axis.title.x = element_text(color = '#ffffff', size = 12),
    axis.title.y = element_text(color = '#ffffff', size = 12),
    axis.text.x = element_text(color = '#ffffff', size = 10),
    axis.text.y = element_text(color = '#ffffff', size = 10, margin = margin(r = -33)),
    
  ),
  
  ncol = 1
)

popular.film <-film.actor %>%
  group_by(Title) %>%
  count(Title) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)
  
ggplot(
  data = popular.film,
  aes(x = fct_reorder(Title, n), y = n)
) + geom_col(
  width = 0.5,
  color = '#000000',
  fill = '#ffffff',
  linewidth = 0.6
) + coord_flip() + labs(
  title = 'Best Rental Movies',
  x = 'Films',
  y = 'Total'
) + geom_text(
  aes(label = n),
  size = 3,
  color = '#ffffff',
  position = position_stack(vjust = 1.03)
) + theme(
  plot.background = element_rect(fill = '#232F72'),
  panel.background = element_rect(fill = '#232F72'),
  panel.grid = element_blank(),
  plot.title = element_text(size = 22, hjust = 0.5, color = '#ffffff', margin = margin(t = 15, b = 15)),
  axis.title.x = element_text(color = '#ffffff'),
  axis.title.y = element_text(color = '#ffffff'),
  axis.text.x = element_text(color = '#ffffff'),
  axis.text.y = element_text(color = '#ffffff')
)

productive.actor <-film.actor %>%
  group_by(Actor) %>%
  count(Actor) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  slice_head(n = 7)

ggplot(
  data = productive.actor,
  aes(x = fct_reorder(Actor, n, .desc = T), y = n)
) + geom_col(
  width = 0.5,
  color = '#000000',
  fill = '#ffffff',
  linewidth = 0.6
) + labs(
  title = 'Best Rental Movies',
  x = 'Films',
  y = 'Total'
) + geom_text(
  aes(label = n),
  size = 5,
  color = '#ffffff',
  position = position_stack(vjust = 1.66)
) + theme(
  plot.background = element_rect(fill = '#232F72'),
  panel.background = element_rect(fill = '#232F72'),
  panel.grid = element_blank(),
  plot.title = element_text(size = 22, hjust = 0.5, color = '#ffffff', margin = margin(t = 15, b = 15)),
  axis.title.x = element_text(color = '#ffffff'),
  axis.title.y = element_text(color = '#ffffff'),
  axis.text.x = element_text(color = '#ffffff'),
  axis.text.y = element_text(color = '#ffffff')
)

corrplot(cor(segment %>% select(`Recency Date`, Frequency, Monetary)), method = 'number')

corrplot(
  cor(segment %>% select(`Recency Date`, Frequency, Monetary)),
  method = 'number',           # Tampilkan angka
  bg = '#232F72',              # ← Background ungu (parameter bg)
  addCoef.col = '#ffffff',       # Warna angka
  tl.col = '#ffffff',            # Warna label variabel
  tl.srt = 45,                 # Rotasi label
  col = colorRampPalette(c('#FDCB6E', '#ffffff', '#00B894'))(200),  # Skema warna korelasi
  number.cex = 1.2,            # Ukuran angka
  tl.cex = 1.1,                # Ukuran label
  cl.pos = 'n'                 # Sembunyikan color legend (opsional)
)


# Set background device ungu
par(
  bg = "#232F72",           # Device background
  fg = "white",              # Foreground default putih
  col.main = "white",        # Title putih
  col.lab = "white",         # Label putih
  col.axis = "white"         # Axis putih
)

# Plot dengan modifikasi warna
corrplot(
  cor(segment %>% select(`Recency Date`, Frequency, Monetary)),
  method = "number",
  addCoef.col = "white",      # Angka putih
  tl.col = "white",           # Text label putih
  tl.srt = 45,
  cl.pos = "n",               # Hilangkan color legend (susah di-styling)
  
  # Warna matriks
  col = colorRampPalette(c("#FD79A8", "#FDCB6E", "#00B894"))(200),
  
  # Hapus background default
  mar = c(0, 0, 2, 0)
)

# Reset par
par(bg = "white")

grid.arrange(
  ggplot(
    data = segment %>% select(`Recency Date`),
    aes(x = '', y = `Recency Date`)
  ) + geom_boxplot(
    fill = '#ffffff',
    color = '#000000'
  ) + stat_boxplot(
    geom = 'errorbar',
    color = '#ffffff'
  ) + theme(
    plot.background = element_rect(fill = '#232F72'),
    panel.background = element_rect(fill = '#232F72'),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = '#ffffff'),
    axis.text.y = element_text(color = '#ffffff')
  ),
  
  ggplot(
    data = segment %>% select(Frequency),
    aes(x = '', y = Frequency)
  ) + geom_boxplot(
    fill = '#ffffff',
    color = '#000000'
  ) + stat_boxplot(
    geom = 'errorbar',
    color = '#ffffff'
  ) + theme(
    plot.background = element_rect(fill = '#232F72'),
    panel.background = element_rect(fill = '#232F72'),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = '#ffffff'),
    axis.text.y = element_text(color = '#ffffff')
  ),
  
  ggplot(
    data = segment %>% select(Monetary),
    aes(x = '', y = Monetary)
  ) + geom_boxplot(
    fill = '#ffffff',
    color = '#000000'
  ) + stat_boxplot(
    geom = 'errorbar',
    color = '#ffffff'
  ) + theme(
    plot.background = element_rect(fill = '#232F72'),
    panel.background = element_rect(fill = '#232F72'),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = '#ffffff'),
    axis.text.y = element_text(color = '#ffffff')
  ),
  
  ncol = 3
)

trim.func <-function(col) {
  q1 <-quantile(x = col, probs = 0.25)
  q3 <-quantile(x = col, probs = 0.75)
  iqr <-q3-q1
  
  lower <- q1 - (1.5 * iqr)
  upper <-q3 + (1.5 * iqr)
  
  col <-if_else(between(x = col, left = lower, right = upper), col, NA)
  
  return(col)
}

segment <-segment %>%
  mutate(
    Frequency = trim.func(Frequency),
    Monetary = trim.func(Monetary)
  ) %>%
  filter(
    !is.na(Frequency),
    !is.na(Monetary)
  )

recipe.model <-recipe(Monetary ~ `Recency Date` + Frequency, data = segment) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_BoxCox(all_outcomes())

prep.model <-recipe.model %>%
  prep(training = segment)

model <-linear_reg() %>%
  set_engine('lm')

workflow.model <-workflow() %>%
  add_recipe(recipe.model) %>%
  add_model(model)

fit.model <-workflow.model %>%
  fit(data = segment)

lm.engine <-fit.model %>%
  extract_fit_engine() 
  
df.result <-lm.engine %>% 
  augment(segment)

tidy(lm.engine)
glance(lm.engine)

shapiro.test(residuals(lm.engine))

ggplot(
  data = tibble(residual = residuals(lm.engine)),
  aes(sample = residual)
) + stat_qq(
  size = 1.5,
  alpha = 0.6
) + stat_qq_line(
  color = 'red'
) + theme_minimal()

ggplot(
  data = tibble(residual = residuals(lm.engine)),
  aes(x = residual)
) + geom_histogram(
  aes(y = after_stat(density)),
  bins = 30,
  fill = 'skyblue',
  color = 'black'
) + theme_minimal()

bptest(lm.engine)
dwtest(lm.engine)
vif(lm.engine)

threshold.leverage <-2 * length(coefficients(lm.engine)) / segment %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

threshold.cooks.distance <-4 / segment %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

print(round(threshold.leverage, 3))
print(round(threshold.cooks.distance, 3))

df.result <-df.result %>%
  mutate(
    flag.leverage = .hat > threshold.leverage,
    flag.cook.distance = .cooksd > threshold.cooks.distance
  )

df.result %>%
  select(flag.cook.distance) %>%
  filter(flag.cook.distance == T)

segment.2 <-df.result %>%
  filter(!flag.cook.distance == T) %>%
  select(`Recency Date`, Frequency, Monetary)

recipe.fit <-recipe(Monetary ~ `Recency Date` + Frequency, data = segment.2) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_BoxCox(all_outcomes())

prep.model.fit <-recipe.fit %>%
  prep(training = segment)

model.fit <-linear_reg() %>%
  set_engine('lm')

workflow.fit <-workflow() %>%
  add_recipe(recipe.fit) %>%
  add_model(model.fit)

fit.model.2 <-workflow.fit %>%
  fit(data = segment.2 %>% select(Monetary, `Recency Date`, Frequency))

lm.engine.fit <-fit.model.2%>%
  extract_fit_engine()

df.result.fit <-lm.engine.fit %>%
  augment(segment.2 %>% select(Monetary, `Recency Date`, Frequency))

tidy(lm.engine.fit)
glance(lm.engine.fit)

shapiro.test(residuals(lm.engine.fit))
bptest(lm.engine.fit)
dwtest(lm.engine.fit)
vif(lm.engine.fit)

threshold.leverage.fit <-2 * length(coefficients(lm.engine.fit)) / segment.2 %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

threshold.cooks.distance.fit <-4 / segment.2 %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

print(round(threshold.leverage.fit, 3))
print(round(threshold.cooks.distance.fit, 3))

df.result.fit <-df.result.fit %>%
  mutate(
    flag.leverage = .hat > threshold.leverage.fit,
    flag.cook.distance = .cooksd > threshold.cooks.distance.fit
  ) 

df.result.fit %>%
  select(flag.cook.distance) %>%
  filter(flag.cook.distance == T)

segment.3 <-df.result.fit %>%
  filter(!flag.cook.distance == T) %>%
  select(`Recency Date`, Frequency, Monetary)

recipe.fit.2 <-recipe(Monetary ~ `Recency Date` + Frequency, data = segment.3) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_BoxCox(all_outcomes())

prep.model.fit.2 <-recipe.fit.2 %>%
  prep(training = segment)

model.fit.2 <-linear_reg() %>%
  set_engine('lm')

workflow.fit.2 <-workflow() %>%
  add_recipe(recipe.fit.2) %>%
  add_model(model.fit.2)

fit.model.3 <-workflow.fit.2 %>%
  fit(data = segment.3 %>% select(Monetary, `Recency Date`, Frequency))

lm.engine.fit.2 <-fit.model.3%>%
  extract_fit_engine()

df.result.fit.2 <-lm.engine.fit.2 %>%
  augment(segment.3 %>% select(Monetary, `Recency Date`, Frequency))

tidy(lm.engine.fit.2)
glance(lm.engine.fit.2)

shapiro.test(residuals(lm.engine.fit.2))
bptest(lm.engine.fit.2)
dwtest(lm.engine.fit.2)
vif(lm.engine.fit.2)

threshold.leverage.fit.2 <-2 * length(coefficients(lm.engine.fit.2)) / segment.3 %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

threshold.cooks.distance.fit.2 <-4 / segment.3 %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

print(round(threshold.leverage.fit.2, 3))
print(round(threshold.cooks.distance.fit.2, 3))

df.result.fit.2 <-df.result.fit.2 %>%
  mutate(
    flag.leverage = .hat > threshold.leverage.fit,
    flag.cook.distance = .cooksd > threshold.cooks.distance.fit
  ) 

df.result.fit.2 %>%
  select(flag.cook.distance) %>%
  filter(flag.cook.distance == T)

segment.4 <-df.result.fit.2 %>%
  filter(!flag.cook.distance == T) %>%
  select(`Recency Date`, Frequency, Monetary)

recipe.fit.3 <-recipe(Monetary ~ `Recency Date` + Frequency, data = segment.4) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_BoxCox(all_outcomes())

prep.model.fit.3 <-recipe.fit.3 %>%
  prep(training = segment)

model.fit.3 <-linear_reg() %>%
  set_engine('lm')

workflow.fit.3 <-workflow() %>%
  add_recipe(recipe.fit.3) %>%
  add_model(model.fit.3)

fit.model.4 <-workflow.fit.3 %>%
  fit(data = segment.4 %>% select(Monetary, `Recency Date`, Frequency))

lm.engine.fit.3 <-fit.model.4%>%
  extract_fit_engine()

df.result.fit.3 <-lm.engine.fit.3 %>%
  augment(segment.4 %>% select(Monetary, `Recency Date`, Frequency))

tidy(lm.engine.fit.3)
glance(lm.engine.fit.3)

shapiro.test(residuals(lm.engine.fit.3))
bptest(lm.engine.fit.3)
dwtest(lm.engine.fit.3)
vif(lm.engine.fit.3)

threshold.leverage.fit.3 <-2 * length(coefficients(lm.engine.fit.3)) / segment.4 %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

threshold.cooks.distance.fit.3 <-4 / segment.4 %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

print(round(threshold.leverage.fit.3, 3))
print(round(threshold.cooks.distance.fit.3, 3))

df.result.fit.3 <-df.result.fit.3 %>%
  mutate(
    flag.leverage = .hat > threshold.leverage.fit,
    flag.cook.distance = .cooksd > threshold.cooks.distance.fit
  ) 

df.result.fit.3 %>%
  select(flag.cook.distance) %>%
  filter(flag.cook.distance == T)

segment.last <-df.result.fit.3 %>%
  filter(!flag.cook.distance == T) %>%
  select(`Recency Date`, Frequency, Monetary)

recipe.fit.last <-recipe(Monetary ~ `Recency Date` + Frequency, data = segment.last) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_BoxCox(all_outcomes())

prep.model.fit.last <-recipe.fit.last %>%
  prep(training = segment)

model.fit.last <-linear_reg() %>%
  set_engine('lm')

workflow.fit.last <-workflow() %>%
  add_recipe(recipe.fit.last) %>%
  add_model(model.fit.last)

fit.model.last <-workflow.fit.last %>%
  fit(data = segment.last %>% select(Monetary, `Recency Date`, Frequency))

lm.engine.fit.last <-fit.model.last%>%
  extract_fit_engine()

df.result.fit.last <-lm.engine.fit.last %>%
  augment(segment.last %>% select(Monetary, `Recency Date`, Frequency))

tidy(lm.engine.fit.last)
glance(lm.engine.fit.last)

shapiro.test(residuals(lm.engine.fit.last))
bptest(lm.engine.fit.last)
dwtest(lm.engine.fit.last)
vif(lm.engine.fit.last)

threshold.leverage.fit.last <-2 * length(coefficients(lm.engine.fit.last)) / segment.last %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

threshold.cooks.distance.fit.last <-4 / segment.last %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

print(round(threshold.leverage.fit.last, 3))
print(round(threshold.cooks.distance.fit.last, 3))

df.result.fit.last <-df.result.fit.last %>%
  mutate(
    flag.leverage = .hat > threshold.leverage.fit,
    flag.cook.distance = .cooksd > threshold.cooks.distance.fit
  ) 

df.result.fit.last %>%
  select(flag.cook.distance) %>%
  filter(flag.cook.distance == T)

segment.final <-df.result.fit.last %>%
  filter(!flag.cook.distance == T) %>%
  select(`Recency Date`, Frequency, Monetary)

recipe.fit.final <-recipe(Monetary ~ `Recency Date` + Frequency, data = segment.final) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_BoxCox(all_outcomes())

prep.model.final <-recipe.fit.final %>%
  prep(training = segment)

model.fit.final <-linear_reg() %>%
  set_engine('lm')

workflow.fit.final <-workflow() %>%
  add_recipe(recipe.fit.final) %>%
  add_model(model.fit.final)

fit.model.final <-workflow.fit.final %>%
  fit(data = segment.final %>% select(Monetary, `Recency Date`, Frequency))

lm.engine.fit.final <-fit.model.final%>%
  extract_fit_engine()

df.result.fit.final <-lm.engine.fit.final %>%
  augment(segment.final %>% select(Monetary, `Recency Date`, Frequency))

tidy(lm.engine.fit.final)
glance(lm.engine.fit.final)

shapiro.test(residuals(lm.engine.fit.final))
bptest(lm.engine.fit.final)
dwtest(lm.engine.fit.final)
vif(lm.engine.fit.final)

threshold.leverage.fit.final <-2 * length(coefficients(lm.engine.fit.final)) / segment.final %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

threshold.cooks.distance.fit.final <-4 / segment.final %>%
  select(`Recency Date`, Frequency, Monetary) %>%
  summarise(n = n()) %>%
  pull(n)

print(round(threshold.leverage.fit.final, 3))
print(round(threshold.cooks.distance.fit.final, 3))

df.result.fit.final <-df.result.fit.final %>%
  mutate(
    flag.leverage = .hat > threshold.leverage.fit,
    flag.cook.distance = .cooksd > threshold.cooks.distance.fit
  ) 

df.result.fit.final %>%
  select(flag.cook.distance) %>%
  filter(flag.cook.distance == T)

coeftest(
  x = lm.engine.fit.final,
  vcov. = vcovHC(lm.engine.fit.final, type = 'HC2')
)

recipe.fit.final$steps %>%
  map_lgl(function(x) inherits(x, 'step_BoxCox')) %>%
  which()

lambda <-tidy(prep.model.final, number = 2) %>%
  select(value) %>%
  pull(value)

segment.final <-df.result.fit.final %>%
  select(Monetary, `Recency Date`, Frequency, .fitted) %>%
  mutate(
    Mresidual = BoxCox(Monetary, lambda = lambda) - .fitted,
    `Monetary Predict` = InvBoxCox(x = .fitted, lambda = lambda),
    Residual = Monetary - `Monetary Predict`
  ) 

print(segment.final)

corrplot(cor(segment.final %>% 
               select(`Recency Date`, Frequency, Mresidual)), method = 'number')

recipe.clustering <-recipe(~ `Recency Date` + Frequency + Mresidual, data = segment.final) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())

prep.recipe.clust <-recipe.clustering %>%
  prep(training = segment.final)

baked.clustering <-bake(prep.recipe.clust, new_data = NULL)

fold <-vfold_cv(data = segment.final, v = 5)

tune <-function(k) {
  spec <-k_means(num_clusters = k) %>%
    set_engine('stats')
  
  workflow.clustering <-workflow() %>%
    add_recipe(recipe.clustering) %>%
    add_model(spec)
  
  kmeans.fit <-fit(workflow.clustering, data = segment.final)
  metric <-glance(kmeans.fit)

  sil.table <-silhouette_avg(object = kmeans.fit, dists = dist(segment.final))
  
  tibble(
    k = k,
    sil = sil.table,
    tot.withinss = metric$tot.withinss,
    betweenss = metric$betweenss,
    totss = metric$totss,
    fit = list(kmeans.fit),
    rasio = betweenss / totss
  )
}

k.result <-map_dfr(1:10, tune)

k.result %>% mutate(rasio = betweenss / totss)

k.result %>%
  ggplot(aes(x = k, y = tot.withinss)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal()

k.result %>%
  ggplot(aes(x = k, y = betweenss / totss)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal()

cluster.model <-hier_clust(
  num_clusters = 5,
  linkage_method = 'ward.D2'
) %>%
  set_engine('stats')

workflow.cluster <-workflow() %>%
  add_recipe(recipe.clustering) %>%
  add_model(cluster.model)

fit.cluster <-workflow.cluster %>%
  fit(data =  segment.final)

cluster <-fit.cluster %>%
  predict(segment.final) %>%
  pull(.pred_cluster)

cluster.result <-segment.final %>%
  mutate(Cluster = cluster)

print(cluster.result)

fviz_cluster(
  list(data = segment.final, cluster = cluster.result$Cluster),
  palette = 'jco',
  main = NULL
) + theme_minimal()

median.recency <-segment.final %>%
  summarise(median.r = median(`Recency Date`)) %>%
  pull(median.r)

median.frequency <-segment.final %>%
  summarise(median.f = median(Frequency)) %>%
  pull(median.f)

median.mresidual <-segment.final %>%
  summarise(median.mr = median(Mresidual)) %>%
  pull(median.mr)

cluster.result <-segment.final %>%
  mutate(
    r.category = if_else(`Recency Date` <= median.recency, 'low', 'high'),
    f.category = if_else(Frequency <= median.frequency, 'low', 'high'),
    mr.category = case_when(
      Mresidual <= quantile(x = Mresidual, probs = 0.25) ~ 'low',
      Mresidual >= quantile(x = Mresidual, probs = 0.75) ~ 'high',
      T ~ 'med'
    ),
    
    Cluster = cluster,
    
    Segmentation = case_when(
      r.category == 'low' & f.category == 'high' & mr.category == 'high' ~ 'Champions',
      r.category == 'low' & f.category == 'high' & mr.category == 'med' ~ 'Loyal Customers',
      r.category == 'low' & f.category == 'low' & mr.category == 'high' ~ 'Potential Loyal',
      r.category == 'low' & f.category == 'low' & mr.category %in% c('med', 'high') ~ 'New Customers',
      `Recency Date` < quantile(x = `Recency Date`, probs = 0.6) & Frequency > quantile(x = Frequency, 
          probs = 0.4) & mr.category == 'high' ~ 'Promising',
      `Recency Date` < quantile(x = `Recency Date`, 0.6) & Frequency > quantile(x = Frequency, 
          probs = 0.4) & mr.category == 'low' ~ 'Need Attention',
      r.category == 'high' & f.category == 'high' & mr.category == 'high' ~ 'Cannot Lose Them',
      r.category == 'high' & f.category == 'high' & mr.category == 'low' ~ 'At Risk',
      r.category == 'high' & f.category == 'low' & mr.category == 'med' ~ 'About To Sleep',
      r.category == 'high' & f.category == 'high' & mr.category == 'low' ~ 'Hibernating',
      `Recency Date` > quantile(x = `Recency Date`, probs = 0.75) & Frequency < quantile(x = Frequency,
          probs = 0.25) ~ 'Lost',
      T ~ 'Others'
    )
  ) %>%
  select(-c(.fitted, Mresidual, `Monetary Predict`, Residual, r.category, f.category, mr.category)) 

ggplot(
  data = cluster.result %>%
    count(Cluster, Segmentation),
  aes(x = Cluster, y = Segmentation, fill = n)
) + geom_tile(
  color = '#ffffff'
) + geom_text(
  aes(label = n),
  color = '#ffffff'
) + scale_fill_gradient(
  low = 'steelblue',
  high = 'darkred'
) + theme(
  panel.background = element_rect(fill = '#232F72'),
  plot.background = element_rect(fill = '#232F72'),
  axis.title.x = element_text(color = '#ffffff'),
  axis.title.y = element_text(color = '#ffffff'),
  axis.text.x = element_text(color = '#ffffff'),
  axis.text.y = element_text(color = '#ffffff')
)








































