# Stackoverflow examples --------------------------------------------------

# https://stackoverflow.com/questions/74731530/is-it-possible-to-impute-multiple-values-from-a-different-dataframe-dependent-on/74731856#74731856

# Starting dataframe
data <- tribble(
  ~ID, ~Excluded, ~colA, ~colB, ~colC, ~col_mean, ~varA, ~varB,
  "A", TRUE,      1,     1,     1,     NA,         "X",   10,
  "B", FALSE,     NA,    2,     2,     NA,        "Y",   20,
  "C", FALSE,     3,     3,     3,     3,         "NA",   30
)

# Dataframe with imputed values
data_imputed <- tribble(
  ~ID, ~Excluded, ~colA, ~colB, ~colC, ~col_mean, ~varA, ~varB,
  "B", FALSE,     2,     2,     2,     2,         "Y",   20,
  "C", FALSE,     3,     3,     3,     3,         "Z",   30
)

# Possible solution 1
left_join(data, data_imputed, by = "ID") %>%
  mutate(across(matches("^col.*.x$"), ~ coalesce(., cur_data()[[sub("\\.x$", ".y", cur_column())]]))) %>%
  select(-ends_with(".y")) %>%
  rename_with(.fn = ~ sub("\\.x$", "", .))

# Target dataframe
data_target <- tribble(
  ~ID, ~Excluded, ~colA, ~colB, ~colC, ~col_mean, ~varA, ~varB,
  "A", TRUE,      1,     1,     1,     1,         "X",   10,
  "B", FALSE,     2,     2,     2,     2,         "Y",   20,
  "C", FALSE,     3,     3,     3,     3,         "Z",   30
)
