

#######################


# Descriptives for household


######################

library (dplyr)
library (ggplot2)

setwd("C:/Users/Daniela/OneDrive/Documentos/UvT/Second year/Thesis/Data bases")

source ("code_FULL_panel.R")

#people living in the same household 

 
t_personas_long <- full_panel %>%
  select(t_personas_2010, t_personas_2013, t_personas_2016) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Wave",
    values_to = "People"
  ) %>%
  mutate(
    Wave = recode(
      Wave,
      t_personas_2010 = "2010",
      t_personas_2013 = "2013",
      t_personas_2016 = "2016"
    )
  )


people_table <- t_personas_long %>%
  group_by(Wave) %>%
  summarise(
    Mean   = mean(People, na.rm = TRUE),
    Median = median(People, na.rm = TRUE),
    SD     = sd(People, na.rm = TRUE),
    IQR    = IQR(People, na.rm = TRUE),
    Min    = min(People, na.rm = TRUE),
    Max    = max(People, na.rm = TRUE)
  )

people_table


#visualization
ggplot(t_personas_long, aes(x = Wave, y = People)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  labs(
    x = "Wave",
    y = "# People",
    title = "# people per household by wave"
  ) +
  theme_minimal()




 #type of household 


type_household_long <- full_panel %>%
  select(tipo_vivienda_2010, tipo_vivienda_2013, tipo_vivienda_2016) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Wave",
    values_to = "Household"
  ) %>%
  mutate(
    Wave = recode(
      Wave,
      tipo_vivienda_2010 = "2010",
      tipo_vivienda_2013 = "2013",
      tipo_vivienda_2016 = "2016"
    )
  )

#visualization 

type_household_table <- type_household_long %>%
  group_by(Wave) %>%
  count(Household, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N)))


ggplot(type_household_table, aes(x = Household, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Household",
    y = "Frequency",
    title = "Type of household frequencies by wave",
    fill = "Wave"
  ) +
  theme_minimal()



#owner of household 

type_ownership_long <- full_panel %>%
  select(tenencia_vivienda_2010, tenencia_vivienda_2013, tenencia_vivienda_2016) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Wave",
    values_to = "Ownership"
  ) %>%
  mutate(
    Wave = recode(
      Wave,
      tenencia_vivienda_2010 = "2010",
      tenencia_vivienda_2013 = "2013",
      tenencia_vivienda_2016 = "2016"
    )
  )

type_ownership_table <- type_ownership_long %>%
  group_by(Wave) %>%
  count(Ownership, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N)))

#visualization 

ggplot(type_ownership_table, aes(x = Ownership, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Ownership",
    y = "Frequency",
    title = "Ownership frequencies by wave",
    fill = "Wave"
  ) +
  theme_minimal()





# debts (Y/N)

debts_long <- full_panel %>%
  select(tienen_deudas_2010, tienen_deudas_2013, tienen_deudas_2016) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Wave",
    values_to = "Debts"
  ) %>%
  mutate(
    Wave = recode(
      Wave,
      tienen_deudas_2010 = "2010",
      tienen_deudas_2013 = "2013",
      tienen_deudas_2016 = "2016"
    )
  )

debts_table <- debts_long %>%
  group_by(Wave) %>%
  count(Debts, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N)))


#visualization 

ggplot(debts_table, aes(x = Debts, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Debts",
    y = "Frequency",
    title = "# of families with debts by wave",
    fill = "Wave"
  ) +
  theme_minimal()








