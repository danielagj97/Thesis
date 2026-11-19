
########

# community

#########


#library (dplyr)
#library (ggplot2)

#setwd("C:/Users/Daniela/OneDrive/Documentos/UvT/Second year/Thesis/Data bases")

#source ("code_FULL_panel.R")





##### INsecurity


tot_insec_long <- full_panel %>%
  select(tot_insec_2010, tot_insec_2013, tot_insec_2016) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Wave",
    values_to = "Insecurity"
  ) %>%
  mutate(
    Wave = recode(
      Wave,
      tot_insec_2010 = "2010",
      tot_insec_2013 = "2013",
      tot_insec_2016 = "2016"
    )
  )

tot_insec_table <- tot_insec_long %>%
  count(Wave, Insecurity, name = "N") %>%
  group_by(Wave) %>%
  mutate(
    Percent = round(100 * N / sum(N), 2)
  ) %>%
  ungroup()

tot_insec_table


#visualization
ggplot(tot_insec_long, aes(x = factor(Insecurity))) +
  geom_bar() +
  facet_wrap(~ Wave) +
  labs(
    x = "Count of typres of violence within the neighborhood",
    y = "Frequency",
    title = "Count of types of insecurity present in the neighborhood by wave"
  ) +
  theme_minimal()




