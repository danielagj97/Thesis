
###################################


##   FULL PANEL (2010-2016)   #####


###################################


setwd("C:/Users/Daniela/OneDrive/Documentos/UvT/Second year/Thesis/Data bases")

source ("code_panel_2010.R") 
source ("code_panel_2013.R")
source ("code_panel_2016.R")


partial_panel <- inner_join (short_panel_2010, short_panel_2013, by = c("consecutivo", "llave_ID_lb", "orden", "sexo"))

#checking for duplicates
length(unique(partial_panel$consecutivo)) == nrow(partial_panel)
n_partial <- data.frame (table (partial_panel$consecutivo))
n_partial [n_partial$Freq > 1,]


full_panel <- inner_join (partial_panel, short_panel_2016, by = c("consecutivo", "llave_ID_lb", "orden", "sexo"))


#checking for duplicates
length(unique(full_panel$consecutivo)) == nrow(full_panel)
n_full <- data.frame (table (full_panel$consecutivo))
n_full [n_full$Freq > 1,]

summary (full_panel)

#SAVING FILE 
save(full_panel, file = "FULL_panel.RData")
  

