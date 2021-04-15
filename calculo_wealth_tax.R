######################################################################
# Simulación de recaudación con impuesto a la riqueza
#
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
######################################################################




#Abrir datos simulados de distribución de riqueza 
  #basado en información de Credit Suisse

##############
#Configuración----
rm(list = ls())

library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(haven, readr, readxl, ggplot2, shiny, tidyverse, knitr,
       foreign,expss,data.table, srvyr, dineq, datapasta, scales)

theme_g <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Verdana",
                          color = "#939486"),
      panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F5F5F3",
                                     color = NA),
      panel.background = element_rect(fill = "#F5F5F3",
                                      color = NA),
      legend.background = element_rect(fill = "#F5F5F3",
                                       color = NA),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
}

wealth_mx <-
  read_dta("~/Documents/Encuestas/Credit Suisse/riqueza.dta")

summary (wealth_mx$riqueza)

#Deciles
wealth_mx <- wealth_mx %>% 
  mutate(factor= 1000,
         decil  = dineq::ntiles.wtd(riqueza, 10, weights = factor))

est_deciles <-  wealth_mx %>% 
  as_survey(weights = c(factor)) %>%
  dplyr::group_by(decil) %>% 
  summarize(
    media = survey_mean(riqueza, na.rm = T),
    cuantil = survey_quantile(riqueza, c(0.25,0.75), na.rm = T)
  ) %>% 
  dplyr::select(-media_se , -cuantil_q25_se, -cuantil_q75_se)



#Percentiles
wealth_mx <- wealth_mx %>% 
  mutate(factor= 1000,
         percentil  = dineq::ntiles.wtd(riqueza, 100, weights = factor))
  
est_percentiles <-  wealth_mx %>% 
  as_survey(weights = c(factor)) %>%
  dplyr::group_by(percentil) %>% 
  summarize(
    media = survey_mean(riqueza, na.rm = T),
    cuantil = survey_quantile(riqueza, c(0.25,0.75), na.rm = T)
  ) %>% 
  dplyr::select(-media_se , -cuantil_q25_se, -cuantil_q75_se)

est_percentiles_2 <-  wealth_mx %>% 
  dplyr::group_by(percentil) %>% 
  summarize(
    minimo = min(riqueza, na.rm = T),
    maximo = max(riqueza, na.rm = T),
    suma = sum(riqueza, na.rm = T)
    )

estimaciones <- est_percentiles %>% 
  left_join(est_percentiles_2) 
write_csv(estimaciones,"www/DB/riqueza_percentiles.csv")

min_p99 <- estimaciones$minimo[100]
min_p91 <- estimaciones$minimo[91]

min_p94 <- estimaciones$minimo[94]
min_p98 <- estimaciones$minimo[98]

gini.wtd(wealth_mx$riqueza, weights = wealth_mx$factor)
#0.5859703


#Modelo 0 ----
modelo0 <- wealth_mx %>% 
  dplyr::mutate(
   # min_p99= min_p99,
    contribuyentes = case_when(
                      riqueza>=min_p99 ~ 1,
                      TRUE ~ 0),
    tarifa= case_when(
                      contribuyentes==1 ~ (0.9/100),
                      TRUE ~ 0),
    recaudacion= riqueza*tarifa,
    riqueza_post_tax = riqueza-recaudacion
  )

write_csv(modelo0,"www/DB/modelo0.csv")
gini.wtd(modelo0$riqueza_post_tax, weights = modelo0$factor)
#0.5859703


modelo0 %>% 
  as_survey(weights = c(factor)) %>%
  #dplyr::group_by(percentil) %>% 
  summarize(
    suma = survey_total(recaudacion, na.rm = T)
  ) 




#Modelo 1 ----
modelo1 <- wealth_mx %>% 
  dplyr::mutate(
    # min_p99= min_p99,
    contribuyentes = case_when(
      riqueza>=min_p99 ~ 1,
      TRUE ~ 0),
    tarifa= case_when(
      contribuyentes==1 ~ (1.4/100),
      TRUE ~ 0),
    recaudacion= riqueza*tarifa,
    riqueza_post_tax = riqueza-recaudacion
  )

write_csv(modelo1,"www/DB/modelo1.csv")
gini.wtd(modelo1$riqueza_post_tax, weights = modelo1$factor)
#0.5859703


modelo1 %>% 
  as_survey(weights = c(factor)) %>%
  #dplyr::group_by(percentil) %>% 
  summarize(
    suma = survey_total(recaudacion, na.rm = T)
  ) 






#Modelo 2 ----
modelo2 <- wealth_mx %>% 
  dplyr::mutate(
    # min_p99= min_p99,
    contribuyentes = case_when(
      riqueza>=min_p91 ~ 1,
      TRUE ~ 0),
    tarifa= case_when(
      contribuyentes==1 ~ (0.9/100),
      TRUE ~ 0),
    recaudacion= riqueza*tarifa,
    riqueza_post_tax = riqueza-recaudacion
  )

write_csv(modelo2,"www/DB/modelo2.csv")
gini.wtd(modelo2$riqueza_post_tax, weights = modelo2$factor)
#0.5859703


modelo2 %>% 
  as_survey(weights = c(factor)) %>%
  #dplyr::group_by(percentil) %>% 
  summarize(
    suma = survey_total(recaudacion, na.rm = T)
  ) 




#Modelo 3 ----
modelo3 <- wealth_mx %>% 
  dplyr::mutate(
    # min_p99= min_p99,
    contribuyentes = case_when(
      riqueza>=min_p91 ~ 1,
      TRUE ~ 0),
    tarifa= case_when(
      contribuyentes==1 ~ (1.4/100),
      TRUE ~ 0),
    recaudacion= riqueza*tarifa,
    riqueza_post_tax = riqueza-recaudacion
  )

write_csv(modelo3,"www/DB/modelo3.csv")
gini.wtd(modelo3$riqueza_post_tax, weights = modelo3$factor)
#0.5859703


modelo3 %>% 
  as_survey(weights = c(factor)) %>%
  #dplyr::group_by(percentil) %>% 
  summarize(
    suma = survey_total(recaudacion, na.rm = T)
  ) 





#Modelo 4 ----
modelo4 <- wealth_mx %>% 
  dplyr::mutate(
    # min_p99= min_p99,
    contribuyentes = case_when(
      riqueza>=min_p91 & riqueza<min_p94 ~ 1,
      riqueza>=min_p94 & riqueza<min_p98 ~ 2,
      riqueza>=min_p98 ~ 3,
      TRUE ~ 0),
    tarifa= case_when(
      contribuyentes==1 ~ (0.9/100),
      contribuyentes==2 ~ (1.4/100),
      contribuyentes==3 ~ (1.9/100),
      TRUE ~ 0),
    recaudacion= riqueza*tarifa,
    riqueza_post_tax = riqueza-recaudacion
  )

write_csv(modelo4,"www/DB/modelo4.csv")
gini.wtd(modelo4$riqueza_post_tax, weights = modelo4$factor)
#0.5859703


modelo4 %>% 
  as_survey(weights = c(factor)) %>%
  #dplyr::group_by(percentil) %>% 
  summarize(
    suma = survey_total(recaudacion, na.rm = T)
  ) 



modelo4 %>% 
  dplyr::group_by(contribuyentes) %>% 
  summarize(
    prom = mean(riqueza, na.rm = T)
  ) 





#Modelo 5 ----
modelo5 <- wealth_mx %>% 
  dplyr::mutate(
    # min_p99= min_p99,
    contribuyentes = case_when(
      riqueza>=min_p99 ~ 1,
      TRUE ~ 0),
    tarifa= case_when(
      contribuyentes==1 ~ (1.9/100),
      TRUE ~ 0),
    recaudacion= riqueza*tarifa,
    riqueza_post_tax = riqueza-recaudacion
  )


write_csv(modelo5,"www/DB/modelo5.csv")
gini.wtd(modelo5$riqueza_post_tax, weights = modelo5$factor)
#0.5859703


modelo5 %>% 
  as_survey(weights = c(factor)) %>%
  #dplyr::group_by(percentil) %>% 
  summarize(
    suma = survey_total(recaudacion, na.rm = T)
  ) 


#Meterle datos de los multimillonarios mexicanos
billionaires <-
tibble::tribble(
                 ~multimillonario_nombre, ~multimillonario_riqueza_billion_dollars,      ~riqueza,
                           "Carlos Slim",                                     52.1,     1.042e+12,
                "Ricardo Salinas Pliego",                                     11.7,      2.34e+11,
                         "Germán Larrea",                                       11,       2.2e+11,
                     "Alberto Bailleres",                                      6.4,      1.28e+11,
         "María Asunción Aramburuzabala",                                      5.4,      1.08e+11,
                       "Jerónimo Arango",                                      4.3,       8.6e+10,
               "Juan Francisco Beckmann",                                      4.3,       8.6e+10,
              "Familia Servitje Montull",                                      3.5,         7e+10,
                "Familia Robinson Bours",                                     2.57,      5.14e+10,
                    "Familia Achar Levy",                                     2.45,       4.9e+10,
               "Familia Espinosa Abdalá",                                     2.45,       4.9e+10,
                       "Carlos Hank Ron",                                        2,         4e+10,
              "Familia Barragán Morales",                                      1.9,       3.8e+10,
               "Familia González Moreno",                                      1.8,       3.6e+10,
               "Familia Michel González",                                      1.7,       3.4e+10,
                     "Roberto Hernández",                                      1.7,       3.4e+10,
                "Antonio Del Valle Ruiz",                                      1.6,       3.2e+10,
                 "Familia González Nova",                                     1.55,       3.1e+10,
                 "Familia Hank González",                                     1.45,       2.9e+10,
              "Cynthia y Bruce Grossman",                                      1.4,       2.8e+10,
                 "Rufino Vigil González",                                      1.2,       2.4e+10,
                     "Alfredo Harp Helu",                                      1.1,       2.2e+10,
                  "Fernando Chico Pardo",                                     0.99,      1.98e+10,
                 "Emilio Azcarraga Jean",                                     0.99,      1.98e+10,
                 "David Peñaloza Alanis",                                     0.99,      1.98e+10,
                        "Familia Losada",                                     0.97,      1.94e+10,
                 "Familia Arroyo Chávez",                                      0.9,       1.8e+10,
                "Familia Jorba Servitje",                                     0.82,      1.64e+10,
                "Familia Chedruai Obeso",                                     0.76,      1.52e+10,
  "Familia Fernández (Arca Continental)",                                      0.7,       1.4e+10,
                "Familia Martín Soberón",                                     0.51,      1.02e+10,
                "Ricardo Martín Bringas",                                      0.5,         1e+10,
                   "Luis Arizpe Jiménez",                                      0.5,         1e+10
  )

billionaires %<>% 
  dplyr::mutate(
    factor=1
  )


glimpse(wealth_mx)
tail(wealth_mx)
wealth_mx_fake<-wealth_mx
wealth_mx<-bind_rows(wealth_mx_fake,billionaires)


#Modelo 7 ----
modelo7 <- wealth_mx %>% 
  dplyr::mutate(
    # min_p99= min_p99,
    contribuyentes = case_when(
      riqueza>=20000000 ~ 1,
      TRUE ~ 0),
    tarifa= case_when(
      contribuyentes==1 ~ (1.9/100),
      TRUE ~ 0),
    recaudacion= riqueza*tarifa,
    riqueza_post_tax = riqueza-recaudacion
  )

table(modelo7$contribuyentes)

write_csv(modelo7,"www/DB/modelo7.csv")
gini.wtd(modelo7$riqueza_post_tax, weights = modelo7$factor)
#0.5859703


modelo7 %>% 
  as_survey(weights = c(factor)) %>%
  #dplyr::group_by(percentil) %>% 
  summarize(
    suma = survey_total(recaudacion, na.rm = T)
  ) 

