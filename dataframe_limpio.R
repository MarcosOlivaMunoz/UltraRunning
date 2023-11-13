library(tidyverse, warn.conflicts = FALSE)
library(ggcorrplot)

load(url("https://www.causeweb.org/tshs/datasets/ultrarunning.RData"))

ultrarunning %>% as_tibble()
ultrarunning <- ultrarunning %>% as_tibble() %>% mutate(sex = factor(sex,
  levels=c(1, 2), labels=c("Male", "Female"))) %>% mutate(pb_surface =
  factor(pb_surface,  levels=c(1, 2, 3, 4), labels=c("Trail", "Track", "Road",
  "Mixed"))) %>% na.omit(); ultrarunning

correlacion <- function(dataframe) {
  dataframe %>% select(c(1,4,6:10)) %>% na.omit(.) %>% cor(.) %>% 
  ggcorrplot(., hc.order = TRUE, type = "lower", 
  colors = c("#FF0000", "#FFFFFF", "#00FF00"))
}

correlacion(ultrarunning)

#================================================

ultrarunning1 <- ultrarunning %>% mutate(teique_sf = ordered(cut(.$teique_sf, 3),labels=c("Bajo","Medio","Alto")))
ultrarunning2 <- ultrarunning %>% mutate(steu_b = ordered(cut(.$steu_b, 3),labels=c("Bajo","Medio","Alto")))
ultrarunning3 <- ultrarunning %>% mutate(stem_b = ordered(cut(.$stem_b, 3),labels=c("Bajo","Medio","Alto")))
ultrarunning_tot <- ultrarunning  %>% mutate(teique_sf = ordered(cut(.$teique_sf, 3),labels=c("Bajo","Medio","Alto"))) %>% mutate(steu_b = ordered(cut(.$steu_b, 3),labels=c("Bajo","Medio","Alto"))) %>% mutate(stem_b = ordered(cut(.$stem_b, 3),labels=c("Bajo","Medio","Alto")))
ultrarunning_cuantitativas <- ultrarunning_tot%>%select(1,4,6,7) %>%na.omit

df1 <- ultrarunning1 %>% filter(teique_sf == "Bajo") %>% select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>% add_row(ultrarunning1 %>% filter(teique_sf == "Medio") %>%
  select(where(is.numeric)) %>% summarise(across(everything(), mean))) %>%add_row(ultrarunning1 %>%
  filter(teique_sf == "Alto") %>% select(where(is.numeric)) %>% summarise(across(everything(), mean)))
df2 <- ultrarunning2 %>% filter(steu_b == "Bajo") %>% select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>% add_row(ultrarunning2 %>% filter(steu_b == "Medio") %>%
  select(where(is.numeric)) %>% summarise(across(everything(), mean))) %>%add_row(ultrarunning2 %>%
  filter(steu_b == "Alto") %>% select(where(is.numeric)) %>% summarise(across(everything(), mean)))
df3 <- ultrarunning3 %>% filter(stem_b == "Bajo") %>% select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>% add_row(ultrarunning3 %>% filter(stem_b == "Medio") %>%
  select(where(is.numeric)) %>% summarise(across(everything(), mean))) %>%add_row(ultrarunning3 %>%
  filter(stem_b == "Alto") %>% select(where(is.numeric)) %>% summarise(across(everything(), mean)))


ultrarunning_cuantitativas <- ultrarunning_tot%>%select(1,4,6,7) %>%na.omit
library(mvnormtest)
# Usaremos el test Shapiro-Wilk para ver si sigue una distribución normal multivariante
x=t(ultrarunning_cuantitativas)
mshapiro.test(x)
#Observamos que no pasa el test pues el p-valor es de 9.742e-06 y por tanto no pasa el test y no sigue una normal multivariante.

qqPlot(x[,1],main="age")
qqPlot(x[,2],main = "pb_elev")
qqPlot(x[,3],main = "pb100k_dec")
qqPlot(x[,4],main = "avg_km")

