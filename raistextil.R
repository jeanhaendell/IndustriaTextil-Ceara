library(tidyverse)
rais <- readxl::read_xlsx("RAIS.xlsx")

##Aplicando um filtro para focar apenas na atividade econômica "produtos têxteis"
RAIS_Textil <- rais %>%
  filter(`Atividade Econômica` == "Produtos Têxteis")

##Respondendo as perguntas

#1-Em quais municípios a indústria de confecção mais emprega no estado do Ceará?
RAIS_Textil %>% 
  group_by(Município) %>% 
  summarize(total_de_empregos = sum(`Número de empregos`, na.rm = T)) %>% 
  arrange(desc(total_de_empregos)) %>% 
  slice(1:5) %>% 
  ggplot(aes(x = reorder(Município, -total_de_empregos), y = total_de_empregos)) +
  geom_col(aes(fill = total_de_empregos), show.legend = F) +
  scale_fill_gradient(low = "royalblue", high = "grey9" ) +
  labs(title = "Número de empregos na indústria de confecção",
       subtitle = "Para municípios do estado do Ceará",
       x = "Município",
       y = "Total de empregos",
       caption = "Fonte = RAIS") +
  theme_bw()

#2-Qual a média salarial e número de empregados da indústria nestes municípios?
RAIS_Textil %>% 
  group_by(Município) %>% 
  summarize(total_de_empregos = sum(`Número de empregos`, na.rm = T),
            media_salarial = mean(`Massa salarial`)) %>% 
  arrange(desc(total_de_empregos)) %>% 
  slice(1:5) %>% 
  ggplot(aes(x = reorder(Município, -media_salarial), y = total_de_empregos)) +
  geom_col(aes(fill = media_salarial)) +
  scale_fill_gradient(low = "lightskyblue", high = "black") +
  labs(title = "Número de empregos e média salarial",
       subtitle = "Para municípios do Estado do Ceará",
       x = "Município",
       y = "Número de empregos",
       fill = "Média salarial") +
  theme_bw()

#3-Quais as principais ocupações empregadas no setor e qual a média salarial delas?
RAIS_Textil %>% 
  group_by(Ocupação) %>% 
  summarize(total_de_empregos = n(),
            media_salarial = mean(`Massa salarial`)) %>% 
  arrange(desc(total_de_empregos)) %>% 
  slice(1:5) %>%
  ggplot(aes(x = total_de_empregos, y = reorder(Ocupação, -media_salarial))) +
  geom_col(aes(fill = media_salarial)) +
  scale_fill_gradient(low = "lightskyblue", high = "black") +
  labs(title = "Número de empregos e média salarial",
       subtitle = "Por ocupação na Indústria Têxtil do Estado do Ceará",
       x = "Número de empregos",
       y = NULL,
       fill = "Média salarial") +
  theme_bw()

#4-O setor emprega mais homens ou mulheres e qual a média salarial deles?
RAIS_Textil %>% 
  group_by(`Sexo Trabalhador`) %>% 
  summarize(total_de_empregos = n(),
            media_salarial = as.integer(mean(`Massa salarial`))) %>% 
  arrange(desc(total_de_empregos)) %>% 
  slice(1:5) %>% 
  ggplot(aes(x = `Sexo Trabalhador`, y = total_de_empregos, fill = `Sexo Trabalhador`)) +
  geom_col( width = 0.3, show.legend = F) +
  annotate("text", x = 1.06, y = 1500, label = "média salarial = 3.030", size = 4.3)+
  annotate("text", x = 2, y = 3800, label = "média salarial = 4.715", size = 4.3 ) +
  labs(title = "Número de Empregos e Média Salarial",
       subtitle = "Por sexo na Indústria Têxtil do Estado do Ceará",
       x = "Sexo do Trabalhador",
       y = "Total de empregos",
       fill = NULL) +
  theme_bw()

#5-Qual o nível de escolaridade dos empregados neste setor?
RAIS_Textil %>% 
  group_by(Escolaridade) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total)) %>% 
  ggplot(aes(x = total, y = reorder(Escolaridade, -total), fill= Escolaridade)) + 
  geom_col(show.legend = F) +
  labs(title = "Nível de Escolaridade De Empregados",
       subtitle = "Na Indústria Têxtil do Estado do Ceará",
       x = "Total de empregados",
       y = "Escolaridade",
       caption = "Fonte = RAIS") +
  theme_bw()