library(tidyverse)
library(Formula)
library(performance)
library(see)
library(patchwork)
library(oaxaca)

dados <- readxl::read_excel('base_cr.xlsx')

#DESCRITIVAS 

dados <- dados %>% 
  mutate(Curso_2 = ifelse(Curso == "ENGENHARIA DE COMPUTAÇÃO"|
                            Curso=="ENGENHARIA MECATRÔNICA"|
                            Curso=="ENGENHARIA MECÂNICA", "ENGENHARIAS", Curso)) %>% 
  mutate(d_gen = ifelse(Sexo == "F", "1", "0")) %>% 
  mutate(CR = as.numeric(CR)) %>% 
  mutate(d_eco = ifelse(Curso=="CIÊNCIAS ECONÔMICAS",1,0)) %>% 
  mutate(d_dir = ifelse(Curso=="DIREITO",1,0)) %>% 
  mutate(d_eng_mec = ifelse(Curso=="ENGENHARIA MECÂNICA",1,0)) %>% 
  mutate(d_eng_mecat = ifelse(Curso=="ENGENHARIA MECATRÔNICA",1,0)) %>% 
  mutate(d_eng_comp = ifelse(Curso=="ENGENHARIA DE COMPUTAÇÃO",1,0))

dados %>% 
  filter(Periodo == 2) %>% 
  ggplot()+
  geom_density(aes(x = as.numeric(CR), fill = Curso_2)) 


#HISTOGRAMAS DE CR POR CURSO

dados %>% 
  filter(Periodo==2 & Curso == "ADMINISTRAÇÃO" | Curso == "CIÊNCIAS ECONÔMICAS") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), color = Curso))

dados %>% 
  filter(Periodo==2 & Curso == "ENGENHARIA MECÂNICA" | Curso == "ENGENHARIA DE COMPUTAÇÃO" |
           Curso == "ENGENHARIA MECATRÔNICA") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), color = Curso))

dados %>% 
  filter(Periodo==2 & Curso == "DIREITO") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR)))

#DISTRIBUIÇÃO DE GÊNERO ENTRE CURSOS

dados %>% 
  filter(Curso=="ADMINISTRAÇÃO") %>% 
  ggplot()+
  geom_bar(aes(x = Sexo, fill = Sexo))

#CONTAGEM E FREQUÊNCIA DE GÊNERO POR CURSO

dados %>% 
  filter(Curso=="ADMINISTRAÇÃO") %>% 
  ggplot()+
  geom_bar(aes(x = Sexo, fill = Sexo))

freq_adm = table(dados$Sexo[dados$Curso=="ADMINISTRAÇÃO"])
prop.table(freq_adm)

dados %>%  
  filter(Curso == "CIÊNCIAS ECONÔMICAS") %>% 
  ggplot()+
  geom_bar(aes(x = Sexo, fill = Sexo))

freq_eco <- table(dados$Sexo[dados$Curso=="CIÊNCIAS ECONÔMICAS"])
prop.table(freq_eco)

dados %>% 
  filter(Curso_2=="ENGENHARIAS") %>% 
  ggplot()+
  geom_bar(aes(Sexo, fill = Sexo))

freq_eng <- table(dados$Sexo[dados$Curso_2=="ENGENHARIAS"])
prop.table(freq_eng)

dados %>% 
  filter(Curso=="DIREITO") %>% 
  ggplot()+
  geom_bar(aes(x = Sexo, fill = Sexo))

freq_dir <- table(dados$Sexo[dados$Curso=="DIREITO"])
prop.table(freq_dir)

#CR POR SEXO EM CADA CURSO 

#ADM
dados %>% 
  filter(Curso == "ADMINISTRAÇÃO") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5))

#ECO
dados %>% 
  filter(Curso == "CIÊNCIAS ECONÔMICAS") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5))

#DIR
dados %>% 
  filter(Curso == "DIREITO") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5))

#COMP
dados %>% 
  filter(Curso == "ENGENHARIA DE COMPUTAÇÃO") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5))

#MECAT
dados %>% 
  filter(Curso == "ENGENHARIA MECATRÔNICA") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5))

#MEC
dados %>% 
  filter(Curso == "ENGENHARIA MECÂNICA") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5))

#HISTOGRAMA DE RANKING POR CURSO

#ADM
dados %>% 
  filter(Curso == "ADMINISTRAÇÃO") %>% 
  ggplot()+
  geom_density(aes(as.numeric(Ranking), fill = Sexo, alpha = 0.5))

dados %>% 
  filter(Curso == "ADMINISTRAÇÃO" & as.numeric(CR) >= quantile(as.numeric(CR), 0.1)) %>% 
  ggplot()+
  geom_bar(aes(x = Sexo, fill = Sexo))

freq_10_adm <- table(dados$Sexo[dados$Curso=="ADMINISTRAÇÃO" & 
                                  dados$CR>= quantile(dados$CR, 0.1)])
prop.table(freq_10_adm)

#ECO
dados %>% 
  filter(Curso == "CIÊNCIAS ECONÔMICAS" & CR >= quantile(CR, 0.1)) %>% 
  ggplot()+
  geom_bar(aes(x = Sexo, fill = Sexo))

freq_10_eco <- table(dados$Sexo[dados$Curso=="CIÊNCIAS ECONÔMICAS" & 
                                  dados$CR>= quantile(dados$CR, 0.1)])
prop.table(freq_10_eco)

#ENGENHARIAS
dados %>% 
  filter(Curso_2 == "CIÊNCIAS ECONÔMICAS" & CR >= quantile(CR, 0.1)) %>% 
  ggplot()+
  geom_bar(aes(x = Sexo, fill = Sexo))

freq_10_eng <- table(dados$Sexo[dados$Curso_2=="CIÊNCIAS ECONÔMICAS" & 
                                  dados$CR>= quantile(dados$CR, 0.1)])
prop.table(freq_10_eng)

#DIREITO 
dados %>% 
  filter(Curso == "DIREITO" & CR >= quantile(CR, 0.1)) %>% 
  ggplot()+
  geom_bar(aes(x = Sexo, fill = Sexo))

freq_10_dir <- table(dados$Sexo[dados$Curso=="DIREITO" & 
                                  dados$CR>= quantile(dados$CR, 0.1)])
prop.table(freq_10_dir)

#EVASÃO PRIMEIRO ANO VS ÚLTIMO

ev_adm_p = prop.table(table(dados$Sexo[dados$Periodo==1|dados$Periodo==2 & dados$Curso=="ADMINISTRAÇÃO"]))
ev_adm_p = as.data.frame(ev_adm_p)
colnames(ev_adm_p) = c("Sexo", "Porcentagem")

ev_adm_p %>% 
  ggplot()+
  geom_col(aes(x = Sexo, y = Porcentagem, fill = Sexo))+
  scale_fill_hue(l=55, c=95)+
  labs(x = "",
       y = "",
       title = "Administração - Primeiro Ano")+
  scale_y_continuous(n.breaks = 10, labels = scales::percent)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

ev_adm_u = prop.table(table(dados$Sexo[dados$Periodo==7|dados$Periodo==8 & dados$Curso=="ADMINISTRAÇÃO"]))
ev_adm_u = as.data.frame(ev_adm_u)
colnames(ev_adm_u) = c("Sexo", "Porcentagem")

ev_adm_u %>% 
  ggplot()+
  geom_col(aes(x = Sexo, y = Porcentagem, fill = Sexo))+
  scale_fill_hue(l=55, c=95)+
  labs(x = "",
       y = "",
       title = "Administração - Último Ano")+
  scale_y_continuous(n.breaks = 10, labels = scales::percent)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

#CR POR SEXO E SEXO

#ADM 

dados %>% 
  filter(Curso=="ADMINISTRAÇÃO") %>% 
  ggplot()+
  geom_boxplot(aes(x = Sexo, y = CR, fill = Sexo))+
  scale_fill_hue(l=55, c=95)+
  labs(title = "Administração")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))+
  coord_flip()

#ECO

dados %>% 
  filter(Curso=="CIÊNCIAS ECONÔMICAS") %>% 
  ggplot()+
  geom_boxplot(aes(x = Sexo, y = CR, fill = Sexo))+
  scale_fill_hue(l=55, c=95)+
  labs(title = "Economia")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))+
  coord_flip()

#DIREITO

dados %>% 
  filter(Curso=="DIREITO") %>% 
  ggplot()+
  geom_boxplot(aes(x = Sexo, y = CR, fill = Sexo))+
  scale_fill_hue(l=55, c=95)+
  labs(title = "Direito")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))+
  coord_flip()

#ENGENHARIA

dados %>% 
  filter(Curso_2=="ENGENHARIAS") %>% 
  ggplot()+
  geom_boxplot(aes(x = Sexo, y = CR, fill = Sexo))+
  scale_fill_hue(l=55, c=95)+
  labs(title = "Engenharias")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))+
  coord_flip()

#TABELAS DE DESEMPENHO

#ADM

desempenho_adm <- dados %>% 
  filter(Curso == "ADMINISTRAÇÃO") %>% 
  group_by(Sexo) %>% 
  summarise(media = mean(CR),
            mediana = median(CR),
            desvio = sd(CR),
            minimo = min(CR),
            maximo = max(CR))

#ECO

desempenho_eco <- dados %>% 
  filter(Curso == "CIÊNCIAS ECONÔMICAS") %>% 
  group_by(Sexo) %>% 
  summarise(media = mean(CR),
            mediana = median(CR),
            desvio = sd(CR),
            minimo = min(CR),
            maximo = max(CR))

#DIREITO

desempenho_dir <- dados %>% 
  filter(Curso == "DIREITO") %>% 
  group_by(Sexo) %>% 
  summarise(media = mean(CR),
            mediana = median(CR),
            desvio = sd(CR),
            minimo = min(CR),
            maximo = max(CR))

#ENGENHARIA

desempenho_eng <- dados %>% 
  filter(Curso_2 == "ENGENHARIAS") %>% 
  group_by(Sexo) %>% 
  summarise(media = mean(CR),
            mediana = median(CR),
            desvio = sd(CR),
            minimo = min(CR),
            maximo = max(CR))

#TESTES DE HIPÓTESE

#ADM

var.test(dados$CR[dados$Sexo=="M" & dados$Curso=="ADMINISTRAÇÃO"],
         dados$CR[dados$Sexo=="F" & dados$Curso=="ADMINISTRAÇÃO"])
t.test(dados$CR[dados$Sexo=="M" & dados$Curso=="ADMINISTRAÇÃO"],
       dados$CR[dados$Sexo=="F" & dados$Curso=="ADMINISTRAÇÃO"],
       var.equal = T)

#ECO

var.test(dados$CR[dados$Sexo=="M" & dados$Curso=="CIÊNCIAS ECONÔMICAS"],
         dados$CR[dados$Sexo=="F" & dados$Curso=="CIÊNCIAS ECONÔMICAS"])
t.test(dados$CR[dados$Sexo=="M" & dados$Curso=="CIÊNCIAS ECONÔMICAS"],
       dados$CR[dados$Sexo=="F" & dados$Curso=="CIÊNCIAS ECONÔMICAS"],
       var.equal = T)

#DIREITO

var.test(dados$CR[dados$Sexo=="M" & dados$Curso=="DIREITO"],
         dados$CR[dados$Sexo=="F" & dados$Curso=="DIREITO"])
t.test(dados$CR[dados$Sexo=="M" & dados$Curso=="DIREITO"],
       dados$CR[dados$Sexo=="F" & dados$Curso=="DIREITO"],
       var.equal = F)

#ENGENHARIA

var.test(dados$CR[dados$Sexo=="M" & dados$Curso_2=="ENGENHARIAS"],
         dados$CR[dados$Sexo=="F" & dados$Curso_2=="ENGENHARIAS"])
t.test(dados$CR[dados$Sexo=="M" & dados$Curso_2=="ENGENHARIAS"],
       dados$CR[dados$Sexo=="F" & dados$Curso_2=="ENGENHARIAS"],
       var.equal = T)

#REGRESSÃO

modelo <- lm(log(CR)~d_gen+Periodo+factor(Curso),
             data = dados)
summary(modelo)

check_model(modelo)

stargazer::stargazer(modelo)
stargazer::stargazer(modelo, type = "html")

#OAXACA 

oaxaca <- oaxaca(formula = log(CR)~Periodo+d_eco+
                   d_dir+d_eng_mec+
                   d_eng_mecat+d_eng_comp|d_gen|
                   d_eco+
                   d_dir+d_eng_mec+
                   d_eng_mecat+d_eng_comp, data = dados,
                 R = 1000)

plot(oaxaca, components = c("endowments","coefficients"))
