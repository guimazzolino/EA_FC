###########=========== GUILHERME MAZZOLINO ================######################

# Carregamento dos pacotes que serão utilizados ao longo de todo o trabalho
library(tidyverse)
library(readxl)
library(ggplot2)
##### ===== Leitura e Limpeza da Base de Dados ====
# Em um primeiro momento, é importante a importação da base, que neste caso se encontra em meu github:
base <- read.csv("https://raw.githubusercontent.com/guimazzolino/EA_FC/main/all_players.csv") %>% 
  select(-1, -`URL`) %>%  # Retiro as linhas que não serão utilizadas
  unique()        # Tratamento da base, retirando possíveis linhas duplicadas
  

# A primeira limpeza será visualizar o dataframe e observar se todos os times possuem o mínimo necessário de 16 jogadores:
# 16 pois os clubes iniciam os jogos com até 11 jogadores e possuem 5 possibilidades de substituir os jogadores!
times_m16 = base %>% 
  group_by(`Club`) %>% 
  summarise( Total = n()<16) %>%
  filter(Total == T)


# Além de ter 16 jogadores, um deles deve ser o goleiro:
times_sGK = base %>% 
  mutate(Tem_gk = if_else(`Position` == "GK", 1,0)) %>% 
  group_by(`Club`,`Tem_gk`) %>% 
  summarise(Total = n()) %>% 
  filter(`Tem_gk` ==1 & Total ==0)


# Como podemos perceber a partir do DataFrame acima, não há times que não possuam goleiros e, portanto, retirarei o objeto para não ocupar memória:
rm(times_sGK)

# Por fim, faz-se necessário retirar a base "times_m16" da base que será utilizada no trabalho:
base <- base %>% 
  filter(! base$`Club` %in% times_m16$Club)

summary(base)

##### ===== Análise Exploratória dos Dados ====
# De maneira geral, se fizermos a média do "Overall" dos jogadores dos times, obteremos os "Melhores times do jogo":
GER_TEAM <- base %>% 
  group_by(`Club`) %>% 
  summarise(GER_TEAM = mean(`Overall`)) %>% 
  ungroup() %>% 
  top_n(10, wt = GER_TEAM)

# A visualização gráfico do gráfico em barra é dada por:
png(filename = "Overall_Clube.png")
p = ggplot(GER_TEAM, aes(x = reorder(Club, GER_TEAM), y = GER_TEAM)) +
  geom_bar(stat = "identity", fill = "coral3", width = 0.5) +
  coord_flip()+
  labs(title = "Média de Overall por Clube",
       x = "Clube",
       y = "Média Overall")

p + geom_text(aes(label = round(GER_TEAM, 1)), vjust = 0.4, color = "black")
dev.off()


# Agora, faremos uma maior exploração das variáveis: como uma impacta em outra, através de Regressões Lineares:

# Por fim, 
teste = c("Age","Pace","Shooting","Passing","Dribbling","Defending","Physicality","Vision")

for (i in teste){
  coeff = cor.test(base$Overall, base[[i]])
  print(paste("Correlation with", i, ":", coeff$estimate))
  }





































