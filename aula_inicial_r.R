##Aula Ferramentas Computacionais 
###Luana Araujo Zutin - Aula 30/10



x = c(1,2,3)
y = c(2, 4, 6)
plot(x,y)

getwd()

dados <- read.csv("C:/Users/moise/Desktop/UNESP/MESTRADO/Disciplinas/FCM/R/FCM/Pokemon_full.csv")

head(dados) # mostra as primeiras linhas
tail(dados,12) # mostra as ultimas linhas

View(dados)

library(tidyverse)

names(dados)

# seleciona colunas
select(dados, name, hp, speed, attack)

# filtra colunas
filter(dados, attack < 50)

# operações

mutate(dados, x = attack+speed) # cria nova variável
mutate(dados, attack = attack/2) # modifica variável
mutate(dados, IMC = weight/(height*height))

dados <- mutate(dados, IMC = weight/(height*height)) # modifica variável

# exemplo operador

# Sem uso de pipe [%>%] 
df <- select(dados, name, hp, attack, speed) 
df <- filter(df, attack < 50) 
df <- mutate(df, x = attack+speed) 

# Com uso de pipe [%>%], normalmente mais rapido, tudo o que esta a esquerda do pipe ele vai pegar e atribuir como primeiro argumento da função seguinte
df <- dados %>%
  select(name, hp, attack, speed) %>%
  filter(attack < 50) %>%
  mutate(x = attack+speed) 

# gsub modifica strings, ou seja, caracters  
x = c("Luana", "Moises", "Barbara")
# Se colocar ponto, o pipe rastreia e substitui. Se nao colocar, ele coloca como primeiro argumento   
x %>%
gsub("Lu","lu", .)

# l>: pipe nativo do R, mas nao substitui 


dados %>%
  filter(height > 10) %>%
  select(name, height, weight) %>%
  mutate(imc = weight/(height*height)) %>%
  ggplot()+
  geom_density(aes( x = imc))

head(dados)
dados %>% head

#comando interessante - "comando geral"
glimpse(dados)

#comandos interessantes
dados %>% pull(IMC) # retorna como vetor, em array
dados %>% select(IMC) # retorna uma coluna

mean(c(1,2,3,4))

dados %>% 
  mutate(media = mean(IMC)) # cria e preenche uma coluna com o mesmo valor

dados %>% 
  summarise(media = mean(IMC), desvio = sd(IMC)) # retorna uma unica coluna, com o valor da media. Resume os dados, retornando uma coluna para cada variavel. 

dados %>% 
  group_by(type) %>% 
  summarise(media = mean(IMC), desvio = sd(IMC)) # retorna uma unica coluna, com o valor da media. Resume os dados, retornando uma coluna para cada variavel. 

dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC)) %>% View # cria e preenche uma coluna com o mesmo valor

# Como utilizar os comandos acima com meus dados reais:
# Caso eu queira filtrar meus dados, pegando só individuos com IMC > média do grupo dele:
  dados %>% 
  group_by(type) %>% 
  mutate(media = mean(IMC)) %>%  
  filter(IMC > media) %>% View 

# Tudo o que for aplicado aqui, sera aplicado por grupo
df <- dados %>% 
    group_by(type) %>% 
    mutate(media = mean(IMC)) 

df %>% 
  mutate(media2 = mean(IMC)) %>% View 

# Como desagrupar?
df %>% 
  ungroup() %>% 
  mutate(media2 = mean(IMC)) %>% View

# Busca padrões
# aceita Regular Expression (ReGex) # pode variar de acordo com o que colocar
grep("saur|fly", dados$name) # retorna a posição que tem
grepl("saur|fly", dados$name) # retorna com True and False

# Com (ReGex), é possível procurar por um ou por outro
grep("[Ss]aur", dados$name) 

x
grep("Lua", x) 

# Com (ReGex), quando coloca . significa qualquer coisa, para encontrar de fato o ., é necessário colocar "\\."
grepl(".", c("a", "b", "c", "0", " "))

dados %>%
  filter(attack > 50)

dados$attack > 50

dados %>%
  filter(grepl("saur|fly", name), attack > 50, type != "grass")

"saur" == "ivysaur"
grepl("saur", "ivysaur")

==================================================
# Juntar dois data frames, juntando dados [juntar linhas ou juntar colunas]
  
# bind row
df1 <- dados %>%
  filter(attack > 70)

df2 <- dados%>%
  filter(attack <= 70)

rbind(df1, df2) # juntar linhas, nao aceita dimensoes e nomes diferentes

# com colunas diferentes
df1 <- dados %>%
  select(attack, speed, weight) %>%
  filter(attack > 70)

df2 <- dados%>%
  select(attack, weight, height, hp) %>%
  filter(attack <= 70)

bind_rows(df1, df2) # juntar linhas, completa se não bater 

# Juntar Colunas 
df1 <- dados %>% head(100)
df2 <- dados %>% tail(100)

cbind(df1, df2) %>% names

bind_cols(df1, df2, .nane_repair = "unique")

# ====================================================

# Fazendo join
# left, right, full, inner

df_resumo <- dados %>% 
  group_by(type) %>% 
  summarise(media = mean(IMC), desvio = sd(IMC)) 

left_join(dados, df_resumo, by = "type") %>% View
left_join(dados, df_resumo, by = "type", "secondary.type") %>% View

# left, right, full, inner: diferença caso tenha coisas faltando nos dados

df_resumo_mis <- df_resumo %>% filter(type != "grass")

left_join(dados, df_resumo_mis, by = c("type")) %>% View # tem coisas nos "dados" que nao tem no df_resumo mas ele completa. 
right_join(dados, df_resumo_mis, by = c("type")) %>% View # as coisas que estao nos "dados" são excluidas, ele mantem apenas o que tem na direita, ou seja, apenas o que tem no arquivo "df_resumo". 

# mantem tudo: full
# joga fora tudo: iner

full_join(dados, df_resumo_mis, by = c("type")) %>% View # tem coisas nos "dados" que nao tem no df_resumo mas ele completa. 
inner_join(dados, df_resumo_mis, by = c("type")) %>% View # as coisas que estao nos "dados" são excluidas, ele mantem apenas o que tem na direita, ou seja, apenas o que tem no arquivo "df_resumo". 

#===============================================================================
##Aula 21/10

library(tidyverse)
dengue_sp <- read.csv2("C:/Users/moise/Desktop/UNESP/MESTRADO/Disciplinas/FCM/R/FCM/dengue_sp.csv")
View(dengue_sp)

dengue_sp <- dengue_sp %>%
  rename(municipio =1)

dengue_sp <- dengue_sp %>%
  filter(municipio != "Total",
         municipio != "&",
         !grepl("IGNORADO", municipio) # ! significa não
  )

tail(dengue_sp)
view(dengue_sp)

## Dados do IBGE

dados_ibge <- read.csv2("tabela4709.csv", skip = 3)

View(dados_ibge)

dados_ibge <- dados_ibge %>%
  rename(Cod=1)

## Manipulando dados
# separar o codigo do municipio 

dengue_sp <- dengue_sp %>% %>%
  mutate(
    codigo = str_extract(municipio, "\\d{6}")
  )

dados_ibge %>%
  mutate(
    Cod2 = str_remove(Cod, "^\\d{6}")
  )

dados_ibge %>%
  mutate(
    Cod2 = str_remove(Cod, "\\d$")
  )

View(dados_ibge)

## Manipulando dados

glimpse(dengue_sp)

dengue_sp <- dengue_sp %>%
  mutate(across(starts_with("X"), as.integer))%>% # transforma em inteiro
  replace(is.na(.), 0) # coloca zero no lugar

dengue_sp <- dengue_sp %>%
  select(-Total) %>%
  pivot_longer(2:12, names_to = "Ano", values_to = "Casos")

dengue_sp %>%
  mutate(Ano = str_remove_all(Ano, "\\D")) %>%
  mutate(Ano = as.integer(Ano))

df_final <- dados_ibge %>%
  select(Cod = Cod2, Inhab = X2022)%>%
  inner_join(dengue_sp, by = c("Cod" = "codigo"))%>% nrow

df_final %>% filter(is.na(Inhab))

df_final <- df_final %>%
  mutate(
    incidencia = Casos/Inhab*100000
  )

df_final %>%
  filter(Casos > 600000)

ggplot(df_final)+
  geom_boxplot(aes())
  
#===============================================
##Aula 30/10
# Lendo dados DBC

library(tidyverse)
library(read.dbc)
#Não precisa carregar um pacote inteiro para usar uma vez só, usando por exemplo: nome_pacote() ja basta

devtools::install_github("danicat/read.dbc")

install.packages("devtools")
devtools::install_github("danicat/read.dbc")

#read.dbc::read.dbc()

dados <- read.dbc::read.dbc("DENGBR24.dbc")

nrow(dados)
names(dados)
glimpse(dados)

dados_sp <- dados %>%
  filter(grepl("^35", ID_MN_RESI))

nrow(dados_sp)

rm(dados) #Libera espaço
glimpse(dados_sp)
write.csv(dados_sp, "dados_dengue_sp.csv")

con <- dbConnect(duckdb::duckdb(), dbdir = "meu_banco.duckdb")

#===============================================

#Aula dia 04/11

#### Dados de mortalidade
#Criar um grafico do numero de mortes por semana de epidemiologica a partir de 2015, por estado e depois filtrar para cancer

setwd("C:/Users/moise/Desktop/UNESP/MESTRADO/Disciplinas/FCM/R/FCM/")

dados_simdo <- read.dbc::read.dbc("DOAC2015.dbc")
glimpse(dados_simdo)

dados <- dados_simdo %>%
  select(DTOBITO, CAUSABAS_O) %>%
  mutate(
    dataobito= as.Date(DTOBITO, "%d%m%Y")
  )

# Variável para saber a primeira semana, segunda semana, etc...
#### 02/03/2015 -> %d%m%Y

as.Date("2015-10-12")-as.Date("2015-10-10")

as.Date("2015-10-12")-as.Date("2015-01-01")

as.integer(as.Date("2015-10-12")-as.Date("2015-01-01"))/7

x <- c(as.Date("2015-10-12"), as.Date("2015-07-12"), as.Date("2015-05-12"))

floor(as.integer(as.Date("2015-10-12")-as.Date("2015-01-01"))/7+1)
floor(as.integer(x=as.Date("2015-01-01"))/7+1)

dados %>%
  mutate(
    semana = floor(as.integer(dataobito-as.Date("2015-01-01"))/7+1),
    diasemana = as.Date("2015-01-01")+7*(semana-1)
  )

roda_arquivo <- function(arquivo){
  dados_simdo <- read.dbc::read.dbc("arquivo")
  glimpse(dados_simdo)
  
  dados <- dados_simdo %>%
    select(DTOBITO, CAUSABAS_O) %>%
    mutate(
      dataobito= as.Date(DTOBITO, "%d%m%Y")
    )
  dados %>%
    mutate(
      semana = floor(as.integer(dataobito-as.Date("2015-01-01"))/7+1),
      diasemana = as.Date("2015-01-01")+7*(semana-1)
    )
  
}

arquivos <- list.files("DOAC2015.dbc", full.names = TRUE)

resultados <- map("DOAC2015.dbc", roda_arquivo)

resultados[[1]]



#### 02/03/2015 -> %d%m%Y


