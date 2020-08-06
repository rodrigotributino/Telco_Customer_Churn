# Carregando alguns pacotes --- ---------------------------------------------

library(tidyverse)    # conjunto de pacotes para manipulação e visualização de dados
library(magrittr)     # pipe (%>%)
library(caret)        # Divisão treino/teste de forma proporcional, modelos
library(ROSE)         # oversample ans undersample
library(car)          # Verifica multicolinearidade entre as variáveis
library(DataExplorer) # Visualização de dados faltantes
library(gridExtra)

# Carregando dataset ---------------------------------------------------------

df <- read_csv("./Dados/datasets_Telco-Customer-Churn.csv")
View(head(df, 30))

# Dimensão dos dados
dim(df)

# Tipo de variáveis
str(df)

# alterando caracteres para fatores
df %<>% 
  mutate_if(is.character, as.factor) 

# transformando o tipo da variável SeniorCitizen para fator
df %<>% 
  mutate(SeniorCitizen = as.factor(if_else(SeniorCitizen == 1, "Yes", "No")))

# Definido os casos de Churn como referência na variável resposta
df %<>% 
  mutate(Churn = fct_relevel(Churn, 'Yes', 'No'))

levels(df$Churn)

str(df)

# Verificando se há valores faltantes no dataset
df %>% is.na() %>% sum()

# visualizando dados faltantes
plot_intro(df, ggtheme = theme_bw())
plot_missing(df, ggtheme = theme_bw(), title = "Porcentual de dados faltantes") 


# Análise Exploratória -------------------------------------------------------

# Medidas de tendência central e de dispersão das variáveis quantitativas
df %>% 
  select_if(is.numeric) %>% 
  summary()

df %>% 
  select(customerID) %>% 
  unique()

# Correlação das variáveis quantitativas

df %>%
  select_if(is.numeric) %>% 
  na.omit() %>% 
  cor() %>% 
  corrplot::corrplot(
    .,
    type = "lower",
    method = 'color',
    tl.srt = 45,
    addCoef.col = TRUE
  )

df %>% 
  select_if(is.numeric) %>% 
  na.omit() %>%
  pairs(
    col = "brown",   
    pch = 18                            
    )

# Distribuição da variável resposta

df %>% 
  ggplot() +
  geom_bar(aes(x = Churn, fill = Churn)) +
  labs(
    title = 'Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# Gênero
df %>% 
  ggplot() +
  geom_bar(aes(x = gender, fill = gender)) +
  labs(
    title = 'Gender',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# SeniorCitizen
df %>% 
  ggplot() +
  geom_bar(aes(x = SeniorCitizen, fill = SeniorCitizen)) +
  labs(
    title = 'Senior Citizen',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# Partner
df %>% 
  ggplot() +
  geom_bar(aes(x = Partner, fill = Partner)) +
  labs(
    title = 'Partner',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# Dependents
df %>% 
  ggplot() +
  geom_bar(aes(x = Dependents, fill = Dependents)) +
  labs(
    title = 'Dependents',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# tenure

grid.arrange(

  df %>% 
    ggplot() +
    geom_histogram(aes(x = tenure), col = 'black', fill = 'lightblue') +
    labs(
      x = NULL,
      y = 'Freq'
    ) +
    theme_bw(),
  
  df %>% 
    ggplot() +
    geom_boxplot(aes(y = tenure), fill = 'lightblue') +
    labs(
      x = NULL,
      y = 'Tenure'
    ) +
    theme_bw() +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x.bottom = element_blank()
      ),

  nrow = 1,
  top = "Tenure"
)

## Analisar os clientes com pouco e com muito tempo.

# PhoneService
df %>% 
  ggplot() +
  geom_bar(aes(x = PhoneService, fill = PhoneService)) +
  labs(
    title = 'Phone Service',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# MultipleLines
df %>% 
  ggplot() +
  geom_bar(aes(x = MultipleLines, fill = MultipleLines)) +
  labs(
    title = 'Multiple Lines',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# InternetService
df %>% 
  ggplot() +
  geom_bar(aes(x = InternetService, fill = InternetService)) +
  labs(
    title = 'Internet Service',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# OnlineSecurity
df %>% 
  ggplot() +
  geom_bar(aes(x = OnlineSecurity, fill = OnlineSecurity)) +
  labs(
    title = 'Online Security',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# OnlineBackup
df %>% 
  ggplot() +
  geom_bar(aes(x = OnlineBackup, fill = OnlineBackup)) +
  labs(
    title = 'Online Backup',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# DeviceProtection
df %>% 
  ggplot() +
  geom_bar(aes(x = DeviceProtection, fill = DeviceProtection)) +
  labs(
    title = 'Device Protection',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# TechSupport
df %>% 
  ggplot() +
  geom_bar(aes(x = TechSupport, fill = TechSupport)) +
  labs(
    title = 'Tech Support',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# StreamingTV 
df %>% 
  ggplot() +
  geom_bar(aes(x = StreamingTV, fill = StreamingTV)) +
  labs(
    title = 'Streaming TV',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# StreamingMovies
df %>% 
  ggplot() +
  geom_bar(aes(x = StreamingMovies, fill = StreamingMovies)) +
  labs(
    title = 'Streaming Movies',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# Contract
df %>% 
  ggplot() +
  geom_bar(aes(x = Contract, fill = Contract)) +
  labs(
    title = 'Contract',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# PaperlessBilling
df %>% 
  ggplot() +
  geom_bar(aes(x = PaperlessBilling, fill = PaperlessBilling)) +
  labs(
    title = 'Paperless Billing',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw()

# PaymentMethod
df %>% 
  ggplot() +
  geom_bar(aes(x = PaymentMethod, fill = PaymentMethod)) +
  labs(
    title = 'Payament Method',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# MonthlyCharges
grid.arrange(  
  df %>% 
    ggplot() +
    geom_histogram(aes(x = MonthlyCharges), col = 'Black', fill = 'lightblue') +
    labs(
      x = NULL,
      y = 'Freq'
    ) +
    theme_bw(),
  
  df %>% 
    ggplot() +
    geom_boxplot(aes(y = MonthlyCharges), fill = 'lightblue') +
    labs(
      x = NULL,
      y = "Monthly Charges"
    ) +
    theme_bw() +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x.bottom = element_blank()
    ),
  nrow = 1,
  top = "Monthly Charges"
)

# Qual o tempo de assinatura das pessoas que pagam mais?
# Média de preço mensal por pacotes

# TotalCharges
grid.arrange(  
  df %>% 
    ggplot() +
    geom_histogram(aes(x = TotalCharges), col = 'Black', fill = 'lightblue') +
    labs(
      x = NULL,
      y = 'Freq'
    ) +
    theme_bw(),
  
  df %>% 
    ggplot() +
    geom_boxplot(aes(y = TotalCharges), fill = 'lightblue') +
    labs(
      x = NULL,
      y = "Total Charges"
    ) +
    theme_bw() +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x.bottom = element_blank()
    ),
  nrow = 1,
  top = "TotalCharges Charges"
)

# Agora vamos analisar a distribuição das covariáveis cruzadas com a nossa 
# variável de interesse. Para isso, vamos utilizar o comando facet_wrap

# Gênero vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = gender, fill = gender)) +
  labs(
    title = 'Gender vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# SeniorCitizen vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = SeniorCitizen, fill = SeniorCitizen)) +
  labs(
    title = 'Senior Citizen vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# Partner vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = Partner, fill = Partner)) +
  labs(
    title = 'Partner vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# Dependents vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = Dependents, fill = Dependents)) +
  labs(
    title = 'Dependents',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# PhoneService vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = PhoneService, fill = PhoneService)) +
  labs(
    title = 'Phone Service vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# MultipleLines vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = MultipleLines, fill = MultipleLines)) +
  labs(
    title = 'Multiple Lines vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# InternetService vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = InternetService, fill = InternetService)) +
  labs(
    title = 'Internet Service vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# OnlineSecurity vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = OnlineSecurity, fill = OnlineSecurity)) +
  labs(
    title = 'Online Security vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# Olhar custo da Fibra ótica

# OnlineBackup vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = OnlineBackup, fill = OnlineBackup)) +
  labs(
    title = 'Online Backup vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# DeviceProtection vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = DeviceProtection, fill = DeviceProtection)) +
  labs(
    title = 'Device Protection vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# TechSupport vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = TechSupport, fill = TechSupport)) +
  labs(
    title = 'Tech Support vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# StreamingTV vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = StreamingTV, fill = StreamingTV)) +
  labs(
    title = 'StreamingTV vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# StreamingMovies vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = StreamingMovies, fill = StreamingMovies)) +
  labs(
    title = 'Streaming Movies vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# Contract vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = Contract, fill = Contract)) +
  labs(
    title = 'Contract vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# PaperlessBilling vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = PaperlessBilling, fill = PaperlessBilling)) +
  labs(
    title = 'Paperless Billing vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  facet_wrap(~Churn)

# PaymentMethod vs Churn
df %>% 
  ggplot() +
  geom_bar(aes(x = PaymentMethod, fill = PaymentMethod)) +
  labs(
    title = 'Payment Method vs Churn',
    x = NULL,
    y = 'Freq'
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Churn)


# tenure vs Churn
grid.arrange(  
  df %>% 
    ggplot() +
    geom_histogram(aes(x = tenure), col = 'black', fill = 'lightblue') +
    labs(
      title = 'Tenure vs Churn',
      x = NULL,
      y = 'Freq'
    ) +
    theme_bw() +
    facet_wrap(~Churn),
  
  df %>% 
    na.omit() %>% 
    ggplot() +
    geom_boxplot(aes(y = tenure, fill = Churn)) +
    labs(
      y = "Tenure"
    ) +
    theme_bw() +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x.bottom = element_blank()
    ),
  
  nrow = 1,
  top = "Tenure vs Churn"
)
  
# MonthlyCharges vs Churn
grid.arrange(  
  df %>% 
    ggplot() +
    geom_histogram(aes(x = MonthlyCharges), col = 'Black', fill = 'lightblue') +
    labs(
      x = NULL,
      y = 'Freq'
    ) +
    theme_bw() +
    facet_wrap(~Churn),
  
  df %>% 
    na.omit() %>% 
    ggplot() +
    geom_boxplot(aes(y = MonthlyCharges, fill = Churn)) +
    labs(
      y = "Monthly Charges"
    ) +
    theme_bw() +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x.bottom = element_blank()
    ),
  
  nrow = 1,
  top = "Monthly Charges vs Churn"
)

# TotalCharges vs Churn
grid.arrange(
  df %>% 
    ggplot() +
    geom_histogram(aes(x = TotalCharges), col = 'Black', fill = 'lightblue') +
    labs(
      x = NULL,
      y = 'Freq'
    ) +
    theme_bw() +
    facet_wrap(~Churn),
  
  
  df %>% 
    na.omit() %>% 
    ggplot() +
      geom_boxplot(aes(y = TotalCharges, fill = Churn)) +
      labs(
        y = "Total Charges"
      ) +
      theme_bw() +
      theme(
        axis.ticks.x = element_blank(),
        axis.text.x.bottom = element_blank()
      ),
  
  nrow = 1,
  top = "Total Charges vs Churn"
)
      

## Preparando os dados para modelagem --------------------------------------

# Excluindo variáveis irrelevantes e também os valores faltantes.
# Poderíamos utilizar outras abordagens para tratar os dados faltantes, por exmeplo
# inputar a média, mediana ou moda da variável em questão.

df_sem_trat <- df %>% 
  select(-customerID) %>%
  na.omit()

# Treino e teste --------------------------------------------------------------
# Para podermos mensurar a qualidade dos modelos vamos separa od dados
# em conjunto de treino e teste.
# A divisão será feita com utilização do pacote "caret", que possibilita
# realizar a divisão de forma proporcional segundo alguma variável.

# Verificando a proporção de categorias
df_sem_trat$Churn %>% 
  table() %>% 
  prop.table() %>% 
  round(., 2)

# Conforme vimos na análise descritiva, as classses são desbalanceadas.

# semente para garantir reprodutibilidade 
set.seed(20200710)

# Separando o conjunto de treino e teste
indices <- createDataPartition(df_sem_trat$Churn, 
                               times = 1, p = 0.8, list = FALSE)

treino <- df_sem_trat[indices, ]
teste <- df_sem_trat[-indices, ] 

# Verificando a proporção de categorias após a divisão do conjunto de dados

treino$Churn %>% 
  table() %>% 
  prop.table() %>% 
  round(., 2)

teste$Churn %>% 
  table() %>% 
  prop.table() %>% 
  round(., 2)

# Modelagem -------------------------------------------------------------------

# Como se trata de um problema de classificação binária, vamos ajustar um modelo
# de regressão logística e verificar a performance do modelo.

modelo_RL <- glm(Churn  ~ .,
                 data = treino,
                 family = binomial(link = "logit"))

summary(modelo_RL)

# Nota-se que alguns coefientes não foram definidos por conta de singularidade.
# Algumas variáveis são perfeitamente correlacionadas.

df_sem_trat %>%
  filter(InternetService == "No") %>% 
  select(InternetService, OnlineSecurity, OnlineBackup,
         DeviceProtection, TechSupport, StreamingTV,
         StreamingMovies) %>% 
  View()

df_sem_trat %>%
  filter(PhoneService == "No") %>% 
  select(PhoneService, MultipleLines) %>% 
  View()

df_sem_trat %<>%
  mutate_if(is.factor, as.character) %>% 
  mutate(
    MultipleLines = if_else(MultipleLines == "No phone service", "No", MultipleLines),
    OnlineSecurity = if_else(OnlineSecurity == "No internet service", "No", OnlineSecurity), 
    OnlineBackup = if_else(OnlineBackup == "No internet service", "No", OnlineBackup),
    DeviceProtection = if_else(DeviceProtection == "No internet service", "No", DeviceProtection), 
    TechSupport = if_else(TechSupport == "No internet service", "No", TechSupport),
    StreamingTV = if_else(StreamingTV == "No internet service", "No", StreamingTV),
    StreamingMovies = if_else(StreamingMovies == "No internet service", "No", StreamingMovies),
    Churn = fct_relevel(Churn, "Yes", "No")
  ) %>%
  mutate_if(is.character, as.factor)


# Ajustando o modelo novamente 

treino <- df_sem_trat[indices, ]
teste <- df_sem_trat[-indices, ] 

modelo_RL <- glm(Churn  ~ .,
                 data = treino,
                 family = binomial(link = "logit"))

summary(modelo_RL)

# Seleção de variáveis usando o método stepwise
step(modelo_RL, direction = "both", trace = FALSE)

modelo_RL_selecao <- glm(formula = Churn ~ SeniorCitizen + Dependents + tenure + MultipleLines + 
                           InternetService + OnlineSecurity + TechSupport + StreamingTV + 
                           StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                           MonthlyCharges + TotalCharges, family = binomial(link = "logit"), 
                         data = treino)

summary(modelo_RL_selecao)

# Nem todas as categorias da variável PayamentMethod foram significantes no modelo.
# Na análise exploratória dos dados, podemos notar que o percentual de Churn
# é bem maior para o método de pagamento Electronic check em relação aos outros.
# Sendo sim, vamos manter apenas duas categórias para o método de pagamento:
# Electronic check e Others (agrupando as outras categorias).

df_sem_trat %<>% 
  mutate(PaymentMethod = case_when(
    PaymentMethod == "Electronic check" ~ "Electronic check",
    TRUE ~ "Others"
      ),
    PaymentMethod = as.factor(PaymentMethod)
    )

# Ajustando o modelo novamente 

treino <- df_sem_trat[indices, ]
teste <- df_sem_trat[-indices, ] 

modelo_RL <- glm(Churn  ~ .,
                 data = treino,
                 family = binomial(link = "logit"))

# Seleção de variáveis usando o método stepwise
step(modelo_RL, direction = "both", trace = FALSE)

modelo_RL_selecao2 <- glm(formula = Churn ~ SeniorCitizen + Dependents + tenure + MultipleLines + 
                            InternetService + OnlineSecurity + TechSupport + StreamingTV + 
                            StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                            MonthlyCharges + TotalCharges, family = binomial(link = "logit"), 
                          data = treino)

summary(modelo_RL_selecao2)


# Removendo a variável Dependents que apresentou p-valor > 0.05
modelo_RL_selecao2 <- glm(formula = Churn ~ SeniorCitizen + tenure + MultipleLines + 
                            InternetService + OnlineSecurity + TechSupport + StreamingTV + 
                            StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                            MonthlyCharges + TotalCharges, 
                          family = binomial(link = "logit"), 
                          data = treino)

summary(modelo_RL_selecao2)

# Removendo a variável Tech Support que apresentou p-valor > 0.05
modelo_RL_selecao2 <- glm(formula = Churn ~ SeniorCitizen + tenure + MultipleLines + 
                            InternetService + OnlineSecurity  + StreamingTV + 
                            StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                            MonthlyCharges + TotalCharges, 
                          family = binomial(link = "logit"), 
                          data = treino)

summary(modelo_RL_selecao2)
vif(modelo_RL_selecao2)

# Temos multicolinearidade no modelo.
# Vimos na análise exploratória dos dados que 
# as variáveis tenure e TotalCharges são altamente correlacionadas. 
# Podemos manter apenas uma das duas variáveis no modelo. Sendo assim, optei 
# seguir com a variável tenure.

modelo_RL_selecao2 <- glm(formula = Churn ~ SeniorCitizen + tenure + MultipleLines + 
                            InternetService + OnlineSecurity  + StreamingTV + 
                            StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                            MonthlyCharges, 
                          family = binomial(link = "logit"), 
                          data = treino)

summary(modelo_RL_selecao2)
vif(modelo_RL_selecao2)

# Ainda temos multicolinearidade no modelo.

pred <- predict(modelo_RL_selecao2, teste, type = 'response')
pred <- predict(modelo_RL_selecao2, treino, type = 'response')



glm.pred <- as.factor( if_else(pred > 0.5, "Yes", "No"))
Y_teste <- as.factor( if_else(treino$Churn == "Yes", "Yes", "No") )

confusionMatrix(data = glm.pred, reference = Y_teste, positive = "Yes")

# O modelo possui uma acurácia bastante baixa

# Vamos tentar uma segunda abordagem, utilizando métodos de 
# oversample e undersample, para tratar o problema de desbalanceamento
# das classes. 

# SOBREAMOSTRAGEM da classe minoritária ------------------------
set.seed(10)

treino$Churn %>% table()
tamanho <- 2 * table(treino$Churn)[[2]]

treino_over <- ovun.sample(Churn~., data = treino, method = 'over', N = tamanho)$data

treino_over %>% 
  select(Churn) %>% 
  table()

modelo_RL_over <- glm(Churn  ~ . - TotalCharges,
                      data = treino_over,
                      family = binomial(link = "logit"))

summary(modelo_RL_over)

# Seleção de variáveis usando o método stepwise
step(modelo_RL_over, direction = "both", trace = FALSE)

# Removendo Total Charges
modelo_RL_over_select <- glm(formula = Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + 
                               MultipleLines + InternetService + OnlineBackup + DeviceProtection + 
                               StreamingTV + StreamingMovies + Contract + PaperlessBilling + 
                               PaymentMethod + MonthlyCharges,
                             family = binomial(link = "logit"), 
                             data = treino_over)

summary(modelo_RL_over_select)
vif(modelo_RL_over_select)

# Fazendo as predições para o conjunto de teste
pred <- predict(modelo_RL_over_select, teste, type = 'response')

glm.pred <- as.factor( if_else(pred > 0.5, "Yes", "No"))
Y_teste <- as.factor( if_else(teste$Churn == "Yes", "Yes", "No") )

confusionMatrix(data = glm.pred, reference = Y_teste, positive = "Yes")

# SUBAMOSTRAGEM da classe majoritária ------------------------
set.seed(10)

treino$Churn %>% table()
tamanho <- 2 * table(treino$Churn)[[1]]

treino_under <- ovun.sample(Churn~., data = treino, method = 'under', N = tamanho)$data

treino_under %>% 
  select(Churn) %>% 
  table()

modelo_RL_under <- glm(Churn  ~ . - TotalCharges,
                      data = treino_under,
                      family = binomial(link = "logit"))

summary(modelo_RL_under)

# Seleção de variáveis usando o método stepwise
step(modelo_RL_under, direction = "both", trace = FALSE)


modelo_RL_under_select <- glm(formula = Churn ~ Dependents + tenure + MultipleLines + InternetService + 
                                OnlineSecurity + StreamingTV + StreamingMovies + Contract + 
                                PaperlessBilling + PaymentMethod + MonthlyCharges, 
                              family = binomial(link = "logit"), 
                              data = treino_under)

summary(modelo_RL_under_select)

# Removendo a variável Dependents (p-valor > 0.05)
modelo_RL_under_select <- glm(formula = Churn ~ tenure + MultipleLines + InternetService + 
                                OnlineSecurity + StreamingTV + StreamingMovies + Contract + 
                                PaperlessBilling + PaymentMethod + MonthlyCharges, 
                              family = binomial(link = "logit"), 
                              data = treino_under)


summary(modelo_RL_under_select)
vif(modelo_RL_under_select)

# Fazendo as predições para o conjunto de teste
pred <- predict(modelo_RL_under_select, teste, type = 'response')

glm.pred <- as.factor( if_else(pred > 0.5, "Yes", "No"))
Y_teste <- as.factor( if_else(teste$Churn == "Yes", "Yes", "No") )

confusionMatrix(data = glm.pred, reference = Y_teste, positive = "Yes")

# Os modelos ajustados com os métodos de oversample e undersample
# apresentaram melhor performance.


# Vamos ajustar o modelo realizando mais alguns tratamentos no banco de dados:
# 1. substituir dados faltantes pela média
# 2. normalizar as variáveis contínuas 

# Tratando as variáveis categóricas novamente
df_tratado <- df %>% 
  select(-customerID) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    MultipleLines = if_else(MultipleLines == "No phone service", "No", MultipleLines),
    OnlineSecurity = if_else(OnlineSecurity == "No internet service", "No", OnlineSecurity), 
    OnlineBackup = if_else(OnlineBackup == "No internet service", "No", OnlineBackup),
    DeviceProtection = if_else(DeviceProtection == "No internet service", "No", DeviceProtection), 
    TechSupport = if_else(TechSupport == "No internet service", "No", TechSupport),
    StreamingTV = if_else(StreamingTV == "No internet service", "No", StreamingTV),
    StreamingMovies = if_else(StreamingMovies == "No internet service", "No", StreamingMovies),
    Churn = fct_relevel(Churn, "Yes", "No")
  ) %>%
  mutate_if(is.character, as.factor)

df_tratado$TotalCharges %>% is.na() %>% sum()

# Substituindo dados faltante pela média
df_tratado %<>% 
  mutate(
    TotalCharges = if_else(
      is.na(TotalCharges),  mean(TotalCharges, na.rm = TRUE), TotalCharges)
    )

df_tratado %>% is.na() %>% sum()

# normalizando as variáveis contínuas  
df_tratado %<>%
  mutate(
    tenure = (tenure - mean(tenure)) / sd(tenure),
    TotalCharges = (TotalCharges - mean(TotalCharges)) / sd(TotalCharges),
    MonthlyCharges = (MonthlyCharges - mean(MonthlyCharges)) / sd(MonthlyCharges)
  )

# Setando semente para reprodutibilidade
set.seed(20200710)

# Separando o conjunto de treino e teste
indices <- createDataPartition(df_tratado$Churn, 
                               times = 1, p = 0.8, list = FALSE)

treino <- df_tratado[indices, ]
teste <- df_tratado[-indices, ] 

# Ajustando modelo
modelo_RL <- glm(Churn  ~ .,
                 data = treino,
                 family = binomial(link = "logit"))

summary(modelo_RL)

# Seleção de variáveis usando o método stepwise
step(modelo_RL, direction = "both", trace = FALSE)

modelo_RL_selecao <- glm(formula = Churn ~ SeniorCitizen + Dependents + tenure + MultipleLines + 
                           InternetService + OnlineSecurity + TechSupport + StreamingTV + 
                           StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                           MonthlyCharges + TotalCharges, family = binomial(link = "logit"), 
                         data = treino)

summary(modelo_RL_selecao)

# Nem todas as categorias da variável método de pagamento foi significante
# vamos agrupar as categorias novamente

df_tratado %<>% 
  mutate(PaymentMethod = case_when(
    PaymentMethod == "Electronic check" ~ "Electronic check",
    TRUE ~ "Others"
  ),
  PaymentMethod = as.factor(PaymentMethod)
  )

treino <- df_tratado[indices, ]
teste <- df_tratado[-indices, ] 

# Ajustando modelo
modelo_RL <- glm(Churn  ~ .,
                 data = treino,
                 family = binomial(link = "logit"))

summary(modelo_RL)

# Seleção de variáveis usando o método stepwise
step(modelo_RL, direction = "both", trace = FALSE)

modelo_RL_selecao <- glm(formula = Churn ~ SeniorCitizen + Dependents + tenure + MultipleLines + 
                           InternetService + OnlineSecurity + TechSupport + StreamingTV + 
                           StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                           MonthlyCharges + TotalCharges,
                         family = binomial(link = "logit"), 
                         data = treino)

summary(modelo_RL_selecao)

# Removendo a variável Tech Support (p-valor > 0.05)
modelo_RL_selecao <- glm(formula = Churn ~ SeniorCitizen + tenure + MultipleLines + 
                           InternetService + OnlineSecurity + StreamingTV + 
                           StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                           MonthlyCharges + TotalCharges,
                         family = binomial(link = "logit"), 
                         data = treino)

summary(modelo_RL_selecao)
vif(modelo_RL_selecao)

# Fazendo as predições para o conjunto de teste
pred <- predict(modelo_RL_selecao, teste, type = 'response')

glm.pred <- as.factor( if_else(pred > 0.5, "Yes", "No"))
Y_teste <- as.factor( if_else(teste$Churn == "Yes", "Yes", "No") )

confusionMatrix(data = glm.pred, reference = Y_teste, positive = "Yes")

# Mesmo com as variáveis padronizadas

# ----------------------------------------------------------------------------

modelo_rf <- train(Churn ~ .,
                   data = df_treino,
                   method = "rpart",
                   tuneLength = 7)

tempo_final <- Sys.time() - tempo_inicio
tempo_final

modelo_rf

# Desempenho do modelo --------------------------------------------------------
# Aplicando aos dados de teste

X_teste <- df_teste %>% select(-Churn)
Y_teste <- df_teste$Churn

pred <- predict(modelo_RL, X_teste, type = 'prob')
table(df_teste$Churn, pred)


pred <- predict(modelo_RF_selec, X_teste, type = 'response')
table(df_teste$Churn, pred)

pred <- predict(modelo_rf, df_teste, type = 'raw')
table(df_teste$Churn, pred)

# Conclusão -------------------------------------------------------------------

# Salvando a workspace 
save.image("./Saídas/projeto_v0.RData") 
