library(ggplot2)
library(haven)
library(psych)
library(effsize)
library(gridExtra)
#Caminho do dataset
df <- read.csv("C:/Users/Ze_Vito/Documents/1.Progreams/RStudio/MicrodadosENEM/Notas provas objetivas e TP_Escola s omiss.csv", sep=',')


#Corrigindo a tipologia dos dads

df$NU_NOTA_MT <- as.double(df$NU_NOTA_MT)
df$NU_NOTA_LC <- as.double(df$NU_NOTA_LC)
df$NU_NOTA_CH <- as.double(df$NU_NOTA_CH)
df$NU_NOTA_CN <- as.double(df$NU_NOTA_CN)
df$NU_NOTA_REDACAO <- as.double(df$NU_NOTA_REDACAO)
df$TP_ESCOLA <-  as.factor(df$TP_ESCOLA)
class(df$TP_ESCOLA)
class(df$NU_NOTA_CN)

#Removendo NULLS, pois sao referentes a pessoas que faltaram as provas
df <- na.omit(df)

# Avaliação preliminar do dataset
head(df)
describe(df)

#COMPARAÇÂO DE NOTA

## ---------------------------------------------------------------------------
## 1. MATEMATICA

violinoMT <-  ggplot(df, aes(x = TP_ESCOLA, y = NU_NOTA_MT, fill = TP_ESCOLA)) + 
                geom_violin(alpha = .5) + 
                guides(fill = guide_legend(title = element_blank())) +
                labs(title= "Matemática \n", 
                     x = element_blank(), 
                     y = "Nota") + 
                theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
                scale_fill_hue(labels = c("Pública", "Privada")) +
                theme_classic()
violinoMT

violinoMT + 
  geom_boxplot(width = .07, alpha = .7, show.legend = FALSE, axes = FALSE) +  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

## ---------------------------------------------------------------------------
## 2. CIENCIAS DA NATUREZA

violinoCN <-  ggplot(df, aes(x = TP_ESCOLA, y = NU_NOTA_CN, fill = TP_ESCOLA)) + 
  geom_violin(alpha = .5) + 
  guides(fill = guide_legend(title = element_blank())) +
  labs(title="Ciências da Natureza \n", 
       x = element_blank(), 
       y = "Nota") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_hue(labels = c("Pública", "Privada")) +
  theme_classic()

violinoCN + 
  geom_boxplot(width = .07, alpha = .7, show.legend = FALSE, axes = FALSE) +  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

## ---------------------------------------------------------------------------
## 3. LINGUAGENS E CODIGOS

violinoLC <-  ggplot(df, aes(x = TP_ESCOLA, y = NU_NOTA_LC, fill = TP_ESCOLA)) + 
  geom_violin(alpha = .5) + 
  guides(fill = guide_legend(title = element_blank())) +
  labs(title="Linguagens e Códigos \n",
       x = element_blank(), 
       y = "Nota") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_hue(labels = c("Pública", "Privada")) +
  theme_classic()

violinoLC + 
  geom_boxplot(width = .07, alpha = .7, show.legend = FALSE) +  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

## ---------------------------------------------------------------------------
## 4. CIENCIAS HUMANAS

violinoCH <-  ggplot(df, aes(x = TP_ESCOLA, y = NU_NOTA_CH, fill = TP_ESCOLA)) + 
  geom_violin(alpha = .5) + 
  guides(fill = guide_legend(title = element_blank())) +
  labs(title="Ciências Humanas \n", 
       x = element_blank(), 
       y = "Nota") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_hue(labels = c("Pública", "Privada")) +
  theme_classic()

violinoCH + 
  geom_boxplot(width = .07, alpha = .7, show.legend = FALSE, axes = FALSE) +  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


## ---------------------------------------------------------------------------
## 5. REDAÇÃO

violinoRED <-  ggplot(df, aes(x = TP_ESCOLA, y = NU_NOTA_REDACAO, fill = TP_ESCOLA)) + 
  geom_violin(alpha = .5) + 
  guides(fill = guide_legend(title = element_blank())) +
  labs(title="Redação \n", 
       x = element_blank(), 
       y = "Nota") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_hue(labels = c("Pública", "Privada")) +
  theme_classic()

violinoRED + 
  geom_boxplot(width = .07, alpha = .7, show.legend = FALSE, axes = FALSE) +  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
