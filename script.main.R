library(tidyverse)
library(scales)
library(ggcorrplot)


df = read.csv("C:/Users/User/Desktop/housespriceICD/all_perth_310121.csv")
at = attributes(df)


#APRESENTACAO DA VARIAVEL PRECO
ggplot(df)+  
  geom_freqpoly(aes(x = PRICE),color = "darkblue")+
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "m"))+
  theme_minimal()

summary(df$PRICE)




#MATRIZ DE CORRELACAO PARA VARIAVEIS NUMERICAS
num.df = select(df, is.numeric, -NEAREST_SCH_RANK)
ggcorrplot(cor(num.df, method = "pearson"), 
           method = "square", type = "full", lab = TRUE, lab_size = 2,tl.cex = 8 )






#RELACAO ENTRE O PRECO E O A DISTANCIA PARA O CENTRO

ggplot(df)+
  geom_freqpoly(aes(x = CBD_DIST),color = "darkblue")+
  scale_x_continuous(labels = label_number(scale = 1e-3, suffix = "Km"))+
  theme_minimal() #DISTRIBUICAO DA VARIAVEL

summary(df$CBD_DIST) # ALGUMAS MEDIDAS

ggplot(df)+
  geom_point(aes(y = PRICE, x = CBD_DIST), color = "darkblue", alpha=0.3)+
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "m"))+
  scale_x_continuous(labels = label_number(scale = 1e-3, suffix = "km"))+
  theme_minimal() #COMPARACAO COM O PRECO

  
cor(df$PRICE, df$CBD_DIST, method = "spearman")


############## LATITUDE E LONGITUDE


ggplot(df)+
  geom_point(aes(y = PRICE, x = LONGITUDE), color = "darkblue", alpha=0.5)+
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "m"))+
  theme_minimal()
summary(df$LONGITUDE)


ggplot(df)+
  geom_point(aes(y = PRICE, x = LATITUDE), color = "darkblue", alpha=0.5)+
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "m"))+
  theme_minimal()


# É POSSIVEL PERCEBER QUE OS PONTOS DE LATITUDE E LONGITUDE ONDE O PRECO DAS CASA 
# É MAIS ALTO, COINCIDE COM AS COORDENADAS DO CENTRO DA CIDADE

##############






#RELACAO ENTRE O PRECO E OS COMODOS DA CASA

ggplot(df)+ 
  geom_bar(aes(x = factor(BEDROOMS)), fill = "darkblue")+
  theme_minimal() #DISTRIBUICAO


ggplot(df)+ 
  geom_bar(aes(x = factor(BATHROOMS)), fill = "darkblue")+
  theme_minimal() #DISTRIBUICAO


ggplot(df)+ 
  geom_boxplot(aes(x = factor(BEDROOMS), y = PRICE,), color = "darkblue")+
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "m"))+
  theme_minimal()#COMPARACAO COM A VARIAVEL PRECO

  
ggplot(df)+
geom_boxplot(aes(x = factor(BATHROOMS), y = PRICE,), color = "darkblue", outlier.stroke = 0.5 )+
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "m"))+
  theme_minimal()#COMPARACAO COM A VARIAVEL PRECO


  





# RELACAO ENTRE O PRECO E O ANO DE CONSTRUCAO

df$BUILD_YEAR_NUM = as.numeric(df$BUILD_YEAR) 
df_clean = df[!is.na(df$BUILD_YEAR_NUM),]
labels = paste(seq(1860, 2000, by = 20), "-", seq(1880, 2020, by = 20) - 1, sep = "")
df_clean$BUILD_YEAR_INTERVAL = cut(df_clean$BUILD_YEAR_NUM, seq(1860, 2020, by = 20), labels = labels, right = TRUE)

ggplot(df_clean)+
  geom_histogram(aes(x = factor(BUILD_YEAR_INTERVAL)), stat="count", fill = "darkblue")+
  theme_minimal()#DISTRIBUICAO

ggplot(df_clean)+
  geom_boxplot(aes(x = factor(BUILD_YEAR_INTERVAL), y = PRICE), color = "darkblue",outliers = TRUE )+
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "m"))+
  theme_minimal()#COMPARACAO COM A VARIAVEL PRECO
  






#############



arrange(summarize(group_by(df, SUBURB), preco_medio = mean(PRICE, na.rm = TRUE)), desc(preco_medio))  
#OS BAIRROS COM PRECO MEDIO MAIS ALTO
arrange(summarize(group_by(df, SUBURB), preco_medio = mean(PRICE, na.rm = TRUE)), preco_medio)  
#OS BAIRROS COM PRECO MEDIO MAIS BAIXO
#######  RELACAO ENTRE AREA UTIL E PRECO

ggplot(df)+
  geom_freqpoly(aes(x = FLOOR_AREA), color = "darkblue")#DISTRIBUICAO


ggplot(df)+
  geom_boxplot(aes(x = FLOOR_AREA, y = PRICE, cut_width(FLOOR_AREA, 100)), color = "darkblue" )






