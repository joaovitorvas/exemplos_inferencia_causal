#Processos iniciais: baixando bibliotecas, organizando dados
install.packages('ggplot2')
library(ggplot2)
load('C:/Users/Aluno/Downloads/CampbellThistlewaite1960.rda') #NOTA: Substituir o caminho absoluto para onde o arquivo está baixado
dados_originais <- data.frame(CampbellThistlewaite1960)
dados_originais$test_score <- as.numeric(dados_originais$test_score) #test_score estava salvo como string, e precisamos desses valores como números para fazer a regressão
df_commended <- dados_originais[2:11, ] #Dataframe com os dados dos alunos que NÃO receberam reconhecimento
df_merit <- dados_originais[12:21, ] #Dataframe com os dados dos alunos que receberam reconhecimento

#Figura 2: calculando as regressões GG e HH
funcao_gg_commended <- lm(scholarship ~ test_score, data = df_commended)
coeficiente_ggc <- as.numeric(coef(funcao_gg_commended)[2])
intercepto_ggc <- as.numeric(coef(funcao_gg_commended)[1])
funcao_gg_merit <- lm(scholarship ~ test_score, data = df_merit)
coeficiente_ggm <- as.numeric(coef(funcao_gg_merit)[2])
intercepto_ggm <- as.numeric(coef(funcao_gg_merit)[1])

funcao_hh_commended <- lm(scholarship_150 ~ test_score, data = df_commended)
coeficiente_hhc <- as.numeric(coef(funcao_hh_commended)[2])
intercepto_hhc <- as.numeric(coef(funcao_hh_commended)[1])
funcao_hh_merit <- lm(scholarship_150 ~ test_score, data = df_merit)
coeficiente_hhm <- as.numeric(coef(funcao_hh_merit)[2])
intercepto_hhm <- as.numeric(coef(funcao_hh_merit)[1])

#Figura 2: grafando as regressões GG e HH
ggplot(data = dados_originais, mapping = aes(x = test_score)) + #Usa dados_originais para colocar todos os pontos
  
  geom_point(aes(y = scholarship), color = 'blue') +
  
  geom_segment(aes(x = 1, y = intercepto_ggc, xend = 10, yend = coeficiente_ggc*10+intercepto_ggc), linetype = 'dashed') + #Grafa a regressão GG de 0 a 10
  geom_segment(aes(x = 11, y = coeficiente_ggm*11+intercepto_ggm, xend = 20, yend = coeficiente_ggm*20+intercepto_ggm), linetype = 'dashed') + #Grafa a regressão GG de 11 a 20
  
  geom_text(aes(x = 1, y = intercepto_ggc + 1, label = 'G'), check_overlap = TRUE) + #Adiciona o rótulo G para nomear a função de 0 a 10
  geom_text(aes(x = 20, y = coeficiente_ggm*20+intercepto_ggm + 1, label = "G'"), check_overlap = TRUE) + #Adiciona o rótulo G' para nomear a função de 11 a 20
  
  geom_point(aes(y = scholarship_150), color = 'red') +
  
  geom_segment(aes(x = 1, y = intercepto_hhc, xend = 10, yend = coeficiente_hhc*10+intercepto_hhc), linetype = 'dashed') + #Grafa a regressão HH de 0 a 10
  geom_segment(aes(x = 11, y = coeficiente_hhm*11+intercepto_hhm, xend = 20, yend = coeficiente_hhm*20+intercepto_hhm), linetype = 'dashed') + #Grafa a regressão HH de 11 a 20
  
  geom_text(aes(x = 1, y = intercepto_hhc + 1, label = 'H'), check_overlap = TRUE) + #Adiciona o rótulo H para nomear a função de 0 a 10
  geom_text(aes(x = 20, y = coeficiente_hhm*20+intercepto_hhm + 1, label = "H'"), check_overlap = TRUE) + #Adiciona o rótulo H para nomear a função de 11 a 20
  
  geom_vline(xintercept = 10.5) + #Coloca a reta para definir o limite e separar os alunos commended dos merit
  
  scale_x_continuous(breaks = seq(1, 20, 1), limits = c(1, 20), minor_breaks = NULL) + #Ajusta o eixo x para ser igual ao do artigo
  scale_y_continuous(breaks = seq(42, 70, 4), limits = c(41, 70)) + #Ajusta o eixo y
  
  labs(title = "Fig. 2. Regression of success in winning scholarships on exposure determiner",
       subtitle = "G-G' - Percent viewing scholarships / H-H' - Percent viewing scholarships of $150 or more",
       x = 'Test scores',
       y = 'Percentage of students winning scholarships')

#Figura 2: efeito causal do reconhecimento sobre bolsas de estudo e sobre bolsas de estudo acima de $150
lim_esquerda_gg <- predict(funcao_gg_commended, data.frame(test_score = 10.5))
lim_direita_gg <- predict(funcao_gg_merit, data.frame(test_score = 10.5))
CACE_gg <- lim_direita_gg - lim_esquerda_gg

lim_esquerda_hh <- predict(funcao_hh_commended, data.frame(test_score = 10.5))
lim_direita_hh <- predict(funcao_hh_merit, data.frame(test_score = 10.5))
CACE_hh <- lim_direita_hh - lim_esquerda_hh

#Figura 3: calculando as regressões II e JJ
funcao_ii_commended <- lm(graduate_study ~ test_score, data = df_commended)
coeficiente_iic <- as.numeric(coef(funcao_ii_commended)[2])
intercepto_iic <- as.numeric(coef(funcao_ii_commended)[1])
funcao_ii_merit <- lm(graduate_study ~ test_score, data = df_merit)
coeficiente_iim <- as.numeric(coef(funcao_ii_merit)[2])
intercepto_iim <- as.numeric(coef(funcao_ii_merit)[1])

funcao_jj_commended <- lm(teacher_researcher ~ test_score, data = df_commended)
coeficiente_jjc <- as.numeric(coef(funcao_jj_commended)[2])
intercepto_jjc <- as.numeric(coef(funcao_jj_commended)[1])
funcao_jj_merit <- lm(teacher_researcher ~ test_score, data = df_merit)
coeficiente_jjm <- as.numeric(coef(funcao_jj_merit)[2])
intercepto_jjm <- as.numeric(coef(funcao_jj_merit)[1])

#Figura 3: grafando as regressões II e JJ
ggplot(data = dados_originais, mapping = aes(x = test_score)) +
  
  geom_point(aes(y = graduate_study), color = 'blue') +
  
  geom_segment(aes(x = 1, y = intercepto_iic, xend = 10, yend = coeficiente_iic*10+intercepto_iic), linetype = 'dashed') +
  geom_segment(aes(x = 11, y = coeficiente_iim*11+intercepto_iim, xend = 20, yend = coeficiente_iim*20+intercepto_iim), linetype = 'dashed') +
  
  geom_text(aes(x = 1, y = intercepto_iic + 1, label = 'I'), check_overlap = TRUE) +
  geom_text(aes(x = 20, y = coeficiente_iim*20+intercepto_iim + 1, label = "I'"), check_overlap = TRUE) +
  
  geom_point(aes(y = teacher_researcher), color = 'red') +
  
  geom_segment(aes(x = 1, y = intercepto_jjc, xend = 10, yend = coeficiente_jjc*10+intercepto_jjc), linetype = 'dashed') +
  geom_segment(aes(x = 11, y = coeficiente_jjm*11+intercepto_jjm, xend = 20, yend = coeficiente_jjm*20+intercepto_jjm), linetype = 'dashed') +
  
  geom_text(aes(x = 1, y = intercepto_jjc - 1, label = 'J'), check_overlap = TRUE) +
  geom_text(aes(x = 20, y = coeficiente_jjm*20+intercepto_jjm - 1, label = "J'"), check_overlap = TRUE) +
  
  geom_vline(xintercept = 10.5) +
  
  scale_x_continuous(breaks = seq(1, 20, 1), limits = c(1, 20), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(18, 46, 4), limits = c(17, 46)) +
  
  labs(title = "Fig. 3. Regression of study and career plans on exposure determiner",
       subtitle = "I-I' - Percent planning to do three or more years of graduate study (PhD or MD) / J-J' - Percent planning to be a college teacher or scientific researcher",
       x = 'Test scores',
       y = 'Percent of students with given study or career plans')

#Figura 3: efeito causal do reconhecimento sobre interesse em mestrado e doutorado e sobre interesse em atuar como professor ou pesquisador
lim_esquerda_ii <- predict(funcao_ii_commended, data.frame(test_score = 10.5))
lim_direita_ii <- predict(funcao_ii_merit, data.frame(test_score = 10.5))
CACE_ii <- lim_direita_ii - lim_esquerda_ii

lim_esquerda_jj <- predict(funcao_jj_commended, data.frame(test_score = 10.5))
lim_direita_jj <- predict(funcao_jj_merit, data.frame(test_score = 10.5))
CACE_jj <- lim_direita_jj - lim_esquerda_jj

#Figura 4: calculando a regressão KK
funcao_kk_commended <- lm(intellectualism ~ test_score, data = df_commended)
coeficiente_kkc <- as.numeric(coef(funcao_kk_commended)[2])
intercepto_kkc <- as.numeric(coef(funcao_kk_commended)[1])
funcao_kk_merit <- lm(intellectualism ~ test_score, data = df_merit)
coeficiente_kkm <- as.numeric(coef(funcao_kk_merit)[2])
intercepto_kkm <- as.numeric(coef(funcao_kk_merit)[1])

#Figura 4: grafando a regressão KK
ggplot(data = dados_originais, mapping = aes(x = test_score)) +
  
  geom_point(aes(y = intellectualism), color = 'blue') +
  
  geom_segment(aes(x = 1, y = intercepto_kkc, xend = 10, yend = coeficiente_kkc*10+intercepto_kkc), linetype = 'dashed') +
  geom_segment(aes(x = 11, y = coeficiente_kkm*11+intercepto_kkm, xend = 20, yend = coeficiente_kkm*20+intercepto_kkm), linetype = 'dashed') +
  
  geom_text(aes(x = 1, y = intercepto_kkc + 0.04, label = 'K'), check_overlap = TRUE) +
  geom_text(aes(x = 20, y = coeficiente_kkm*20+intercepto_kkm - 0.04, label = "K'"), check_overlap = TRUE) +
  
  geom_vline(xintercept = 10.5) +
  
  scale_x_continuous(breaks = seq(1, 20, 1), limits = c(1, 20), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(3.63, 4.11, 0.08), limits = c(3.60, 4.11)) +
  
  labs(title = "Fig. 4. Regression of attitudes toward intellectualism on exposure determiner",
       x = 'Test scores',
       y = 'Mean score on intellectualism scale')

#Figura 4: efeito causal do reconhecimento sobre intelectualismo
lim_esquerda_kk <- predict(funcao_kk_commended, data.frame(test_score = 10.5))
lim_direita_kk <- predict(funcao_kk_merit, data.frame(test_score = 10.5))
CACE_kk <- lim_direita_kk - lim_esquerda_kk
