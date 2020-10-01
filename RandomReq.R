library(caTools);
library(e1071);
library(caret);
library(randomForest);

base = read.csv('census_data.csv',header = TRUE, sep = ",",stringsAsFactors = FALSE,col.names = c("Idade","Classe_trabalhista","Calculo_census","Escolaridade","Numero_escolar","Estado_Civil","Ocupacao","Relacionamento_conjugal","Raca","Sexo","Ganho_capital","Perca_capital","Hrs_trab_sem","Pais_origem","Ganha_mais_que_50k"));

base$Classe_trabalhista<- replace(base$Classe_trabalhista, base$Classe_trabalhista==" ?","unknown");
base$Ocupacao<- replace(base$Ocupacao, base$Ocupacao==" ?","unknown");
base$Pais_origem<- replace(base$Pais_origem, base$Pais_origem==" ?","0");

base[is.na(base)] = "unknown";
datatest<- data.frame()
datatest<- base[is.na(base$Classe_trabalhista),]

# Tratamento dos campos
base$Classe_trabalhista = factor(base$Classe_trabalhista, levels = c(' Federal-gov', ' Local-gov', ' Private', ' Self-emp-inc', ' Self-emp-not-inc', ' State-gov', ' Without-pay',' Never-worked','unknown'), labels = c(1, 2, 3, 4, 5, 6, 7,8,9));
base$Escolaridade = factor(base$Escolaridade, levels = c(' 10th', ' 11th', ' 12th', ' 1st-4th', ' 5th-6th', ' 7th-8th', ' 9th', ' Assoc-acdm', ' Assoc-voc', ' Bachelors', ' Doctorate', ' HS-grad', ' Masters', ' Preschool', ' Prof-school', ' Some-college'), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16));
base$Estado_Civil = factor(base$Estado_Civil, levels = c(' Divorced', ' Married-AF-spouse', ' Married-civ-spouse', ' Married-spouse-absent', ' Never-married', ' Separated', ' Widowed'), labels = c(1, 2, 3, 4, 5, 6, 7));
base$Ocupacao = factor(base$Ocupacao, levels = c(' Adm-clerical', ' Armed-Forces', ' Craft-repair', ' Exec-managerial', ' Farming-fishing', ' Handlers-cleaners', ' Machine-op-inspct', ' Other-service', ' Priv-house-serv', ' Prof-specialty', ' Protective-serv', ' Sales', ' Tech-support', ' Transport-moving','unknown'), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0))
base$Relacionamento_conjugal = factor(base$Relacionamento_conjugal, levels = c(' Husband', ' Not-in-family', ' Other-relative', ' Own-child', ' Unmarried', ' Wife'), labels = c(1, 2, 3, 4, 5, 6));
base$Raca = factor(base$Raca, levels = c(' Amer-Indian-Eskimo', ' Asian-Pac-Islander', ' Black', ' Other', ' White'), labels = c(1, 2, 3, 4, 5));
base$Sexo = factor(base$Sexo, levels = c(' Female', ' Male'), labels = c(0, 1));
base$Pais_origem = factor(base$Pais_origem, levels = c(' Cambodia', ' Canada', ' China', ' Columbia', ' Cuba', ' Dominican-Republic', ' Ecuador', ' El-Salvador', ' England', ' France', ' Germany', ' Greece', ' Guatemala', ' Haiti', ' Holand-Netherlands', ' Honduras', ' Hong', ' Hungary', ' India', ' Iran', ' Ireland', ' Italy', ' Jamaica', ' Japan', ' Laos', ' Mexico', ' Nicaragua', ' Outlying-US(Guam-USVI-etc)', ' Peru', ' Philippines', ' Poland', ' Portugal', ' Puerto-Rico', ' Scotland', ' South', ' Taiwan', ' Thailand', ' Trinadad&Tobago', ' United-States', ' Vietnam', ' Yugoslavia','0'), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,0));
base$Ganha_mais_que_50k = factor(base$Ganha_mais_que_50k, levels = c(' <=50K', ' >50K'), labels = c(0, 1));

s1 = scale(base[, c(1,3,11:13)]);
base[, 1] = scale(base[, 1]);
base[, 3] = scale(base[, 3]);
#base[, 5] = scale(base[, 5]);
base[, 11:13] = scale(base[, 11:13]);



base[5] <- NULL

#Divisão e treino

set.seed(1);
divisao = sample.split(base$Ganha_mais_que_50k, SplitRatio = 0.85);
base_treino = subset(base, divisao == TRUE);
base_teste = subset(base, divisao == FALSE);

set.seed(1);

classi = randomForest(x = base_treino[-14], y = base_treino$Ganha_mais_que_50k, ntree = 500);

previsoes = predict(classi, newdata = base_teste[-14]);

matriz_confusao = table(base_teste[, 14], previsoes);

confusionMatrix(matriz_confusao);



#endfile
