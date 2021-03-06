#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

#* @apiTitle Plumber Test API!!

# * @filter cors
cors<- function (res) {   
  res$setHeader("Access-Control-Allow-Origin", "*") 
  plumber::forward() 
}


#* @json
#* @get /test
test<- function(){
  "Conectado"
}

#* @json
#* @post /prever
previ <- function(idade,classeTrabalhista, calculoCensus, escolaridade, estadoCivil, ocupacao, relacionamentoConjugal,
                  raca, sexo, ganhoCapital, percaCapital, hrsTrab, paisOrigem){
  
  d <-data.frame(Idade= as.numeric(idade),Classe_trabalhista = classeTrabalhista, Calculo_census = as.numeric(calculoCensus), Escolaridade = escolaridade,
                 Estado_Civil = estadoCivil, Ocupacao = ocupacao, Relacionamento_conjugal = relacionamentoConjugal,
                 Raca = raca, Sexo = sexo , Ganho_capital = as.numeric(ganhoCapital), Perca_capital = as.numeric(percaCapital), Hrs_trab_sem = as.numeric(hrsTrab), Pais_origem = paisOrigem, stringsAsFactors = FALSE);
  
  d$Classe_trabalhista<- replace(d$Classe_trabalhista, d$Classe_trabalhista==" ?","unknown");
  d$Ocupacao<- replace(d$Ocupacao, d$Ocupacao==" ?","unknown");
  d$Pais_origem<- replace(d$Pais_origem, d$Pais_origem==" ?","0");
  
  
  d$Classe_trabalhista = factor(d$Classe_trabalhista, levels = c(' Federal-gov', ' Local-gov', ' Private', ' Self-emp-inc', ' Self-emp-not-inc', ' State-gov', ' Without-pay',' Never-worked','unknown'), labels = c(1, 2, 3, 4, 5, 6, 7,8,9));
  d$Escolaridade = factor(d$Escolaridade, levels = c(' 10th', ' 11th', ' 12th', ' 1st-4th', ' 5th-6th', ' 7th-8th', ' 9th', ' Assoc-acdm', ' Assoc-voc', ' Bachelors', ' Doctorate', ' HS-grad', ' Masters', ' Preschool', ' Prof-school', ' Some-college'), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16));
  d$Estado_Civil = factor(d$Estado_Civil, levels = c(' Divorced', ' Married-AF-spouse', ' Married-civ-spouse', ' Married-spouse-absent', ' Never-married', ' Separated', ' Widowed'), labels = c(1, 2, 3, 4, 5, 6, 7));
  d$Ocupacao = factor(d$Ocupacao, levels = c(' Adm-clerical', ' Armed-Forces', ' Craft-repair', ' Exec-managerial', ' Farming-fishing', ' Handlers-cleaners', ' Machine-op-inspct', ' Other-service', ' Priv-house-serv', ' Prof-specialty', ' Protective-serv', ' Sales', ' Tech-support', ' Transport-moving','unknown'), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0))
  d$Relacionamento_conjugal = factor(d$Relacionamento_conjugal, levels = c(' Husband', ' Not-in-family', ' Other-relative', ' Own-child', ' Unmarried', ' Wife'), labels = c(1, 2, 3, 4, 5, 6));
  d$Raca = factor(d$Raca, levels = c(' Amer-Indian-Eskimo', ' Asian-Pac-Islander', ' Black', ' Other', ' White'), labels = c(1, 2, 3, 4, 5));
  d$Sexo = factor(d$Sexo, levels = c(' Female', ' Male'), labels = c(0, 1));
  d$Pais_origem = factor(d$Pais_origem, levels = c(' Cambodia', ' Canada', ' China', ' Columbia', ' Cuba', ' Dominican-Republic', ' Ecuador', ' El-Salvador', ' England', ' France', ' Germany', ' Greece', ' Guatemala', ' Haiti', ' Holand-Netherlands', ' Honduras', ' Hong', ' Hungary', ' India', ' Iran', ' Ireland', ' Italy', ' Jamaica', ' Japan', ' Laos', ' Mexico', ' Nicaragua', ' Outlying-US(Guam-USVI-etc)', ' Peru', ' Philippines', ' Poland', ' Portugal', ' Puerto-Rico', ' Scotland', ' South', ' Taiwan', ' Thailand', ' Trinadad&Tobago', ' United-States', ' Vietnam', ' Yugoslavia','0'), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,0));
  
  d[, 1] = scale(d[, 1], center = attributes(s1)$`scaled:center`[1], scale = attributes(s1)$`scaled:scale`[1]);
  d[, 3] = scale(d[, 3], center = attributes(s1)$`scaled:center`[2], scale = attributes(s1)$`scaled:scale`[2]);
  d[, 10:12] = scale(d[, 10:12], center = attributes(s1)$`scaled:center`[3:5], scale = attributes(s1)$`scaled:scale`[3:5]);
  
  predict(classi, newdata = d);
  
}

#plumb(file="teste/plumber.R")$run(port = 8888, swagger = TRUE);
