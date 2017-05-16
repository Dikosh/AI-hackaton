# Загружаем необходимые для нас библиотеки

# Если захотите запустить на своем сервере то надо скомпилировать нижеуказанный код убрав "#"
# Данные ПК: ОЗУ выше 32Гб, и если хотите запустить на своем ПК, то нужно будет скачать дополнительные библиотеки
# Многие библиотеки собирались из архивных источников CRAN
# Если возникнут проблемы 87072964488
# F-score 0.80-0.84 на тренировочных данных
install.packages("devtools")
library(devtools)
install_version("xgboost", version = "0.4-3", repos = "http://cran.us.r-project.org") # Пакет xgboost обязательно 0,4-3 версии должен быть
# это можно посмотреть вызвав session_info()
install.packages("jsonlite")
install.packages("tm")
install.packages("readr")
install.packages("tokenizers")
install.packages("caret")
install.packages("Matrix")
install.packages("mlr")
library(devtools)
library(jsonlite)
library(tm)
library(readr)
require(tokenizers)
library(tm)
library(caret)
library(xgboost)
library(Matrix)

session_info()
# Импортируем данные
train <- fromJSON("train.json")
test <- fromJSON("test.json")
test$text <- tokenize_word_stems(test$text)
train$text <- tokenize_word_stems(train$text)
combi <- train
combi_test <- test

######################## Багажник стоп слов
stop_words  <- c("c","а","алло","без","белый","близко","более","больше","большой","будем",
                 "будет","будете","будешь","будто","буду","будут","будь","бы","бывает","бывь",
                 "был","была","были","было","быть","в","важная","важное","важные","важный","вам",
                 "вами","вас","ваш","ваша","ваше","ваши","вверх","вдали","вдруг","ведь","везде",
                 "вернуться","весь","вечер","взгляд","взять","вид","видел","видеть","вместе","вне",
                 "вниз","внизу","во","вода","война","вокруг","вон","вообще","вопрос","восемнадцатый",
                 "восемнадцать","восемь","восьмой","вот","впрочем","времени","время","все","все еще",
                 "всегда","всего","всем","всеми","всему","всех","всею","всю","всюду","вся","всё",
                 "второй","вы","выйти","г","где","главный","глаз","говорил","говорит","говорить",
                 "год","года","году","голова","голос","город","да","давать","давно","даже","далекий",
                 "далеко","дальше","даром","дать","два","двадцатый","двадцать","две","двенадцатый",
                 "двенадцать","дверь","двух","девятнадцатый","девятнадцать","девятый","девять",
                 "действительно","дел","делал","делать","делаю","дело","день","деньги","десятый",
                 "десять","для","до","довольно","долго","должен","должно","должный","дом","дорога",
                 "друг","другая","другие","других","друго","другое","другой","думать","душа","е","его",
                 "ее","ей","ему","если","есть","еще")
stop_words2 <- c("ещё","ею","её","ж","ждать","же","жена","женщина","жизнь","жить","за","занят","занята",
                 "занято","заняты","затем","зато","зачем","здесь","земля","знать","значит","значить","и",
                 "иди","идти","из","или","им","имеет","имел","именно","иметь","ими","имя","иногда","их",
                 "к","каждая","каждое","каждые","каждый","кажется","казаться","как","какая","какой","кем",
                 "книга","когда","кого","ком","комната","кому","конец","конечно","которая","которого",
                 "которой","которые","который","которых","кроме","кругом","кто","куда","лежать","лет","ли",
                 "лицо","лишь","лучше","любить","люди","м","маленький","мало","мать","машина","между","меля",
                 "менее","меньше","меня","место","миллионов","мимо","минута","мир","мира","мне","много",
                 "многочисленная","многочисленное","многочисленные","многочисленный","мной","мною","мог",
                 "могу","могут","мож","может","может быть","можно","можхо","мои","мой","мор","москва",
                 "мочь","моя","моё","мы","на","наверху","над","надо","назад","наиболее","найти","наконец",
                 "нам","нами","народ","нас","начала","начать","наш","наша","наше","наши","не","него",
                 "недавно","недалеко","нее","ней","некоторый","нельзя","нем","немного","нему","непрерывно",
                 "нередко","несколько","нет","нею","неё","ни","нибудь","ниже","низко","никакой","никогда",
                 "никто","никуда","ним","ними","них","ничего","ничто","но","новый","нога","ночь","ну","нужно",
                 "нужный","нх","о","об","оба","обычно","один","одиннадцатый","одиннадцать","однажды","однако",
                 "одного","одной","оказаться","окно","около","он","она","они","оно","опять","особенно",
                 "остаться","от","ответить","отец","откуда","отовсюду","отсюда","очень")
stop_words1 <- c("первый","перед","писать","плечо","по","под","подойди","подумать","пожалуйста","позже",
                 "пойти","пока","пол","получить","помнить","понимать","понять","пор","пора","после",
                 "последний","посмотреть","посреди","потом","потому","почему","почти","правда","прекрасно",
                 "при","про","просто","против","процентов","путь","пятнадцатый","пятнадцать","пятый","пять",
                 "работа","работать","раз","разве","рано","раньше","ребенок","решить","россия","рука",
                 "русский","ряд","рядом","с","с кем","сам","сама","сами","самим","самими","самих",
                 "само","самого","самой","самом","самому","саму","самый","свет","свое","своего",
                 "своей","свои","своих","свой","свою","сделать","сеаой","себе","себя","сегодня",
                 "седьмой","сейчас","семнадцатый","семнадцать","семь","сидеть","сила","сих","сказал",
                 "сказала","сказать","сколько","слишком","слово","случай","смотреть","сначала","снова",
                 "со","собой","собою","советский","совсем","спасибо","спросить","сразу","стал","старый",
                 "стать","стол","сторона","стоять","страна","суть","считать","т","та","так","такая",
                 "также","таки","такие","такое","такой","там","твои","твой","твоя","твоё","те","тебе",
                 "тебя","тем","теми","теперь","тех","то","тобой")
stop_words3 <- c("тобою","товарищ","тогда","того","тоже","только","том","тому","тот","тою","третий",
                 "три","тринадцатый","тринадцать","ту","туда","тут","ты","тысяч","у","увидеть","уж",
                 "уже","улица","уметь","утро","хороший","хорошо","хотел бы","хотеть","хоть","хотя",
                 "хочешь","час","часто","часть","чаще","чего","человек","чем","чему","через",
                 "четвертый","четыре","четырнадцатый","четырнадцать","что","чтоб","чтобы","чуть",
                 "шестнадцатый","шестнадцать","шестой","шесть","эта","эти","этим","этими","этих",
                 "это","этого","этой","этом","этому","этот","эту","я","являюсь")

########################    
corpus <- Corpus(VectorSource(combi$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c(stopwords=stop_words))
corpus <- tm_map(corpus, removeWords, c(stopwords=stop_words1))
corpus <- tm_map(corpus, removeWords, c(stopwords=stop_words2))
corpus <- tm_map(corpus, removeWords, c(stopwords=stop_words3))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]
#document matrix
frequencies <- DocumentTermMatrix(corpus) 
rm(corpus)
sparse <- removeSparseTerms(frequencies, 1 - 3/nrow(frequencies))
newsparse <- as.data.frame(as.matrix(sparse))
dim(newsparse)


# Метим
colnames(newsparse) <- make.names(colnames(newsparse))
newsparse$cuisine <- as.factor(c(train$sentiment))
# Смотрим пропорции слов
table(newsparse$cuisine)
##################
newsparse_negative <- newsparse[newsparse$cuisine=="negative",]
newsparse_neutral <- newsparse[newsparse$cuisine=="neutral",]
newsparse_positive <- newsparse[newsparse$cuisine=="positive",]
library(devtools)
install.packages("SparkR")
session_info()
#split data 
#library(caret)
#negative
intrain<-createDataPartition(y=newsparse_negative$cuisine,p=0.01,list=FALSE)
newsparse_negative_2<-newsparse_negative[-intrain,]
newsparse_negative_t <- rbind(newsparse_negative,newsparse_negative_2)
#table(newsparse_negative_t$cuisine)
#neutral
#intrain<-createDataPartition(y=newsparse_neutral$cuisine,p=0.32,list=FALSE)
#newsparse_neutral_2<-newsparse_neutral[-intrain,]
#table(newsparse_neutral_2$cuisine)
### Neutral22
#neutral
intrain<-createDataPartition(y=newsparse_neutral$cuisine,p=0.28,list=FALSE)
newsparse_neutral_22<-newsparse_neutral[-intrain,]
#table(newsparse_neutral_22$cuisine)

#newsparse1 <- rbind(newsparse_negative_t,newsparse_neutral_2,newsparse_positive)

newsparse2 <- rbind(newsparse_negative_t,newsparse_neutral_22,newsparse_positive)
#table(newsparse2$cuisine)
##################



#Делим данные на тренировочные и тестируемые 

intrain<-createDataPartition(y=newsparse2$cuisine,p=0.8,list=FALSE)
mytrain<-newsparse2[intrain,]

#############
corpus <- Corpus(VectorSource(combi_test$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c(stopwords=stop_words))
corpus <- tm_map(corpus, removeWords, c(stopwords=stop_words1))
corpus <- tm_map(corpus, removeWords, c(stopwords=stop_words2))
corpus <- tm_map(corpus, removeWords, c(stopwords=stop_words3))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
#document matrix
frequencies_test <- DocumentTermMatrix(corpus) 
rm(corpus)
sparse <- removeSparseTerms(frequencies_test, 1 - 3/nrow(frequencies_test))
newsparse_test <- as.data.frame(as.matrix(sparse))
dim(newsparse_test)



#############
mytest<-newsparse_test


# Делаем матрицу из train test
ctrain <- xgb.DMatrix(Matrix(data.matrix(mytrain[,!colnames(mytrain) %in% c('cuisine')])), label = as.numeric(mytrain$cuisine)-1)
#advanced data set preparation
dtest <- xgb.DMatrix(Matrix(data.matrix(mytest[,!colnames(mytest) %in% c('cuisine')]))) 
watchlist <- list(train = ctrain, test = dtest)


# multiclass model using softmax
#first model
xgbmodel <- xgboost(data = ctrain, max.depth = 25, eta = 0.7,eval_metric="mlogloss", nround = 200, objective = "multi:softmax", num_class = 4, verbose = 1, watchlist = watchlist)
#second model
xgbmodel2 <- xgboost(data = ctrain, max.depth = 20, eta = 0.2, nrounds = 250, objective = "multi:softmax", num_class = 4, watchlist = watchlist)

#third model
xgbmodel3 <- xgboost(data = ctrain, max.depth = 25, gamma = 2, min_child_weight = 2, eta = 0.1, nround = 250, objective = "multi:softmax", num_class = 4, verbose = 2,watchlist = watchlist)

#xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(mytest[, !colnames(mytest) %in% c('cuisine')]))
#xgbmodel.predict.text <- levels(mytrain$cuisine)[xgbmodel.predict + 1]
#predict 2
#xgbmodel.predict2 <- predict(xgbmodel2, newdata = data.matrix(mytest[, !colnames(mytest) %in% c('cuisine')])) 
#xgbmodel.predict2.text <- levels(mytrain$cuisine)[xgbmodel.predict2 + 1]
#predict 3
#xgbmodel.predict3 <- predict(xgbmodel3, newdata = data.matrix(mytest[, !colnames(mytest) %in% c('cuisine')])) 
#xgbmodel.predict3.text <- levels(mytrain$cuisine)[xgbmodel.predict3 + 1]

xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(mytest_final))
xgbmodel.predict.text <- levels(mytrain$cuisine)[xgbmodel.predict + 1]
#predict 2
xgbmodel.predict2 <- predict(xgbmodel2, newdata = data.matrix(mytest_final)) 
xgbmodel.predict2.text <- levels(mytrain$cuisine)[xgbmodel.predict2 + 1]
#predict 3
xgbmodel.predict3 <- predict(xgbmodel3, newdata = data.matrix(mytest_final)) 
xgbmodel.predict3.text <- levels(mytrain$cuisine)[xgbmodel.predict3 + 1]
###
#data frame for predict 1
submit_match1 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict.text))
colnames(submit_match1) <- c('id','cuisine')
submit_match1 <- data.table(submit_match1, key = 'id')
#data frame for predict 2
submit_match2 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict2.text))
colnames(submit_match2) <- c('id','cuisine')
submit_match2 <- data.table(submit_match2, key = 'id')
#data frame for predict 3
submit_match3 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict3.text))
colnames(submit_match3) <- c('id','cuisine')
submit_match3 <- data.table(submit_match3, key = 'id')

#ensembling 
submit_match3$cuisine2 <- submit_match2$cuisine 
submit_match3$cuisine1 <- submit_match1$cuisine

#function to find the maximum value row wise
Mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}
x <- Mode(submit_match3[,c("cuisine","cuisine2","cuisine1")])
y <- apply(submit_match3,1,Mode)
final_submit <- data.frame(id= submit_match3$id, cuisine = y)
#view submission file
data.table(final_submit)
#final submission
write.csv(final_submit, 'ensemble.csv', row.names = FALSE)
table(final_submit$cuisine)
###


# Смотрим на Accuracy
#sum(diag(table(mytest$cuisine, xgbmodel.predict)))/nrow(mytest)
#sum(diag(table(mytest$cuisine, xgbmodel.predict3)))/nrow(mytest)
#sum(diag(table(mytest$cuisine, xgbmodel.predict2)))/nrow(mytest) 
# Model 1
#confusionMatrix(xgbmodel.predict.text,mytest$cuisine) # confusion matrix
#mat <-table(mytest$cuisine, xgbmodel.predict) # table TP TN FP FN
#(precision <- diag(mat) / rowSums(mat)) # Precision
#recall <- (diag(mat) / colSums(mat)) # Recall
#f_meas <- 2*precision*recall/(precision+recall) # F-measure
#mean(f_meas) # F measure средний общий
#f_meas # F measure по отдельности

