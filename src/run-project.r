setwd("E:\\Dropbox\\Kaggle\\05_Titanic\\kaggle-titanic")
library('ProjectTemplate')
load.project()

# Выживаемость в зависимости от пола пассажира
qplot(factor(survived), data = train, geom="bar", fill = factor(sex))
table(train$sex, train$survived)
# На странице конкурса есть бенчмарк, который использует большое количество
# выживших среди женщин и не выживших среди мужчин
# женщина? - возвращается 1, мужчина? - возвращается 0.

# Выживаемость по возрасту в начальных данных
qplot(factor(age), data = train[complete.cases(train),], geom="bar", fill = factor(survived))
# Выживаемость по возрасту после замены отсутствующих данных медианными значениями
qplot(factor(age), data = clean.train, geom="bar", fill = factor(survived))
# Большой пик, его надо сгладить, распределяя отсутствующие данные по возрастам посложнее

# Выживаемость по классу пассажира
qplot(factor(pclass), data = train[complete.cases(train),], geom="bar", fill = factor(survived))
# Пассажиры третьего класса выживают меньше. Чем меньше класс пассажира, тем больше выживаемость
qplot(factor(survived), data = train[complete.cases(train),], geom="bar", fill = factor(pclass))

# Выживаемость по уровню каюты. А - наверху, G - в самом низу.
# Странно, что выживаемость не зависит от уровня каюты, так, как я ожидал
# (чем ниже уровень, тем меньше выживаемость)
cabin.nums <- which(train$cabin != "")
cabin.names <- sapply(train$cabin[cabin.nums], substr, start = 1, stop = 1)
attr(cabin.names, "names") <- NULL
cabin.names <- as.factor(cabin.names)
qplot(cabin.names, data = train[cabin.nums,], geom="bar", fill = factor(survived))