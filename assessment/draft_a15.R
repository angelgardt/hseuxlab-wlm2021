library(tidyverse)

# 1

bf <- read_tsv('/Users/antonangelgardt/Downloads/FFM/big_five_bffm.csv')

sapply(sapply(bf, is.na), sum)
bf %>% na.omit() -> bf
str(bf)

## Где-то тут они ещё могут отобрать 10 000 / 100 000 / 300 000 случайных наблюдений,
## и делать домашку на них
## если машина не вывозит 400 Мб данных


# 2
corrplot::corrplot(cor(bf %>% select(paste0('EXT', 1:10))))
corrplot::corrplot(cor(bf %>% select(paste0('EST', 1:10))))
corrplot::corrplot(cor(bf %>% select(paste0('AGR', 1:10))))
corrplot::corrplot(cor(bf %>% select(paste0('CSN', 1:10))))
corrplot::corrplot(cor(bf %>% select(paste0('OPN', 1:10))))

qgraph::qgraph(cor(bf %>% select(1:50)), layout = 'spring')


# 3
## НАДО ПЕРЕКОДИРОВАТЬ ОБРАТНЫЕ ВОПРОСЫ, но мне лень…


# ------
tibble(item_in_scale = 1:10,
       EXT = c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
       EST = c(-1, 1, -1, 1, -1, -1, -1, -1, -1, -1),
       AGR = c(-1, 1, -1, 1, -1, 1, -1, 1, 1, 1),
       CSN = c(1, -1, 1, -1, 1, -1, 1, -1, 1, 1),
       OPN = c(1, -1, 1, -1, 1, -1, 1, 1, 1, 1)) %>% write_tsv('direction_matrix_bffm.csv')


# ----
library(lavaan)
library(semPlot)


# 4

mdl1 <- "
ext =~ EXT1 + EXT2 + EXT3 + EXT4 + EXT5 + EXT6 + EXT7 + EXT8 + EXT9 + EXT10
est =~ EST1 + EST2 + EST3 + EST4 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10
agr =~ AGR1 + AGR2 + AGR3 + AGR4 + AGR5 + AGR6 + AGR7 + AGR8 + AGR9 + AGR10
csn =~ CSN1 + CSN2 + CSN3 + CSN4 + CSN5 + CSN6 + CSN7 + CSN8 + CSN9 + CSN10
opn =~ OPN1 + OPN2 + OPN3 + OPN4 + OPN5 + OPN6 + OPN7 + OPN8 + OPN9 + OPN10
"

model1 <- cfa(mdl1, data = bf %>% select(1:50))


# 5

fitmeasures(model1, c('chisq', 'cfi', 'tli', 'srmr', 'rmsea'))


# 6
summary(model1)


# 7

semPaths(model1, 'std')

# 8

modificationindices(model1) %>% arrange(desc(mi)) %>% head(5)


# 9

mdl2 <- "
ext =~ EXT1 + EXT2 + EXT3 + EXT4 + EXT5 + EXT6 + EXT7 + EXT8 + EXT9 + EXT10
est =~ EST1 + EST2 + EST3 + EST4 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10
agr =~ AGR1 + AGR2 + AGR3 + AGR4 + AGR5 + AGR6 + AGR7 + AGR8 + AGR9 + AGR10
csn =~ CSN1 + CSN2 + CSN3 + CSN4 + CSN5 + CSN6 + CSN7 + CSN8 + CSN9 + CSN10
opn =~ OPN1 + OPN2 + OPN3 + OPN4 + OPN5 + OPN6 + OPN7 + OPN8 + OPN9 + OPN10
EST7 ~~  EST8
OPN1 ~~  OPN8
OPN2 ~~  OPN4
"


model2 <- cfa(mdl2, data = bf %>% select(1:50))

fitmeasures(model2, c('chisq', 'cfi', 'tli', 'srmr', 'rmsea'))
summary(model2)

semPaths(model2, 'std')


# 10

anova(model2, model1)
