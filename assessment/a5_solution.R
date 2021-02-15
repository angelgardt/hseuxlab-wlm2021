library(tidyverse)
theme_set(theme_bw())

# 1
### грузим данные
base_time <- readxl::read_excel('obr_base.xlsx', 2, skip = 1)
base_acc <- readxl::read_excel('obr_base.xlsx', 1, skip = 1)
super_time <- readxl::read_excel('obr_super.xlsx', 2, skip = 1)
super_acc <- readxl::read_excel('obr_super.xlsx', 1, skip = 1)


# 2
### выбираем нужное
base_time %>%
  slice(1:24) -> base_time
base_acc %>% 
  slice(1:24) %>% 
  select(20:21) -> base_socdem
base_acc %>% 
  slice(1:24) %>% 
  select(1:17) -> base_acc

super_acc %>%
  select(20:21) -> super_socdem
super_acc %>%
  slice(1:24) %>%
  select(1:17) -> super_acc


# 3, 4, 5
base_time %>% 
  bind_rows(super_time) %>% # соединяет датасеты времени реакции
  mutate(group = rep(c('base', 'super'), each = 24)) %>%  # добоавляем переменную group
  rename(id = `№_resp`) %>% # переименовываем переменную-идентификатор
  mutate(id = 1:48) %>% # делаем идентификаторы уникальными
  pivot_longer(cols = -c('id', 'group'), values_to = 'reaction_time') %>% # переводим в длинный формат
  inner_join( # будем соединять с датасетом точности
    base_acc %>% # препроцессим датасеты точности прямо внутри функции
      bind_rows(super_acc) %>% # соединяем датасеты точности
      rename(id = `№_resp`) %>% # переименовываем переменную-идентификатор
      mutate(id = 1:48) %>% # делаем идентификаторы уникальными
      pivot_longer(cols = -id, values_to = 'accuracy') # переводим в длинный формат
  ) %>% 
  inner_join( # добавляем соцдем
    base_socdem %>% # препроцессим датасеты соцдема прямо внутри функции
      bind_rows(super_socdem) %>% # соединяем датасеты соцдема
      rename(sex = ...1, # переименовываем переменные
             age = ...2) %>% 
      mutate(id = 1:48) # добавляем идентификатор
  ) %>% 
  separate(name, c('memory_setsize', 'visual_setsize'), '_stim_') -> hybrid # разделяем колонку с условиями на две и сохраняем объект

str(hybrid)


# 6

hybrid %>% 
  group_by(memory_setsize, # группируем по трём переменным
           visual_setsize,
           group) %>% 
  summarise(min = min(reaction_time), # считаем минимум
            mean = mean(reaction_time), # считаем среднее
            max = max(reaction_time)) # считаем максимум

# 7

hybrid %>% 
  filter(memory_setsize == 3 & visual_setsize == 8) %>% 
  ggplot(aes(age, reaction_time)) +
  geom_point() +
  geom_smooth(method="lm")


#8

hybrid %>% 
  filter(memory_setsize == 3 & visual_setsize == 8) %>% 
  ggplot(aes(age, reaction_time, color = group)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values = c('blue', 'orange'))


# 9

hybrid %>% 
  ggplot(aes(accuracy,
             fill = interaction(visual_setsize, memory_setsize))) +
  geom_histogram(binwidth = 0.05) +
  facet_grid(visual_setsize ~ memory_setsize) +
  guides(fill = FALSE)


# 10

hybrid %>% 
  ggplot(aes(group, reaction_time, color = sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Уровень категорий",
       y = "Время реакции",
       color = "Пол")


# 11

gridExtra::grid.arrange(
  hybrid %>% 
    ggplot(aes(visual_setsize, accuracy)) +
    stat_summary(fun = mean, geom = 'point', size = 2) +
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar'),
  hybrid %>% 
    ggplot(aes(memory_setsize, accuracy)) +
    stat_summary(fun = mean, geom = 'point', size = 2) +
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar')
)


# 12

group_labs <- c(base = "Базовые категории", super = "Суперординатные категории")
pd <- position_dodge(0.7)

hybrid %>% ggplot(aes(memory_setsize, reaction_time * 1000,
                         color = as_factor(visual_setsize),
                         group = visual_setsize)) +
  stat_summary(fun = mean, geom = 'line', position = pd, linetype = 'dotted') +
  stat_summary(fun = mean, geom = 'point', size = 3, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', position = pd) +
  facet_wrap(~group, labeller = labeller(group = group_labs)) +
  theme(legend.position = 'bottom') +
  xlab('Количество категорий для запоминания') +
  ylab('Время реакции (мс)') +
  scale_color_manual(name = 'Количество стимулов в пробе',
                     values = c("darkred", "darkorange", "darkgreen", "darkblue")) +
  scale_shape_discrete(name = 'Тип категории', labels = c("Базовая", "Суперординатная")) +
  labs(caption = "отображён 95% доверительный интервал")
