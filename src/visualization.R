

library(magrittr)
library(ggplot2)

df = readr::read_csv('data/simulation-results.csv')

df_long = df %>%
  tidyr::pivot_longer(cols = c('learning_hits', 'learning_obs', 'deployment_hits', 'deployment_obs'),
                      names_sep = '_', names_to = c('phase', 'metric')) %>%
  tidyr::pivot_wider(names_from = 'metric') 

# calculate model performance of the different strategies
# mystery: summarization produces >1 row p.g. so had to filter
strategies_statistics <- df_long %>% 
  dplyr::group_by(phase, strategy) %>% 
  dplyr::mutate(p = sum(hits) / sum(obs)) %>%
  dplyr::summarise(precision = mean(p),
                   se = p / sqrt(length(p))) %>% 
  dplyr::filter(seq_along(precision) == 1)

# order strategies by the one which works best on average
strategies_ordered = strategies_statistics %>% 
  dplyr::filter(phase == 'deployment') %>% 
  dplyr::arrange(precision) %>%
  print() %>%
  dplyr::pull(strategy)


#### Visualize results

ggplot(df_long) +
  geom_hline(yintercept = mean(get_data(1)$y), lty = 'dashed', size = 1) +
  geom_boxplot(aes(x = strategy, y = hits/obs, fill = algo)) +
  facet_wrap(~factor(phase, levels = c('learning', 'deployment')), ncol = 1, scales='free_x') +
  scale_y_continuous(name = 'Precision', labels = function(x) paste0(x*100, '%')) +
  scale_x_discrete(limits = strategies_ordered) +
  labs(x = NULL) +
  theme(legend.position = 'bottom') +
  theme(strip.text = element_text(face = 'bold'))

ggplot(df_long) +
  geom_hline(yintercept = mean(get_data(1)$y), lty = 'dashed', size = 1) +
  geom_boxplot(aes(x = strategy, y = hits/obs, fill = factor(phase, levels = c('learning', 'deployment')))) +
  facet_wrap(~algo, ncol = 1, scales='free_x') +
  scale_y_continuous(name = 'Precision', labels = function(x) paste0(x*100, '%')) +
  scale_x_discrete(limits = strategies_ordered) +
  labs(x = NULL) +
  labs(fill = 'phase') +
  theme(legend.position = 'bottom') +
  theme(strip.text = element_text(face = 'bold'))
