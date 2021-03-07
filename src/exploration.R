
source('src/data-gen.R')
source('src/strategy.R')


seed = 830
df = get_data(seed = seed)
cat(round(mean(df$y), 3)*100, '% fraude')



ggplot(df) +
  geom_point(aes(x1, x2, col = y)) +
  facet_wrap(~group)

# ggplot(tidyr::pivot_longer(df, cols = c(x1, x2))) +
#   geom_density(aes(value, fill = y, group = group),  alpha = 0.5) +
#   scale_fill_manual(values = c(`FALSE` = 'darkgrey', `TRUE` = 'red')) +
#   facet_wrap(~name)

algo = 'glm'

run_strategy(df, strategy = 'random'                , algo = algo, seed = seed + 1, verbose = FALSE)
run_strategy(df, strategy = 'greedy'                , algo = algo, seed = seed + 1, verbose = FALSE)
run_strategy(df, strategy = 'prob'                  , algo = algo, seed = seed + 1, verbose = FALSE)
run_strategy(df, strategy = 'prob-sq'               , algo = algo, seed = seed + 1, verbose = FALSE)
run_strategy(df, strategy = 'active'                , algo = algo, seed = seed + 1, verbose = FALSE)
run_strategy(df, strategy = 'active-interquartile'  , algo = algo, seed = seed + 1, verbose = FALSE)
run_strategy(df, strategy = 'active-semigreedy'     , algo = algo, seed = seed + 1, verbose = FALSE)
run_strategy(df, strategy = 'active-greedy'         , algo = algo, seed = seed + 1, verbose = FALSE)
run_strategy(df, strategy = 'active-prob'           , algo = algo, seed = seed + 1, verbose = FALSE)

