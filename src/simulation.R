

source('src/data-gen.R')
source('src/strategy.R')

seeds = 1:1000

strategies = c(
  'random'               
  ,'greedy'               
  ,'prob'                 
  ,'prob-sq'              
  ,'active'               
  ,'active-interquartile' 
  ,'active-semigreedy'    
  ,'active-greedy'        
  ,'active-prob'
)

algos = c('glm', 'tree')


gr = expand.grid(seed = seeds, strategy = strategies, algo = algos, stringsAsFactors = FALSE)

nc = parallel::detectCores() - 1 
cl = parallel::makeCluster(nc)
parallel::clusterExport(cl, varlist = ls())
res = parallel::clusterApplyLB(cl = cl, 
                               x = seeds,
                               fun = function(seed) { 
                                 tryCatch(expr = {
                                   df = get_data(seed = seed)
                                   g = gr[gr$seed == seed, ]
                                   res = lapply(seq_len(nrow(g)), function(i) {
                                     run_strategy(df = df,
                                                  strategy = g$strategy[i],
                                                  algo = g$algo[i],
                                                  seed = seed + 1,
                                                  verbose = FALSE)
                                   })
                                 }, error = function(e) c(NA, NA, NA, NA))
                                 df_res = do.call(rbind.data.frame, res)
                                 names(df_res) = unlist(lapply(c('learning', 'deployment'), function(x) paste(x, c('hits', 'obs'), sep = '_')))
                                 df_res = cbind.data.frame(g, df_res)
                                 return(df_res)
                               })
parallel::stopCluster(cl)

success = res[lapply(res, ncol) %>% (function(x) x == ncol(res[[1]]))]

df_res = do.call(rbind.data.frame, success)
df_res = tibble::as_tibble(df_res)

readr::write_csv(df_res, 'data/simulation-results.csv')
