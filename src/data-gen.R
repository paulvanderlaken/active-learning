
make_group_df = function(n, name, x1m = 0, x1sd = 1, x2m = 0, x2sd = 1) {
  data.frame(
    x1 = rnorm(n, mean = x1m, sd = x1sd),
    x2 = rnorm(n, mean = x2m, sd = x2sd),
    group = name,
    y = grepl('fraude', name)
    )
}

get_data = function(seed = 1) {
  set.seed(seed)
  
  rnorm(1)
  
  dfs = list(
    make_group_df(5000, 'ok1'),
    make_group_df(3000, 'ok2',     x1m = rnorm(1), x2m = rnorm(1)),
    make_group_df(100,  'fraude1', x1m = rnorm(1), x2m = rnorm(1)),
    make_group_df(50,   'fraude2', x1m = rnorm(1), x2m = rnorm(1), x1sd = runif(1)),
    make_group_df(30,   'fraude3', x1m = rnorm(1), x2m = rnorm(1), x2sd = runif(1)),
    make_group_df(10,   'fraude4', x1m = rnorm(1), x2m = rnorm(1), x1sd = runif(1),  x2sd = runif(1)),
    NULL
  )

  df = do.call(rbind.data.frame, dfs)
  
  df$id = seq_len(nrow(df))
  
  return(df)
}
