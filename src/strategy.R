library(ggplot2)

get_slope = function(mod) {
  coef(mod)[2]/(-coef(mod)[3])
}
get_intercept = function(mod) {
  coef(mod)[1]/(-coef(mod)[3])
}

draw_greedy = function(prob, n) {
  sort(prob, decreasing = TRUE)[seq_len(n)]
}
draw_probabilistic = function(prob, n) {
  sample(prob, size = n, prob = prob)
}
draw_closest = function(prob, n, to) {
  sort(abs(to - prob), decreasing = FALSE)[seq_len(n)]
}

train_model = function(df, algo, weighted = TRUE) {
  
  formula = y ~ x1 + x2
  
  if (weighted) {
    weights_table = 1 - prop.table(table(df$y))
    weights = weights_table[match(df$y, table = names(weights_table))]
  } else {
    weighs = rep(1, nrow(df))
  }
  
  if (algo == 'glm') {
    return(glm(formula, family = 'quasibinomial', data = df, weights = weights))
  } else if (algo == 'tree') {
    return(rpart::rpart(formula, data = df, method = 'class', weights = weights))
  }
}

get_predictions = function(mod, algo, ...) {
  if (algo == 'glm'){
    odds = predict(mod, ...)
    return(exp(odds)/(1+exp(odds)))
  } else if (algo == 'tree') {
    return(predict(mod, ..., type = 'prob')[ , 2])
  }
}


run_strategy =  function(df, strategy, algo = 'tree', iterations = 52, seed = 1, verbose = TRUE) {
  
  set.seed(seed)
  
  label_capacity = 8
  nonfraude_labeled = 50
  fraude_labeled = 10
  
  i = 1
  
  while (i <= iterations) {
    
    if (i == 1) {
      df$labeled = FALSE
      idx_to_label = c(sample(which(df$y == 0), nonfraude_labeled),
                       sample(which(df$y == 1), fraude_labeled))
      if (verbose) {
        df_grid = expand.grid(x1 = seq(min(df$x1), max(df$x1), length.out = 20),
                              x2 = seq(min(df$x2), max(df$x2), length.out = 20))
      }
    }
    
    df$labeled[df$id %in% idx_to_label] = TRUE
    
    in_train = df$labeled
    train = df[in_train, ]
    test = df[!in_train, ]
    
    mod = train_model(df = train, algo = algo)
    
    hits = sum(train$y)
    obs = nrow(train)
    precision = mean(train$y)
    
    if (verbose) {
      df_grid$y_pred = get_predictions(mod, newdata = df_grid, algo = algo)
      
      p = ggplot(df)
      
      p = p + geom_tile(aes(x1, x2, fill = y_pred), data = df_grid)
      
      p = p + geom_point(aes(x1, x2, col = as.factor(y)))
      
      subtitle = paste0('Process precision: ', round(precision, 3) * 100, '%', ' ', paste0('(', obs, '/', seen, ')'))
      
      p = p + 
        scale_color_manual(values = c(`FALSE` = 'darkgrey', `TRUE` = 'red')) +
        scale_fill_gradient(low = 'white', high = 'yellow', limits = c(0, 1)) +
        facet_wrap(~labeled) +
        labs(col = 'Fraude') +
        labs(title = paste('Iteration', i, '-', strategy)) +
        labs(subtitle = subtitle) +
        theme_minimal()
    }
    
    if (strategy == 'random') {
      idx_to_label = sample(test$id, size = label_capacity)
    } else {
      
      prob = get_predictions(mod, newdata = test, algo = algo)
      names(prob) = test$id
      
      if (strategy == 'greedy') {
        uitval = draw_greedy(prob, label_capacity)
      } else if (strategy == 'prob') {
        uitval = draw_probabilistic(prob, n = label_capacity)
      } else if (strategy == 'prob-sq') {
        uitval = draw_probabilistic(prob^2, n = label_capacity)
      } else if (strategy == 'active') {
        uitval = draw_closest(prob, label_capacity, to = 0.5)
      } else if (strategy == 'active-interquartile') {
        prob = setNames(as.numeric(prob > 0.25 & prob < 0.75) + 0.01, nm = names(prob))
        uitval = draw_probabilistic(prob = prob, n = label_capacity)
      } else if (strategy == 'active-semigreedy') {
        dist_to_boundary = 0.5 - prob
        dist_to_boundary[dist_to_boundary < 0] = 0.5
        metric = 0.5 - dist_to_boundary
        uitval = draw_probabilistic(prob = 0.01 + metric^2, label_capacity)
      } else if (strategy == 'active-greedy') {
        uitval = c(draw_closest(prob, n = label_capacity/2, to = 0.5), 
                draw_greedy(prob, n = label_capacity/2))
      } else if (strategy == 'active-prob') {
        dist_to_boundary = abs(0.5 - prob)
        uitval = draw_probabilistic(prob = 0.5 - dist_to_boundary, n = label_capacity)
      }
      idx_to_label = as.integer(names(uitval))
    }
    
    i = i + 1
  }
  
  test$predictions = get_predictions(mod = mod, algo = algo, newdata = test)
  test = test[order(-test$predictions), ]
  remaining_fraude = sum(test$y)
  remaining_obs = nrow(test)
  remaining_hits = sum(test$y[seq_len(remaining_fraude)])
  remaining_precision = remaining_hits / remaining_fraude
  
  if (verbose) {
    subtitle2 = paste0(
      subtitle,
      ' -- ', 'Model precision: ', round(remaining_precision, 3) * 100, '%', 
      ' ', paste0('(', remaining_fraude, '/', remaining_obs, ')')
    )
    p = p + labs(subtitle = subtitle2)
    print(p)
  }
  
  return(c(hits, obs, remaining_hits, remaining_fraude))
}




