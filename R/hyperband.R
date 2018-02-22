


hyperband = function(
  # hyperband params:
  max.perf = TRUE,
  max.ressources = 81, 
  prop.discard = 3,
  id, 
  par.set, 
  sample.fun,
  #init.fun,
  train.fun,
  performance.fun
  ) { 
  # |sMax + 1| are the total number of brackets to try 
  sMax =  floor(log(max.ressources, base = prop.discard))
  B = (sMax + 1)*max.ressources
  # initialize a list for the best configuration of each bracket
  bracketWinners = as.list(numeric(sMax + 1))
  # begin hyperband
  for(s in sMax:0) {
    catf("Beginning with bracket %s", s)
    brack = bracket$new(
      max.perf = max.perf,
      max.ressources = max.ressources,
      prop.discard = prop.discard,
      s = s,
      B = B, 
      id = id,
      par.set = par.set,
      sample.fun = sample.fun,
      train.fun = train.fun,
      performance.fun = performance.fun
    )
    brack$run()
    bracketWinners[[s + 1]] = brack
  }
  return(rev(bracketWinners))
}

