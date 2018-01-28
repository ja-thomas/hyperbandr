


hyperband = function(
  # hyperband params:
  max.ressources = 81, 
  prop.discard = 3,
  # new param:
  bracket.winner = TRUE,
  max.perf = TRUE,
  # Algorithm params:
  #configuration, 
  #initial.budget, 
  #init.fun,
  # bracket params:
  id, 
  par.set, 
  sample.fun, 
  train.fun, 
  performance.fun) { 
  # |sMax + 1| are the total number of brackets to try 
  sMax =  floor(log(max.ressources, base = prop.discard))
  # B are the total ressources to allocate each bracket
  B = (sMax + 1)*max.ressources
  # initialize a list for the best configuration of each bracket
  bracketWinners = as.list(numeric(sMax + 1))
  # begin hyperband
  for(s in sMax:0) {
    catf("Beginning with bracket %s", s)
    brack = bracket$new(
      id = id,
      par.set = config,
      sample.fun = sample.fun,
      train.fun = train.fun,
      performance.fun = performance.fun,
      s = s,
      B = B,
      max.ressources = max.ressources,
      prop.discard = prop.discard,
      max.perf = max.perf
    )
    brack$run()
    bracketWinners[[s + 1]] = brack
  }
  return(rev(bracketWinners))
}

