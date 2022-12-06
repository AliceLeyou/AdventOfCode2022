library(readr)

winning = c("A Y", "B Z", "C X")
loosing = c("A Z", "B X", "C Y")

rockCombos = c("A Y", "B X", "C Z")
paperCombos = c("B Y", "C X", "A Z")

getOutcomeScore = function(guide) {
  if(guide %in% winning) return (6)
  if(guide %in% loosing) return (0)
  return (3)
}

getStrategyScore = function(guide) {
  return (ifelse(substr(guide, 3, 3) == 'X', 1, ifelse(substr(guide, 3, 3) == 'Y', 2, 3)))
}

getDesiredOutcomeScore = function(guide) {
  return (ifelse(substr(guide, 3, 3) == 'X', 0, ifelse(substr(guide, 3, 3) == 'Y', 3, 6)))
}

getDesiredOutcomeStrategyScore = function(guide) {
  if(guide %in% rockCombos) return (1)
  if(guide %in% paperCombos) return (2)
  return (3)
}

strategyGuide = read_lines("Input2", n_max = 2501)

partOneStrategyGuide = lapply(strategyGuide, FUN = function(guide) return (getStrategyScore(guide) + getOutcomeScore(guide)))
print(sum(as.numeric(partOneStrategyGuide)))

partTwoStrategyGuide = lapply(strategyGuide, FUN = function(guide) return (getDesiredOutcomeStrategyScore(guide) + getDesiredOutcomeScore(guide)))
print(sum(as.numeric(partTwoStrategyGuide)))