library(readr)

splitBackpacks = function(backpack) {
  return (c(substr(backpack, 1, nchar(backpack) / 2), substr(backpack, nchar(backpack) / 2 + 1, nchar(backpack))))
}

findCommonItems = function(compartments) {
  common = c()
  for(item in unlist(strsplit(compartments[1], split = ""))) {
    for(com in unlist(strsplit(compartments[2], split = ""))) {
      if(item == com) {
        common = append(common, item)
      }
    }
  }
  return (unique(common))
}

convertItemToPriority = function(item) {
  if(item == toupper(item)) {
    return (utf8ToInt(item) - 38)
  }
  return (utf8ToInt(item) - 96)
}


backpacks = read_lines("Input3", n_max=300)
splitBackpacks = lapply(backpacks, splitBackpacks)
commonItems = lapply(splitBackpacks, findCommonItems)
priorities = lapply(commonItems, convertItemToPriority)

print(sum(as.numeric(priorities)))

findCommonItemsInGroup = function(group) {
  common = c()
  sortedGroup = lapply(group, function(gr) return ((sort(unlist(strsplit(gr, split = ""))))))
  
  i = 1
  j = 1
  k = 1
  
  while(i <= length(sortedGroup[[1]]) && j <= length(sortedGroup[[2]]) && k <= length(sortedGroup[[3]])) {
    if(sortedGroup[[1]][i] == sortedGroup[[2]][j] && sortedGroup[[1]][i] == sortedGroup[[3]][k]) {
      common = append(common, sortedGroup[[1]][i])
      i = i + 1
      j = j + 1
      k = k + 1
    } else if(sortedGroup[[1]][i] < sortedGroup[[2]][j]) {
      i = i + 1
    } else if(sortedGroup[[2]][j] < sortedGroup[[3]][k]) {
      j = j + 1
    } else {
      k = k + 1
    }
  }
  
  return (unique(common))
}

groups = split(backpacks, ceiling(seq_along(backpacks) / 3))
groupBadge = lapply(groups, findCommonItemsInGroup)
groupPriorities = lapply(groupBadge, convertItemToPriority)
print(sum(as.numeric(groupPriorities)))