library(readr)
library(purrr)

parseInstruction = function(instruction) {
  words = unlist(strsplit(instruction, split = " "))
  return (c(as.numeric(words[2]), as.numeric(words[4]), as.numeric(words[6])))
}

moveCrate = function(crateStacks, from, to) {
  elementToMove = crateStacks[[from]][length(crateStacks[[from]])]
  crateStacks[[from]] = crateStacks[[from]][-length(crateStacks[[from]])]
  crateStacks[[to]] = append(crateStacks[[to]], elementToMove)
  return (crateStacks)
}

moveMultipleCrates = function(crateStacks, crateAmount, from, to) {
  for(i in 1:crateAmount) {
    crateStacks = moveCrate(crateStacks, from, to)
  }
  return (crateStacks)
}

lines = read_lines("Day 5/Input", n_max = 8)
instructions = read_lines("Day 5/Input", skip = 10, n_max = 502)

stacks = vector("list", length = 9)

for(line in rev(lines)) {
  stackIndex = 1
  charIndex = 2
  chars = unlist(strsplit(line, split = ""))
  while(charIndex <= length(chars)) {
    if(chars[charIndex] != " ") {
      stacks[[stackIndex]] = append(stacks[[stackIndex]], chars[charIndex])
    }
    stackIndex = stackIndex + 1
    charIndex = charIndex + 4
  }
}

parsedInstructions = lapply(instructions, parseInstruction)

stacks9000 = stacks
for(instruction in parsedInstructions) {
  stacks9000 = moveMultipleCrates(stacks9000, instruction[1], instruction[2], instruction[3])
}
print(reduce(stacks9000, function(acc, value) return (paste(acc, value[length(value)], sep = "")), .init = ""))

moveMultipleCratesAsModel9001 = function(crateStacks, crateAmount, from, to) {
  stackLength = length(crateStacks[[from]])
  elementsToMove = crateStacks[[from]][(stackLength - crateAmount + 1):stackLength]
  crateStacks[[from]] = crateStacks[[from]][(-stackLength + crateAmount - 1):-stackLength]
  crateStacks[[to]] = append(crateStacks[[to]], elementsToMove)
  return (crateStacks)
}

stacks9001 = stacks
for(instruction in parsedInstructions) {
  stacks9001 = moveMultipleCratesAsModel9001(stacks9001, instruction[1], instruction[2], instruction[3])
}
print(reduce(stacks9001, function(acc, value) return (paste(acc, value[length(value)], sep = "")), .init = ""))