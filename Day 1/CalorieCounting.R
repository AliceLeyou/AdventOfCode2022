library(readr)

calories = read_lines("Day 1/Input", n_max = 2256)

caloriesByElf = c()

currentElf = 1
for(calorie in calories) {
  if(is.na(as.numeric(calorie))) {
    currentElf = currentElf + 1
    next
  }
  if(length(caloriesByElf) < currentElf) {
    caloriesByElf = append(caloriesByElf, as.numeric(calorie))
  } else {
    caloriesByElf[currentElf] = caloriesByElf[currentElf] + as.numeric(calorie)
  }
}

print(max(caloriesByElf))
print(sum(sort(caloriesByElf, decreasing = TRUE)[1:3]))