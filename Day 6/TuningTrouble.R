library(readr)

getEndOfSignalMarker = function(signal, amountOfUniqueSymbolsRequired) {
  i = 1
  while(i <= length(signal)) {
    if(length(unique(signal[i:(i + amountOfUniqueSymbolsRequired - 1)])) == amountOfUniqueSymbolsRequired) {
      return (i + amountOfUniqueSymbolsRequired - 1)
    }
    i = i + 1
  }
}

signalInput = read_lines("Day 6/Input", n_max = 1)

signalChars = unlist(strsplit(signalInput, split = ""))
print(getEndOfSignalMarker(signalChars, 4))

print(getEndOfSignalMarker(signalChars, 14))