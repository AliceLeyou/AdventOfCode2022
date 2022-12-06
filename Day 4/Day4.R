library(readr)

toRange = function(assignment) {
  rangeVals = unlist(strsplit(assignment, split = "-"))
  return (rangeVals[1]:rangeVals[2])
}

doAssignmentsRelate = function(splittedAssignment, relationFn) {
  return (relationFn(toRange(splittedAssignment[[1]]) %in% toRange(splittedAssignment[[2]])) || 
            relationFn(toRange(splittedAssignment[[2]]) %in% toRange(splittedAssignment[[1]])))
}

assignments = read_lines("Input4", n_max = 1000)
splittedAssignments = strsplit(assignments, split = ",")

isAssignmentDuplicated = lapply(splittedAssignments, function(splittedAssignment) return (doAssignmentsRelate(splittedAssignment, all)))
print(length(isAssignmentDuplicated[isAssignmentDuplicated == TRUE]))

doAssignmentsOverlap = lapply(splittedAssignments, function(splittedAssignment) return (doAssignmentsRelate(splittedAssignment, any)))
print(length(doAssignmentsOverlap[doAssignmentsOverlap == TRUE]))