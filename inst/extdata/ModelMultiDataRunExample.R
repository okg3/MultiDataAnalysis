\dontrun{
#### Working with Multiple Datasets ####
# One x dataset vs groups
out <- ModelMultiData(x = xExample1, groups = groupsExample1)

# List of x datasets vs groups - same rownames
out <- ModelMultiData(x = list(xExample1, xExample2), groups = groupsExample1)

# List of x datasets vs groups - different rownames
out <- ModelMultiData(x = list(xExample1, xExample3), groups = groupsExample1)

# List of x datasets vs y - same rownames
out <- ModelMultiData(x = list(xExample1, xExample2), y = yExample1)

# List of x datasets vs y - y rownames differ, x rownames identical
out <- ModelMultiData(x = list(xExample1, xExample2), y = yExample2)

# List of x datasets vs y - x and y rownames differ
out <- ModelMultiData(x = list(xExample1, xExample3), y = yExample2)

# x vs y with groups
out <- ModelMultiData(x = list(xExample1, xExample2), y = yExample1,
                      groups = groupsExample1)

# x vs y with groups - mismatching columns
out <- ModelMultiData(x = list(xExample1, xExample2), y = yExample3,
                      groups = groupsExample2)

#### Adjusting the Formula ####
# x vs groups - change yName
out <- ModelMultiData(x = xExample1, groups = groupsExample1, yName = "group",
                      FUN = glm, family = binomial())

# x vs groups - exclude variables
out <- ModelMultiData(x = xExample1, groups = groupsExample1, yName = "group",
                      FUN = glm, family = binomial(),
                      excludeVars = "measure")

# x vs groups - include variables
out <- ModelMultiData(x = xExample1, groups = groupsExample1, yName = "group",
                      FUN = glm, family = binomial(),
                      includeVars = "batch")


# supply formula - x vs groups
out <- ModelMultiData(x = xExample1, groups = groupsExample1,
                      formula = measure ~ x + batch,
                      xName = "x")

# supply formula - list of x vs y - x names not provided
names(xListUnnamedExample)
out <- ModelMultiData(x = xListUnnamedExample, y = yExample1,
                      formula = y ~ x1 + x2)

# supply formula - list of x vs y - xNames provided
names(xListNamedExample)
out <- ModelMultiData(x = xListNamedExample, y = yExample1,
                      formula = y ~ first + second)

out <- ModelMultiData(x = xListUnnamedExample, y = yExample1,
                      formula = y ~ a + b, xName = c("a", "b"))

# supply formula - list of x vs y - yName provided
out <- ModelMultiData(x = xListUnnamedExample, y = yExample1,
                      yName = "outcome", formula = outcome ~ x1 + x2)

# supply formula - x vs y with groups
out <- ModelMultiData(x = xListUnnamedExample, y = yExample1,
                      groups = groupsExample1,
                      formula = y ~ x1 + x2 + batch)

# Change return variable to include all
out <- ModelMultiData(x = xListUnnamedExample, y = yExample1,
                      groups = groupsExample1,
                      formula = y ~ x1 + x2 + batch,
                      returnVars = "*")
# Change return variable to batch
out <- ModelMultiData(x = xListUnnamedExample, y = yExample1,
                      groups = groupsExample1,
                      formula = y ~ x1 + x2 + batch,
                      returnVars = "batch")

#### Comparisons ####
# Supply comparisons matrix
out <- ModelMultiData(x = xListUnnamedExample, y = yExample1,
                      groups = groupsExample1,
                      formula = y ~ x1 + x2 + batch,
                      comparisons = comparisonsExample)

#### Stratified analysis using by ####
# x vs y with groups and one by parameter
out <- ModelMultiData(x = xListUnnamedExample, y = yExample1,
                      groups = groupsExample1,
                      formula = y ~ x1 + x2 + measure,
                      by = "batch")

# x vs y with groups and two by parameters
out <- ModelMultiData(x = xListUnnamedExample, y = yExample1,
                      groups = groupsExample1,
                      formula = y ~ x1 + x2 + measure,
                      by = c("group","batch"))

}
