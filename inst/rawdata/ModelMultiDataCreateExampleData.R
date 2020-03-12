xExample1 <- matrix(replicate(100, rnorm(20, mean = 0, sd = 1)),
             nrow = 100, ncol = 20, byrow = TRUE,
             dimnames = list(
               paste0("observation", 1:100),
               paste0("sample", 1:20)))

xExample2 <- matrix(replicate(100, rnorm(20, mean = 0, sd = 1)),
                    nrow = 100, ncol = 20, byrow = TRUE,
                    dimnames = list(
                      paste0("observation", 1:100),
                      paste0("sample", 1:20)))

xExample3 <- matrix(replicate(10, rnorm(20, mean = 0, sd = 1)),
                    nrow = 10, ncol = 20, byrow = TRUE,
                    dimnames = list(
                      paste0("observation", 1:10),
                      paste0("sample", 1:20)))


yExample1 <- matrix(replicate(100, rnorm(20, mean = 0, sd = 1)),
                    nrow = 100, ncol = 20, byrow = TRUE,
                    dimnames = list(
                      paste0("observation", 1:100),
                      paste0("sample", 1:20)))

yExample2 <- matrix(replicate(10, rnorm(20, mean = 0, sd = 1)),
                    nrow = 10, ncol = 20, byrow = TRUE,
                    dimnames = list(
                      paste0("outcome", 1:10),
                      paste0("sample", 1:20)))

yExample3 <- matrix(replicate(100, rnorm(19, mean = 0, sd = 1)),
                    nrow = 100, ncol = 19, byrow = TRUE,
                    dimnames = list(
                      paste0("observation", 1:100),
                      paste0("sample", 1:19)))

groupsExample1 <- data.table::data.table(
  sample = paste0("sample", 1:20),
  measure = rnorm(20, mean = 20, sd = 2),
  group = sample(x = c("Treated", "Untreated"), size = 20,
                 replace = TRUE, prob = c(.5,.5)),
  batch = sample(x = 1:4, size = 20, replace = TRUE,
                 prob = rep.int(.25, 4))
)

groupsExample2 <- data.table::data.table(
  sample = paste0("sample", 2:20),
  measure = rnorm(19, mean = 20, sd = 2),
  group = sample(x = c("Treated", "Untreated"), size = 19,
                 replace = TRUE, prob = c(.5,.5)),
  batch = sample(x = 1:4, size = 19, replace = TRUE,
                 prob = rep.int(.25, 4))
)

xListNamedExample <- list(first = xExample1, second = xExample2)
xListUnnamedExample <- list(xExample1, xExample2)

comparisonsExample <- cbind(sample(paste0("observation", 1:100), 10),
                     sample(paste0("observation", 1:100), 10),
                     sample(paste0("observation", 1:100), 10))
colnames(comparisonsExample) <- c("y", "x1", "x2")

comparisonsExample2 <- comparisonsExample
colnames(comparisonsExample2) <- c("y", "test1", "test2")

comparisonsExample3 <- cbind(sample(1:100, 10),
                             sample(1:100, 10),
                             sample(1:100, 10))
colnames(comparisonsExample3) <- c("y", "x1", "x2")


save.image(file = "ModelMultiDataExampleData.rda")

