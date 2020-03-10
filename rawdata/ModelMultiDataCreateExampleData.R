xExample1 <- matrix(rep.int(rnorm(10, mean = 0, sd = 1), 100),
             nrow = 100, ncol = 10,
             dimnames = list(
               paste0("event", 1:100),
               paste0("ind", 1:10)))

xExample2 <- matrix(rep.int(rnorm(10, mean = 0, sd = 1), 100),
             nrow = 100, ncol = 10,
             dimnames = list(
               paste0("event", 1:100),
               paste0("ind", 1:10)))

xExample3 <- matrix(rep.int(rnorm(10, mean = 0, sd = 1), 10),
             nrow = 10, ncol = 10,
             dimnames = list(
               paste0("event", 1:10),
               paste0("ind", 1:10)))


yExample1 <- matrix(rep.int(rnorm(10, mean = 0, sd = 1), 100),
            nrow = 100, ncol = 10,
            dimnames = list(
              paste0("event", 1:100),
              paste0("ind", 1:10)))

yExample2 <- matrix(rep.int(rnorm(10, mean = 0, sd = 1), 10),
            nrow = 10, ncol = 10,
            dimnames = list(
              paste0("outcome", 1:10),
              paste0("ind", 1:10)))

yExample3 <- matrix(rep.int(rnorm(9, mean = 0, sd = 1), 100),
            nrow = 100, ncol = 9,
            dimnames = list(
              paste0("event", 1:100),
              paste0("ind", 1:9)))

groupsExample1 <- data.table(
  sample = paste0("ind", 1:10),
  measure = rnorm(10, mean = 20, sd = 2),
  group = sample(x = c("a", "b"), size = 10, replace = TRUE, prob = c(.5,.5)),
  batch = sample(x = c("1", "2"), size = 10, replace = TRUE, prob = c(.5,.5))
)

groupsExample2 <- data.table(
  sample = paste0("ind", 2:10),
  measure = rnorm(9, mean = 20, sd = 2),
  group = sample(x = c("a", "b"), size = 9, replace = TRUE, prob = c(.5,.5)),
  batch = sample(x = c("1", "2"), size = 9, replace = TRUE, prob = c(.5,.5))
)

xListNamedExample <- list(first = xExample1, second = xExample2)
xListUnnamedExample <- list(xExample1, xExample2)

comparisonsExample <- cbind(sample(paste0("event", 1:100), 10),
                     sample(paste0("event", 1:100), 10),
                     sample(paste0("event", 1:100), 10))
colnames(comparisonsExample) <- c("y", "x1", "x2")

save.image(file = "ModelMultiDataExampleData.rda")

