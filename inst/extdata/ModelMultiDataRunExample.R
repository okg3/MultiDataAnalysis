\dontrun{
#### Working with Multiple Datasets ####

# Linear model with x and groups
out <- ModelMultiData(x = RNAEdDataQCed$Frequency, groups = RNAEdSampleInfo,
                      excludeVars = "status")
out <- out[order(out$fdr), ]
head(out)

# Logistic regression with x and groups
out <- ModelMultiData(formula = status ~ frequency, x = RNAEdDataQCed$Frequency,
                      groups = RNAEdSampleInfo, xName = "frequency",
                      FUN = glm, family = binomial())
out <- out[order(out$fdr), ]
head(out)

# Linear model with x and y
out <- ModelMultiData(x = RNAEdDataQCed$Frequency,
                      y = RNAEdDataQCed$`Coverage-q25`)
out <- out[order(out$fdr), ]
head(out)

# Model with list of x matrices and groups
out <- ModelMultiData(formula = status ~ frequency + coverage + age,
                      x = list(frequency = RNAEdDataQCed$Frequency,
                               coverage = RNAEdDataQCed$`Coverage-q25`),
                      groups = RNAEdSampleInfo, FUN = glm, family = binomial(),
                      returnVars = "*")
out <- out[order(out$fdr), ]
head(out)

# Stratified analysis using by
out <- ModelMultiData(formula = age ~ frequency + coverage + batch,
                      x = list(frequency = RNAEdDataQCed$Frequency,
                               coverage = RNAEdDataQCed$`Coverage-q25`),
                      groups = RNAEdSampleInfo, by = "status")
out <- lapply(out, function(x) x[order(x$fdr), ])
lapply(out, head)

# Linear model with x and y using comparisons parameter
head(RNAEdCombinations)
out <- ModelMultiData(formula = y ~ x1 + age,
                      x = RNAEdGenotypes, y = RNAEdDataQCed$Frequency,
                      groups = RNAEdSampleInfo, comparisons = RNAEdCombinations)
out <- out[order(out$fdr), ]
head(out)

}
