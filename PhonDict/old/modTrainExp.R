library(stringr)

te <- readLines("train_exp.txt")
head(te, 50)

prob <- str_extract(te,  "FOO: ([0-9.]+)")
prob <- str_replace(prob, "FOO: ", "PROB ")
loc <- which(!is.na(prob))

for (ll in rev(loc)) {
    print(ll)
    te <- append(te, prob[ll], after=ll)
}

head(te, 50)

## write out the new file
fileConn <- file("newtrain_exp.txt")
writeLines(te, fileConn)
close(fileConn)
