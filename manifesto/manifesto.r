library(foreign)
data <- read.dta('MPDataset.dta')
sweden <- droplevels(subset(data, country == 'sweden'))
levels(sweden$party) <- c("MP",
                          "V",
                          "S",
                          "FP",
                          "KD",
                          "M",
                          "SD",
                          "C",
                          "NyD")
sweden$party <- factor(sweden$party, levels=levels(sweden$party)[c(2,3,1,8,5,4,6,7,9)])
colors <- c("#b70410", "#f9232b", "#79cf49", "#00993c", "#211974",
            "#5cb7e9", "#0049d8", "#dedd37", "#ffff41")
# These are the additive scaled dimensions from Lowe et.al., pp. 135 sqq.
freemarket.l <- c(401, 402)
freemarket.r <- c(403, 412, 413, 415)
environment.l <- c(501, 416)
environment.r <- c(410)
stateconomy.l <- c(403, 404, 406, 412, 413, 504, 506, 701)
stateconomy.r <- c(401, 402, 407, 414, 505)
stateservices.l <- c(504, 506)
stateservices.r <- c(505, 507)
logrile.l <- c(103, 105, 106, 107, 202, 403, 404, 406, 412, 413, 504, 506, 701)
logrile.r <- c(104, 201, 203, 305, 401, 402, 407, 414, 505, 601, 603, 605, 606)
logplaneco.l <- c(403, 404, 412)
logplaneco.r <- c(401, 414)
libcons.l <- c(103, 105, 106, 107, 202)
libcons.r <- c(104, 201, 203, 305, 601, 603, 605, 606)
logitm <- function(dataset, vars.l, vars.r) {
    log(rowSums(dataset[paste0("per", vars.l)] + 0.5) / rowSums(dataset[paste0("per", vars.r)] + 0.5))
}
sweden$freemarket <- logitm(sweden, freemarket.l, freemarket.r)
sweden$environment <- logitm(sweden, environment.l, environment.r)
sweden$stateconomy <- logitm(sweden, stateconomy.l, stateconomy.r)
sweden$stateservices <- logitm(sweden, stateservices.l, stateservices.r)
sweden$logrile <- -logitm(sweden, logrile.l, logrile.r)
sweden$logplaneco <- logitm(sweden, logplaneco.l, logplaneco.r)
sweden$libcons <- logitm(sweden, libcons.l, libcons.r)
# These are logit scales for bipolar categories (again, Lowe et.al., pp. 131 sqq.)
logits <- function(dataset, var.l, var.r) {
    log(rowSums(dataset[paste0("per", var.l)] + 0.5) / rowSums(dataset[paste0("per", var.r)] + 0.5))
}
sweden$foreignalliances <- logits(sweden, 101, 102)
sweden$militarism <- -logits(sweden, 105, 104)
sweden$internationalism <- logits(sweden, 107, 109)
sweden$logeu <- logits(sweden, 108, 110)
sweden$constitutionalism <- logits(sweden, 203, 204)
sweden$decentralization <- logits(sweden, 301, 302)
sweden$protectionism <- logits(sweden, 406, 407)
sweden$keynesian <- logits(sweden, 409, 414)
sweden$nationalism <- -logits(sweden, 602, 601)
sweden$morality <- -logits(sweden, 604, 603)
sweden$multiculturalism <- logits(sweden, 607, 608)
sweden$laborpolicy <- logits(sweden, 701, 702)
sweden$logwelfare <- logits(sweden, 504, 505)
sweden$education <- logits(sweden, 506, 507)

## TODO: plot interesting 2d graph(s)
library(ggplot2)
library(reshape2)
library(Hmisc)

# Plot all variables from Lowe et.al.
vars <- c("freemarket", "environment", "stateconomy", "stateservices", "logrile",
          "logplaneco", "libcons", "foreignalliances", "militarism", "internationalism",
          "logeu", "constitutionalism", "decentralization", "protectionism", "keynesian",
          "nationalism", "morality", "multiculturalism", "laborpolicy", "logwelfare",
          "education")
for(var in vars) {
    pdf(paste0(var, ".pdf"), width=11.70, height=4.1)
    p <- ggplot(sweden, aes_string(x="edate", y=var, group="party", colour="party"))
    print(
      p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
          labs(x = "År") +
          stat_summary(aes(group=country), fun.data="mean_cl_normal", geom="smooth", colour="#666666", fill="#cccccc")
    )
    dev.off()
}

# References
# Lowe, W., Benoit, K., Mikhaylov, S. and Laver, M. 2011. ”Scaling Policy Preferences from Coded Political Texts.” Legislative Studies Quarterly 36 (1): 123–155.
