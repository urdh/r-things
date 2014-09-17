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

## TODO: Plot average and/or block in at least some of these
library(ggplot2)
library(reshape2)
library(Hmisc)
# Seats in Riksdagen (absolute)
pdf("absseat.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=absseat, group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Platser i Riksdagen") + ylim(0, 200)
)
dev.off()

# Seats in Riksdagen (relative)
pdf("relseat.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=(absseat / totseats), group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Andel platser i Riksdagen") + ylim(0, 0.6)
)
dev.off()

# Election results, percent
pdf("pervote.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=pervote, group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Valresultat (%)") + ylim(0, 60) +
    geom_hline(yintercept = 4, colour = "#333333", linetype = "dashed")
)
dev.off()

# Left-right score
pdf("rile.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=rile, group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "RILE-poäng") + ylim(-100, 100) +
    stat_summary(aes(group=country), fun.data="mean_cl_normal", geom="smooth", colour="#333333")
)
dev.off()

# Facets of economy, welfare etc
pdf("facets.pdf", width=11.70, height=8.27)
facetize <- melt(
  subset(sweden, select=c(edate, party, planeco, markeco, welfare, intpeace)),
                 id.vars = c("edate", "party"))
levels(facetize$variable) <- c("Planekonomi", "Marknadsekonomi",
                               "Välfärd", "Internationell fred")
p <- ggplot(facetize, aes(x=edate, y=value, group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(0, 100) +
    facet_grid(variable ~ .)
)
dev.off()

# Military score
pdf("military.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=(per104 - per105), group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(-25, 25) +
    stat_summary(aes(group=country), fun.data="mean_cl_normal", geom="smooth", colour="#333333")
)
dev.off()

# EU score
pdf("eu.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=(per108 - per110), group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(-25, 25) +
    stat_summary(aes(group=country), fun.data="mean_cl_normal", geom="smooth", colour="#333333")
)
dev.off()

# Human rights score
pdf("human-rights.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=per201, group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(0, 25)
)
dev.off()

# Democracy score
pdf("democracy.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=per202, group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(0, 25)
)
dev.off()

# Anti-growth economy score
pdf("anti-growth.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=per416, group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(-25, 25)
)
dev.off()

# Environmental protection score
pdf("environment.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=per501, group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(0, 50)
)
dev.off()

# Culture score
pdf("culture.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=per502, group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(0, 25)
)
dev.off()

# Equality score
pdf("equality.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=per503, group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(0, 50)
)
dev.off()

# Welfare score
pdf("welfare.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=(per504 - per505), group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(-50, 50)
)
dev.off()

# Education score
pdf("education.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=(per506 - per507), group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(-25, 25)
)
dev.off()

# Law and order score
pdf("law-order.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=per605, group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(0, 25)
)
dev.off()

# Multiculturalism score
pdf("multiculture.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=(per607 - per608), group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(-25, 25) +
    stat_summary(aes(group=country), fun.data="mean_cl_normal", geom="smooth", colour="#333333")
)
dev.off()

# Labour groups score
pdf("labour.pdf", width=11.70, height=4.1)
p <- ggplot(sweden, aes(x=edate, y=(per701 - per702), group=party, colour=party))
print(
p + geom_line() + geom_point() + scale_colour_manual(name="Parti", values=colors) +
    labs(x = "Valår", y = "Poäng") + ylim(-25, 25)
)
dev.off()
