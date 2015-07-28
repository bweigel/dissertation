## ---- cache=FALSE, echo=FALSE--------------------------------------------
knitr::opts_chunk$set(dev='tikz', error=T, warning=F, message=F, size='tiny', echo=F)

## ---- echo=F, message=FALSE, results='asis', fig.margin=T----------------
library(magrittr)
library(dplyr)
library(FrF2)
library(ggplot2)
library(xtable)
options(xtable.comment = FALSE)
options(xtable.booktabs = TRUE)

load("/home/mori/IPB/Experimente/WEB346 - Noncatecholic substrates/subdocs/WEB346.results.rda")

motif <- data.frame(substrate = 1:17, 
                    motif = c(rep(c("phenolic", "catecholic", "4-O-Me", "3-O-Me"), 2), 
                              rep("phenolic", 2),
                              "phenolic", "catecholic", "4-O-Me", "3-O-Me", "phenolic", "catecholic", "catecholic"),
                    select = c(rep(T, 9), rep(F, 2), rep(T, 3), rep(F, 3)))
WEB346.results <- merge(WEB346.results, motif, by="substrate")
WEB346.results$pH <- ifelse(WEB346.results$pH > 8, "high", "low")
WEB346.results %<>% mutate(#pH = factor(pH, levels = c("low", "high"), ordered = T),
                           Mg = as.factor(Mg),#
                           motif = factor(motif, labels=c(LETTERS[1:4]) , levels=c("phenolic", "catecholic", "4-O-Me", "3-O-Me"))) 

WEB346.results %<>% filter(select == T, variant != "none") %>%
  select(substrate, value, pH, Mg, variant, motif)
rm(motif)

xtable(head(WEB346.results), caption = "First rows of dataframe")

## ---- results='asis', fig.margin=TRUE------------------------------------
levtab <- xtable(data.frame(labels=c(LETTERS[1:4]) , motif=c("phenolic", "catecholic", "4-O-Me", "3-O-Me")), caption="Labels in the data.frame and their corresponding motif.")
print(levtab, floating.environment='margintable')

## ---- fig.height=3.5, fig.width=6, fig.cap="Main effetcs plots for the factor variables. Clearly the motif seems to have the biggest impact. Catecholic moieties are converted most effectively."----

a<-WEB346.results %>%
  group_by(motif) %>%
  summarise_each(funs(mean(., na.rm=TRUE)), value)
names(a)[1] <- "state"
a %<>% mutate(state = as.factor(state), ME = "motif")

b<-WEB346.results %>%
  group_by(Mg) %>%
  summarise_each(funs(mean(., na.rm=TRUE)), value)
names(b)[1] <- "state"
b%<>% mutate(state = as.factor(state), ME = "Mg")

c<-WEB346.results %>%
  group_by(pH) %>%
  summarise_each(funs(mean(., na.rm=TRUE)), value)
names(c)[1] <- "state"
c %<>% mutate(state = as.factor(state), ME = "pH")

d<-WEB346.results %>%
  group_by(variant) %>%
  summarise_each(funs(mean(., na.rm=TRUE)), value)
names(d)[1] <- "state"
d %<>% mutate(state = as.factor(state), ME = "variant")

MEgg <- rbind(a,b,c, d)

ggplot(MEgg, aes(x=state, y=value)) + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust = 0),
        axis.title.x=element_text(vjust = 0)) +
  geom_line(aes(group = ME), color="cornflowerblue", size=1) +
  geom_point(size=3) + 
  facet_wrap(~ME, nrow=1, scales="free_x") +
  labs(y="mean $\\frac{\\mathrm{SAH}}{\\mathrm{flavone}} (\\mathrm{AU})$", x="factor level") +
  geom_hline(aes(yintercept=mean(MEgg$value)), linetype=2, size=1)

## ---- fig.height=3, fig.width=2.5, fig.margin=T, fig.cap='Interaction plot for Mg and pH. The lines suggest an interaction between pH and Mg, but this is not enough evidence to say wether that interaction is significant. red -- no magnesium, blue -- magnesium added.'----
d<-WEB346.results %>%
  group_by(Mg,pH) %>%
  summarise_each(funs(mean(., na.rm=TRUE), sd(., na.rm=TRUE)), value)

ggplot(d, aes(x=pH, y=mean, group = Mg)) + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust = 0),
        axis.title.x=element_text(vjust = 0),
        legend.position="none") +
  geom_line(aes(color = Mg), size=1) +
  geom_point(size=3) +
  labs(y="mean $\\frac{\\mathrm{SAH}}{\\mathrm{flavone}} (\\mathrm{AU})$", x="pH") +
  geom_hline(aes(yintercept=mean(d$mean)), linetype=2, size=1)

## ---- fig.margin = F, fig.cap="RegressionTree of the data built with the `rpart`-package. The motif effects the conversion most. It seems that only the variant is influenced by pH and Mg.", dev='CairoPDF', echo=F, fig.width=10, fig.height=8----
library(rpart)
library(rpart.plot)
library(rattle)

data.tree <- rpart(value ~ ., 
                   data=WEB346.results %>% select(-c(substrate)),
                   control = c(minsplit = 3,
                               minbucket = 3,
                               cp = 0.01))
fancyRpartPlot(data.tree, digits=3)
rm(data.tree)

## ---- results='asis'-----------------------------------------------------
data.lm <- lm(value ~ pH*Mg, data=WEB346.results)
summary(data.lm) %>% xtable(.,caption="A linear model with pH and Mg as factors is not a lot better than random guesses. p-values are very high. Thus this alone does not explain the variance.")

## ---- results='asis'-----------------------------------------------------
data.lm <- lm(value ~ pH*Mg*motif*variant, data=WEB346.results)
#summary(data.lm) %>% xtable(.,caption="When the motif is included the model describes the data much better. The low p-values even suggest, that there is an interaction effect between pH and substrates with catecholic motifs, as well as an interaction pH:Mg:Catechol.")

## ---- fig.margin = F, fig.cap="RegressionTree of the data of only substrates with phenolic, 3'-OMe and 4'-OMe moieties. Catecholic substrates are omitted.", dev='CairoPDF', fig.width=10, fig.height=8----
data.tree <- rpart(value ~ ., 
                   data=WEB346.results %>% filter(motif != "B") %>% select(-substrate),
                   control = c(minsplit = 3,
                               minbucket = 3,
                               cp = 0.01))
fancyRpartPlot(data.tree, digits=3)

## ---- fig.margin=TRUE, fig.cap="Lasso regression on the model."----------
library(glmnet)

data <- WEB346.results 

# create model matrix for glmnet function
# needs to be a sparse matrix because of mixed numerical and factor variables
x<-sparse.model.matrix(value~pH*Mg*variant*motif, data = data)
y <- data[,2]

grid <- 10^seq(10,-10,length.out = 100)
lasso <- glmnet(x, y, family = "gaussian", alpha = 1, lambda=grid)
plot(lasso)

## ---- eval=F, echo=T-----------------------------------------------------
## x<-sparse.model.matrix(value~pH*Mg*variant*motif, data = data)
## y <- data[,2]
## cv.lasso <- cv.glmnet(x,y,alpha=1,nfolds=5)

## ---- results='asis', fig.margin=T, fig.cap=c("Cross validation results for lasso regression. The best model only needs around 10 variables to describe the data with low error.", "The variables that have non-zero coefficients.")----
cv.lasso <- cv.glmnet(x,y,alpha=1,nfolds=5)
bestlam <- cv.lasso$lambda.min
lasso.coeff <- predict(cv.lasso, type="coefficients", s=bestlam)

plot(cv.lasso)

rm(levtab)
plot(lasso.coeff)
df <- data.frame(variable = dimnames(lasso.coeff)[[1]][summary(lasso.coeff)$i],
           coefficient = summary(lasso.coeff)$x)
xtable(df, caption="Variables and coefficients that were retained. Non-zero coefficients not shown.", digits = 4)

