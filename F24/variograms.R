---
   
   ## Variograms
   
   Variogram plots are one way to assess the how strong the serial correlation is
```{r cd4, echo=F}
cd4 <- read.csv("~/Desktop/teaching/PHS-651/data/Gelman-data/cd4/allvar.csv",header=T)

cd4 <- cd4 %>% 
   as.data.frame() %>% 
   dplyr::filter(treatmnt==1) %>% 
   rename(age_baseline = baseage,
          age = visage) %>% 
   mutate(time = age - age_baseline) %>% 
   select(!(treatmnt))

```

```{r variogram, echo=F, fig.align='center', fig.height=8.5, fig.width=15}
library(joineR)
library(splines)

# fit outcome against time with some splines #
fit <- lm( sqrt(CD4PCT) ~ ns( time, knots=c(0.5, 1, 1.5)),
           data=cd4)

# find the residuals #
resids <- residuals( fit )

# calculate the variogram #
vario <- variogram( indv = cd4$newpid,
                    time = cd4$time,
                    Y = resids )

# only take the complete cases #
vario$svar <- vario$svar[complete.cases(vario$svar),]

# plot variogram #
plot( vario$svar[,1], vario$svar[,2], pch=".", ylim=c(0, 1.2*var(resids)),
      cex=4, cex.axis=2, cex.lab=2)

# smooth line of variogram over time #
lines( smooth.spline(vario$svar[,1], vario$svar[,2],df=3), lwd=3 )

# show where the total variation is #
abline(h=var(resids), lty=2, lwd=3)

```