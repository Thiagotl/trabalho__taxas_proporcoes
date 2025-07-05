
deriexp <- function(x, variavel, cen=1, dados=xq, b0=-0.305){
  beta = c(0.003, 3.315, 2.554, -0.225, -0.128, -0.221)
  b1 <- beta[variavel]
  bn <- beta[-c(variavel)]
  xn <- dados[cen, -c(variavel)]
  multexp <- exp(b0)
  for (i in 1:length(bn)){
    multexp <- multexp * exp((bn[i]*xn[i]))
  }
  multexp <- multexp[1, 1]
  multexp <- multexp*exp((x*b1))
  imp <- (b1*multexp)/((1+multexp)^2)
  return(imp)
}

impacto_idade <- function(x){deriexp(x, 1, cen=13)}
impacto_SREM <- function(x){deriexp(x, 2, cen=13)}
impacto_PSP <- function(x){deriexp(x, 3, cen=13)}
impacto_DEP <- function(x){deriexp(x, 4, cen=13)}
impacto_CA <- function(x){deriexp(x, 5, cen=13)}
impacto_TAB <- function(x){deriexp(x, 6, cen=13)}

plot(impacto_idade, xlim = c(0, 100))
plot(impacto_SREM)
plot(impacto_PSP)
plot(impacto_DEP)
plot(impacto_CA) #so assumi 0 e 1
plot(impacto_TAB) #so assumi 0 e 1

#####################################################



impacto_idade_cen1 <- function(x){deriexp(x, 1, cen=1)}
impacto_idade_cen2 <- function(x){deriexp(x, 1, cen=2)}
impacto_idade_cen3 <- function(x){deriexp(x, 1, cen=3)}
impacto_idade_cen4 <- function(x){deriexp(x, 1, cen=4)}
impacto_idade_cen5 <- function(x){deriexp(x, 1, cen=5)}
impacto_idade_cen6 <- function(x){deriexp(x, 1, cen=6)}
impacto_idade_cen7 <- function(x){deriexp(x, 1, cen=7)}
impacto_idade_cen8 <- function(x){deriexp(x, 1, cen=8)}
impacto_idade_cen9 <- function(x){deriexp(x, 1, cen=9)}
impacto_idade_cen10 <- function(x){deriexp(x, 1, cen=10)}
impacto_idade_cen11 <- function(x){deriexp(x, 1, cen=11)}
impacto_idade_cen12 <- function(x){deriexp(x, 1, cen=12)}
impacto_SREM_cen1 <- function(x){deriexp(x, 2, cen=1)}
impacto_SREM_cen2 <- function(x){deriexp(x, 2, cen=2)}
impacto_SREM_cen3 <- function(x){deriexp(x, 2, cen=3)}
impacto_SREM_cen4 <- function(x){deriexp(x, 2, cen=4)}
impacto_SREM_cen5 <- function(x){deriexp(x, 2, cen=5)}
impacto_SREM_cen6 <- function(x){deriexp(x, 2, cen=6)}
impacto_SREM_cen7 <- function(x){deriexp(x, 2, cen=7)}
impacto_SREM_cen8 <- function(x){deriexp(x, 2, cen=8)}
impacto_SREM_cen9 <- function(x){deriexp(x, 2, cen=9)}
impacto_SREM_cen10 <- function(x){deriexp(x, 2, cen=10)}
impacto_SREM_cen11 <- function(x){deriexp(x, 2, cen=11)}
impacto_SREM_cen12 <- function(x){deriexp(x, 2, cen=12)}
impacto_PSP_cen1 <- function(x){deriexp(x, 3, cen=1)}
impacto_PSP_cen2 <- function(x){deriexp(x, 3, cen=2)}
impacto_PSP_cen3 <- function(x){deriexp(x, 3, cen=3)}
impacto_PSP_cen4 <- function(x){deriexp(x, 3, cen=4)}
impacto_PSP_cen5 <- function(x){deriexp(x, 3, cen=5)}
impacto_PSP_cen6 <- function(x){deriexp(x, 3, cen=6)}
impacto_PSP_cen7 <- function(x){deriexp(x, 3, cen=7)}
impacto_PSP_cen8 <- function(x){deriexp(x, 3, cen=8)}
impacto_PSP_cen9 <- function(x){deriexp(x, 3, cen=9)}
impacto_PSP_cen10 <- function(x){deriexp(x, 3, cen=10)}
impacto_PSP_cen11 <- function(x){deriexp(x, 3, cen=11)}
impacto_PSP_cen12 <- function(x){deriexp(x, 3, cen=12)}
impacto_DEP_cen1 <- function(x){deriexp(x, 4, cen=1)}
impacto_DEP_cen2 <- function(x){deriexp(x, 4, cen=2)}
impacto_DEP_cen3 <- function(x){deriexp(x, 4, cen=3)}
impacto_DEP_cen4 <- function(x){deriexp(x, 4, cen=4)}
impacto_DEP_cen5 <- function(x){deriexp(x, 4, cen=5)}
impacto_DEP_cen6 <- function(x){deriexp(x, 4, cen=6)}
impacto_DEP_cen7 <- function(x){deriexp(x, 4, cen=7)}
impacto_DEP_cen8 <- function(x){deriexp(x, 4, cen=8)}
impacto_DEP_cen9 <- function(x){deriexp(x, 4, cen=9)}
impacto_DEP_cen10 <- function(x){deriexp(x, 4, cen=10)}
impacto_DEP_cen11 <- function(x){deriexp(x, 4, cen=11)}
impacto_DEP_cen12 <- function(x){deriexp(x, 4, cen=12)}
impacto_CA_cen1 <- function(x){deriexp(x, 5, cen=1)}
impacto_CA_cen2 <- function(x){deriexp(x, 5, cen=2)}
impacto_CA_cen3 <- function(x){deriexp(x, 5, cen=3)}
impacto_CA_cen4 <- function(x){deriexp(x, 5, cen=4)}
impacto_CA_cen5 <- function(x){deriexp(x, 5, cen=5)}
impacto_CA_cen6 <- function(x){deriexp(x, 5, cen=6)}
impacto_CA_cen7 <- function(x){deriexp(x, 5, cen=7)}
impacto_CA_cen8 <- function(x){deriexp(x, 5, cen=8)}
impacto_CA_cen9 <- function(x){deriexp(x, 5, cen=9)}
impacto_CA_cen10 <- function(x){deriexp(x, 5, cen=10)}
impacto_CA_cen11 <- function(x){deriexp(x, 5, cen=11)}
impacto_CA_cen12 <- function(x){deriexp(x, 5, cen=12)}
impacto_TAB_cen1 <- function(x){deriexp(x, 6, cen=1)}
impacto_TAB_cen2 <- function(x){deriexp(x, 6, cen=2)}
impacto_TAB_cen3 <- function(x){deriexp(x, 6, cen=3)}
impacto_TAB_cen4 <- function(x){deriexp(x, 6, cen=4)}
impacto_TAB_cen5 <- function(x){deriexp(x, 6, cen=5)}
impacto_TAB_cen6 <- function(x){deriexp(x, 6, cen=6)}
impacto_TAB_cen7 <- function(x){deriexp(x, 6, cen=7)}
impacto_TAB_cen8 <- function(x){deriexp(x, 6, cen=8)}
impacto_TAB_cen9 <- function(x){deriexp(x, 6, cen=9)}
impacto_TAB_cen10 <- function(x){deriexp(x, 6, cen=10)}
impacto_TAB_cen11 <- function(x){deriexp(x, 6, cen=11)}
impacto_TAB_cen12 <- function(x){deriexp(x, 6, cen=12)}

      # idade-1,SREM-2, PSP-3, DEP-4,  CA-5,  TAB-6
beta = c(0.003, 3.315, 2.554, -0.225, -0.128, -0.221)
xq <- data.frame(
  idade = c(29, 29, 29, 29, 40, 40, 40, 40, 52, 52, 52, 52, 40.71),
  SREM = c(0.2, 0.2, 0.2, 0.2, 0.22, 0.22, 0.22, 0.22, 0.25, 0.25, 0.25, 0.25, 0.2263),
  PSP = c(0.51, 0.51, 0.51, 0.51, 0.58, 0.58, 0.58, 0.58, 0.63, 0.63, 0.63, 0.63, 0.53),
  DEP = c(1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1.63),
  CA = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0.43),
  TAB = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0.34)
)

ate1 <- seq(0, 1, by = 0.01)

##IDADE

#Para Quartil 1 variando CA e TAB.
plot(impacto_idade_cen1, xlim=c(0, 100), ylim=c(0.00038, 0.00056)) #CA=0, TAB=0
lines(impacto_idade_cen2(0:100), col="RED") #CA=1, TAB=0
lines(impacto_idade_cen3(0:100), col="ORANGE") #CA=0, TAB=1
lines(impacto_idade_cen4(0:100), col="PURPLE") #CA=1, TAB=1


#Para Quartil 2 variando CA e TAB.
plot(impacto_idade_cen5, xlim=c(0, 100), ylim=c(0.00032, 0.00050))
lines(impacto_idade_cen6(0:100), col="RED")
lines(impacto_idade_cen7(0:100), col="ORANGE")
lines(impacto_idade_cen8(0:100), col="PURPLE")

#Para Quartil 3 variando CA e TAB.
plot(impacto_idade_cen9, xlim=c(0, 100), ylim=c(0.00037, 0.00056))
lines(impacto_idade_cen10(0:100), col="RED")
lines(impacto_idade_cen11(0:100), col="ORANGE")
lines(impacto_idade_cen12(0:100), col="PURPLE")

#Para CA e TAB = 1, variando Quartil.
plot(impacto_idade_cen1, xlim=c(0, 100), ylim=c(0.00032, 0.00056), col="RED") #Q1
lines(impacto_idade_cen5(0:100), col="ORANGE") #Q2
lines(impacto_idade_cen9(0:100), col="PURPLE") #Q3

##SREM

#Para Quartil 1 variando CA e TAB.
plot(impacto_SREM_cen1, ylim=c(0, 0.8)) #CA=0, TAB=0
lines(x=ate1, y=impacto_SREM_cen2(ate1), col="RED") #CA=1, TAB=0
lines(x=ate1, y=impacto_SREM_cen3(ate1), col="ORANGE") #CA=0, TAB=1
lines(x=ate1, y=impacto_SREM_cen4(ate1), col="PURPLE") #CA=1, TAB=1


#Para Quartil 2 variando CA e TAB.
plot(impacto_SREM_cen5, ylim=c(0, 0.75))
lines(x=ate1, y=impacto_SREM_cen6(ate1), col="RED")
lines(x=ate1, y=impacto_SREM_cen7(ate1), col="ORANGE")
lines(x=ate1, y=impacto_SREM_cen8(ate1), col="PURPLE")

#Para Quartil 3 variando CA e TAB.
plot(impacto_SREM_cen9, ylim=c(0, 0.8))
lines(x=ate1, y=impacto_SREM_cen10(ate1), col="RED")
lines(x=ate1, y=impacto_SREM_cen11(ate1), col="ORANGE")
lines(x=ate1, y=impacto_SREM_cen12(ate1), col="PURPLE")

#Para CA e TAB = 1, variando Quartil.
plot(impacto_SREM_cen1, col="RED", ylim=c(0, 0.8))
lines(x=ate1, y=impacto_SREM_cen5(ate1), col="ORANGE")
lines(x=ate1, y=impacto_SREM_cen9(ate1), col="PURPLE")

##PSP

#Para Quartil 1 variando CA e TAB.
plot(impacto_PSP_cen1, ylim=c(0.1, 0.7)) #CA=0, TAB=0
lines(x=ate1, y=impacto_PSP_cen2(ate1), col="RED") #CA=1, TAB=0
lines(x=ate1, y=impacto_PSP_cen3(ate1), col="ORANGE") #CA=0, TAB=1
lines(x=ate1, y=impacto_PSP_cen4(ate1), col="PURPLE") #CA=1, TAB=1


#Para Quartil 2 variando CA e TAB.
plot(impacto_PSP_cen5, ylim=c(0.1, 0.7))
lines(x=ate1, y=impacto_PSP_cen6(ate1), col="RED")
lines(x=ate1, y=impacto_PSP_cen7(ate1), col="ORANGE")
lines(x=ate1, y=impacto_PSP_cen8(ate1), col="PURPLE")

#Para Quartil 3 variando CA e TAB.
plot(impacto_PSP_cen9, ylim=c(0.1, 0.7))
lines(x=ate1, y=impacto_PSP_cen10(ate1), col="RED")
lines(x=ate1, y=impacto_PSP_cen11(ate1), col="ORANGE")
lines(x=ate1, y=impacto_PSP_cen12(ate1), col="PURPLE")

#Para CA e TAB = 1, variando Quartil.
plot(impacto_PSP_cen1, col="RED", ylim=c(0.1, 0.7))
lines(x=ate1, y=impacto_PSP_cen5(ate1), col="ORANGE")
lines(x=ate1, y=impacto_PSP_cen9(ate1), col="PURPLE")

##DEP

#Para Quartil 1 variando CA e TAB.
plot(impacto_DEP_cen1, ylim=c(-0.041, -0.028)) #CA=0, TAB=0
lines(x=ate1, y=impacto_DEP_cen2(ate1), col="RED") #CA=1, TAB=0
lines(x=ate1, y=impacto_DEP_cen3(ate1), col="ORANGE")#CA=0, TAB=1
lines(x=ate1, y=impacto_DEP_cen4(ate1), col="PURPLE") #CA=1, TAB=1


#Para Quartil 2 variando CA e TAB.
plot(impacto_DEP_cen5, ylim=c(-0.036, -0.022))
lines(x=ate1, y=impacto_DEP_cen6(ate1), col="RED")
lines(x=ate1, y=impacto_DEP_cen7(ate1), col="ORANGE")
lines(x=ate1, y=impacto_DEP_cen8(ate1), col="PURPLE")

#Para Quartil 3 variando CA e TAB.
plot(impacto_DEP_cen9, ylim=c(-0.032, -0.018))
lines(x=ate1, y=impacto_DEP_cen10(ate1), col="RED")
lines(x=ate1, y=impacto_DEP_cen11(ate1), col="ORANGE")
lines(x=ate1, y=impacto_DEP_cen12(ate1), col="PURPLE")

#Para CA e TAB = 1, variando Quartil.
plot(impacto_DEP_cen1, col="RED", ylim=c(-0.039, -0.018))
lines(x=ate1, y=impacto_DEP_cen5(ate1), col="ORANGE")
lines(x=ate1, y=impacto_DEP_cen9(ate1), col="PURPLE")

##CA

#Para Quartil 1 variando TAB.
plot(impacto_CA_cen1, ylim=c(-0.024, -0.0185)) # TAB=0
lines(x=ate1, y=impacto_CA_cen3(ate1), col="ORANGE") # TAB=1

#Para Quartil 2 variando CA e TAB.
plot(impacto_CA_cen5, ylim=c(-0.021, -0.015))
lines(x=ate1, y=impacto_CA_cen7(ate1), col="ORANGE")

#Para Quartil 3 variando CA e TAB.
plot(impacto_CA_cen9, ylim=c(-0.023, -0.017))
lines(x=ate1, y=impacto_CA_cen11(ate1), col="ORANGE")

#Para TAB = 1, variando Quartil.
plot(impacto_CA_cen1, col="RED", ylim=c(-0.024, -0.015))
lines(x=ate1, y=impacto_CA_cen5(ate1), col="ORANGE")
lines(x=ate1, y=impacto_CA_cen9(ate1), col="PURPLE")

##TAB

#Para Quartil 1 variando CA.
plot(impacto_TAB_cen1, ylim=c(-0.04, -0.032)) # CA=0
lines(x=ate1, y=impacto_TAB_cen2(ate1), col="ORANGE") # CA=1

#Para Quartil 2 variando CA.
plot(impacto_TAB_cen5, ylim=c(-0.036, -0.022))
lines(x=ate1, y=impacto_TAB_cen6(ate1), col="ORANGE")

#Para Quartil 3 variando CA.
plot(impacto_TAB_cen9, ylim=c(-0.04, -0.028))
lines(x=ate1, y=impacto_TAB_cen10(ate1), col="ORANGE")

#Para CA = 1, variando Quartil.
plot(impacto_TAB_cen1, col="RED", ylim=c(-0.04, -0.022))
lines(x=ate1, y=impacto_TAB_cen5(ate1), col="ORANGE")
lines(x=ate1, y=impacto_TAB_cen9(ate1), col="PURPLE")
