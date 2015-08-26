library(magrittr)
library(dplyr)

data.frame(ion = c("[M+H-CH3]+."),
           mass = )

the.atoms <- data.frame(atom = c("H", "C", "O"),
                        mass = c(1.007825, 12.000, 15.994915))

the.frags <- data.frame(fragment = c("CH3.", "CH4", "CO", "H", "CO2", "H2O", "CH3OH", "C2H2O", "C2H2", "CH2O", "HCO2H"),
                        mass = c(15.0235, 16.0313, 27.9949, 1.0073, 43.9898, 18.0106, 32.0262, 42.0106, 26.0157, 30.0106, 46.0055))


library(dplyr)
library(data.table)

the.formulae <- expand.grid(O = 0:8, H=0:20, C=0:20)

the.atoms %<>% data.table
the.formulae %<>% mutate(mass = {O*the.atoms[atom == "O"]$mass+
                                  H*the.atoms[atom == "H"]$mass  +
                                  C*the.atoms[atom == "C"]$mass},
                        formula = paste("C",C,"H",H,"O",O, sep=""),
                        DBE = (2*C-H+2)/2
) %>% filter(DBE >= 0)#%>% filter(DBE %% round(DBE, 0) == 0) 

ppm.calc <- function(exact, accurate){abs(exact-accurate)/exact * 1e6}

poss.sumform <- function(mass.in, the.formulae, ppm.in=1){
  the.formulae %<>% mutate(ppm = ppm.calc(mass.in, mass))
  the.formulae %>% filter(ppm <= ppm.in)
}