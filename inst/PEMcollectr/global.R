library(shiny)
library(PEMcollectr)
options(shiny.maxRequestSize=60*1024^2)
con <- connect_pg('PEM')
