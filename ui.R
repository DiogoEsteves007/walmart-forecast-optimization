library(shiny)
library(shinydashboard)
library(DT)

dadosWalmart <- read.csv("walmart.csv")


leitura_otimizacao_planos_totais <- function(metodo,mes){
  if(metodo=="Diff_Ev"){
    if(mes=="Normal"){
      de_normal_total <- read.csv(paste0("Matrizes_efetivas/DE/DE_Normal.csv"))
      return(de_normal_total)
    }else if(mes==1){
      de_m1_total <- read.csv(paste0("Matrizes_efetivas/DE/DE_Mes1.csv"))
      return(de_m1_total)
    }else if(mes==2){
      de_m2_total <- read.csv(paste0("Matrizes_efetivas/DE/DE_Mes2.csv"))
      return(de_m2_total)
    }else if(mes==3){
      de_m3_total <- read.csv(paste0("Matrizes_efetivas/DE/DE_Mes3.csv"))
      return(de_m3_total)
    }else if(mes==4){
      de_m4_total <- read.csv(paste0("Matrizes_efetivas/DE/DE_Mes4.csv"))
      return(de_m4_total)
    }else if(mes==5){
      de_m5_total <- read.csv(paste0("Matrizes_efetivas/DE/DE_Mes5.csv"))
      return(de_m5_total)
    }else if(mes==6){
      de_m6_total <- read.csv(paste0("Matrizes_efetivas/DE/DE_Mes6.csv"))
      return(de_m6_total)
    }else if(mes==7){
      de_m7_total <- read.csv(paste0("Matrizes_efetivas/DE/DE_Mes7.csv"))
      return(de_m7_total)
    }else if(mes==8){
      de_m8_total <- read.csv(paste0("Matrizes_efetivas/DE/DE_Mes8.csv"))
      return(de_m8_total)
    }
  }else if(metodo == "GE"){
    if(mes=="Normal"){
      ge_normal_total <- read.csv(paste0("Matrizes_efetivas/GE/GE_Normal.csv"))
      return(ge_normal_total)
    }else if(mes==1){
      ge_m1_total <- read.csv(paste0("Matrizes_efetivas/GE/GE_Mes1.csv"))
      return(ge_m1_total)
    }else if(mes==2){
      ge_m2_total <- read.csv(paste0("Matrizes_efetivas/GE/GE_Mes2.csv"))
      return(ge_m2_total)
    }else if(mes==3){
      ge_m3_total <- read.csv(paste0("Matrizes_efetivas/GE/GE_Mes3.csv"))
      return(ge_m3_total)
    }else if(mes==4){
      ge_m4_total <- read.csv(paste0("Matrizes_efetivas/GE/GE_Mes4.csv"))
      return(ge_m4_total)
    }else if(mes==5){
      ge_m5_total <- read.csv(paste0("Matrizes_efetivas/GE/GE_Mes5.csv"))
      return(ge_m5_total)
    }else if(mes==6){
      ge_m6_total <- read.csv(paste0("Matrizes_efetivas/GE/GE_Mes6.csv"))
      return(ge_m6_total)
    }else if(mes==7){
      ge_m7_total <- read.csv(paste0("Matrizes_efetivas/GE/GE_Mes7.csv"))
      return(ge_m7_total)
    }else if(mes==8){
      ge_m8_total <- read.csv(paste0("Matrizes_efetivas/GE/GE_Mes8.csv"))
      return(ge_m8_total)
    }
  }else if(metodo == "HillC_Add"){
    if(mes=="Normal"){
      hcA_normal_total <- read.csv(paste0("Matrizes_efetivas/HC_aditivo/HC_aditivo_Normal.csv"))
      return(hcA_normal_total)
    }else if(mes==1){
      hcA_m1_total <- read.csv(paste0("Matrizes_efetivas/HC_aditivo/HC_aditivo_Mes1.csv"))
      return(hcA_m1_total)
    }else if(mes==2){
      hcA_m2_total <- read.csv(paste0("Matrizes_efetivas/HC_aditivo/HC_aditivo_Mes2.csv"))
      return(hcA_m2_total)
    }else if(mes==3){
      hcA_m3_total <- read.csv(paste0("Matrizes_efetivas/HC_aditivo/HC_aditivo_Mes3.csv"))
      return(hcA_m3_total)
    }else if(mes==4){
      hcA_m4_total <- read.csv(paste0("Matrizes_efetivas/HC_aditivo/HC_aditivo_Mes4.csv"))
      return(hcA_m4_total)
    }else if(mes==5){
      hcA_m5_total <- read.csv(paste0("Matrizes_efetivas/HC_aditivo/HC_aditivo_Mes5.csv"))
      return(hcA_m5_total)
    }else if(mes==6){
      hcA_m6_total <- read.csv(paste0("Matrizes_efetivas/HC_aditivo/HC_aditivo_Mes6.csv"))
      return(hcA_m6_total)
    }else if(mes==7){
      hcA_m7_total <- read.csv(paste0("Matrizes_efetivas/HC_aditivo/HC_aditivo_Mes7.csv"))
      return(hcA_m7_total)
    }else if(mes==8){
      hcA_m8_total <- read.csv(paste0("Matrizes_efetivas/HC_aditivo/HC_aditivo_Mes8.csv"))
      return(hcA_m8_total)
    }
  }else if(metodo == "HillC_Mult"){
    if(mes=="Normal"){
      hcM_normal_total <- read.csv(paste0("Matrizes_efetivas/HC_multiplicativo/HC_multiplicativo_Normal.csv"))
      return(hcM_normal_total)
    }else if(mes==1){
      hcM_m1_total <- read.csv(paste0("Matrizes_efetivas/HC_multiplicativo/HC_multiplicativo_Mes1.csv"))
      return(hcM_m1_total)
    }else if(mes==2){
      hcM_m2_total <- read.csv(paste0("Matrizes_efetivas/HC_multiplicativo/HC_multiplicativo_Mes2.csv"))
      return(hcM_m2_total)
    }else if(mes==3){
      hcM_m3_total <- read.csv(paste0("Matrizes_efetivas/HC_multiplicativo/HC_multiplicativo_Mes3.csv"))
      return(hcM_m3_total)
    }else if(mes==4){
      hcM_m4_total <- read.csv(paste0("Matrizes_efetivas/HC_multiplicativo/HC_multiplicativo_Mes4.csv"))
      return(hcM_m4_total)
    }else if(mes==5){
      hcM_m5_total <- read.csv(paste0("Matrizes_efetivas/HC_multiplicativo/HC_multiplicativo_Mes5.csv"))
      return(hcM_m5_total)
    }else if(mes==6){
      hcM_m6_total <- read.csv(paste0("Matrizes_efetivas/HC_multiplicativo/HC_multiplicativo_Mes6.csv"))
      return(hcM_m6_total)
    }else if(mes==7){
      hcM_m7_total <- read.csv(paste0("Matrizes_efetivas/HC_multiplicativo/HC_multiplicativo_Mes7.csv"))
      return(hcM_m7_total)
    }else if(mes==8){
      hcM_m8_total <- read.csv(paste0("Matrizes_efetivas/HC_multiplicativo/HC_multiplicativo_Mes8.csv"))
      return(hcM_m8_total)
    }
  }else if(metodo == "MC"){
    if(mes=="Normal"){
      mc_normal_total <- read.csv(paste0("Matrizes_efetivas/MC/MC_Normal.csv"))
      return(mc_normal_total)
    }else if(mes==1){
      mc_m1_total <- read.csv(paste0("Matrizes_efetivas/MC/MC_Mes1.csv"))
      return(mc_m1_total)
    }else if(mes==2){
      mc_m2_total <- read.csv(paste0("Matrizes_efetivas/MC/MC_Mes2.csv"))
      return(mc_m2_total)
    }else if(mes==3){
      mc_m3_total <- read.csv(paste0("Matrizes_efetivas/MC/MC_Mes3.csv"))
      return(mc_m3_total)
    }else if(mes==4){
      mc_m4_total <- read.csv(paste0("Matrizes_efetivas/MC/MC_Mes4.csv"))
      return(mc_m4_total)
    }else if(mes==5){
      mc_m5_total <- read.csv(paste0("Matrizes_efetivas/MC/MC_Mes5.csv"))
      return(mc_m5_total)
    }else if(mes==6){
      mc_m6_total <- read.csv(paste0("Matrizes_efetivas/MC/MC_Mes6.csv"))
      return(mc_m6_total)
    }else if(mes==7){
      mc_m7_total <- read.csv(paste0("Matrizes_efetivas/MC/MC_Mes7.csv"))
      return(mc_m7_total)
    }else if(mes==8){
      mc_m8_total <- read.csv(paste0("Matrizes_efetivas/MC/MC_Mes8.csv"))
      return(mc_m8_total)
    }
  }else if(metodo == "NSGA-II"){
    if(mes=="Normal"){
      nsga_normal_total <- read.csv(paste0("Matrizes_efetivas/NSGA/NSGA_Normal.csv"))
      return(nsga_normal_total)
    }else if(mes==1){
      nsga_m1_total <- read.csv(paste0("Matrizes_efetivas/NSGA/NSGA_Mes1.csv"))
      return(nsga_m1_total)
    }else if(mes==2){
      nsga_m2_total <- read.csv(paste0("Matrizes_efetivas/NSGA/NSGA_Mes2.csv"))
      return(nsga_m2_total)
    }else if(mes==3){
      nsga_m3_total <- read.csv(paste0("Matrizes_efetivas/NSGA/NSGA_Mes3.csv"))
      return(nsga_m3_total)
    }else if(mes==4){
      nsga_m4_total <- read.csv(paste0("Matrizes_efetivas/NSGA/NSGA_Mes4.csv"))
      return(nsga_m4_total)
    }else if(mes==5){
      nsga_m5_total <- read.csv(paste0("Matrizes_efetivas/NSGA/NSGA_Mes5.csv"))
      return(nsga_m5_total)
    }else if(mes==6){
      nsga_m6_total <- read.csv(paste0("Matrizes_efetivas/NSGA/NSGA_Mes6.csv"))
      return(nsga_m6_total)
    }else if(mes==7){
      nsga_m7_total <- read.csv(paste0("Matrizes_efetivas/NSGA/NSGA_Mes7.csv"))
      return(nsga_m7_total)
    }else if(mes==8){
      nsga_m8_total <- read.csv(paste0("Matrizes_efetivas/NSGA/NSGA_Mes8.csv"))
      return(nsga_m8_total)
    }
  }else if(metodo == "SANN_Add"){
    if(mes=="Normal"){
      s_A_normal_total <- read.csv(paste0("Matrizes_efetivas/SANN_aditivo/SANN_aditivo_Normal.csv"))
      return(s_A_normal_total)
    }else if(mes==1){
      s_A_m1_total <- read.csv(paste0("Matrizes_efetivas/SANN_aditivo/SANN_aditivo_Mes1.csv"))
      return(s_A_m1_total)
    }else if(mes==2){
      s_A_m2_total <- read.csv(paste0("Matrizes_efetivas/SANN_aditivo/SANN_aditivo_Mes2.csv"))
      return(s_A_m2_total)
    }else if(mes==3){
      s_A_m3_total <- read.csv(paste0("Matrizes_efetivas/SANN_aditivo/SANN_aditivo_Mes3.csv"))
      return(s_A_m3_total)
    }else if(mes==4){
      s_A_m4_total <- read.csv(paste0("Matrizes_efetivas/SANN_aditivo/SANN_aditivo_Mes4.csv"))
      return(s_A_m4_total)
    }else if(mes==5){
      s_A_m5_total <- read.csv(paste0("Matrizes_efetivas/SANN_aditivo/SANN_aditivo_Mes5.csv"))
      return(s_A_m5_total)
    }else if(mes==6){
      s_A_m6_total <- read.csv(paste0("Matrizes_efetivas/SANN_aditivo/SANN_aditivo_Mes6.csv"))
      return(s_A_m6_total)
    }else if(mes==7){
      s_A_m7_total <- read.csv(paste0("Matrizes_efetivas/SANN_aditivo/SANN_aditivo_Mes7.csv"))
      return(s_A_m7_total)
    }else if(mes==8){
      s_A_m8_total <- read.csv(paste0("Matrizes_efetivas/SANN_aditivo/SANN_aditivo_Mes8.csv"))
      return(s_A_m8_total)
    }
  }else if(metodo == "SANN_Mult"){
    if(mes=="Normal"){
      s_M_normal_total <- read.csv(paste0("Matrizes_efetivas/SANN_multiplicativo/SANN_Multiplicativo_Normal.csv"))
      return(s_M_normal_total)
    }else if(mes==1){
      s_M_m1_total <- read.csv(paste0("Matrizes_efetivas/SANN_multiplicativo/SANN_Multiplicativo_Mes1.csv"))
      return(s_M_m1_total)
    }else if(mes==2){
      s_M_m2_total <- read.csv(paste0("Matrizes_efetivas/SANN_multiplicativo/SANN_Multiplicativo_Mes2.csv"))
      return(s_M_m2_total)
    }else if(mes==3){
      s_M_m3_total <- read.csv(paste0("Matrizes_efetivas/SANN_multiplicativo/SANN_Multiplicativo_Mes3.csv"))
      return(s_M_m3_total)
    }else if(mes==4){
      s_M_m4_total <- read.csv(paste0("Matrizes_efetivas/SANN_multiplicativo/SANN_Multiplicativo_Mes4.csv"))
      return(s_M_m4_total)
    }else if(mes==5){
      s_M_m5_total <- read.csv(paste0("Matrizes_efetivas/SANN_multiplicativo/SANN_Multiplicativo_Mes5.csv"))
      return(s_M_m5_total)
    }else if(mes==6){
      s_M_m6_total <- read.csv(paste0("Matrizes_efetivas/SANN_multiplicativo/SANN_Multiplicativo_Mes6.csv"))
      return(s_M_m6_total)
    }else if(mes==7){
      s_M_m7_total <- read.csv(paste0("Matrizes_efetivas/SANN_multiplicativo/SANN_Multiplicativo_Mes7.csv"))
      return(s_M_m7_total)
    }else if(mes==8){
      s_M_m8_total <- read.csv(paste0("Matrizes_efetivas/SANN_multiplicativo/SANN_Multiplicativo_Mes8.csv"))
      return(s_M_m8_total)
    }
  }
}

leitura_otimizacao <- function(metodo,mes){
  if(metodo=="Diff_Ev"){
    if(mes=="Normal"){
      de_normal <- read.csv(paste0("ResultadosOtimizacao/DE/Normal_Differential_Evolution_resultados.csv"))
      return(de_normal)
    }else if(mes==1){
      de_m1 <- read.csv(paste0("ResultadosOtimizacao/DE/Mes1_Differential_Evolution_resultados.csv"))
      return(de_m1)
    }else if(mes==2){
      de_m2 <- read.csv(paste0("ResultadosOtimizacao/DE/Mes2_Differential_Evolution_resultados.csv"))
      return(de_m2)
    }else if(mes==3){
      de_m3 <- read.csv(paste0("ResultadosOtimizacao/DE/Mes3_Differential_Evolution_resultados.csv"))
      return(de_m3)
    }else if(mes==4){
      de_m4 <- read.csv(paste0("ResultadosOtimizacao/DE/Mes4_Differential_Evolution_resultados.csv"))
      return(de_m4)
    }else if(mes==5){
      de_m5 <- read.csv(paste0("ResultadosOtimizacao/DE/Mes5_Differential_Evolution_resultados.csv"))
      return(de_m5)
    }else if(mes==6){
      de_m6 <- read.csv(paste0("ResultadosOtimizacao/DE/Mes6_Differential_Evolution_resultados.csv"))
      return(de_m6)
    }else if(mes==7){
      de_m7 <- read.csv(paste0("ResultadosOtimizacao/DE/Mes7_Differential_Evolution_resultados.csv"))
      return(de_m7)
    }else if(mes==8){
      de_m8 <- read.csv(paste0("ResultadosOtimizacao/DE/Mes8_Differential_Evolution_resultados.csv"))
      return(de_m8)
    }
  }else if(metodo == "GE"){
    if(mes=="Normal"){
      ge_normal <- read.csv(paste0("ResultadosOtimizacao/GE/Normal_Genetic_Algorithm_resultados.csv"))
      return(ge_normal)
    }else if(mes==1){
      ge_m1 <- read.csv(paste0("ResultadosOtimizacao/GE/Mes1_Genetic_Algorithm_resultados.csv"))
      return(ge_m1)
    }else if(mes==2){
      ge_m2 <- read.csv(paste0("ResultadosOtimizacao/GE/Mes2_Genetic_Algorithm_resultados.csv"))
      return(ge_m2)
    }else if(mes==3){
      ge_m3 <- read.csv(paste0("ResultadosOtimizacao/GE/Mes3_Genetic_Algorithm_resultados.csv"))
      return(ge_m3)
    }else if(mes==4){
      ge_m4 <- read.csv(paste0("ResultadosOtimizacao/GE/Mes4_Genetic_Algorithm_resultados.csv"))
      return(ge_m4)
    }else if(mes==5){
      ge_m5 <- read.csv(paste0("ResultadosOtimizacao/GE/Mes5_Genetic_Algorithm_resultados.csv"))
      return(ge_m5)
    }else if(mes==6){
      ge_m6 <- read.csv(paste0("ResultadosOtimizacao/GE/Mes6_Genetic_Algorithm_resultados.csv"))
      return(ge_m6)
    }else if(mes==7){
      ge_m7 <- read.csv(paste0("ResultadosOtimizacao/GE/Mes7_Genetic_Algorithm_resultados.csv"))
      return(ge_m7)
    }else if(mes==8){
      ge_m8 <- read.csv(paste0("ResultadosOtimizacao/GE/Mes8_Genetic_Algorithm_resultados.csv"))
      return(ge_m8)
    }
  }else if(metodo == "HillC_Add"){
    if(mes=="Normal"){
      hcA_normal <- read.csv(paste0("ResultadosOtimizacao/HC_aditivo/Normal_Hill_Climbing_Aditivo_resultados.csv"))
      return(hcA_normal)
    }else if(mes==1){
      hcA_m1 <- read.csv(paste0("ResultadosOtimizacao/HC_aditivo/Mes1_Hill_Climbing_Aditivo_resultados.csv"))
      return(hcA_m1)
    }else if(mes==2){
      hcA_m2 <- read.csv(paste0("ResultadosOtimizacao/HC_aditivo/Mes2_Hill_Climbing_Aditivo_resultados.csv"))
      return(hcA_m2)
    }else if(mes==3){
      hcA_m3 <- read.csv(paste0("ResultadosOtimizacao/HC_aditivo/Mes3_Hill_Climbing_Aditivo_resultados.csv"))
      return(hcA_m3)
    }else if(mes==4){
      hcA_m4 <- read.csv(paste0("ResultadosOtimizacao/HC_aditivo/Mes4_Hill_Climbing_Aditivo_resultados.csv"))
      return(hcA_m4)
    }else if(mes==5){
      hcA_m5 <- read.csv(paste0("ResultadosOtimizacao/HC_aditivo/Mes5_Hill_Climbing_Aditivo_resultados.csv"))
      return(hcA_m5)
    }else if(mes==6){
      hcA_m6 <- read.csv(paste0("ResultadosOtimizacao/HC_aditivo/Mes6_Hill_Climbing_Aditivo_resultados.csv"))
      return(hcA_m6)
    }else if(mes==7){
      hcA_m7 <- read.csv(paste0("ResultadosOtimizacao/HC_aditivo/Mes7_Hill_Climbing_Aditivo_resultados.csv"))
      return(hcA_m7)
    }else if(mes==8){
      hcA_m8 <- read.csv(paste0("ResultadosOtimizacao/HC_aditivo/Mes8_Hill_Climbing_Aditivo_resultados.csv"))
      return(hcA_m8)
    }
  }else if(metodo == "HillC_Mult"){
    if(mes=="Normal"){
      hcM_normal <- read.csv(paste0("ResultadosOtimizacao/HC_multiplicativo/Normal_Hill_Climbing_Multiplicativo_resultados.csv"))
      return(hcM_normal)
    }else if(mes==1){
      hcM_m1 <- read.csv(paste0("ResultadosOtimizacao/HC_multiplicativo/Mes1_Hill_Climbing_Multiplicativo_resultados.csv"))
      return(hcM_m1)
    }else if(mes==2){
      hcM_m2 <- read.csv(paste0("ResultadosOtimizacao/HC_multiplicativo/Mes2_Hill_Climbing_Multiplicativo_resultados.csv"))
      return(hcM_m2)
    }else if(mes==3){
      hcM_m3 <- read.csv(paste0("ResultadosOtimizacao/HC_multiplicativo/Mes3_Hill_Climbing_Multiplicativo_resultados.csv"))
      return(hcM_m3)
    }else if(mes==4){
      hcM_m4 <- read.csv(paste0("ResultadosOtimizacao/HC_multiplicativo/Mes4_Hill_Climbing_Multiplicativo_resultados.csv"))
      return(hcM_m4)
    }else if(mes==5){
      hcM_m5 <- read.csv(paste0("ResultadosOtimizacao/HC_multiplicativo/Mes5_Hill_Climbing_Multiplicativo_resultados.csv"))
      return(hcM_m5)
    }else if(mes==6){
      hcM_m6 <- read.csv(paste0("ResultadosOtimizacao/HC_multiplicativo/Mes6_Hill_Climbing_Multiplicativo_resultados.csv"))
      return(hcM_m6)
    }else if(mes==7){
      hcM_m7 <- read.csv(paste0("ResultadosOtimizacao/HC_multiplicativo/Mes7_Hill_Climbing_Multiplicativo_resultados.csv"))
      return(hcM_m7)
    }else if(mes==8){
      hcM_m8 <- read.csv(paste0("ResultadosOtimizacao/HC_multiplicativo/Mes8_Hill_Climbing_Multiplicativo_resultados.csv"))
      return(hcM_m8)
    }
  }else if(metodo == "MC"){
    if(mes=="Normal"){
      mc_normal <- read.csv(paste0("ResultadosOtimizacao/MC/Normal_Monte_Carlo_resultados.csv"))
      return(mc_normal)
    }else if(mes==1){
      mc_m1 <- read.csv(paste0("ResultadosOtimizacao/MC/Mes1_Monte_Carlo_resultados.csv"))
      return(mc_m1)
    }else if(mes==2){
      mc_m2 <- read.csv(paste0("ResultadosOtimizacao/MC/Mes2_Monte_Carlo_resultados.csv"))
      return(mc_m2)
    }else if(mes==3){
      mc_m3 <- read.csv(paste0("ResultadosOtimizacao/MC/Mes3_Monte_Carlo_resultados.csv"))
      return(mc_m3)
    }else if(mes==4){
      mc_m4 <- read.csv(paste0("ResultadosOtimizacao/MC/Mes4_Monte_Carlo_resultados.csv"))
      return(mc_m4)
    }else if(mes==5){
      mc_m5 <- read.csv(paste0("ResultadosOtimizacao/MC/Mes5_Monte_Carlo_resultados.csv"))
      return(mc_m5)
    }else if(mes==6){
      mc_m6 <- read.csv(paste0("ResultadosOtimizacao/MC/Mes6_Monte_Carlo_resultados.csv"))
      return(mc_m6)
    }else if(mes==7){
      mc_m7 <- read.csv(paste0("ResultadosOtimizacao/MC/Mes7_Monte_Carlo_resultados.csv"))
      return(mc_m7)
    }else if(mes==8){
      mc_m8 <- read.csv(paste0("ResultadosOtimizacao/MC/Mes8_Monte_Carlo_resultados.csv"))
      return(mc_m8)
    }
  }else if(metodo == "NSGA-II"){
    if(mes=="Normal"){
      nsga_normal <- read.csv(paste0("ResultadosOtimizacao/NSGA/Normal_NSGA_resultados.csv"))
      return(nsga_normal)
    }else if(mes==1){
      nsga_m1 <- read.csv(paste0("ResultadosOtimizacao/NSGA/Mes1_NSGA_resultados.csv"))
      return(nsga_m1)
    }else if(mes==2){
      nsga_m2 <- read.csv(paste0("ResultadosOtimizacao/NSGA/Mes2_NSGA_resultados.csv"))
      return(nsga_m2)
    }else if(mes==3){
      nsga_m3 <- read.csv(paste0("ResultadosOtimizacao/NSGA/Mes3_NSGA_resultados.csv"))
      return(nsga_m3)
    }else if(mes==4){
      nsga_m4 <- read.csv(paste0("ResultadosOtimizacao/NSGA/Mes4_NSGA_resultados.csv"))
      return(nsga_m4)
    }else if(mes==5){
      nsga_m5 <- read.csv(paste0("ResultadosOtimizacao/NSGA/Mes5_NSGA_resultados.csv"))
      return(nsga_m5)
    }else if(mes==6){
      nsga_m6 <- read.csv(paste0("ResultadosOtimizacao/NSGA/Mes6_NSGA_resultados.csv"))
      return(nsga_m6)
    }else if(mes==7){
      nsga_m7 <- read.csv(paste0("ResultadosOtimizacao/NSGA/Mes7_NSGA_resultados.csv"))
      return(nsga_m7)
    }else if(mes==8){
      nsga_m8 <- read.csv(paste0("ResultadosOtimizacao/NSGA/Mes8_NSGA_resultados.csv"))
      return(nsga_m8)
    }
  }else if(metodo == "SANN_Add"){
    if(mes=="Normal"){
      s_A_normal <- read.csv(paste0("ResultadosOtimizacao/SANN_aditivo/Normal_SANN_Aditivo_resultados.csv"))
      return(s_A_normal)
    }else if(mes==1){
      s_A_m1 <- read.csv(paste0("ResultadosOtimizacao/SANN_aditivo/Mes1_SANN_Aditivo_resultados.csv"))
      return(s_A_m1)
    }else if(mes==2){
      s_A_m2 <- read.csv(paste0("ResultadosOtimizacao/SANN_aditivo/Mes2_SANN_Aditivo_resultados.csv"))
      return(s_A_m2)
    }else if(mes==3){
      s_A_m3 <- read.csv(paste0("ResultadosOtimizacao/SANN_aditivo/Mes3_SANN_Aditivo_resultados.csv"))
      return(s_A_m3)
    }else if(mes==4){
      s_A_m4 <- read.csv(paste0("ResultadosOtimizacao/SANN_aditivo/Mes4_SANN_Aditivo_resultados.csv"))
      return(s_A_m4)
    }else if(mes==5){
      s_A_m5 <- read.csv(paste0("ResultadosOtimizacao/SANN_aditivo/Mes5_SANN_Aditivo_resultados.csv"))
      return(s_A_m5)
    }else if(mes==6){
      s_A_m6 <- read.csv(paste0("ResultadosOtimizacao/SANN_aditivo/Mes6_SANN_Aditivo_resultados.csv"))
      return(s_A_m6)
    }else if(mes==7){
      s_A_m7 <- read.csv(paste0("ResultadosOtimizacao/SANN_aditivo/Mes7_SANN_Aditivo_resultados.csv"))
      return(s_A_m7)
    }else if(mes==8){
      s_A_m8 <- read.csv(paste0("ResultadosOtimizacao/SANN_aditivo/Mes8_SANN_Aditivo_resultados.csv"))
      return(s_A_m8)
    }
  }else if(metodo == "SANN_Mult"){
    if(mes=="Normal"){
      s_M_normal <- read.csv(paste0("ResultadosOtimizacao/SANN_multiplicativo/Normal_SANN_Multiplicativo_resultados.csv"))
      return(s_M_normal)
    }else if(mes==1){
      s_M_m1 <- read.csv(paste0("ResultadosOtimizacao/SANN_multiplicativo/Mes1_SANN_Multiplicativo_resultados.csv"))
      return(s_M_m1)
    }else if(mes==2){
      s_M_m2 <- read.csv(paste0("ResultadosOtimizacao/SANN_multiplicativo/Mes2_SANN_Multiplicativo_resultados.csv"))
      return(s_M_m2)
    }else if(mes==3){
      s_M_m3 <- read.csv(paste0("ResultadosOtimizacao/SANN_multiplicativo/Mes3_SANN_Multiplicativo_resultados.csv"))
      return(s_M_m3)
    }else if(mes==4){
      s_M_m4 <- read.csv(paste0("ResultadosOtimizacao/SANN_multiplicativo/Mes4_SANN_Multiplicativo_resultados.csv"))
      return(s_M_m4)
    }else if(mes==5){
      s_M_m5 <- read.csv(paste0("ResultadosOtimizacao/SANN_multiplicativo/Mes5_SANN_Multiplicativo_resultados.csv"))
      return(s_M_m5)
    }else if(mes==6){
      s_M_m6 <- read.csv(paste0("ResultadosOtimizacao/SANN_multiplicativo/Mes6_SANN_Multiplicativo_resultados.csv"))
      return(s_M_m6)
    }else if(mes==7){
      s_M_m7 <- read.csv(paste0("ResultadosOtimizacao/SANN_multiplicativo/Mes7_SANN_Multiplicativo_resultados.csv"))
      return(s_M_m7)
    }else if(mes==8){
      s_M_m8 <- read.csv(paste0("ResultadosOtimizacao/SANN_multiplicativo/Mes8_SANN_Multiplicativo_resultados.csv"))
      return(s_M_m8)
    }
  }
}

organizar_funcionarios <- function(funcionarios) {
  tabela <- data.frame(
    Categoria = c("Juniores", "Normais", "Senior"),
    Dep1 = c(funcionarios[1], funcionarios[5], funcionarios[9]),
    Dep2 = c(funcionarios[2], funcionarios[6], funcionarios[10]),
    Dep3 = c(funcionarios[3], funcionarios[7], funcionarios[11]),
    Dep4 = c(funcionarios[4], funcionarios[8], funcionarios[12])
  )
  
  return(tabela)
}
organizar_encomendas <- function(encomendas) {
  tabela <- data.frame(
    Semana = c("Semana_1", "Semana_2", "Semana_3", "Semana_4"),
    Dep1 = c(encomendas[1], encomendas[2], encomendas[3], encomendas[4]),
    Dep2 = c(encomendas[5], encomendas[6], encomendas[7], encomendas[8]),
    Dep3 = c(encomendas[9], encomendas[10], encomendas[11], encomendas[12]),
    Dep4 = c(encomendas[13], encomendas[14], encomendas[15], encomendas[16])
  )
  
  return(tabela)
}

ui <- dashboardPage(
  dashboardHeader(title = "TIAPOSE"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Previsões", tabName = "previsoes"),
      menuItem("Otimização", tabName = "otimizacao"),
      menuItem("Dados", tabName = "dados")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "previsoes",
              h1("Página de Previsões"),
              h3("Previsão com o Melhor Modelo - ARIMA"),
              # selectInput("modelo", label = "Modelo de Previsão:",
              #             choices = c("Arima", "Holt Winters", "Prophet", "MLPE"),
              #             selected = "Arima"),
              dataTableOutput("tabela_previsoes"), 
              h3("Growing Window - Selecione o mês:"),
              selectInput("semana", label = "Mês:",
                          choices = 1:8,
                          selected = 1),
              dataTableOutput("tabela_previsoes_growing"),
              h3("Métricas GrowingWindow:"),
              fluidRow(
                column(6, 
                       h4("Dep1", style = "text-align: center;"),
                       h4(textOutput("dep1_title"), style = "text-align: center;"),
                       imageOutput("grafico_slecionado")
                ),
                column(6, 
                       h4("Dep2", style = "text-align: center;"),
                       h4(textOutput("dep2_title"), style = "text-align: center;"),
                       imageOutput("grafico_slecionado_2")
                )
              ),
              fluidRow(
                column(6, 
                       h4("Dep3", style = "text-align: center;"),
                       h4(textOutput("dep3_title"), style = "text-align: center;"),
                       imageOutput("grafico_slecionado_3")
                ),
                column(6, 
                       h4("Dep4", style = "text-align: center;"),
                       h4(textOutput("dep4_title"), style = "text-align: center;"),
                       imageOutput("grafico_slecionado_4")
                )
              )
      ),
      tabItem(tabName = "otimizacao",
              selectInput("mes_Otimizacao", label = "Vendas:",
                          c("Normal", 1:8),
                          selected = "Normal"),
              selectInput("metodo_Otimizacao", label = "Metodo:",
                          c("Diff_Ev", "GE", "HillC_Add", "HillC_Mult","MC","NSGA-II","SANN_Add","SANN_Mult"),
                                                    selected = "MC"),
              # textOutput("texto_caixa1"),
              # textOutput("texto_caixa2"),
              # textOutput("texto_caixa3"),
              h3("Plano de Contratação de Funcionários - Por tipo e departamento"),
              uiOutput("tabela_func"),
              tags$br(),
              h3("Divisão de Custos Total - Por tipo"),
              uiOutput("tabela_custos_tipo"),
              tags$br(),
              h3("Número de Funcionários (total)"),
              textOutput("texto_caixa_effort_trab"),
              tags$br(),
              h3("Suporte Máximo do funcionários - Por departamento"),
              uiOutput("tabela_suporte_max"),
              tags$br(),
              h3("Plano de Encomendas - Por semana e departamento"),
              uiOutput("tabela_enc"),
              tags$br(),
              h3("Número de encomendas (total)"),
              textOutput("texto_caixa_effort_encomendas"),
              tags$br(),
              h3("Vendas Semanais - Quantidade vendida"),
              uiOutput("tabela_vendas_semanais"),
              tags$br(),
              h3("Vendas Semanais - Ganho USD"),
              uiOutput("tabela_vendas_semanais_USD"),
              tags$br(),
              h3("Total de Receita (Ganho)"),
              textOutput("texto_caixa_total_rev"),
              tags$br(),
              h3("Stock - Por semanda e departamento"),
              uiOutput("tabela_stock"),
              tags$br(),
              h3("Custo Stock - Por semanda e departamento"),
              uiOutput("tabela_custo_stock"),
              tags$br(),
              h3("Custo total de stock"),
              textOutput("texto_caixa_custo_total_stock"),
              tags$br(),
              h3("Custo total do plano"),
              textOutput("texto_caixa_custo_total_plano"),
              tags$br(),
              textOutput("texto_caixa3"),
              tags$div(style = "margin-top: 20px;", textOutput("texto_caixa3")),
              
      ),
      tabItem(tabName = "dados",
              h2("Página de Dados"),
              dataTableOutput("tabela_dados")
      )
    )
  )
)
# 
# atualizar_texto_caixas <- function(csv) {
#   texto_caixa1 <- paste("Texto para caixa 1 - Método:", toString(csv$Solucao))
#   texto_caixa2 <- paste("Texto para caixa 2 - Método:", toString(csv$Valor_Solucao))
#   texto_caixa3 <- paste("Texto para caixa 3 - Método:", toString(csv))
#   return(list(texto_caixa1, texto_caixa2, texto_caixa3))
# }

server <- function(input, output, session) {
  
  # Renderiza a tabela com os dados
  output$tabela_dados <- DT::renderDT({
    dadosWalmart
    
  })
  
  observe({
    req(input$semana)  # Garante que um modelo e uma semana foram selecionados antes de continuar
    #modelo <- tolower(input$modelo)
    semana <- input$semana
    
    arquivo_previsao <- paste0("Pred_Normal/previsoes_finais_arima.csv")
    arquivo_growing <- paste0("GrowingFinalArima/matriz_final_Arima","_",semana, ".csv")
    
    # grafico_normal_1 <- paste0("ARIMA - Dep1.jpg")
    # grafico_normal_2 <- paste0("ARIMA - Dep2.jpg")
    # grafico_normal_3 <- paste0("ARIMA - Dep3.jpg")
    # grafico_normal_4 <- paste0("ARIMA - Dep4.jpg")
    
    valores_reais_growing_total <- read.csv(paste0("ComparacaoGrowing/valores_reais_",semana,".csv"), header = FALSE,as.is = TRUE)
    valores_reais_growing<- valores_reais_growing_total$V1[1:16]
    
    
    gráficos_growing <- list.files("ArimaGrowingGraphs", full.names = TRUE)

    if (file.exists(arquivo_previsao) && file.exists(arquivo_growing)) {
      output$tabela_previsoes <- DT::renderDT({
        round(read.csv(arquivo_previsao, header = TRUE))
      })
      output$tabela_previsoes_growing <- DT::renderDT({
        #round(read.csv(arquivo_growing, header = TRUE))
        
        tabela_existente <- round(read.csv(arquivo_growing, header = TRUE))
        
        # Lendo os valores reais
        valores_reais_growing_total <- read.csv(paste0("ComparacaoGrowing/valores_reais_", semana, ".csv"), header = FALSE, as.is = TRUE)
        valores_reais_growing <- valores_reais_growing_total$V1[1:16]
        valores_reais_growing <- matrix(valores_reais_growing, ncol = 4)
        
        tabela_com_colunas <- cbind(VR_dep1 = valores_reais_growing[, 1],
                                    "PREV_dep1" = tabela_existente$V1,
                                    VR_dep2 = valores_reais_growing[, 2],
                                    "PREV_dep2" = tabela_existente$V2,
                                    VR_dep3 = valores_reais_growing[, 3],
                                    "PREV_dep3" = tabela_existente$V3,
                                    VR_dep4 = valores_reais_growing[, 4],
                                    "PREV_dep4" = tabela_existente$V4)
        
        datatable(tabela_com_colunas, 
                  options = list(dom = 't', pageLength = 20),
                  rownames = FALSE) %>%
          formatStyle(columns = c(2, 4, 6, 8), backgroundColor = "lightgreen") # Aplicando cor de fundo verde claro para as colunas de previsão
        # Retornando a tabela modificada
        #tabela_com_colunas
      })
      
    } else {
      # Se o arquivo correspondente ao modelo selecionado não existir, retorna NULL
      output$tabela_previsoes <- DT::renderDT({
        print("Outro")
        NULL
      })
    }
    
    req(input$mes_Otimizacao)
    req(input$metodo_Otimizacao)
    mes_escolhido <- input$mes_Otimizacao
    metodo_escolhido <- input$metodo_Otimizacao
    dados_totais <- leitura_otimizacao_planos_totais(metodo_escolhido,mes_escolhido)
    dados <- leitura_otimizacao(metodo_escolhido,mes_escolhido)
    #print(dados_totais)
     
     funcionarios <- dados$Solucao[1:12]
     encomendas <- dados$Solucao[13:28]
     lucro <- dados$Valor_Solucao[1:2]
     
     tabela_funcionarios <- organizar_funcionarios(funcionarios)
     tabela_encomendas <- organizar_encomendas(encomendas)
     
     output$tabela_func <- renderUI({
       HTML(
         paste0(
           "<table border='1' style='width:100%; text-align: left;'>",
           "<tr><th>Tipo de Funcionário</th><th>Dep1</th><th>Dep2</th><th>Dep3</th><th>Dep4</th></tr>",
           paste(
             "<tr><td>", tabela_funcionarios$Categoria[1], "</td><td>", tabela_funcionarios$Dep1[1], "</td><td>", tabela_funcionarios$Dep2[1], "</td><td>", tabela_funcionarios$Dep3[1], "</td><td>", tabela_funcionarios$Dep4[1], "</td></tr>",
             "<tr><td>", tabela_funcionarios$Categoria[2], "</td><td>", tabela_funcionarios$Dep1[2], "</td><td>", tabela_funcionarios$Dep2[2], "</td><td>", tabela_funcionarios$Dep3[2], "</td><td>", tabela_funcionarios$Dep4[2], "</td></tr>",
             "<tr><td>", tabela_funcionarios$Categoria[3], "</td><td>", tabela_funcionarios$Dep1[3], "</td><td>", tabela_funcionarios$Dep2[3], "</td><td>", tabela_funcionarios$Dep3[3], "</td><td>", tabela_funcionarios$Dep4[3], "</td></tr>",
             sep = ""
           ),
           "</table>"
         )
       )
     })
     output$tabela_enc <- renderUI({
       HTML(
         paste0(
           "<table border='1' style='width:100%; text-align: left;'>",
           "<tr><th>Semana</th><th>Dep1</th><th>Dep2</th><th>Dep3</th><th>Dep4</th></tr>",
           paste(
             "<tr><td>", tabela_encomendas$Semana[1], "</td><td>", tabela_encomendas$Dep1[1], "</td><td>", tabela_encomendas$Dep2[1], "</td><td>", tabela_encomendas$Dep3[1], "</td><td>", tabela_encomendas$Dep4[1], "</td></tr>",
             "<tr><td>", tabela_encomendas$Semana[2], "</td><td>", tabela_encomendas$Dep1[2], "</td><td>", tabela_encomendas$Dep2[2], "</td><td>", tabela_encomendas$Dep3[2], "</td><td>", tabela_encomendas$Dep4[2], "</td></tr>",
             "<tr><td>", tabela_encomendas$Semana[3], "</td><td>", tabela_encomendas$Dep1[3], "</td><td>", tabela_encomendas$Dep2[3], "</td><td>", tabela_encomendas$Dep3[3], "</td><td>", tabela_encomendas$Dep4[3], "</td></tr>",
             "<tr><td>", tabela_encomendas$Semana[4], "</td><td>", tabela_encomendas$Dep1[4], "</td><td>", tabela_encomendas$Dep2[4], "</td><td>", tabela_encomendas$Dep3[4], "</td><td>", tabela_encomendas$Dep4[4], "</td></tr>",
             sep = ""
           ),
           "</table>"
         )
       )
     })
     output$tabela_custos_tipo <- renderUI({
       HTML(
         paste0(
           "<table border='1' style='width:100%; text-align: left;'>",
           "<tr><th>Tipo de custo</th><th>Custos</th></tr>",
           paste(
             "<tr><td>Funcionários</td><td>", dados_totais$Total_custo_trabalhadores[1], "</td></tr>",
             "<tr><td>Encomendas</td><td>", dados_totais$Total_custo_encomendas[2],"</td></tr>",
             "<tr><td>Stock</td><td>", dados_totais$Total_custo_stock[1], "</td></tr>",
             sep = ""
           ),
           "</table>"
         )
       )
     })
     output$texto_caixa_effort_trab <- renderText({
       paste("Effort > Total de funcionários:", dados_totais$Effort_total_trabalhadores[1])
     })
     output$tabela_suporte_max <- renderUI({
       HTML(
         paste0(
           "<table border='1' style='width:100%; text-align: left;'>",
           "<tr><th>Departamento</th><th>Suporte Máximo</th></tr>",
           paste(
             "<tr><td>Dep1</td><td>", dados_totais$Trabalhadores_max_suport_dep[1], "</td></tr>",
             "<tr><td>Dep2</td><td>", dados_totais$Trabalhadores_max_suport_dep[2],"</td></tr>",
             "<tr><td>Dep3</td><td>", dados_totais$Trabalhadores_max_suport_dep[3], "</td></tr>",
             "<tr><td>Dep4</td><td>", dados_totais$Trabalhadores_max_suport_dep[4], "</td></tr>",
             sep = ""
           ),
           "</table>"
         )
       )
     })
     output$texto_caixa_effort_encomendas <- renderText({
       paste("Effort > Total de encomendas:", dados_totais$Effort_total_encomendas[1])
     })
     output$tabela_vendas_semanais <- renderUI({
       HTML(
         paste0(
           "<table border='1' style='width:100%; text-align: left;'>",
           "<tr><th>Semana</th><th>Dep1</th><th>Dep2</th><th>Dep3</th><th>Dep4</th></tr>",
           paste(
             "<tr><td>", "Semana_1", "</td><td>", dados_totais$Sales_realizadas.1[1], "</td><td>", dados_totais$Sales_realizadas.2[1], "</td><td>", dados_totais$Sales_realizadas.3[1], "</td><td>", dados_totais$Sales_realizadas.4[1], "</td></tr>",
             "<tr><td>", "Semana_2", "</td><td>", dados_totais$Sales_realizadas.1[2], "</td><td>", dados_totais$Sales_realizadas.2[2], "</td><td>", dados_totais$Sales_realizadas.3[2], "</td><td>", dados_totais$Sales_realizadas.4[2], "</td></tr>",
             "<tr><td>", "Semana_3", "</td><td>", dados_totais$Sales_realizadas.1[3], "</td><td>", dados_totais$Sales_realizadas.2[3], "</td><td>", dados_totais$Sales_realizadas.3[3], "</td><td>", dados_totais$Sales_realizadas.4[3], "</td></tr>",
             "<tr><td>", "Semana_4", "</td><td>", dados_totais$Sales_realizadas.1[4], "</td><td>", dados_totais$Sales_realizadas.2[4], "</td><td>", dados_totais$Sales_realizadas.3[4], "</td><td>", dados_totais$Sales_realizadas.4[4], "</td></tr>",
             sep = ""
           ),
           "</table>"
         )
       )
     })
     output$tabela_vendas_semanais_USD <- renderUI({
       HTML(
         paste0(
           "<table border='1' style='width:100%; text-align: left;'>",
           "<tr><th>Semana</th><th>Dep1</th><th>Dep2</th><th>Dep3</th><th>Dep4</th></tr>",
           paste(
             "<tr><td>", "Semana_1", "</td><td>", dados_totais$Sales_realizadas_USD.1[1], "</td><td>", dados_totais$Sales_realizadas_USD.2[1], "</td><td>", dados_totais$Sales_realizadas_USD.3[1], "</td><td>", dados_totais$Sales_realizadas_USD.4[1], "</td></tr>",
             "<tr><td>", "Semana_2", "</td><td>", dados_totais$Sales_realizadas_USD.1[2], "</td><td>", dados_totais$Sales_realizadas_USD.2[2], "</td><td>", dados_totais$Sales_realizadas_USD.3[2], "</td><td>", dados_totais$Sales_realizadas_USD.4[2], "</td></tr>",
             "<tr><td>", "Semana_3", "</td><td>", dados_totais$Sales_realizadas_USD.1[3], "</td><td>", dados_totais$Sales_realizadas_USD.2[3], "</td><td>", dados_totais$Sales_realizadas_USD.3[3], "</td><td>", dados_totais$Sales_realizadas_USD.4[3], "</td></tr>",
             "<tr><td>", "Semana_4", "</td><td>", dados_totais$Sales_realizadas_USD.1[4], "</td><td>", dados_totais$Sales_realizadas_USD.2[4], "</td><td>", dados_totais$Sales_realizadas_USD.3[4], "</td><td>", dados_totais$Sales_realizadas_USD.4[4], "</td></tr>",
             sep = ""
           ),
           "</table>"
         )
       )
     })
     output$texto_caixa_total_rev <- renderText({
       paste("Total de receita das vendas:", dados_totais$Total_rev[1])
     })
     output$tabela_stock <- renderUI({
       HTML(
         paste0(
           "<table border='1' style='width:100%; text-align: left;'>",
           "<tr><th>Semana</th><th>Dep1</th><th>Dep2</th><th>Dep3</th><th>Dep4</th></tr>",
           paste(
             "<tr><td>", "Semana_1", "</td><td>", dados_totais$Stock_val.1[1], "</td><td>", dados_totais$Stock_val.2[1], "</td><td>", dados_totais$Stock_val.3[1], "</td><td>", dados_totais$Stock_val.4[1], "</td></tr>",
             "<tr><td>", "Semana_2", "</td><td>", dados_totais$Stock_val.1[2], "</td><td>", dados_totais$Stock_val.2[2], "</td><td>", dados_totais$Stock_val.3[2], "</td><td>", dados_totais$Stock_val.4[2], "</td></tr>",
             "<tr><td>", "Semana_3", "</td><td>", dados_totais$Stock_val.1[3], "</td><td>", dados_totais$Stock_val.2[3], "</td><td>", dados_totais$Stock_val.3[3], "</td><td>", dados_totais$Stock_val.4[3], "</td></tr>",
             "<tr><td>", "Semana_4", "</td><td>", dados_totais$Stock_val.1[4], "</td><td>", dados_totais$Stock_val.2[4], "</td><td>", dados_totais$Stock_val.3[4], "</td><td>", dados_totais$Stock_val.4[4], "</td></tr>",
             sep = ""
           ),
           "</table>"
         )
       )
     })
     output$tabela_custo_stock <- renderUI({
       HTML(
         paste0(
           "<table border='1' style='width:100%; text-align: left;'>",
           "<tr><th>Semana</th><th>Dep1</th><th>Dep2</th><th>Dep3</th><th>Dep4</th></tr>",
           paste(
             "<tr><td>", "Semana_1", "</td><td>", dados_totais$Stock_val_USD.1[1], "</td><td>", dados_totais$Stock_val_USD.2[1], "</td><td>", dados_totais$Stock_val_USD.3[1], "</td><td>", dados_totais$Stock_val_USD.4[1], "</td></tr>",
             "<tr><td>", "Semana_2", "</td><td>", dados_totais$Stock_val_USD.1[2], "</td><td>", dados_totais$Stock_val_USD.2[2], "</td><td>", dados_totais$Stock_val_USD.3[2], "</td><td>", dados_totais$Stock_val_USD.4[2], "</td></tr>",
             "<tr><td>", "Semana_3", "</td><td>", dados_totais$Stock_val_USD.1[3], "</td><td>", dados_totais$Stock_val_USD.2[3], "</td><td>", dados_totais$Stock_val_USD.3[3], "</td><td>", dados_totais$Stock_val_USD.4[3], "</td></tr>",
             "<tr><td>", "Semana_4", "</td><td>", dados_totais$Stock_val_USD.1[4], "</td><td>", dados_totais$Stock_val_USD.2[4], "</td><td>", dados_totais$Stock_val_USD.3[4], "</td><td>", dados_totais$Stock_val_USD.4[4], "</td></tr>",
             sep = ""
           ),
           "</table>"
         )
       )
     })
     output$texto_caixa_custo_total_stock <- renderText({
       paste("Total de custo do stock:", dados_totais$Total_custo_stock[1])
     })
     output$texto_caixa_custo_total_plano <- renderText({
       total_custo_trabalhadores_f <- dados_totais$Total_custo_trabalhadores[1]
       total_custo_encomendas_f <- dados_totais$Total_custo_encomendas[1]
       total_custo_stock_f <- dados_totais$Total_custo_stock[1]
       total_custo_plano_f <- total_custo_trabalhadores_f + total_custo_encomendas_f + total_custo_stock_f
       
       paste("Total de custo do plano:", total_custo_trabalhadores_f, "+", total_custo_encomendas_f, "+", total_custo_stock_f, "=", total_custo_plano_f)
       
     })
     
     
     output$texto_caixa3 <- renderText({
       if (lucro[1] == lucro[2]) {
         paste("Lucro Total do Mês:", lucro[1], "| Esforço Lucro Total do Mês:", dados_totais$Effort_mes[1])
       } else {
         paste("Lucro Total do Mês:", lucro[1], "| Esforço Lucro Total do Mês:", lucro[2])
       }
     })
     
    
    
  })
  
  observeEvent(input$tabs, {
    # Atualiza o item selecionado na barra lateral quando o menu é clicado
    updateTabItems(session, "tabs", input$tabs)
  })
  
  output$grafico_slecionado <- renderImage({
    req(input$semana)
    
    # Cria o nome do arquivo correspondente ao modelo e semana selecionados
    #modelo <- tolower(input$modelo)
    semana <- input$semana
    arquivo_grafico <- normalizePath(file.path('ArimaGrowingGraphs', paste0('rplot_1_', semana, '.jpg')))
    
    # Verifica se o arquivo de gráfico existe
    if (file.exists(arquivo_grafico)) {
      list(src = arquivo_grafico,
           contentType = 'image/jpg',
           width = 400,
           height = 300,
           alt = "This is alternate text")    

      } else {
      h3("Gráfico não encontrado para o modelo e semana selecionados.")
    }
  }, deleteFile = FALSE)
  
  output$dep1_title <- renderText({
    req(input$semana)
    semana <- input$semana
    dep1Metricas <- read.csv(paste0("medianaMetricas/metricas_final_", semana, ".csv"), header = TRUE,as.is = TRUE)
    paste("MAE-", round(dep1Metricas[1, 1],2),
    "| NMAE-", round(dep1Metricas[1, 2],2))
  })
  
  
  output$grafico_slecionado_2 <- renderImage({
    req(input$semana)
    
    # Cria o nome do arquivo correspondente ao modelo e semana selecionados
    #modelo <- tolower(input$modelo)
    semana <- input$semana
    arquivo_grafico <- normalizePath(file.path('ArimaGrowingGraphs', paste0('rplot_2_', semana, '.jpg')))
    
    # Verifica se o arquivo de gráfico existe
    if (file.exists(arquivo_grafico)) {
      list(src = arquivo_grafico,
           contentType = 'image/jpg',
           width = 400,
           height = 300,
           alt = "This is alternate text")    } else {
             h3("Gráfico não encontrado para o modelo e semana selecionados.")
           }
  }, deleteFile = FALSE)
  
  
  output$dep2_title <- renderText({
    req(input$semana)
    semana <- input$semana
    dep1Metricas <- read.csv(paste0("medianaMetricas/metricas_final_", semana, ".csv"), header = TRUE,as.is = TRUE)
    paste("MAE-", round(dep1Metricas[2, 1],2),
          "| NMAE-", round(dep1Metricas[2, 2],2))
  })
  
  output$grafico_slecionado_3 <- renderImage({
    req(input$semana)
    
    # Cria o nome do arquivo correspondente ao modelo e semana selecionados
    #modelo <- tolower(input$modelo)
    semana <- input$semana
    arquivo_grafico <- normalizePath(file.path('ArimaGrowingGraphs', paste0('rplot_3_', semana, '.jpg')))
    
    # Verifica se o arquivo de gráfico existe
    if (file.exists(arquivo_grafico)) {
      list(src = arquivo_grafico,
           contentType = 'image/jpg',
           width = 400,
           height = 300,
           alt = "This is alternate text")    } else {
             h3("Gráfico não encontrado para o modelo e semana selecionados.")
           }
  }, deleteFile = FALSE)
  
  output$dep3_title <- renderText({
    req(input$semana)
    semana <- input$semana
    dep1Metricas <- read.csv(paste0("medianaMetricas/metricas_final_", semana, ".csv"), header = TRUE,as.is = TRUE)
    paste("MAE-", round(dep1Metricas[3, 1],2),
          "| NMAE-", round(dep1Metricas[3, 2],2))
  })
  
  
  
  output$grafico_slecionado_4 <- renderImage({
    req(input$semana)
    
    # Cria o nome do arquivo correspondente ao modelo e semana selecionados
    #modelo <- tolower(input$modelo)
    semana <- input$semana
    arquivo_grafico <- normalizePath(file.path('ArimaGrowingGraphs', paste0('rplot_4_', semana, '.jpg')))
    
    # Verifica se o arquivo de gráfico existe
    if (file.exists(arquivo_grafico)) {
      list(src = arquivo_grafico,
           contentType = 'image/jpg',
           width = 400,
           height = 300,
           alt = "This is alternate text")    
      } else {
             h3("Gráfico não encontrado para o modelo e semana selecionados.")
           }
  }, deleteFile = FALSE)
  
  output$dep4_title <- renderText({
    req(input$semana)
    semana <- input$semana
    dep1Metricas <- read.csv(paste0("medianaMetricas/metricas_final_", semana, ".csv"), header = TRUE,as.is = TRUE)
    paste("MAE-", round(dep1Metricas[4, 1],2),
          "| NMAE-", round(dep1Metricas[4, 2],2))
  })
  
  
  
  
}
shinyApp(ui = ui, server = server)
