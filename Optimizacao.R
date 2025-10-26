###############################################################################
# Carregamento de librarias
###############################################################################

library(zoo)
library(stats)
library(genalg) # Para o genetic
library(DEoptim)
library(mco) # load mco package
library(data.table)


source("hill.R") #  hclimbing is defined here


###############################################################################
# Variaveis de Teste:
###############################################################################

# # Vetor vendas( apenas utilizada para a primeira função)
# vendas <- c(54480,42221,36267,35283,
#             159460,156945,146388,132156,
#             63584,62888,62768,60279,
#             127009,124560,123346,117375)
# 
# # Vendas por semana de cada departamento
# vendas_matrix <- matrix(c(54480,42221,36267,35283,
#                     159460,156945,146388,132156,
#                     63584,62888,62768,60279,
#                     127009,124560,123346,117375),nrow =4, byrow=FALSE)
# 
# # Funcionários contratados
# s1_matrix <- matrix(c(5,4,3,2,
#                    6,5,4,3,
#                    7,6,5,4),nrow =3, byrow=TRUE)
# 
# # Encomendas realizadas:
# order_matrix <- matrix(c(61662,0,12985,39924,
#                          78292,0,55403,75160,
#                          56434,0,69133,62131,
#                          24182,0,37167,99708),nrow =4, byrow=FALSE)

###############################################################################
# FUNÇÕES PARA CALCULO DE PROFIT:
###############################################################################

# Função carregamento Previsão final
realPrevCarre <- function(){
  realArima <- read.csv("Pred_Normal/previsoes_finais_arima.csv", header = TRUE,stringsAsFactors = FALSE)
  colnames(realArima) <- rep("", length(colnames(realArima)))
  
  # Remover os nomes dos índices
  rownames(realArima) <- NULL
  
  # Converter os valores para números e arredondar
  realArima <- round(as.matrix(realArima), 0)
  
  assign("realArima", realArima, envir = .GlobalEnv)
}

# Função selecinar previsão final como vendas
select_real_prev <- function(){
    assign(paste0("vendas_matrix"), realArima, envir = .GlobalEnv)
}

# Função carregamento GrowingIWindow:
growingWindowCarre <- function(){
  G_Ar_1 <- read.csv("GrowingFinalArima/matriz_final_Arima_1.csv", header = TRUE,stringsAsFactors = FALSE)
  G_Ar_2 <- read.csv("GrowingFinalArima/matriz_final_Arima_2.csv", header = TRUE,stringsAsFactors = FALSE)
  G_Ar_3 <- read.csv("GrowingFinalArima/matriz_final_Arima_3.csv", header = TRUE,stringsAsFactors = FALSE)
  G_Ar_4 <- read.csv("GrowingFinalArima/matriz_final_Arima_4.csv", header = TRUE,stringsAsFactors = FALSE)
  G_Ar_5 <- read.csv("GrowingFinalArima/matriz_final_Arima_5.csv", header = TRUE,stringsAsFactors = FALSE)
  G_Ar_6 <- read.csv("GrowingFinalArima/matriz_final_Arima_6.csv", header = TRUE,stringsAsFactors = FALSE)
  G_Ar_7 <- read.csv("GrowingFinalArima/matriz_final_Arima_7.csv", header = TRUE,stringsAsFactors = FALSE)
  G_Ar_8 <- read.csv("GrowingFinalArima/matriz_final_Arima_8.csv", header = TRUE,stringsAsFactors = FALSE)
  
  #print(round(t(matrix(dadosTeste$V1, ncol = 4, byrow = TRUE))))
  colnames(G_Ar_1) <- rep("", length(colnames(G_Ar_1)))
  rownames(G_Ar_1) <- NULL
  G_Ar_1 <- round(as.matrix(G_Ar_1), 0)
  assign("G_Ar_1", G_Ar_1, envir = .GlobalEnv)
  colnames(G_Ar_2) <- rep("", length(colnames(G_Ar_2)))
  rownames(G_Ar_2) <- NULL
  G_Ar_2 <- round(as.matrix(G_Ar_2), 0)
  assign("G_Ar_2", G_Ar_2, envir = .GlobalEnv)
  colnames(G_Ar_3) <- rep("", length(colnames(G_Ar_3)))
  rownames(G_Ar_3) <- NULL
  G_Ar_3 <- round(as.matrix(G_Ar_3), 0)
  assign("G_Ar_3", G_Ar_3, envir = .GlobalEnv)
  colnames(G_Ar_4) <- rep("", length(colnames(G_Ar_4)))
  rownames(G_Ar_4) <- NULL
  G_Ar_4 <- round(as.matrix(G_Ar_4), 0)
  assign("G_Ar_4", G_Ar_4, envir = .GlobalEnv)
  colnames(G_Ar_5) <- rep("", length(colnames(G_Ar_5)))
  rownames(G_Ar_5) <- NULL
  G_Ar_5 <- round(as.matrix(G_Ar_5), 0)
  assign("G_Ar_5", G_Ar_5, envir = .GlobalEnv)
  colnames(G_Ar_6) <- rep("", length(colnames(G_Ar_6)))
  rownames(G_Ar_6) <- NULL
  G_Ar_6 <- round(as.matrix(G_Ar_6), 0)
  assign("G_Ar_6", G_Ar_6, envir = .GlobalEnv)
  colnames(G_Ar_7) <- rep("", length(colnames(G_Ar_7)))
  rownames(G_Ar_7) <- NULL
  G_Ar_7 <- round(as.matrix(G_Ar_7), 0)
  assign("G_Ar_7", G_Ar_7, envir = .GlobalEnv)
  colnames(G_Ar_8) <- rep("", length(colnames(G_Ar_8)))
  rownames(G_Ar_8) <- NULL
  G_Ar_8 <- round(as.matrix(G_Ar_8), 0)
  assign("G_Ar_8", G_Ar_8, envir = .GlobalEnv)
}

# Função selecionar mes GrowingWundow
select_mes_Ar_growing <- function(flag){
  if(flag == 1) {
    assign(paste0("vendas_matrix"), G_Ar_1, envir = .GlobalEnv)
  } else if(flag == 2) {
    assign(paste0("vendas_matrix"), G_Ar_2, envir = .GlobalEnv)
  } else if(flag == 3) {
    assign(paste0("vendas_matrix"), G_Ar_3, envir = .GlobalEnv)
  } else if(flag == 4) {
    assign(paste0("vendas_matrix"), G_Ar_4, envir = .GlobalEnv)
  } else if(flag == 5) {
    assign(paste0("vendas_matrix"), G_Ar_5, envir = .GlobalEnv)
  } else if(flag == 6) {
    assign(paste0("vendas_matrix"), G_Ar_6, envir = .GlobalEnv)
  } else if(flag == 7) {
    assign(paste0("vendas_matrix"), G_Ar_7, envir = .GlobalEnv)
  } else if(flag == 8) {
    assign(paste0("vendas_matrix"), G_Ar_8, envir = .GlobalEnv)
  }
}

# Soma das vendas semanais por departamento:
soma_vendas_sema <- function(vendas_previstas){
  vendas_mensais_dep <- rollapply(vendas, width = 4, FUN = sum, by = 4)
  for (i in 1:length(vendas_mensais_dep)) {
    assign(paste0("soma_", i), vendas_mensais_dep[i], envir = .GlobalEnv)
  }
}

# Função Custo total funcionários:
custo_funcionarios <- function(matrix_func){
  valor <- 0
  for (i in 1:4) {
    valor <- valor + matrix_func[1,i] * 6000
  }
  for (i in 1:4) {
    valor <- valor + matrix_func[2,i] * 8000
  }
  for (i in 1:4) {
    valor <- valor + matrix_func[3,i] * 9750
  }
  assign(paste0("total_c_funcionarios"), valor, envir = .GlobalEnv)
  
  return(valor)
}

# Função contar numero de funcionários contratados:
contar_funcionarios <- function(matrix_func){
  assign(paste0("ValorT_funcionarios"), sum(matrix_func), envir = .GlobalEnv)
  
  return(sum(matrix_func))
}

# Função suporte maximo pelos funcionarios (Por departamento):
max_suporte_departamento <- function(matrix_func){
  suporte_maximo <- numeric(nrow(matrix_func))
  for (i in 1:ncol(matrix_func)) {
    suporte_maximo[i] <- matrix_func[1, i] * 4000 +
      matrix_func[2, i] * 7000 +  
      matrix_func[3, i] * 9500    
  }
  return(suporte_maximo)
}

# Função calcular custo orders:
custo_encomendas <- function(matrix_func){
  valor <- 0
  for (i in 1:4) {
    valor <- valor + matrix_func[i,1] * 6
  }
  for (i in 1:4) {
    valor <- valor + matrix_func[i,2] * 8
  }
  for (i in 1:4) {
    valor <- valor + matrix_func[i,3] * 9
  }
  for (i in 1:4) {
    valor <- valor + matrix_func[i,4] * 11
  }
  assign(paste0("total_c_encomendas"), valor, envir = .GlobalEnv)
  return(valor)
}

# Função numero total de encomendas por departamento x semana:
contar_encomendas <- function(matrix_ord){
  num_encomendas <- sum(matrix_ord > 0)
  assign(paste0("NumeroT_encomendas"), num_encomendas, envir = .GlobalEnv)
  return(num_encomendas)
}

# Função numero vendas por departamento (ATENÇÃO: Empregados, encomendas e previsões):
numero_vendas_dep <- function(matriz_func, matriz_encomendas, matriz_pred){
  
  aux_1 <- max_suporte_departamento(matriz_func)[1] # Valor máximo de auxílio para dep 1
  aux_2 <- max_suporte_departamento(matriz_func)[2] # Valor máximo de auxílio para dep 2
  aux_3 <- max_suporte_departamento(matriz_func)[3] # Valor máximo de auxílio para dep 3
  aux_4 <- max_suporte_departamento(matriz_func)[4] # Valor máximo de auxílio para dep 4
  
  # Inicialize a matriz_sales com as mesmas dimensões que matriz_encomendas
  matriz_sales <- matrix(0, nrow = nrow(matriz_encomendas), ncol = ncol(matriz_encomendas))
  
  matriz_enc_com_restante <- matriz_encomendas
  
  for (i in 1:4) { # Linha
    for (j in 1:4) { # Coluna
      if (j == 1) { # Departamento 1
        if (matriz_enc_com_restante[i, j] <= aux_1) {
          if (matriz_enc_com_restante[i, j] >= matriz_pred[i, j]) {
            matriz_sales[i, j] <- matriz_pred[i, j]
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j] - matriz_pred[i, j])
            }
          } else {
            matriz_sales[i, j] <- matriz_enc_com_restante[i, j]
          }
        } else {
          if (matriz_enc_com_restante[i, j] <= matriz_pred[i, j]) {
            matriz_sales[i, j] <- aux_1
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j]-aux_1)
            }
          } else {
            if (matriz_pred[i, j] <= aux_1) {
              matriz_sales[i, j] <- matriz_pred[i, j]
            } else {
              matriz_sales[i, j] <- aux_1
            }
          }
        }
      } else if (j == 2) { # Departamento 2
        if (matriz_enc_com_restante[i, j] <= aux_2) {
          if (matriz_enc_com_restante[i, j] >= matriz_pred[i, j]) {
            matriz_sales[i, j] <- matriz_pred[i, j]
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j] - matriz_pred[i, j])
            }
          } else {
            matriz_sales[i, j] <- matriz_enc_com_restante[i, j]
          }
        } else {
          if (matriz_enc_com_restante[i, j] <= matriz_pred[i, j]) {
            matriz_sales[i, j] <- aux_2
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j]-aux_2)
            }
          } else {
            if (matriz_pred[i, j] <= aux_2) {
              matriz_sales[i, j] <- matriz_pred[i, j]
            } else {
              matriz_sales[i, j] <- aux_2
            }
          }
        }
      } else if (j == 3) { # Departamento 3
        if (matriz_enc_com_restante[i, j] <= aux_3) {
          if (matriz_enc_com_restante[i, j] >= matriz_pred[i, j]) {
            matriz_sales[i, j] <- matriz_pred[i, j]
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j] - matriz_pred[i, j])
            }
          } else {
            matriz_sales[i, j] <- matriz_enc_com_restante[i, j]
          }
        } else {
          if (matriz_enc_com_restante[i, j] <= matriz_pred[i, j]) {
            matriz_sales[i, j] <- aux_3
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j]-aux_3)
            }
          } else {
            if (matriz_pred[i, j] <= aux_3) {
              matriz_sales[i, j] <- matriz_pred[i, j]
            } else {
              matriz_sales[i, j] <- aux_3
            }
          }
        }
      } else if (j == 4) { # Departamento 4
        if (matriz_enc_com_restante[i, j] <= aux_4) {
          if (matriz_enc_com_restante[i, j] >= matriz_pred[i, j]) {
            matriz_sales[i, j] <- matriz_pred[i, j]
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j] - matriz_pred[i, j])
            }
          } else {
            matriz_sales[i, j] <- matriz_enc_com_restante[i, j]
    
          }
        } else {
          if (matriz_enc_com_restante[i, j] <= matriz_pred[i, j]) {
            matriz_sales[i, j] <- aux_4
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j]-aux_4)
            }
          } else {
            if (matriz_pred[i, j] <= aux_4) {
              matriz_sales[i, j] <- matriz_pred[i, j]
            } else {
              matriz_sales[i, j] <- aux_4
            }
          }
        }
      }
    }
  }
  assign(paste0("matriz_sales"), matriz_sales, envir = .GlobalEnv)
  #print(matriz_sales)
  return(matriz_sales)
}

# Função numero vendas por departamento em USD
numero_vendas_dep_USD <- function(matriz_sales){
  matriz_usd <- matriz_sales
  matriz_usd[, 1] <- matriz_usd[, 1] * 8
  matriz_usd[, 2] <- matriz_usd[, 2] * 10
  matriz_usd[, 3] <- matriz_usd[, 3] * 12
  matriz_usd[, 4] <- matriz_usd[, 4] * 16
  return(matriz_usd)
}

# Função Ganho total das vendas
total_revenue <- function(matriz_obtida_funcao_USD){
  assign(paste0("total_revenue_v"), sum(matriz_obtida_funcao_USD), envir = .GlobalEnv)
  return(sum(matriz_obtida_funcao_USD))
}

# Função numero de stock por semana de cada departamento:
stock <- function(matriz_func, matriz_encomendas, matriz_pred){
  aux_1 <- max_suporte_departamento(matriz_func)[1] # Valor máximo de auxílio para dep 1
  aux_2 <- max_suporte_departamento(matriz_func)[2] # Valor máximo de auxílio para dep 2
  aux_3 <- max_suporte_departamento(matriz_func)[3] # Valor máximo de auxílio para dep 3
  aux_4 <- max_suporte_departamento(matriz_func)[4] # Valor máximo de auxílio para dep 4
  
  # Inicialize a matriz_sales com as mesmas dimensões que matriz_encomendas
  matriz_stock <- matrix(0, nrow = nrow(matriz_encomendas), ncol = ncol(matriz_encomendas))
  
  matriz_enc_com_restante <- matriz_encomendas
  
  for (i in 1:4) { # Linha
    for (j in 1:4) { # Coluna
      if (j == 1) { # Departamento 1
        if (matriz_enc_com_restante[i, j] <= aux_1) {
          if (matriz_enc_com_restante[i, j] >= matriz_pred[i, j]) {
            matriz_stock[i, j] <- matriz_enc_com_restante[i, j]-matriz_pred[i, j]
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j]-matriz_pred[i, j])
            }
            
          } else {
            matriz_stock[i, j] <- 0
          }
        } else {
          if (matriz_enc_com_restante[i, j] <= matriz_pred[i, j]) {
            matriz_stock[i, j] <- matriz_enc_com_restante[i, j]-aux_1
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + matriz_enc_com_restante[i, j]-aux_1
            }
          } else {
            if (matriz_pred[i, j] <= aux_1) {
              matriz_stock[i, j] <- 0
            } else {
              matriz_stock[i, j] <- 0
            }
          }
        }
      } else if (j == 2) { # Departamento 2
        if (matriz_enc_com_restante[i, j] <= aux_2) {
          if (matriz_enc_com_restante[i, j] >= matriz_pred[i, j]) {
            matriz_stock[i, j] <- matriz_enc_com_restante[i, j]-matriz_pred[i, j]
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j]-matriz_pred[i, j])
            }
            
          } else {
            matriz_stock[i, j] <- 0
          }
        } else {
          if (matriz_enc_com_restante[i, j] <= matriz_pred[i, j]) {
            matriz_stock[i, j] <- matriz_enc_com_restante[i, j]-aux_2
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + matriz_enc_com_restante[i, j]-aux_2
            }
          } else {
            if (matriz_pred[i, j] <= aux_2) {
              matriz_stock[i, j] <- 0
            } else {
              matriz_stock[i, j] <- 0
            }
          }
        }
      } else if (j == 3) { # Departamento 3
        if (matriz_enc_com_restante[i, j] <= aux_3) {
          if (matriz_enc_com_restante[i, j] >= matriz_pred[i, j]) {
            matriz_stock[i, j] <- matriz_enc_com_restante[i, j]-matriz_pred[i, j]
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j]-matriz_pred[i, j])
            }
            
          } else {
            matriz_stock[i, j] <- 0
          }
        } else {
          if (matriz_enc_com_restante[i, j] <= matriz_pred[i, j]) {
            matriz_stock[i, j] <- matriz_enc_com_restante[i, j]-aux_3
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + matriz_enc_com_restante[i, j]-aux_3
            }
          } else {
            if (matriz_pred[i, j] <= aux_3) {
              matriz_stock[i, j] <- 0
            } else {
              matriz_stock[i, j] <- 0
            }
          }
        }
      } else if (j == 4) { # Departamento 4
        if (matriz_enc_com_restante[i, j] <= aux_4) {
          if (matriz_enc_com_restante[i, j] >= matriz_pred[i, j]) {
            matriz_stock[i, j] <- matriz_enc_com_restante[i, j]-matriz_pred[i, j]
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + abs(matriz_enc_com_restante[i, j]-matriz_pred[i, j])
            }
            
          } else {
            matriz_stock[i, j] <- 0
          }
        } else {
          if (matriz_enc_com_restante[i, j] <= matriz_pred[i, j]) {
            matriz_stock[i, j] <- matriz_enc_com_restante[i, j]-aux_4
            if (i + 1 <= nrow(matriz_enc_com_restante)) {
              matriz_enc_com_restante[i + 1, j] <- matriz_enc_com_restante[i + 1, j] + matriz_enc_com_restante[i, j]-aux_4
            }
            
          } else {
            if (matriz_pred[i, j] <= aux_4) {
              matriz_stock[i, j] <- 0
            } else {
              matriz_stock[i, j] <- 0
            }
          }
        }
      }
    }
  }
  assign(paste0("matriz_stock"), matriz_stock, envir = .GlobalEnv)
  #print(matriz_stock)
  return(matriz_stock)
}

# Função numero de stock por semana de cada departamento em USD:
stock_USD <- function(matriz_stock){
  matriz_stock_usd <- matriz_stock
  
  matriz_stock_usd[, 1] <- matriz_stock_usd[, 1] * 3
  matriz_stock_usd[, 2] <- matriz_stock_usd[, 2] * 5
  matriz_stock_usd[, 3] <- matriz_stock_usd[, 3] * 6
  matriz_stock_usd[, 4] <- matriz_stock_usd[, 4] * 8
  
  return(matriz_stock_usd)
}

# Função custo total de stock
total_cost_stock <- function(matriz_obtida_funcao_Stock_USD){
  assign(paste0("total_c_stock"), sum(matriz_obtida_funcao_Stock_USD), envir = .GlobalEnv)
  
  return(sum(matriz_obtida_funcao_Stock_USD))
}

# Função custo total (ganhos-despesas)
total_cost <- function(total_c_funcionarios, total_c_encomendas, total_c_stock){
  total <- total_c_funcionarios+total_c_encomendas+total_c_stock
  assign(paste0("total_cost_valor"), total, envir = .GlobalEnv)
  
  return(total)
}

# Função calcular profit mes
month_profit <- function(total_revenue, total_cost){
  #ADICIONAR O ROUND()
  mp <- total_revenue-total_cost
  return(mp)
}

# Função calcular esforço\forcionários no mes
month_effort <- function(NumeroT_encomendas, ValorT_funcionarios){
  me <- NumeroT_encomendas+ValorT_funcionarios
  return(me)
}

# Função calcular limite maximo de funcionários de cada tipo segundo as vendas previstas
upper_funcionarios <- function(matriz_prev_vendas){
  juniores <- ceiling(matriz_prev_vendas / 4000)
  normais <- ceiling(matriz_prev_vendas / 7000)
  seniores <- ceiling(matriz_prev_vendas / 9500)
  
  # Calculando a soma das colunas
  juniores_total <- colSums(juniores)
  normais_total <- colSums(normais)
  seniores_total <- colSums(seniores)
  
  #totais <- c(Juniores = juniores_total, Normais = normais_total, Seniores = seniores_total)
  totais <- c(juniores_total, normais_total, seniores_total)
  return(totais)
}

# Função calcular o valor maximo de encomendas
upper_encomendas <- function(matriz_encomendas){
  # Posso encomendas tudo de uma vez
  #soma <- colSums(matriz_encomendas)
  
  max_vals <- numeric(length(matriz_encomendas))
  for (col in 1:ncol(matriz_encomendas)) {
    for (row in 1:nrow(matriz_encomendas)) {
      max_vals[row + (col - 1) * nrow(matriz_encomendas)] <- sum(matriz_encomendas[row:nrow(matriz_encomendas), col])
    }
  }
  return(max_vals)
  #return(soma)
}

# Função para guardar os resultados da otimização em csv
salvar_resultados <- function(flag,nome_modelo, solucao, valor_solucao) {
  resultados <- data.frame(Solucao = solucao, Valor_Solucao = rep(valor_solucao, length(solucao)))
  #write.csv(resultados, file = paste0("ResultadosOtimizacao/",flag,"_",nome_modelo, "_resultados.csv"), row.names = FALSE)
  if(nome_modelo=="Monte_Carlo"){
    write.csv(resultados, file = paste0("ResultadosOtimizacao/MC/",flag,"_",nome_modelo, "_resultados.csv"), row.names = FALSE)
  }else if(nome_modelo =="Hill_Climbing_Aditivo"){
    write.csv(resultados, file = paste0("ResultadosOtimizacao/HC_aditivo/",flag,"_",nome_modelo, "_resultados.csv"), row.names = FALSE)
  }else if(nome_modelo =="Hill_Climbing_Multiplicativo"){
    write.csv(resultados, file = paste0("ResultadosOtimizacao/HC_multiplicativo/",flag,"_",nome_modelo, "_resultados.csv"), row.names = FALSE)
  } else if(nome_modelo =="SANN_Aditivo"){
    write.csv(resultados, file = paste0("ResultadosOtimizacao/SANN_aditivo/",flag,"_",nome_modelo, "_resultados.csv"), row.names = FALSE)
  } else if(nome_modelo =="SANN_Multiplicativo"){
    write.csv(resultados, file = paste0("ResultadosOtimizacao/SANN_multiplicativo/",flag,"_",nome_modelo, "_resultados.csv"), row.names = FALSE)
  } else if(nome_modelo =="Genetic_Algorithm"){
    write.csv(resultados, file = paste0("ResultadosOtimizacao/GE/",flag,"_",nome_modelo, "_resultados.csv"), row.names = FALSE)
  } else if(nome_modelo =="Differential_Evolution"){
    write.csv(resultados, file = paste0("ResultadosOtimizacao/DE/",flag,"_",nome_modelo, "_resultados.csv"), row.names = FALSE)
  } else if(nome_modelo =="NSGA"){
    write.csv(resultados, file = paste0("ResultadosOtimizacao/NSGA/",flag,"_",nome_modelo, "_resultados.csv"), row.names = FALSE)
  }  
}

csv_plano_completo <- function(modelo,mes,solucao,vendas){
  
  vendas_matrix <- vendas
  func <- solucao[1:12]
  encomendas <- solucao[13:28]
  matriz_func <- matrix(func, nrow = 3, byrow = TRUE)
  matriz_encomendas_pre <- matrix(encomendas, nrow = 4, byrow = TRUE)
  matriz_encomendas <- t(matriz_encomendas_pre)
  
  Total_custo_trabalhadores <- custo_funcionarios(matriz_func)
  Effort_total_trabalhadores <- contar_funcionarios(matriz_func)
  Trabalhadores_max_suport_dep <- max_suporte_departamento(matriz_func)
  
  Total_custo_encomendas <- custo_encomendas(matriz_encomendas)
  Effort_total_encomendas <- contar_encomendas(matriz_encomendas)
  Sales_realizadas <- numero_vendas_dep(matriz_func,matriz_encomendas,vendas_matrix)
  Sales_realizadas_USD <- numero_vendas_dep_USD(matriz_sales)
  Total_rev <- total_revenue(Sales_realizadas_USD)
  Stock_val <- stock(matriz_func,matriz_encomendas,vendas_matrix)
  Stock_val_USD <- stock_USD(matriz_stock)
  Total_custo_stock <- total_cost_stock(Stock_val_USD)
  Total_custo <- total_cost(total_c_funcionarios,total_c_encomendas,total_c_stock)
  Effort_mes <- month_effort(NumeroT_encomendas,ValorT_funcionarios)
  
  resultados <- data.frame(
    Total_custo_trabalhadores = Total_custo_trabalhadores,
    Effort_total_trabalhadores = Effort_total_trabalhadores,
    Trabalhadores_max_suport_dep = Trabalhadores_max_suport_dep,
    Total_custo_encomendas = Total_custo_encomendas,
    Effort_total_encomendas = Effort_total_encomendas,
    Sales_realizadas = Sales_realizadas,
    Sales_realizadas_USD = Sales_realizadas_USD,
    Total_rev = Total_rev,
    Stock_val = Stock_val,
    Stock_val_USD = Stock_val_USD,
    Total_custo_stock = Total_custo_stock,
    Total_custo = Total_custo,
    Effort_mes = Effort_mes
  )
  
  dir.create(file.path("Matrizes_efetivas", modelo), showWarnings = FALSE, recursive = TRUE)
  nome_arquivo <- file.path("Matrizes_efetivas", modelo, paste0(modelo, "_", mes, ".csv"))
  
  write.csv(resultados, nome_arquivo, row.names = FALSE)
}


###############################################################################
# FUNÇÃO EVAL PARA PROFIT:
###############################################################################
# 
# s1 <- c(
#   5, 4, 3, 2,
#   6, 5, 4, 3,
#   7, 6, 5, 4,
#   61662, 78292,56434,24182,
#   0,0,0,0,
#   12985,55403,69133,37167,
#   39924,75160,62131,99708
# )
# 
# 
# s2 <- c(
#   5, 4, 3, 2,
#   6, 5, 4, 3,
#   7, 6, 5, 4,
#   61662, 0,12985,39924,
#   78292,0,55403,75160,
#   56434,0,69133,62131,
#   24182,0,37167,99708
# )

# Definição da solução como vetor
s <- rep(0, 28)  # Vetor de 28 elementos, inicialmente preenchido com zeros

# FUNÇÃO EVAL NORMAL
eval <- function(s) {
   s <- round(s)
  
  func <- s[1:12]
  encomendas <- s[13:28]
  matriz_func <- matrix(func, nrow = 3, byrow = TRUE)
  matriz_encomendas_pre <- matrix(encomendas, nrow = 4, byrow = TRUE)
  matriz_encomendas <- t(matriz_encomendas_pre)
  
  # Ganhos:
  nvendas <- numero_vendas_dep(matriz_func,matriz_encomendas,vendas_matrix)
  nvendas_UDS <- numero_vendas_dep_USD(nvendas)
  total_ganho <- total_revenue(nvendas_UDS)
  
  # Gastos:
  total_de_funcionarios <- custo_funcionarios(matriz_func)
  total_de_encomendas <- custo_encomendas(matriz_encomendas)
  matriz_stock1 <- stock(matriz_func,matriz_encomendas,vendas_matrix)
  stock_em_USD <- stock_USD(matriz_stock1)
  total_custo_de_stock <- total_cost_stock(stock_em_USD)
  
  # Total de lucro:
  total_gasto <- total_cost(total_de_funcionarios,total_de_encomendas,total_custo_de_stock)
  profit_mes <- total_ganho - total_gasto
  return(profit_mes)
}

#======================

# FUNÇÃO EVAL NEGATIVO (PARA MINIMIZANTES)
eval2 <- function(s) {
  s <- round(s)
  
  func <- s[1:12]
  encomendas <- s[13:28]
  matriz_func <- matrix(func, nrow = 3, byrow = TRUE)
  matriz_encomendas_pre <- matrix(encomendas, nrow = 4, byrow = TRUE)
  matriz_encomendas <- t(matriz_encomendas_pre)
  
  # Ganhos:
  nvendas <- numero_vendas_dep(matriz_func,matriz_encomendas,vendas_matrix)
  nvendas_UDS <- numero_vendas_dep_USD(nvendas)
  total_ganho <- total_revenue(nvendas_UDS)
  
  # Gastos:
  total_de_funcionarios <- custo_funcionarios(matriz_func)
  total_de_encomendas <- custo_encomendas(matriz_encomendas)
  matriz_stock1 <- stock(matriz_func,matriz_encomendas,vendas_matrix)
  stock_em_USD <- stock_USD(matriz_stock1)
  total_custo_de_stock <- total_cost_stock(stock_em_USD)
  
  # Total de lucro:
  total_gasto <- total_cost(total_de_funcionarios,total_de_encomendas,total_custo_de_stock)
  profit_mes = -(total_ganho - total_gasto)
  return(profit_mes)
}

#======================

# FUNÇÃO EVAL PARA DUPLO OBJETIVO:
eval3 <- function(s) {
  s <- round(s)
  
  func <- s[1:12]
  encomendas <- s[13:28]
  matriz_func <- matrix(func, nrow = 3, byrow = TRUE)
  matriz_encomendas_pre <- matrix(encomendas, nrow = 4, byrow = TRUE)
  matriz_encomendas <- t(matriz_encomendas_pre)
  
  # Ganhos:
  nvendas <- numero_vendas_dep(matriz_func,matriz_encomendas,vendas_matrix)
  nvendas_UDS <- numero_vendas_dep_USD(nvendas)
  total_ganho <- total_revenue(nvendas_UDS)
  
  # Gastos:
  total_de_funcionarios <- custo_funcionarios(matriz_func)
  total_de_encomendas <- custo_encomendas(matriz_encomendas)
  matriz_stock1 <- stock(matriz_func,matriz_encomendas,vendas_matrix)
  stock_em_USD <- stock_USD(matriz_stock1)
  total_custo_de_stock <- total_cost_stock(stock_em_USD)
  
  # Total de lucro:
  total_gasto <- total_cost(total_de_funcionarios,total_de_encomendas,total_custo_de_stock)
  profit_mes = -(total_ganho - total_gasto)
  
  #Esforço:
  num_enc = contar_encomendas(encomendas)
  num_func = contar_funcionarios(func)
  esforco = month_effort(num_enc,num_func)
  
  return(c(profit_mes,esforco))
}

#======================
# Teste da função eval
#print(eval(s1))

###############################################################################
# Teste individual de funções
###############################################################################

# soma_vendas_sema(vendas) #-> vai dar soma das vendas por semana de cada dep
# custo_funcionarios(s1_matrix)
# contar_funcionarios(s1_matrix)
# max_suporte_departamento(s1_matrix)
# custo_encomendas(order_matrix)
# contar_encomendas(order_matrix)
# numero_vendas_dep(s1_matrix,order_matrix,vendas_matrix)
# numero_vendas_dep_USD(matriz_sales)
# total_revenue(numero_vendas_dep_USD(matriz_sales))
# stock(s1_matrix,order_matrix,vendas_matrix)
# stock_USD(matriz_stock)
# total_cost_stock(stock_USD(matriz_stock))
# total_cost(total_c_funcionarios,total_c_encomendas,total_c_stock)
# month_profit(total_revenue,total_cost_valor)
# month_effort(NumeroT_encomendas,ValorT_funcionarios)

###############################################################################
# Metodos de otimização
###############################################################################

# Função correr todos os modelos:
modelos_otimizacao <- function(flag){
  # Funções Auxiliares:
  mcsearch=function(fn,lower,upper,N,type="max",...)
  { D=length(lower)
  s=matrix(nrow=N,ncol=D) # set the search space 
  for(i in 1:N) s[i,]=runif(D,lower,upper)
  fsearch(s,fn,type,...) # best solution
  }
  
  fsearch=function(search,fn,type="max",...)
  {
    x=apply(search,1,fn,...) # run fn over all search rows
    ib=switch(type,min=which.min(x),max=which.max(x))
    return(list(index=ib,sol=search[ib,],eval=x[ib]))
  }
  
  m_rastrigin=function(x) # x is a solution
  { 
    f = eval(x)
    EV<<- EV+1 # increase evaluations
    if(f>BEST2) BEST2<<- f # update current BEST
    if(EV<=MAXIT) F[EV]<<- BEST2 # update BEST for EV
    return(f)
  }
  
  m_rastrigin2=function(x) # x is a solution
  {
    f = eval2(x)
    EV<<- EV+1 # increase evaluations
    if(f<BEST2) BEST2<<- f # update current BEST
    if(EV<=MAXIT) F[EV]<<- BEST2 # update BEST for EV
    return(f)
  }
  
  N=10000
  REPORT=N/20
  MAXIT=N
  EV=0
  BEST2= -99999999999 
  F=rep(NA,MAXIT)
  
  #Monte Carlo
  print("=================================== Monte Carlo ===========================")
  MC=mcsearch(fn=m_rastrigin,lower=lower,upper=upper,N=N,type="max")
  cat("best solution:",round(MC$sol),"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")
  print(eval(MC$sol))
  plot(F,col="blue",type="l",lwd=2,main=paste("convergence of rastrigin Monte Carlo(D=",D,")"),xlab="evaluations",ylab="value")
  salvar_resultados(flag,"Monte_Carlo", round(MC$sol), eval(MC$sol))
  csv_plano_completo("MC",flag,round(MC$sol),vendas_matrix)
  print("=================================== Proximo Modelo ===========================")
  
  #Hill Climbling - Aditivo:
  N=10000
  REPORT=N/20
  MAXIT=N
  EV=0
  BEST2= -99999999999
  F=rep(NA,MAXIT)
  print("\n=================================== Hill Climbing - Aditivo ===========================")
  rchange1=function(par,lower,upper)
  { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.25,round=TRUE) }
  HC=hclimbing(par=par,fn=m_rastrigin,change=rchange1,lower=lower,upper=upper,type="max",
               control=list(maxit=MAXIT,REPORT=REPORT,digits=2))
  cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
  print(eval(HC$sol))
  plot(F,col="blue",type="l",lwd=2,main=paste("convergence of rastrigin Hill Climbing(D=",D,")"),xlab="evaluations",ylab="value")
  salvar_resultados(flag,"Hill_Climbing_Aditivo", round(HC$sol), eval(HC$sol))
  csv_plano_completo("HC_aditivo",flag,round(HC$sol),vendas_matrix)
  print("=================================== Proximo Modelo ===========================")
  
  #Hill Climbling - Multiplicativo:
  N=10000
  REPORT=N/20
  MAXIT=N
  EV=0
  BEST2= -99999999999
  F=rep(NA,MAXIT)
  print("\n=================================== Hill Climbing - Multiplicativo ===========================")
  rchange4=function(par,lower,upper)
  { hchange2(par,lower=lower,upper=upper,rnorm,mean=1,sd=0.15,round=TRUE) }
  HC=hclimbing(par=par,fn=m_rastrigin,change=rchange4,lower=lower,upper=upper,type="max",
               control=list(maxit=MAXIT,REPORT=REPORT,digits=2))
  cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
  print(eval(HC$sol))
  plot(F,col="blue",type="l",lwd=2,main=paste("convergence of rastrigin Hill Climbing(D=",D,")"),xlab="evaluations",ylab="value")
  salvar_resultados(flag,"Hill_Climbing_Multiplicativo", round(HC$sol), eval(HC$sol))
  csv_plano_completo("HC_multiplicativo",flag,round(HC$sol),vendas_matrix)
  print("=================================== Proximo Modelo ===========================")
  
  # SANN - Aditivo:
  N=10000
  REPORT=N/20 
  MAXIT=N
  EV=0
  BEST2= 99999999999
  F=rep(NA,MAXIT)
  print("\n=================================== SANN - ADITIVO ===========================")
  rchange2=function(par)
  { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.45,round=TRUE) }
  CSANN=list(maxit=MAXIT,temp=10,trace=TRUE)
  SA=optim(par=par,fn=m_rastrigin2,method="SANN",gr=rchange2,control=CSANN)
  SA$par <- round(SA$par)
  cat("best solution:",SA$par,"evaluation function",SA$value,"\n")
  print(eval(SA$par))
  plot(F,col="blue",type="l",lwd=2,main=paste("convergence of rastrigin SANN add(D=",D,")"),xlab="evaluations",ylab="value")
  salvar_resultados(flag,"SANN_Aditivo", round(SA$par), eval(SA$par))
  csv_plano_completo("SANN_aditivo",flag,round(SA$par),vendas_matrix)
  print("=================================== Proximo Modelo ===========================")
  
  # SANN - Multiplicativo:
  N=10000
  REPORT=N/20
  MAXIT=N
  EV=0
  BEST2= 99999999999
  F=rep(NA,MAXIT)
  print("\n=================================== SANN - MULTIPLICATIVO ===========================")
  rchange3=function(par)
  { hchange2(par,lower=lower,upper=upper,rnorm,mean=1,sd=0.15,round=TRUE) }
  CSANN=list(maxit=MAXIT,temp=10,trace=TRUE)
  SA=optim(par=par,fn=m_rastrigin2,method="SANN",gr=rchange3,control=CSANN)
  SA$par <- round(SA$par)
  cat("best solution:",SA$par,"evaluation function",SA$value,"\n")
  print(eval(SA$par))
  plot(F,col="blue",type="l",lwd=2,main=paste("convergence of rastrigin SANN mult (D=",D,")"),xlab="evaluations",ylab="value")
  salvar_resultados(flag,"SANN_Multiplicativo", round(SA$par), eval(SA$par))
  csv_plano_completo("SANN_Multiplicativo",flag,round(SA$par),vendas_matrix)
  print("=================================== Proximo Modelo ===========================")
  
  # Genetic Algorithm
  N=10000
  REPORT=N/20
  MAXIT=N
  EV=0
  BEST2= 99999999999
  F=rep(NA,MAXIT)
  print("\n=================================== Genetic Alg ===========================")
  rbga = rbga(lower,upper,popSize = 100, iters = 100, mutationChance = 0.33, elitism = 1, evalFunc = m_rastrigin2)
  PMAX = which.min(rbga$evaluations)
  rbga$population[PMAX, ] = round(rbga$population[PMAX, ])
  cat("Rbga: best solution:",rbga$population[PMAX,],"evaluation function",eval(rbga$population[PMAX,]),"\n")
  print(eval(rbga$population[PMAX, ]))
  plot(F,col="blue",type="l",lwd=2,main=paste("convergence of rastrigin Genetic (D=",D,")"),xlab="evaluations",ylab="value")
  salvar_resultados(flag,"Genetic_Algorithm", rbga$population[PMAX, ], eval(rbga$population[PMAX, ]))
  csv_plano_completo("GE",flag,rbga$population[PMAX, ],vendas_matrix)
  print("=================================== Proximo Modelo ===========================")
  
  # Differential Evolution
  N=10000
  REPORT=N/20
  MAXIT=N
  EV=0
  BEST2= 99999999999
  F=rep(NA,MAXIT)
  print("\n=================================== DE Model ===========================")
  result <- DEoptim(m_rastrigin2, lower, upper)  
  print(round(result$optim$bestmem))
  print(eval(round(result$optim$bestmem))) # O DE minimiza a função logo temos de fazer -Lucro
  plot(F,col="blue",type="l",lwd=2,main=paste("convergence of rastrigin DE (D=",D,")"),xlab="evaluations",ylab="value")
  salvar_resultados(flag,"Differential_Evolution", round(result$optim$bestmem), eval(round(result$optim$bestmem)))
  csv_plano_completo("DE",flag,round(result$optim$bestmem),vendas_matrix)
  print("=================================== Proximo Modelo ===========================")
  
  # Duplo Objetivo - Maximizar lucro, minimizar esforço:
  print("\n=================================== NSGA-II ===========================")
  m=2
  cat("real value task:\n")
  G=nsga2(fn=eval3,idim=D,odim=m,
          lower.bounds=lower,upper.bounds=upper,
          popsize=20,generations=1:100)
  I=which(G[[100]]$pareto.optimal)
  for(i in I)
  {
    x=round(G[[100]]$par[i,],digits=2); cat(x)
    cat(" f=(",round(eval3(x)[1],2),",",round(eval3(x)[2],2),")",
        "\n",sep="")
  }
  
  print(round(G[[100]]$par[which.min(G[[100]]$value[,1]),]))
  #print(eval3(round(G[[100]]$par[which.min(G[[100]]$value[,1]),])))
  solucao <- eval3(round(G[[100]]$par[which.min(G[[100]]$value[,1]),]))
  cat(-solucao[1]," ",solucao[2])
  salvar_resultados(flag,"NSGA", round(G[[100]]$par[which.min(G[[100]]$value[,1]),]), c(-solucao[1],solucao[2]))
  csv_plano_completo("NSGA",flag,round(G[[100]]$par[which.min(G[[100]]$value[,1]),]),vendas_matrix)
  print("=========================================================================")
  
  pdf(file="nsga.pdf",paper="special",height=5,width=5)
  par(mar=c(4.0,4.0,0.1,0.1))
  I=1:100
  for(i in I)
  { P=G[[i]]$value
  COL=paste("gray",round(76-i*0.75),sep="")
  if(i==1){
    plot(P, xlim=c(0, 1.1 * max(P[,1])), ylim=c(0, 1.1 * max(P[,2])),
         xlab="f1", ylab="f2", cex=0.5, col=COL, main="Pareto Front Evolution")
  }
  Pareto=P[G[[i]]$pareto.optimal,]
  points(P,type="p",pch=1,cex=0.5,col=COL)
  if(is.matrix(Pareto))
  { I=sort.int(Pareto[,1],index.return=TRUE)
  Pareto=Pareto[I$ix,]
  lines(Pareto,type="l",cex=0.5,col=COL)
  }
  }
  dev.off()
}

###############################################################################
# Inicialização de variáveis
###############################################################################
# Tem 2 opções ou utiliza os vários metodos de otimização com as vendas previstas(forecast) ou com o mes do growing window

# Opção1: Carregamento da matriz vendas:
# Vendas do forecast Normal:
# realPrevCarre()
# select_real_prev()

# Opção2: Vendas do Growing Window
# growingWindowCarre()
# select_mes_Ar_growing(8) #Passar o mes que quero do growing window (1-8)

D=28
lower <- rep(0,D)
upper <- rep(c(upper_funcionarios(vendas_matrix),upper_encomendas(vendas_matrix)))

N=10000
REPORT=N/20 # report results
par <- floor(runif(D,lower, upper))

realPrevCarre()
growingWindowCarre()

select_real_prev()
modelos_otimizacao("Normal")
select_mes_Ar_growing(1)
modelos_otimizacao("Mes1")
select_mes_Ar_growing(2)
modelos_otimizacao("Mes2")
select_mes_Ar_growing(3)
modelos_otimizacao("Mes3")
select_mes_Ar_growing(4)
modelos_otimizacao("Mes4")
select_mes_Ar_growing(5)
modelos_otimizacao("Mes5")
select_mes_Ar_growing(6)
modelos_otimizacao("Mes6")
select_mes_Ar_growing(7)
modelos_otimizacao("Mes7")
select_mes_Ar_growing(8)
modelos_otimizacao("Mes8")



###############################################################################
