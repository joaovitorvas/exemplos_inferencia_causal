# =============================================================================
# Replicação: Qi & Li (2024) — Etapa 2
# Diagrama Causal (FCI/IC*) + Cálculo RCT via Backdoor Criterion
# =============================================================================

library(pcalg)
library(dagitty)
library(igraph)

set.seed(42)


# =============================================================================
# 1. CARREGAR DADOS
# =============================================================================

df <- readRDS("nhanes_completo_03_06.rds")
message("Linhas carregadas: ", nrow(df))

if (nrow(df) == 0) stop("Dataset vazio — rode nhanes_coleta_limpeza.R primeiro.")


# =============================================================================
# 2. PREPARAR MATRIX PARA O FCI
# =============================================================================

vars_modelo <- c("exposicao_coca", "obesidade", "idade", "genero", "raca",
                 "escolaridade", "fumo", "atividade_fisica",
                 "hiperlipidemia", "diabetes")

# Manter apenas vars que existem no dataframe
vars_modelo <- vars_modelo[vars_modelo %in% colnames(df)]
message("Variaveis no modelo: ", paste(vars_modelo, collapse = ", "))

# Garantir que tudo é numérico (alguns campos podem ainda ser character)
df_model <- df[, vars_modelo]
df_model[] <- lapply(df_model, function(x) suppressWarnings(as.numeric(as.character(x))))

# Diagnóstico de NAs
message("\n--- NAs por variavel ---")
for (v in colnames(df_model)) {
  n_na <- sum(is.na(df_model[[v]]))
  pct  <- round(100 * n_na / nrow(df_model), 1)
  message(sprintf("  %-20s  NA: %d (%.1f%%)", v, n_na, pct))
}

# Remover colunas com > 60% de NAs ou variância zero
colunas_validas <- sapply(df_model, function(x) {
  n_ok <- sum(!is.na(x))
  vari <- var(x, na.rm = TRUE)
  n_ok / nrow(df_model) >= 0.4 && !is.na(vari) && vari > 0
})
if (any(!colunas_validas)) {
  message("Removendo colunas invalidas: ",
          paste(names(colunas_validas)[!colunas_validas], collapse = ", "))
  df_model <- df_model[, colunas_validas, drop = FALSE]
}

# Normalizar variáveis contínuas
for (col in c("idade", "atividade_fisica")) {
  if (col %in% colnames(df_model)) {
    x <- df_model[[col]]
    if (length(unique(x[!is.na(x)])) > 10) {
      df_model[[col]] <- as.numeric(scale(x))
      message("Normalizada: ", col)
    }
  }
}

# Remover linhas com qualquer NA restante
df_model <- df_model[complete.cases(df_model), ]
message("\nLinhas completas para FCI: ", nrow(df_model),
        " x ", ncol(df_model), " variaveis")

if (nrow(df_model) < 100)
  stop("Menos de 100 linhas completas — revise as variaveis do modelo.")


# =============================================================================
# 3. ALGORITMO FCI (IC*)
# =============================================================================

n_vars <- ncol(df_model)
n_obs  <- nrow(df_model)
alpha  <- 0.01

mat_cor <- cor(df_model, use = "complete.obs")

# Garantir matriz positiva definida
autovalores <- eigen(mat_cor, only.values = TRUE)$values
message("Menor autovalor: ", round(min(autovalores), 8))
if (min(autovalores) <= 1e-8) {
  message("Aplicando correcao ridge...")
  diag(mat_cor) <- diag(mat_cor) + 1e-6
}

suffStat <- list(C = mat_cor, n = n_obs)

message("\n=== Rodando FCI (IC*) — ", n_vars, " vars, ", n_obs, " obs ===")
fci_fit <- fci(
  suffStat  = suffStat,
  indepTest = gaussCItest,
  alpha     = alpha,
  p         = n_vars,
  labels    = colnames(df_model),
  verbose   = FALSE
)
message("FCI concluido.")


# =============================================================================
# 4. VISUALIZAR O PAG
# =============================================================================

amat <- fci_fit@amat
rownames(amat) <- colnames(df_model)
colnames(amat) <- colnames(df_model)

message("\nMatriz de adjacencia (PAG):")
print(amat)

# Extrair arestas direcionadas: amat[i,j]=2 e amat[j,i]=3 -> i --> j
n      <- nrow(amat)
adj_dir <- matrix(0, n, n, dimnames = list(colnames(amat), colnames(amat)))
for (i in 1:n) for (j in 1:n)
  if (amat[i,j] == 2 && amat[j,i] == 3) adj_dir[i,j] <- 1

g <- graph_from_adjacency_matrix(adj_dir, mode = "directed", diag = FALSE)

cor_no <- function(name) {
  if (name %in% c("exposicao_coca","obesidade")) return("#D85A30")
  if (name == "diabetes")                         return("#7F77DD")
  return("#888780")
}
V(g)$color <- sapply(V(g)$name, cor_no)

png("dag_fci.png", width = 900, height = 700, res = 120)
plot(g,
     vertex.color       = V(g)$color,
     vertex.label.color = "white",
     vertex.label.cex   = 0.75,
     vertex.size        = 30,
     edge.arrow.size    = 0.5,
     edge.color         = "#444441",
     layout             = layout_with_fr(g),
     main               = "PAG — FCI/IC* (Qi & Li 2024)")
dev.off()
message("Grafico salvo: dag_fci.png")


# =============================================================================
# 5. DAG DO ARTIGO + BACKDOOR CRITERION
# =============================================================================

dag_artigo <- dagitty('dag {
  exposicao_coca [exposure]
  obesidade      [outcome]

  exposicao_coca -> obesidade
  exposicao_coca -> diabetes
  diabetes       -> obesidade

  idade            -> obesidade
  idade            -> exposicao_coca
  genero           -> obesidade
  genero           -> exposicao_coca
  raca             -> obesidade
  escolaridade     -> obesidade
  escolaridade     -> exposicao_coca
  fumo             -> obesidade
  atividade_fisica -> obesidade
  atividade_fisica -> exposicao_coca
  hiperlipidemia   -> obesidade
  hiperlipidemia   -> exposicao_coca
}')

backdoor_sets <- adjustmentSets(dag_artigo, "exposicao_coca", "obesidade")
message("\n=== Conjuntos de ajuste (backdoor criterion) ===")
print(backdoor_sets)


# =============================================================================
# 6. CÁLCULO RCT VIA BACKDOOR CRITERION
# =============================================================================
# Equação 1: P(Y=1 | do(X=x)) = Σ_z P(Y=1 | X=x, Z=z) * P(Z=z)
# Z = diabetes

message("\n=== Calculo RCT ===")

df_rct <- df[complete.cases(df[, c("exposicao_coca","obesidade","diabetes")]),
             c("exposicao_coca","obesidade","diabetes")]

# Garantir numérico
df_rct[] <- lapply(df_rct, function(x) suppressWarnings(as.numeric(as.character(x))))
df_rct   <- df_rct[complete.cases(df_rct), ]

message("Observacoes para calculo RCT: ", nrow(df_rct))

tab_z <- table(df_rct$diabetes)
p_z   <- prop.table(tab_z)
message("Distribuicao diabetes (Z):")
print(tab_z)

calcular_p_y_do_x <- function(x_val, df, p_z) {
  soma <- 0
  for (z_str in names(p_z)) {
    z_num <- as.numeric(z_str)
    sub   <- df[df$exposicao_coca == x_val & df$diabetes == z_num, ]
    if (nrow(sub) == 0) {
      message("  Aviso: sem obs para X=", x_val, ", Z=", z_num); next
    }
    p_y_xz <- mean(sub$obesidade, na.rm = TRUE)
    soma   <- soma + p_y_xz * p_z[z_str]
    message(sprintf("  P(Y=1|X=%d,Z=%d)=%.4f  P(Z=%d)=%.4f  contribuicao=%.4f",
                    x_val, z_num, p_y_xz, z_num, p_z[z_str],
                    p_y_xz * p_z[z_str]))
  }
  soma
}

message("\nCalculando P(Y=1 | do(X=1)):")
p_y1_do_x1 <- calcular_p_y_do_x(1, df_rct, p_z)

message("\nCalculando P(Y=1 | do(X=0)):")
p_y1_do_x0 <- calcular_p_y_do_x(0, df_rct, p_z)

message("\n=== RESULTADOS ===")
message("P(Y=1 | do(X=1)) = ", round(p_y1_do_x1, 10))
message("P(Y=1 | do(X=0)) = ", round(p_y1_do_x0, 10))
message("Diferenca causal  = ", round(p_y1_do_x1 - p_y1_do_x0, 6))

message("\nComparacao com o artigo:")
message("  Esperado do(X=1): 0.4157  |  Obtido: ", round(p_y1_do_x1, 4))
message("  Esperado do(X=0): 0.3199  |  Obtido: ", round(p_y1_do_x0, 4))
message("  Diferenca esperada: 0.0958  |  Obtida: ",
        round(p_y1_do_x1 - p_y1_do_x0, 4))


# =============================================================================
# 7. SALVAR
# =============================================================================

saveRDS(
  list(
    p_y1_do_x1    = p_y1_do_x1,
    p_y1_do_x0    = p_y1_do_x0,
    p_z           = p_z,
    dag_artigo    = dag_artigo,
    backdoor_sets = backdoor_sets,
    df_rct        = df_rct
  ),
  "resultados_rct.rds"
)

message("\nSalvo: resultados_rct.rds")
message("Proxima etapa: nhanes_pns.R")