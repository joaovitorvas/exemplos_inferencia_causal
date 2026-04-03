# =============================================================================
# Replicação: Qi & Li (2024) — Etapa 1 v4
# CORRECAO DEFINITIVA: nhanesA retorna factors com labels em texto para
# todos os campos categóricos. Todos os campos são convertidos com
# as.character() antes de qualquer comparação.
# =============================================================================

library(nhanesA)
library(dplyr)
set.seed(42)

cache_nhanes <- function(nome) {
  dir.create("cache", showWarnings = FALSE)
  path <- file.path("cache", paste0(nome, ".rds"))
  if (file.exists(path)) { message("[cache] ", nome); return(readRDS(path)) }
  message("[download] ", nome)
  d <- nhanes(nome); saveRDS(d, path); d
}

merge_ciclo <- function(...) {
  Reduce(function(a, b) merge(a, b, by = "SEQN", all.x = TRUE), list(...))
}

preparar_ciclo <- function(df_raw, nome_ciclo) {
  message("\n--- Ciclo ", nome_ciclo, " | linhas brutas: ", nrow(df_raw), " ---")
  
  colunas_raw  <- c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2",
                    "BMXBMI","SMQ020","PAQ180","BPQ080","DIQ010","FFQ0006A")
  colunas_fins <- c("id","genero","idade","raca","escolaridade",
                    "bmi","fumo","atividade_fisica","hiperlipidemia",
                    "diabetes","frequencia_coca")
  
  faltando <- setdiff(colunas_raw, colnames(df_raw))
  if (length(faltando) > 0)
    stop("Colunas ausentes no ciclo ", nome_ciclo, ": ",
         paste(faltando, collapse = ", "))
  
  df <- df_raw[, colunas_raw]
  colnames(df) <- colunas_fins
  
  # Converter TODOS os factors para character imediatamente
  df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
  
  # Adultos apenas
  df <- df[!is.na(df$idade) & as.numeric(df$idade) >= 20, ]
  df$idade <- as.numeric(df$idade)
  message("Apos idade >= 20: ", nrow(df))
  
  # BMI numérico
  df$bmi <- as.numeric(df$bmi)
  
  # Y: obesidade
  df$obesidade <- ifelse(!is.na(df$bmi) & df$bmi >= 30, 1, 0)
  
  # X: Diet Coke
  # FFQ0006A labels: "Almost never or never", "About 1/4 of the time",
  #                  "About 1/2 of the time", "About 3/4 of the time",
  #                  "Almost always or always", "Blank", "Error"
  df <- df[!df$frequencia_coca %in% c("Blank","Error") | is.na(df$frequencia_coca), ]
  df$exposicao_coca <- case_when(
    df$frequencia_coca == "Almost never or never" ~ 0,
    df$frequencia_coca %in% c("About 1/4 of the time",
                              "About 1/2 of the time",
                              "About 3/4 of the time",
                              "Almost always or always") ~ 1,
    TRUE ~ NA_real_
  )
  message("Apos excluir Blank/Error: ", nrow(df))
  message("  exposicao_coca  0: ", sum(df$exposicao_coca == 0, na.rm = TRUE),
          " | 1: ", sum(df$exposicao_coca == 1, na.rm = TRUE),
          " | NA: ", sum(is.na(df$exposicao_coca)))
  
  # Diabetes — labels: "Yes", "No", "Borderline", "Don't know"
  df$diabetes <- case_when(
    df$diabetes == "Yes" ~ 1,
    df$diabetes == "No"  ~ 0,
    TRUE                 ~ NA_real_   # Borderline e Don't know viram NA
  )
  message("  diabetes        0: ", sum(df$diabetes == 0, na.rm = TRUE),
          " | 1: ", sum(df$diabetes == 1, na.rm = TRUE),
          " | NA: ", sum(is.na(df$diabetes)))
  
  # Hiperlipidemia — labels: "Yes", "No"
  df$hiperlipidemia <- case_when(
    df$hiperlipidemia == "Yes" ~ 1,
    df$hiperlipidemia == "No"  ~ 0,
    TRUE                       ~ NA_real_
  )
  
  # Fumo — SMQ020 labels: "Yes", "No"
  df$fumo <- case_when(
    df$fumo == "Yes" ~ 1,
    df$fumo == "No"  ~ 0,
    TRUE             ~ NA_real_
  )
  
  # Atividade física — PAQ180 labels numéricos ou texto dependendo do ciclo
  # Converter para numérico diretamente (1=Muito ativo … 4=Inativo)
  df$atividade_fisica <- suppressWarnings(as.numeric(df$atividade_fisica))
  df$atividade_fisica <- ifelse(df$atividade_fisica %in% 1:4,
                                df$atividade_fisica, NA_real_)
  
  # Limpeza final — só variaveis essenciais obrigatórias
  antes <- nrow(df)
  df <- df[complete.cases(df[, c("exposicao_coca","obesidade","diabetes")]), ]
  message("Apos remover NAs essenciais: ", nrow(df),
          " (removidas: ", antes - nrow(df), ")")
  
  df
}

# =============================================================================
# Download
# =============================================================================
message("=== Ciclo C ===")
df_raw_c <- merge_ciclo(
  cache_nhanes("DEMO_C"), cache_nhanes("BMX_C"),  cache_nhanes("SMQ_C"),
  cache_nhanes("PAQ_C"),  cache_nhanes("BPQ_C"),  cache_nhanes("DIQ_C"),
  cache_nhanes("FFQRAW_C")
)

message("\n=== Ciclo D ===")
df_raw_d <- merge_ciclo(
  cache_nhanes("DEMO_D"), cache_nhanes("BMX_D"),  cache_nhanes("SMQ_D"),
  cache_nhanes("PAQ_D"),  cache_nhanes("BPQ_D"),  cache_nhanes("DIQ_D"),
  cache_nhanes("FFQRAW_D")
)

# =============================================================================
# Limpeza
# =============================================================================
df_c <- preparar_ciclo(df_raw_c, "C")
df_d <- preparar_ciclo(df_raw_d, "D")

# =============================================================================
# União e verificações
# =============================================================================
df_completo <- rbind(df_c, df_d)

message("\n=== RESULTADO FINAL ===")
message("Total de linhas: ", nrow(df_completo))
message("Proporcao obesos:   ",
        round(100 * mean(df_completo$obesidade, na.rm = TRUE), 1), "%")
message("Proporcao expostos: ",
        round(100 * mean(df_completo$exposicao_coca, na.rm = TRUE), 1), "%")

message("\nDistribuicao exposicao_coca:")
print(table(df_completo$exposicao_coca, useNA = "always"))
message("\nDistribuicao obesidade:")
print(table(df_completo$obesidade, useNA = "always"))
message("\nDistribuicao diabetes:")
print(table(df_completo$diabetes, useNA = "always"))

# Salvar
file.remove("nhanes_completo_03_06.rds")
saveRDS(df_completo, "nhanes_completo_03_06.rds")
message("\nSalvo: nhanes_completo_03_06.rds")
message("Proxima etapa: nhanes_diagrama_causal.R")