# =============================================================================
# Replicação: Qi & Li (2024) — Etapa 3 v3
# Ajustes:
#   1. "Old" definido como idade >= 55 (em vez de >= 60)
#   2. FFQ dietético: "consome" = frequência >= 1x/semana (em vez de qualquer consumo)
# =============================================================================

library(dplyr)
library(nhanesA)

set.seed(42)


# =============================================================================
# 1. CARREGAR DADOS
# =============================================================================

df  <- readRDS("nhanes_completo_03_06.rds")
rct <- readRDS("resultados_rct.rds")

p_y1_do_x1 <- rct$p_y1_do_x1
p_y1_do_x0 <- rct$p_y1_do_x0

df$exposicao_coca <- suppressWarnings(as.numeric(as.character(df$exposicao_coca)))
df$obesidade      <- suppressWarnings(as.numeric(as.character(df$obesidade)))
df$diabetes       <- suppressWarnings(as.numeric(as.character(df$diabetes)))

message("Linhas: ", nrow(df))


# =============================================================================
# 2. REPROCESSAR ATIVIDADE FÍSICA (PAQ180)
# =============================================================================

cache_nhanes <- function(nome) {
  dir.create("cache", showWarnings = FALSE)
  path <- file.path("cache", paste0(nome, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  d <- nhanes(nome); saveRDS(d, path); d
}

paq_c <- cache_nhanes("PAQ_C")
paq_d <- cache_nhanes("PAQ_D")

mapear_paq180 <- function(paq) {
  x <- as.character(paq$PAQ180)
  data.frame(
    SEQN = paq$SEQN,
    atividade_fisica = case_when(
      grepl("heavy work|carries heavy",         x, ignore.case = TRUE) ~ 1L,
      grepl("lift.*light|climb stairs",         x, ignore.case = TRUE) ~ 2L,
      grepl("stand.*walk.*lot|walks.*lot",      x, ignore.case = TRUE) ~ 3L,
      grepl("sit.*during the day|sits.*during", x, ignore.case = TRUE) ~ 4L,
      TRUE ~ NA_integer_
    ),
    stringsAsFactors = FALSE
  )
}

ativ <- rbind(mapear_paq180(paq_c), mapear_paq180(paq_d))
df$atividade_fisica <- NULL
df <- merge(df, ativ, by.x = "id", by.y = "SEQN", all.x = TRUE)
message("NAs atividade_fisica: ", sum(is.na(df$atividade_fisica)))


# =============================================================================
# 3. CARREGAR E PROCESSAR VARIÁVEIS DIETÉTICAS
# =============================================================================
# Limiar: "consome" = frequência >= 1x/semana
# Racional: subgrupos do artigo (Old Man Hamburger etc.) requerem consumo
# habitual — frequências baixas (1-6x/ano, mensais) não caracterizam o padrão
# dietético descrito no artigo.

message("\n=== Carregando dados dietéticos ===")
ffq_c <- cache_nhanes("FFQRAW_C")
ffq_d <- cache_nhanes("FFQRAW_D")

cols_dieta <- c("SEQN","FFQ0013","FFQ0015","FFQ0017","FFQ0020",
                "FFQ0048","FFQ0058","FFQ0060","FFQ0076","FFQ0085")

# Frequências abaixo de 1x/semana — consideradas "não consome"
freq_baixa <- c("never",
                "1-6 times per year",
                "7-11 times per year",
                "1 time per month",
                "2-3 times per month")

binarizar_ffq <- function(ffq_raw) {
  d <- ffq_raw[, intersect(cols_dieta, colnames(ffq_raw))]
  d[] <- lapply(d, function(x) if (is.factor(x)) as.character(x) else x)
  for (col in setdiff(colnames(d), "SEQN")) {
    d[[col]] <- case_when(
      d[[col]] %in% freq_baixa            ~ 0,
      d[[col]] %in% c("Blank","Error")    ~ NA_real_,
      !is.na(d[[col]])                    ~ 1,
      TRUE                                ~ NA_real_
    )
  }
  d
}

dieta <- rbind(binarizar_ffq(ffq_c), binarizar_ffq(ffq_d))
colnames(dieta) <- c("id","salada","batata_frita","hamburguer","cachorro_quente",
                     "sorvete","bala","pipoca","cerveja","calda")

message("Hamburguer >= 1x/semana (0=não, 1=sim):")
print(table(dieta$hamburguer, useNA = "always"))

df_full <- merge(df, dieta, by = "id", all.x = TRUE)
message("Dataset completo: ", nrow(df_full), " linhas")


# =============================================================================
# 4. FUNÇÕES DE PNS
# =============================================================================

pns_bounds_geral <- function(df, yx, yx_prime) {
  p_y     <- mean(df$obesidade, na.rm = TRUE)
  p_xy    <- mean(df$exposicao_coca == 1 & df$obesidade == 1, na.rm = TRUE)
  p_xpy_p <- mean(df$exposicao_coca == 0 & df$obesidade == 0, na.rm = TRUE)
  p_xy_p  <- mean(df$exposicao_coca == 1 & df$obesidade == 0, na.rm = TRUE)
  p_xpy   <- mean(df$exposicao_coca == 0 & df$obesidade == 1, na.rm = TRUE)
  lb <- max(0, yx - yx_prime, p_y - yx_prime, yx - p_y)
  ub <- min(yx, 1 - yx_prime, p_xy + p_xpy_p, yx - yx_prime + p_xy_p + p_xpy)
  c(lb = round(lb, 6), ub = round(ub, 6))
}

pns_bounds_refinado <- function(df_sub) {
  n <- nrow(df_sub)
  if (n < 385) return(NULL)
  z_vals  <- sort(unique(df_sub$diabetes[!is.na(df_sub$diabetes)]))
  soma_lb <- 0
  soma_ub <- 0
  for (z in z_vals) {
    sub_z  <- df_sub[!is.na(df_sub$diabetes) & df_sub$diabetes == z, ]
    p_z    <- nrow(sub_z) / n
    if (p_z == 0) next
    sub_x1 <- sub_z[!is.na(sub_z$exposicao_coca) & sub_z$exposicao_coca == 1, ]
    sub_x0 <- sub_z[!is.na(sub_z$exposicao_coca) & sub_z$exposicao_coca == 0, ]
    if (nrow(sub_x1) == 0 || nrow(sub_x0) == 0) next
    p_y_x1_z <- mean(sub_x1$obesidade, na.rm = TRUE)
    p_y_x0_z <- mean(sub_x0$obesidade, na.rm = TRUE)
    soma_lb  <- soma_lb + max(0, p_y_x1_z - p_y_x0_z) * p_z
    soma_ub  <- soma_ub + min(p_y_x1_z, 1 - p_y_x0_z) * p_z
  }
  c(lb = round(soma_lb, 5), ub = round(soma_ub, 5))
}

calcular_subgrupo <- function(nome, filtro, df) {
  filtro[is.na(filtro)] <- FALSE
  sub <- df[filtro & complete.cases(df[, c("exposicao_coca","obesidade","diabetes")]), ]
  n   <- nrow(sub)
  if (n < 385) {
    message(sprintf("  %-45s  n=%d  (< 385, ignorado)", nome, n))
    return(NULL)
  }
  b <- pns_bounds_refinado(sub)
  if (is.null(b)) return(NULL)
  message(sprintf("  %-45s  n=%4d  PNS: [%.5f, %.5f]", nome, n, b["lb"], b["ub"]))
  data.frame(subgrupo=nome, n=n, lb=b["lb"], ub=b["ub"], stringsAsFactors=FALSE)
}


# =============================================================================
# 5. PNS — POPULAÇÃO GERAL
# =============================================================================

message("\n=== PNS — População Geral ===")
bg <- pns_bounds_geral(df_full, p_y1_do_x1, p_y1_do_x0)
message("Fronteiras Tian & Pearl:  [", bg["lb"], ", ", bg["ub"], "]")
message("Artigo esperado:          [0.096, 0.405]")
br <- pns_bounds_refinado(df_full)
message("Refinado Theorem 2:       [", br["lb"], ", ", br["ub"], "]")


# =============================================================================
# 6. SUBGRUPOS DEMOGRÁFICOS (Table 2)
# =============================================================================
# AJUSTE: "Old" = idade >= 55 (testando em paralelo com >= 60)

message("\n=== PNS — Subgrupos Demográficos (Table 2) ===")
message("Testando corte de idade: 55 e 60\n")

eh_homem  <- df_full$genero == "Male"
eh_mulher <- df_full$genero == "Female"

for (corte in c(55, 60)) {
  eh_velho <- !is.na(df_full$idade) & df_full$idade >= corte
  eh_jovem <- !is.na(df_full$idade) & df_full$idade <  corte
  educ_baixa <- df_full$escolaridade %in% c(
    "Less Than 9th Grade",
    "9-11th Grade (Includes 12th grade with no diploma)"
  )
  
  message(sprintf("\n--- Corte idade >= %d ---", corte))
  bind_rows(Filter(Negate(is.null), list(
    calcular_subgrupo(paste0("Activity High (>=", corte, ")"),    df_full$atividade_fisica == 1, df_full),
    calcular_subgrupo(paste0("Man (>=", corte, ")"),              eh_homem,                      df_full),
    calcular_subgrupo(paste0("Age ", corte, "+"),                 eh_velho,                      df_full),
    calcular_subgrupo(paste0("Old Man (>=", corte, ")"),          eh_homem & eh_velho,           df_full),
    calcular_subgrupo(paste0("Old Act Low (>=", corte, ")"),      eh_velho & df_full$atividade_fisica >= 3, df_full),
    calcular_subgrupo(paste0("Old Hyperlipid (>=", corte, ")"),   eh_velho & df_full$hiperlipidemia == 1,   df_full),
    calcular_subgrupo(paste0("Old Man Educ Low (>=", corte, ")"), eh_homem & eh_velho & educ_baixa,         df_full),
    calcular_subgrupo(paste0("Old Man Act Low (>=", corte, ")"),  eh_homem & eh_velho & df_full$atividade_fisica >= 3, df_full),
    calcular_subgrupo(paste0("Woman (>=", corte, ")"),            eh_mulher,                     df_full),
    calcular_subgrupo(paste0("Age <", corte),                     eh_jovem,                      df_full),
    calcular_subgrupo(paste0("Young Woman (<", corte, ")"),       eh_mulher & eh_jovem,          df_full),
    calcular_subgrupo(paste0("Young Woman Act High (<", corte,")"),eh_mulher & eh_jovem & df_full$atividade_fisica == 1, df_full)
  )))
}

message("\nArtigo Table 2 (referência):")
tab2_ref <- data.frame(
  subgrupo = c("Activity High","Man","Age60+","Old Man","Old Activity Low",
               "Old Hyperlipidemia Yes","Old Man Education Low","Old Man Activity Low",
               "Woman","Age60-","Young Woman","Young Woman Activity High"),
  lb = c(0.10122,0.12298,0.14792,0.16865,0.14637,0.15442,0.16102,0.22938,
         0.08430,0.09140,0.07391,0.05077),
  ub = c(0.39544,0.40726,0.43166,0.41567,0.47675,0.40864,0.41829,0.53263,
         0.39075,0.37639,0.37845,0.34600)
)
print(tab2_ref)


# =============================================================================
# 7. SUBGRUPOS DIETÉTICOS (Table 3)
# =============================================================================
# Testando os dois cortes de idade também

message("\n=== PNS — Subgrupos Dietéticos (Table 3) ===")

for (corte in c(55, 60)) {
  eh_velho <- !is.na(df_full$idade) & df_full$idade >= corte
  eh_jovem <- !is.na(df_full$idade) & df_full$idade <  corte
  
  message(sprintf("\n--- Corte idade >= %d ---", corte))
  bind_rows(Filter(Negate(is.null), list(
    calcular_subgrupo(paste0("Old Man Hamburger (>=",  corte,")"), eh_homem & eh_velho & df_full$hamburguer == 1,       df_full),
    calcular_subgrupo(paste0("Old Man Hotdog (>=",     corte,")"), eh_homem & eh_velho & df_full$cachorro_quente == 1, df_full),
    calcular_subgrupo(paste0("Old Man Fries (>=",      corte,")"), eh_homem & eh_velho & df_full$batata_frita == 1,    df_full),
    calcular_subgrupo(paste0("Old Man Icecream (>=",   corte,")"), eh_homem & eh_velho & df_full$sorvete == 1,         df_full),
    calcular_subgrupo(paste0("Old Man Candy (>=",      corte,")"), eh_homem & eh_velho & df_full$bala == 1,            df_full),
    calcular_subgrupo(paste0("Old Man Beer (>=",       corte,")"), eh_homem & eh_velho & df_full$cerveja == 1,         df_full),
    calcular_subgrupo(paste0("YW No Hamburger (<",     corte,")"), eh_mulher & eh_jovem & df_full$hamburguer == 0,     df_full),
    calcular_subgrupo(paste0("YW No Popcorn (<",       corte,")"), eh_mulher & eh_jovem & df_full$pipoca == 0,         df_full),
    calcular_subgrupo(paste0("YW Salad (<",            corte,")"), eh_mulher & eh_jovem & df_full$salada == 1,         df_full),
    calcular_subgrupo(paste0("YW No Syrup (<",         corte,")"), eh_mulher & eh_jovem & df_full$calda == 0,          df_full),
    calcular_subgrupo(paste0("YW No Fries (<",         corte,")"), eh_mulher & eh_jovem & df_full$batata_frita == 0,   df_full),
    calcular_subgrupo(paste0("YW No Hotdog (<",        corte,")"), eh_mulher & eh_jovem & df_full$cachorro_quente == 0,df_full)
  )))
}

message("\nArtigo Table 3 (referência):")
tab3_ref <- data.frame(
  subgrupo = c("Old Man Hamburger","Old Man Hotdog","Old Man Fries","Old Man Icecream",
               "Old Man Candy","Old Man Beer","Young Woman No Hamburger",
               "Young Woman No Popcorn","Young Woman Salad","Young Woman No Syrup",
               "Young Woman No Fries","Young Woman No Hotdog"),
  lb = c(0.29909,0.24406,0.20028,0.18539,0.16524,0.14397,
         0.00905,0.02081,0.07663,0.01923,0.00198,0.00000),
  ub = c(0.57860,0.48335,0.47119,0.47663,0.37878,0.42206,
         0.22395,0.29678,0.29593,0.23076,0.21941,0.19487)
)
print(tab3_ref)


# =============================================================================
# 8. SALVAR
# =============================================================================

saveRDS(
  list(bounds_geral=bg, bounds_ref=br,
       tab2_ref=tab2_ref, tab3_ref=tab3_ref),
  "resultados_pns.rds"
)
message("\nSalvo: resultados_pns.rds")