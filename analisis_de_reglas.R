library(dplyr)
library(arules)
library(fim4r)
library(arulesViz)

# Funciones --------------------------------------------------------------------------------

quantil_min <- function(x,n_cuantiles){
  largo <- 0
  while (largo != n_cuantiles+2) {
    
    breaks_cuant <- quantile(x, probs = seq(0, 1, length.out = n_cuantiles + 1), na.rm = TRUE, include.lowest = T)
    
    largo <- breaks_cuant |> 
      unique() |> 
      length()  
    
    if(largo != n_cuantiles+2){
      n_cuantiles <- n_cuantiles-1
    }
  }
  return(cut(x, breaks = c(-Inf,breaks_cuant), labels = NULL))
}

# categorizar_por_cuantiles <- function(df, vars_continuas, n_cuantiles = 4) {
#   df_categorizado <- df %>%
#     mutate(across(all_of(vars_continuas), ~ cut(.x, breaks = quantile(.x, probs = seq(0, 1, length.out = n_cuantiles + 1), na.rm = TRUE, include.lowest = TRUE), labels = NULL), .names = "cat_{col}"))
#   return(df_categorizado)
# }


categorizar_por_cuantiles <- function(df, vars_continuas, n_cuantiles = 4) {
  df_categorizado <- df %>%
    mutate(across(all_of(vars_continuas), ~ quantil_min(.x,n_cuantiles), .names = "cat_{col}")) |> 
    mutate(across(contains("cat_"), ~ as.character(.x)))
  return(df_categorizado)
}

realizar_pruebas <- function(df, vars_categoricas, var_comparacion) {
  resultados <- data.frame(Variable = character(), P_Valor_Chi2 = numeric(), P_Valor_Fisher = numeric(), stringsAsFactors = FALSE)
  
  for (var in vars_categoricas) {
    tabla <- table(df[[var]], df[[var_comparacion]])
    
    chi2_test <- tryCatch(chisq.test(tabla), error = function(e) NULL)
    p_valor_chi2 <- if (!is.null(chi2_test)) chi2_test$p.value else NA
    
    fisher_test <- tryCatch(fisher.test(tabla), error = function(e) NULL)
    p_valor_fisher <- if (!is.null(fisher_test)) fisher_test$p.value else NA
    
    resultados <- rbind(resultados, data.frame(Variable = var, P_Valor_Chi2 = p_valor_chi2, P_Valor_Fisher = p_valor_fisher))
  }
  
  return(resultados)
}



resumen_categorias <- function(df) {
  # # Verificar que el data frame solo contenga variables categóricas
  # if (!all(sapply(df, is.factor))) {
  #   stop("El data frame debe contener solo variables categóricas (factores).")
  # }
  
  # Inicializar una lista para almacenar los resultados
  resultados <- list()
  
  # Iterar sobre cada columna del data frame
  for (col in names(df)) {
    # Obtener la tabla de frecuencias
    frecuencias <- table(df[[col]])
    frecuencias_relativas <- prop.table(frecuencias)
    
    # Encontrar la categoría con menor frecuencia
    min_frec <- min(frecuencias)
    min_cat <- names(frecuencias)[which.min(frecuencias)]
    min_frec_rel <- frecuencias_relativas[min_cat]
    
    # Encontrar la categoría con mayor frecuencia
    max_frec <- max(frecuencias)
    max_cat <- names(frecuencias)[which.max(frecuencias)]
    max_frec_rel <- frecuencias_relativas[max_cat]
    
    # Almacenar los resultados en la lista
    resultados[[col]] <- data.frame(
      Variable = col,
      Categoria_Min = min_cat,
      Frecuencia_Min = min_frec,
      Frecuencia_Rel_Min = min_frec_rel,
      Categoria_Max = max_cat,
      Frecuencia_Max = max_frec,
      Frecuencia_Rel_Max = max_frec_rel
    )
  }
  
  # Combinar todos los resultados en un solo data frame
  resultado_final <- do.call(rbind, resultados)
  
  return(resultado_final)
}

# Generacion variables  --------------------------------------------------------------------------------
set.seed(123)
df <- data.frame(
  var1 = rnorm(100),
  var2 = rnorm(100),
  var3 = rnorm(100),
  var4 = sample(c("P1", "P2","P3","P4"), 100, replace = TRUE),
  grupo = sample(c("A", "B"), 100, replace = TRUE)
)


vars_continuas <- c("var1", "var2", "var3")

df_categorizado <- categorizar_por_cuantiles(df, vars_continuas,n_cuantiles = 10)

summary(df_categorizado |> select(all_of(c(paste0("cat_",vars_continuas),"var4"))))


vars_categor_ori <- df |> 
  names() |> 
  as_tibble() |> 
  filter(!value %in% vars_continuas) |> 
  pull()


df_categorizado |> 
  select(all_of(c(paste0("cat_",vars_continuas),vars_categor_ori))) |> 
  as_tibble() |> 
  resumen_categorias()


vars_categoricas <- grep("^cat_", names(df_categorizado), value = TRUE)

var_comparacion <- "grupo"

resultados <- realizar_pruebas(df_categorizado, vars_categoricas, var_comparacion)

table(df_categorizado[["cat_var1"]], df_categorizado[["grupo"]])

vars_no_indep<- gsub("cat_","",resultados |>  filter(P_Valor_Chi2 > 0.05) |> pull(Variable) )




# Creacion transacciones --------------------------------------------------------------
df_categorizado |>  select(all_of(vars_no_indep),grupo)

#Se crean las transacciones
transacciones <- as(df_categorizado[, c(vars_no_indep, "grupo")], "transactions")
#transacciones <- as(df_categorizado, "transactions")

#Se observan las frecuencias de los Items
itemFrequencyPlot(transacciones, topN=30, col="pink")

#Se inspeccionan las transacciones
inspect(transacciones[1:10])

#Se filtran las transacciones menores al 0.005
transacciones <- transacciones[, itemFrequency(transacciones)>0.005]

#Visualizacion de transacciones
df_transacciones <-transacciones |> 
  as( Class = "data.frame") |> 
  as_tibble()

#tamano de las transacciones
#size(transacciones)

summary(size(transacciones))

quantile(size(transacciones), probs = seq(0,1,0.1))

data.frame(size(transacciones)) %>%
  ggplot(aes(x = size(transacciones))) +
  geom_histogram() +
  labs(title = "Distribución del tamaño de las transacciones",
       x = "Tamaño") +
  theme_bw()


itemFrequency(x = transacciones, type = "relative") %>% 
  sort(decreasing = TRUE) %>%
  head(5)


itemFrequency(x = transacciones, type = "absolute") |> 
  sort(decreasing = TRUE) %>% 
  head(5)



# reglas
# reglas <- apriori(data = transacciones,
#                   parameter = list(support = soporte,
#                                    confidence = 0.70,
#                                    # Se especifica que se creen reglas
#                                    target = "rules"))


# Creacion de reglas ---------------------------------------------------------------
reglas <- fim4r(transacciones, 
                method = "fpgrowth", 
                target = "rules", 
                supp = .1, 
                conf = .1)

# Se ven las reglas en general
reglas |> 
  sort(by ="confidence") |> 
  inspect()

# Reglas con grupo B
reglas_b <- reglas |> 
  subset(subset = rhs %in% c("grupo=B") )

# Se ven las reglas de la opcion b
reglas_b |> 
  sort(by ="confidence") |> 
  inspect()




reglas_1<- apriori(data = transacciones,
                   parameter = list(supp = 0.1, conf = 0.1, target = "rules"))

reglas_1 |> 
  sort(by ="confidence") |> 
  inspect()

reglas_1_b <- reglas_1 |> 
  subset(subset = rhs %in% c("grupo=B") )

reglas_1_b |> 
  sort(by ="confidence") |> 
  inspect()

# Lift Igual a 1 indica que hay independencia entre las variables, lif > 1 signifca que las variables estan correlacionadas positivamente, lift < 1 correlacionadas negativamente
reglas_1_b |> 
  sort(by ="confidence") |> 
  inspect() 

# Se agrega el coverage (La probabilidad de que ocuura el antecedente (soporte del antecedente)
# Se agrega test de fisher (Valor p de asociacion,si es mayor a 0.1 entonces no hay asociacion estadística entre lhs y rhs)
metricas <- interestMeasure(reglas_1_b,measure = c("coverage","fishersExactTest"),transactions = transacciones)
quality(reglas_1_b) <- cbind(quality(reglas_1_b), metricas)

#####################################


# Analisis de reglas------------------------------------------------------------

plot(reglas_b, measure=c("support", "confidence"), shading="lift",col="#FF66CC")


reglas_1_b |> 
  as("data.frame") |> 
  as_tibble() |> 
  tidyr::separate_wider_delim(cols= rules, delim  = " => ",names = c("caracts","targets")) |> 
  mutate(caracts =  stringr::str_replace_all(caracts, ",(?=[:alpha:])",">>>")) |> 
  tidyr::separate_wider_delim(cols= caracts, delim  = ">>>",names_sep = "_",too_few = "align_start") |> 
  mutate(across(contains(c("caracts","targets")),~gsub("\\{|\\}","",.x))) |> 
  filter(!caracts_1 == "") |> 
  arrange(-confidence)
  
