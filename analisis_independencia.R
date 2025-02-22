library(dplyr)

categorizar_por_cuantiles <- function(df, vars_continuas, n_cuantiles = 4) {
  df_categorizado <- df %>%
    mutate(across(all_of(vars_continuas), ~ cut(.x, breaks = quantile(.x, probs = seq(0, 1, length.out = n_cuantiles + 1), na.rm = TRUE, include.lowest = TRUE), labels = NULL), .names = "cat_{col}"))
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

set.seed(123)
df <- data.frame(
  var1 = rnorm(100),
  var2 = rnorm(100),
  var3 = rnorm(100),
  grupo = sample(c("A", "B"), 100, replace = TRUE)
)


vars_continuas <- c("var1", "var2", "var3")

df_categorizado <- categorizar_por_cuantiles(df, vars_continuas,n_cuantiles = 10)

vars_categoricas <- grep("^cat_", names(df_categorizado), value = TRUE)

var_comparacion <- "grupo"

resultados <- realizar_pruebas(df_categorizado, vars_categoricas, var_comparacion)

table(df_categorizado[["cat_var1"]], df_categorizado[["grupo"]])

vars_no_indep<- gsub("cat_","",resultados |>  filter(P_Valor_Chi2 > 0.05) |> pull(Variable) )






################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
library(arules)
library(fim4r)
library(arulesViz)



df_categorizado |>  select(all_of(vars_no_indep),grupo)

#Se crean las transacciones
transacciones <- as(df_categorizado[, c(vars_no_indep, "grupo")], "transactions")

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


reglas_1_b |> 
  sort(by ="confidence") |> 
  inspect() 



as(reglas_b, "data.frame") |> 
  as_tibble()
#####################################

plot(reglas_b, measure=c("support", "confidence"), shading="lift",col="#FF66CC")

######################################################################################

################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################


install.packages("arules")
install.packages("rCBA")
install.packages("fim4r ")
library(arules)
library(rCBA)
library(fim4r)

datos <- data.frame(
  edad = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70),
  ingresos = c(2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000),
  producto_comprado = c("Pan", "Leche", "Pan", "Leche", "Pan", "Leche", "Pan", "Leche", "Pan", "Leche")
)

datos$edad_cat <- cut(datos$edad, breaks = quantile(datos$edad, probs = seq(0, 1, by = 0.1)), include.lowest = TRUE, labels = FALSE)
datos$ingresos_cat <- cut(datos$ingresos, breaks = quantile(datos$ingresos, probs = seq(0, 1, by = 0.1)), include.lowest = TRUE, labels = FALSE)



split(datos$producto_comprado, datos$edad)


datos_combinados <- paste(datos$edad_cat, datos$ingresos_cat, datos$producto_comprado, sep = "_")

# Convertir a formato de transacción
transacciones <- as(split(datos_combinados, seq_along(datos_combinados)), "transactions")

# Ver las transacciones generadas
inspect(transacciones)

transacciones <- as(datos[, c("edad_cat", "ingresos_cat", "producto_comprado")], "transactions")
transacciones <- as(datos[, c("producto_comprado")], "transactions")

itemFrequencyPlot(transacciones, topN=30, col="pink")

transacciones <- transacciones[, itemFrequency(transacciones)>0.005]







frecuentes <-  arules::fpgrowth(transacciones, parameter = list(support = 0.5))

frecuentes <- fim4r(transacciones, method = "fpgrowth", target = "rules", supp = .1, conf = .4)

summary(frecuentes)

inspect(frecuentes)

reglas <- ruleInduction(frecuentes, transacciones, confidence = 0.7)

inspect(reglas)

