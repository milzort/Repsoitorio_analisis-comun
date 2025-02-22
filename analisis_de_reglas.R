library(dplyr)
library(arules)
library(fim4r)
library(arulesViz)

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


reglas_1_b |> 
  as("data.frame") |> 
  as_tibble() |> 
  tidyr::separate_wider_delim(cols= rules, delim  = " => ",names = c("caracts","targets")) |> 
  mutate(caracts =  stringr::str_replace_all(caracts, ",(?=[:alpha:])",">>>")) |> 
  tidyr::separate_wider_delim(cols= caracts, delim  = ">>>",names_sep = "_",too_few = "align_start") |> 
  mutate(across(contains(c("caracts","targets")),~gsub("\\{|\\}","",.x))) |> 
  filter(!caracts_1 == "") |> 
  arrange(-confidence)
  
