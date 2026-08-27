library(fcaR)

datos_animales <- matrix(
  c(1, 0, 1, 0, 0,  # Gato
    1, 0, 1, 0, 0,  # Perro
    1, 0, 0, 1, 0,  # Delfín
    1, 0, 0, 1, 0,  # Ballena
    0, 1, 1, 0, 1,  # Gorrión
    0, 1, 0, 1, 0), # Pingüino
  nrow = 6, 
  byrow = TRUE
)

rownames(datos_animales) <- c("Gato", "Perro", "Delfín", "Ballena", "Gorrión", "Pingüino")
colnames(datos_animales) <- c("Mamifero", "Ave", "Terrestre", "Acuatico", "Vuela")

# Inicializar el Contexto Formal
fc <- FormalContext$new(datos_animales)
print(fc)

# Extraer los conceptos formales
fc$find_concepts()
print(fc$concepts)

# Extraer la base de implicaciones lógicas
fc$find_implications()
print(fc$implications)

# Operadores de derivación
S_objetos <- Set$new(attributes = fc$objects)
S_objetos$assign(Gato = 1, Perro = 1)
print(fc$intent(S_objetos))

S_atributos <- Set$new(attributes = fc$attributes)
S_atributos$assign(Mamifero = 1, Acuatico = 1)
print(fc$extent(S_atributos))