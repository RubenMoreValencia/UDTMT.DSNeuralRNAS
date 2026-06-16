# Script de integracion semantica controlada
# Este script NO consolida conocimiento automaticamente.
# Carga los archivos generados y permite revisar candidatos antes de copiarlos a las librerias destino.

lexico <- utils::read.csv('lexico_propuesto.csv', stringsAsFactors = FALSE)
relaciones <- utils::read.csv('relaciones_validadas_localmente.csv', stringsAsFactors = FALSE)
reglas <- utils::read.csv('reglas_validadas_localmente.csv', stringsAsFactors = FALSE)
tests <- utils::read.csv('tests_sugeridos.csv', stringsAsFactors = FALSE)

print(lexico)
print(relaciones)
print(reglas)
print(tests)

# Paso recomendado:
# 1. Revisar categorias lexicas.
# 2. Revisar relaciones con validacion_local == 'validada_localmente'.
# 3. Convertir tests sugeridos en pruebas testthat dentro de cada paquete afectado.
# 4. Ejecutar devtools::document(), devtools::load_all(), devtools::test().
# 5. Integrar solo si no se rompen pruebas anteriores.
