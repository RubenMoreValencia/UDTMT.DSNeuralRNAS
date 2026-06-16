library(Lexico.DSNeuralRNAS)
library(PLN.DSNeuralRNAS)
library(UDTMT.DSNeuralRNAS)

dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
texto <- "La calma llega despues de las lluvias y la perdida disminuye"
pregunta <- "Que cambia en el texto?"

u <- crear_udtmt_ds(texto, pregunta, dic)
interpretar_memoria_udtmt_ds(u)
generar_reporte_udtmt_ds(u, archivo = "outputs/reporte_udtmt_validacion.md")
