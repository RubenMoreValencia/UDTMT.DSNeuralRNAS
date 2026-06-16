devtools::document()
devtools::load_all()
devtools::test()

dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
texto <- "La calma llega despues de las lluvias y la perdida disminuye"
pregunta <- "Que cambia en el texto?"

u <- crear_udtmt_ds(texto, pregunta, dic)
resumir_udtmt_ds(u)
trazar_udtmt_ds(u)
