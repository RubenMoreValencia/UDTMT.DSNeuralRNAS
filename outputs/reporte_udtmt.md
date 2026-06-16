# Reporte UDT-MT DSNeuralRNAS

## 1. Identificacion de la arquitectura

- Arquitectura: UDT-MT DSNeuralRNAS
- Es LSTM: FALSE
- Intencion interpretada: transformacion

## 2. Resumen dinamico textual

- Numero de tokens: 11
- Numero de stocks informativos: 6
- Numero de flujos textuales: 8
- Numero de evidencias: 3

## 3. Lectura de memoria

La unidad conserva 6 stocks informativos. Los tokens principales son: calma, llega, despues, lluvias, perdida, disminuye.

La unidad registra 8 flujos textuales u operadores. Los flujos observados son: la, llega, despues, de, las, y, la, disminuye.

La memoria textual conserva 11 tokens, 6 stocks y 8 flujos. La memoria de pregunta identifica la intencion como 'transformacion'.

## 4. Trayectoria interpretativa

estado o condicion inicial: calma | transicion o flujo textual: llega, despues, disminuye | contexto: la, de, las, lluvias | cambio final sugerido: disminuye

## 5. Respuesta trazable

El texto puede leerse como una trayectoria de transformacion que parte de calma; la transicion se apoya en llega, despues, disminuye; el contexto aparece en la, de, las, lluvias; el cambio final se refuerza con disminuye.

## 6. Ruta UDT-MT

`texto_crudo -> resolucion_lexica_pln -> pregunta_interpretada -> stocks_informativos -> flujos_textuales -> memoria_textual_trazable -> trayectoria_interpretativa -> salida_para_adtr`

## 7. Conclusión

La unidad UDT-MT se interpreta como una memoria textual trazable de tipo 'UDT-MT DSNeuralRNAS'. No corresponde a una LSTM; organiza memoria mediante stocks, flujos y evidencia.

## 8. Tabla compacta de stocks

| id_stock | tipo_stock | token | rol_funcional | peso |
| --- | --- | --- | --- | --- |
| S1 | stock_afectivo | calma | stock_afectivo | alto |
| S2 | stock_flujo_accion | llega | flujo_accion | alto |
| S3 | stock_transicion_temporal | despues | transicion_temporal | medio |
| S4 | stock_contexto | lluvias | contexto | alto |
| S5 | stock_evidencia | perdida | evidencia | alto |
| S6 | stock_flujo_transformacion | disminuye | flujo_transformacion | alto |

## 9. Tabla compacta de flujos

| id_flujo | tipo_flujo | token | rol_funcional | peso |
| --- | --- | --- | --- | --- |
| F1 | marcador_referencial | la | marcador_referencial | medio |
| F2 | flujo_accion | llega | flujo_accion | alto |
| F3 | transicion_temporal | despues | transicion_temporal | alto |
| F4 | marcador_referencial | de | marcador_referencial | medio |
| F5 | marcador_referencial | las | marcador_referencial | medio |
| F6 | operador_adicion | y | operador_adicion | alto |
| F7 | marcador_referencial | la | marcador_referencial | medio |
| F8 | flujo_transformacion | disminuye | flujo_transformacion | alto |
