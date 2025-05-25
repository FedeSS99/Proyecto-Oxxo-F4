# Datathon Tec de Monterrey 2025 
# Reto OXXO

En este repositorio, se encuentra el despliegue del reto práctico de OXXO para el Datathon 2025 del Tecnológico de Monterrey.

## Objetivo
Desarrollar un modelo de predicción que a partir de una ubicación (latitud/longitud)
determine si una tienda Oxxo tiene alto potencial de éxito, cumpla con su meta de venta.

## Propuesta
- Construcción de una variable de respuesta basada en la tendencia al incremento/decremento de venta de cada tienda.
- Modelo XGboost para determinar la para determinar la clase 0: "No exitoso" y 1: "Exitoso"

## One-Page
- Dashboard de resultados e insights en los datos:
- [App.R](https://github.com/FedeSS99/Proyecto-Oxxo-F4/blob/main/app.R) : Shiny App
- [Carpeta de datos](https://github.com/FedeSS99/Proyecto-Oxxo-F4/tree/main/data) : Datos procesados
- [Modelo XGBoost](https://github.com/FedeSS99/Proyecto-Oxxo-F4/blob/main/02_Pruebas_Modelo.R) : Modelo de predicción de éxito.
- [Presentación](https://www.canva.com/design/DAGoY2uC-h8/Xd38-62Gd-gB0yyA6qI98g/view?utm_content=DAGoY2uC-h8&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=hf2818e8f1e) : Presentación

## Descripción de la métrica de Éxito:
La métrica propuesta se construyó utilizando también las diferencias de las *Ventas/Venta Meta*, pero ahora contaremos con $a_{ponderada}$ y $b_{ponderada}$ que toma en cuenta las magnitudes absolutas de las ganancias/perdidas:
- $a_{ponderada}$: La suma de todas las ganancias (diferencias positivas).
$$a_{ponderada} = \sum y_{i}I(y_{i} \geq 0)$$
- $b_{ponderada}$: La suma de todas las perdidas absolutas (diferencias negativas).
$$b_{ponderada} = \sum |y_{i}|I(y_{i} < 0)$$

Es fácil ver que tanto $a$ y $b$ están acotados entre 0 y 1. Además, la suma también se ubica entre 0 y 1. Sin embargo, si definimos $c_{ponderada}$ tal que sea la diferencia entre $a_{ponderada}$ con $b_{ponderada}$
$$c_{ponderada} = \frac{a_{ponderada} - b_{ponderada}}{a_{ponderada} + b_{ponderada}}$$

Esta última cantidad se localiza entre -1 y 1; siendo $c_{ponderada} = -1$ indicador de una tienda con únicamente perdidas y $c_{ponderada} = 1$ indicador de una tienda con únicamente ganancias.


