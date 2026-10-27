## Capítulo: Introducción
## Versión en script R de introduccion.qmd
## Este capítulo no contiene chunks de código; el texto se conserva como comentarios.

## ---------------------------------------------------------------------------
## Variable dependiente
## ---------------------------------------------------------------------------

## | Variable        | Medición (ENEMDU)                                          | Tipo     |
## |-----------------|------------------------------------------------------------|----------|
## | Ingreso laboral | Logaritmo del ingreso laboral mensual del trabajador       | Continua |

## El ingreso se expresa en logaritmos para interpretar los coeficientes como
## cambios porcentuales.

## ---------------------------------------------------------------------------
## Variables independientes de interés
## ---------------------------------------------------------------------------

## | Variable            | Medición                                                                                                                          | Fuente         |
## |---------------------|-----------------------------------------------------------------------------------------------------------------------------------|----------------|
## | Ola de calor        | Número de días en el período de referencia (mes o trimestre) con temperatura máxima por encima de un umbral, por ejemplo, el percentil 90 local durante al menos tres días seguidos | NASA POWER o ERA5 |
## | Sequía              | Índice estandarizado de precipitación (SPI) a 3 o 6 meses; se define sequía cuando el SPI es menor o igual a -1                  | NASA POWER o ERA5 |
## | Tipo de trabajador  | Informal = 1, formal = 0 (según la definición del INEC, por afiliación a la seguridad social o sector informal)                   | ENEMDU         |

## Para evaluar los efectos desiguales se incluirían interacciones entre cada
## evento extremo y las variables de heterogeneidad: informalidad, género
## (mujer = 1), nivel educativo (por ejemplo, años de escolaridad o niveles) y
## sector económico (agricultura, industria, comercio y servicios, construcción).

## ---------------------------------------------------------------------------
## Variables de control
## ---------------------------------------------------------------------------

## | Variable                                   | Medición                                                | Función                                                  |
## |--------------------------------------------|---------------------------------------------------------|----------------------------------------------------------|
## | Edad y edad al cuadrado                    | Años cumplidos                                          | Perfil de experiencia                                    |
## | Estado civil y jefatura del hogar          | Variables binarias                                      | Características personales                               |
## | Tamaño del hogar y niños pequeños          | Número de miembros; presencia de menores de 5 años      | Carga de cuidado                                         |
## | Autoidentificación étnica                  | Categorías del INEC                                     | Diferencias persistentes                                 |
## | Área de residencia                         | Urbano = 0, rural = 1                                   | Exposición y acceso a empleo                             |
## | Grupo ocupacional y tamaño de la empresa   | Categorías de la ENEMDU                                 | Condiciones del puesto                                   |
## | Horas trabajadas                           | Horas semanales en la ocupación principal               | Diferencia el ingreso por jornada                        |
## | Temperatura y precipitación medias         | Promedio del período                                    | Aísla los extremos del clima promedio                    |
## | Efectos fijos geográficos                  | Variables binarias por provincia (o por la unidad disponible) | Controla diferencias estables entre territorios    |
## | Efectos fijos de tiempo                    | Variables binarias por año y trimestre o mes            | Controla choques comunes, como la inflación o la pandemia |
