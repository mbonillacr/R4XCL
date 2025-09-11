# R4XCL: Un Puente de Integración entre R y Excel

R4XCL es una herramienta de código abierto diseñada para democratizar el análisis de datos avanzado, integrando la potencia computacional del lenguaje de programación **R** directamente en el entorno familiar de **Microsoft Excel**. Utilizando [**BERT (Basic Excel R Toolkit)**](https://bert-toolkit.com/) como puente tecnológico, R4XCL permite a usuarios sin conocimientos de programación ejecutar análisis estadísticos avanzados, modelado matemático complejo y visualizaciones de alta calidad como si fueran funciones nativas de Excel.

Este proyecto fue desarrollado como parte de la tesis de maestría de Minor Bonilla Gómez en Matemática Aplicada en la Universidad de Costa Rica.

---

### Características Clave

* **Análisis Avanzado en Entorno Familiar**: Permite a los usuarios de Excel acceder a un amplio catálogo de funciones de R para econometría, aprendizaje automático, análisis de datos y gráficos sin tener que abandonar su entorno de trabajo familiar.
* **Reproducibilidad y Rigor Científico**: Los cálculos se ejecutan mediante scripts de R, lo que garantiza la trazabilidad y la reproducibilidad, a diferencia de los métodos manuales de Excel que son propensos a errores.
* **Biblioteca Modular y Extensible**: La arquitectura de R4XCL está diseñada para que los usuarios y desarrolladores puedan añadir nuevas funciones y módulos personalizados con facilidad.
* **Gestión de Dependencias**: Incluye una solución robusta para gestionar las librerías de R, lo que asegura la compatibilidad y la estabilidad del entorno analítico.

---

### Instalación y Configuración

#### Requisitos Previos

* **Microsoft Windows** (7, 10, 11).
* **Microsoft Excel** (2007 o superior, 32 o 64 bits).
* Conexión a internet para las descargas iniciales.

#### Paso 1: Instalación de BERT Toolkit

1.  Cierra todas las instancias de Microsoft Excel.
2.  Descarga e instala el [BERT Toolkit](https://bert-toolkit.com/) desde su sitio web oficial. Este proceso instalará automáticamente una versión compatible de R.
3.  Una vez finalizada la instalación, verás una nueva pestaña en la cinta de opciones de Excel llamada "BERT".

#### Paso 2: Configuración de las Librerías R4XCL

1.  Descarga el repositorio de R4XCL desde GitHub.
2.  Copia el contenido de la carpeta `LIBRERIA` en la siguiente ruta: `Documents\BERT\functions`.
3.  Reinicia Excel para que las nuevas librerías se carguen correctamente.

#### Paso 3: Instalación de Dependencias de R

R4XCL incluye una función para instalar automáticamente todos los paquetes de R necesarios desde un repositorio local para garantizar la estabilidad.

1.  Crea una subcarpeta llamada `Paquetes` dentro de tu carpeta principal de R4XCL. Descarga todos los archivos de paquetes (`.tar.gz`) de la carpeta `DEPENDENCIAS` del repositorio de GitHub y colócalos allí
