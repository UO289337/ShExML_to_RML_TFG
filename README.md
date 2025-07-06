# TFG
## Generación de Datos RDF a partir de Datos Tabulares

**Rubén Pérez Dafonte**

Tutores:
- Jose Emilio Labra Gayo
- Ángel Iglesias Préstamo


## Uso del programa

### Requisitos previos
Para poder ejecutar el programa es necesario tener instalado Rust en el ordenador en el que se vaya a ejecutar. En Windows se puede descargar desde https://www.rust-lang.org/tools/install.

Para más información acerca de como descargar e instalar Rust, así como para saber más acerca del lenguaje, se recomienda consultar el libro de Rust en https://doc.rust-lang.org/book/.

### Ejecución
El programa se debe ejecutar desde línea de comandos, pero ofrece 2 tipos de interfaces, por línea de comandos (CLI) o gráfica.

Para ejecutar el programa por línea de comandos se debe escribir lo siguiente desde el directorio raíz del programa:

```bash
cargo run -- -f <path_fichero_ShExML> -o <path_fichero_RML>
```

Se pueden escribir la -f y la -o en mayúsculas. El parámetro f indica el path o ruta al fichero ShExML de entrada y el parámetro o indica el path donde se quiere escribir el fichero RML de salida. Al utilizar esta interfaz todos los mensajes se mostrarán por línea de comandos.

En el caso de la interfaz gráfica, esta se ejecuta mediante:

```bash
cargo run -- -g [-G] [-graphics] [-Graphics]
```

Al utilizar esta interfaz se utilizan diálogos para indicar los ficheros de entrada y salida; todos los mensajes se muestran en diálogos.

En la carpeta examples se encuentran algunos ejemplos de ficheros ShExML correctos que se pueden utilizar para comprobar el correcto funcionamiento del programa.

### Limitaciones
El compilador no acepta todas las características de ShExML. A continuación, se detallan aquellas que están aceptadas:

* Todos los prefijos (PREFIX), incluido el por defecto (`:`).
* Las fuentes (SOURCE) que apunten a un fichero CSV, en remoto (URI) o local (path), o a una base de datos (JDBC URL).
* Declaración explícita de consultas SQL (QUERY)
* Iteradores sobre CSV o bases de datos, sin anidamientos
* Expresiones básicas o con UNION
* Shapes sin anidamientos

### Notas
El fichero CSV que se incluye es de ejemplo para que se vea como sería un fichero real en el caso de que se mapeara directamente a datos RDF
