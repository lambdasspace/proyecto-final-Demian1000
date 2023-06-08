# Conversor de regex a AFD mínimo

## Demian Alejandro Monterrubio Acosta
## No. Cuenta: 317529180

### **Explicación:**
Este proyecto consiste en un programa que pueda leer expresiones regulares por la línea de comandos, y genere el afd mínimo que reconozca el lenguaje generado por la expresión.

Los operadores válidos para expresiones regulares se presentan a continuación, en su orden de precedencia, los operadores binarios asocian a la izquierda:

|símbolo | operador| precedencia |
|--------|---------|-------------|
| \| | disyunción | 3
| αβ| concatenacion| 2
| * | estrella de kleene | 1
| + | cerradura positiva | 1
| ? | 0 o 1 ocurrencia | 1
| ()| paréntesis |  0

Además, tenemos los siguientes símbolos terminales:

[] : listas de terminales

'a' : un caracter

~ : negación de un caracter

. : cualquier caracter

\- : la cadena vacía (epsilon)

Una explicacíon más detallada del proyecto se encuentra en el documento ../Reporte.pdf de la carpeta padre.

#### Salida
Cuando este proyecto se ejecuta, por la línea de comandos pedirá una expresión regular válida, una vez sea dada, generará el afd mínimo, y lo guardará en formato svg en el archivo **afd.svg** que se encuentra en esta misma carpeta.

La salida es un afd, en el cual los estados finales están coloreados de amarillo, el estado inicial es el estado 0, (siempre el que está más a la izquierda), las aristas siempre salen de los costados de los vértices y llegan a un vértice por su parte superior o inferior.

### **Compilación y uso:**
(Este proyecto fue desarrollado usando stack).

#### Compilado:
> stack build

#### Ejecución:
> stack exec regex-afd-exe

### **Dependencias:**
Este proyecto ocupa las siguientes dependencias para funcionar:
  - unordered-containers-0.2.19.1
  - hashable-1.4.2.0