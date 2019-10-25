#!/bin/sh

java -cp path/to/your/salsa1.1.5.jar:. salsac.SalsaCompiler pa2/*.salsa
javac -classpath path/to/your/salsa1.1.5.jar:. pa2/*.java
