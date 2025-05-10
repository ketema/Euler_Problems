JAVAC=javac
JAVA=java

all: MatrixProduct.class

test: MatrixProductTest.class
	$(JAVA) MatrixProductTest

MatrixProduct.class: MatrixProduct.java
	$(JAVAC) MatrixProduct.java

MatrixProductTest.class: MatrixProductTest.java
	$(JAVAC) MatrixProductTest.java

run: MatrixProduct.class
	$(JAVA) MatrixProduct matrix.txt

clean:
	rm -f *.class
