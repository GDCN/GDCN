GDCN
====

Distribute ALL the computations!

Build project (with tests):
mvn install

Build projects but skip tests:
mvn -Dmaven.test.skip=true install

Run CLI with the following command (from directory GDCN_proj):
mvn -f UI/pom.xml exec:java