
# Project tools
RM     := rm -rf

# The make rules:

# compile all the scala in src
all:
	sbt assembly

test:
	sbt test
	
# clean up all of the compiled files
clean:
	$(RM) *.jar
	$(RM) *.s
	sbt clean

.PHONY: all test clean
