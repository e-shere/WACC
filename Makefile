
# Project tools
RM     := rm -rf

# The make rules:

# compile all the scala in src
all:
	sbt assembly

# clean up all of the compiled files
clean:
	$(RM) *.jar
	sbt clean

.PHONY: all clean
