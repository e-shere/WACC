# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler

# Useful locations

ANTLR_DIR	 := antlr_config
SOURCE_DIR	 := src
ANTLR_SOURCE_DIR := $(SOURCE_DIR)/antlr
OUTPUT_DIR	 := bin

# Project tools

ANTLR	:= antlrBuild
MKDIR	:= mkdir -p
JAVAC	:= javac
RM	:= rm -rf

# Configure project Java flags

JFLAGS	:= -sourcepath $(SOURCE_DIR) -d $(OUTPUT_DIR) -cp lib/antlr-4.9.3-complete.jar 


# The make rules:

# run the antlr build script then attempts to compile all .java files within src/antlr
all:
	# cd $(ANTLR_DIR) && ./$(ANTLR) 
	# $(MKDIR) $(OUTPUT_DIR)
	# $(JAVAC) $(JFLAGS) $(ANTLR_SOURCE_DIR)/*.java
	sbt compile

# clean up all of the compiled files
clean:
	$(RM) $(OUTPUT_DIR) $(SOURCE_DIR)/antlr

.PHONY: all clean
