INCLUDE_PATH    := ./src
SOURCE_PATH     := ./src
DEPENDENCY_PATH := ./src/dep
OBJECT_PATH     := ./src/obj

PROGRAM_NAME    := clang-complete

LDLIBS := $(shell llvm-config --ldflags) $(shell llvm-config --libs all) -lstdc++ -lclang
CFLAGS += $(shell llvm-config --cflags) -Wall -Wextra -pedantic -O3


include makefile.mk
