INCLUDE_PATH    := ./src
SOURCE_PATH     := ./src
DEPENDENCY_PATH := ./src/dep
OBJECT_PATH     := ./src/obj

PROGRAM_NAME    := clang-complete
LLVM_CONFIG     := llvm-config

LDLIBS := $(shell $(LLVM_CONFIG) --ldflags) $(shell $(LLVM_CONFIG) --libs all) -lstdc++ -lclang
CFLAGS += $(shell $(LLVM_CONFIG) --cflags) -Wall -Wextra -pedantic -O3


include makefile.mk
