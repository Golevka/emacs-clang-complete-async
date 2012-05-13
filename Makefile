INCLUDE_PATH    := ./src
SOURCE_PATH     := ./src
DEPENDENCY_PATH := ./src/dep
OBJECT_PATH     := ./src/obj

PROGRAM_NAME    := clang-complete

EXTERNAL_LIBS := $(wildcard /usr/lib/llvm/*.a)
LDLIBS := `llvm-config --ldflags --libs all` -lstdc++

CFLAGS += `llvm-config --cflags` -Wall -Wextra -pedantic -O3
#CC = g++

include makefile.mk


# default:
# 	g++ syntax.c -fno-rtti	\
# 		`llvm-config --cflags --ldflags --libs all` \
# 		/usr/lib/llvm/libLLVM*.a /usr/lib/llvm/libclang*.a	\
# 		-o syntax
