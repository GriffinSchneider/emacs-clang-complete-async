INCLUDE_PATH    := ./src
SOURCE_PATH     := ./src
DEPENDENCY_PATH := ./src/dep
OBJECT_PATH     := ./src/obj

PROGRAM_NAME    := clang-complete

LDLIBS := $(shell llvm-config --ldflags) -lclang -Wl,-rpath,$(shell llvm-config --libdir)
CFLAGS += -std=c99 $(shell llvm-config --cflags) -Wall -Wextra -pedantic -O3

include makefile.mk
