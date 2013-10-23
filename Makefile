INCLUDE_PATH    := ./src
SOURCE_PATH     := ./src
DEPENDENCY_PATH := ./src/dep
OBJECT_PATH     := ./src/obj

PROGRAM_NAME    := clang-complete

LDLIBS := $(shell $(LLVM_CONFIG) --ldflags) -lclang -Wl,-rpath,$(shell $(LLVM_CONFIG) --libdir)
CFLAGS += -std=c99 $(shell $(LLVM_CONFIG) --cflags) -Wall -Wextra -pedantic -O3

include makefile.mk
