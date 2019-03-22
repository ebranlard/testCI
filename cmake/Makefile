MAKE=make
ifeq ($(OS),Windows_NT)
	OS=Windows
	suffix=-nmake
	CMAKE_ARGS=-G"NMake Makefiles"
	MAKE=nmake
	LIBEXT='.dll'
	EXEEXT='.exe'
else
    OS=$(shell uname -s)
    ifeq ($(OS),Linux)
		LIBEXT='.so'
		LIBPRE='lib'
    else ifeq ($(OS),Darwin)
		LIBEXT='.dylib'
    endif
endif

all: compile$(suffix) test

build$(suffix):
	@echo "------------------------------------------------------------"
	mkdir build$(suffix)

compile$(suffix): build$(suffix)
	@echo "------------------------------------------------------------"
	cd build$(suffix) && cmake $(CMAKE_ARGS) .. && $(MAKE)

clean:
	cd build$(suffix) && $(MAKE) clean

test: testprog testpy

testpy:
	@echo "------------------------------------------------------------"
	python WrapLib.py build$(suffix)/$(LIBPRE)purelib$(LIBEXT)
	@echo "------------------------------------------------------------"

testprog:
	@echo "------------------------------------------------------------"
	@./build$(suffix)/test_matlib$(EXEEXT)
	@./build$(suffix)/test_lapack$(EXEEXT)
	@./build$(suffix)/test_lapack_min$(EXEEXT)



