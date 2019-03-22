MAKE=make
ifeq ($(OS),Windows_NT)
	suffix=-nmake
	CMAKE_ARGS=-G"NMake Makefiles"
	MAKE=nmake
endif

all: compile$(suffix) test

build$(suffix):
	@echo "------------------------------------------------------------"
	mkdir build$(suffix)

compile$(suffix): build$(suffix)
	@echo "------------------------------------------------------------"
	cd build$(suffix) && cmake $(CMAKE_ARGS) .. && $(MAKE)



test: testprog testpy

testpy:
	@echo "------------------------------------------------------------"
ifeq ($(OS),Windows_NT)
	python WrapLib.py build-nmake/purelib.dll 
else
	python WrapLib.py build/libpurelib.so 
endif
	@echo "------------------------------------------------------------"

testprog:
	@echo "------------------------------------------------------------"
ifeq ($(OS),Windows_NT)
	./build$(suffix)/test_matlib.exe
else
	./build/test_matlib
endif
