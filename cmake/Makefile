all: compile test


ifeq ($(OS),Windows_NT)
build-nmake:
	@echo "------------------------------------------------------------"
	mkdir build-nmake

compile-nmake: build-nmake
	@echo "------------------------------------------------------------"
	cd build-nmake && cmake -G"NMake Makeilfes" .. && nmake
else
build:
	@echo "------------------------------------------------------------"
	mkdir build

compile: build
	@echo "------------------------------------------------------------"
	cd build && cmake .. && make
endif




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
	./build/test_matlib
else
	./build-nmake/test_matlib.exe
endif
