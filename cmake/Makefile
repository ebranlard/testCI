all: compile test

build:
	@echo "------------------------------------------------------------"
	mkdir build

compile: build
	@echo "------------------------------------------------------------"
	cd build && cmake .. && make

test: testprog testpy

testpy:
	@echo "------------------------------------------------------------"
ifeq ($(OS),Windows_NT)
	python WrapLib.py build/libpurelib.dll 
else
	python WrapLib.py build/libpurelib.so 
endif
	@echo "------------------------------------------------------------"

testprog:
	@echo "------------------------------------------------------------"
	./build/test_matlib
