MAKE=make
ifeq ($(OS),Windows_NT)
	OS=Windows
	suffix=-nmake
	CMAKE_ARGS=-G"NMake Makefiles"
	MAKE=nmake
	RMDIR=rmdir
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
	RMDIR=rm -rf
endif
BUILD_DIR=build$(suffix)


all: compile test

$(BUILD_DIR):
	@echo "------------------------------------------------------------"
	mkdir $(BUILD_DIR)

compile: $(BUILD_DIR)
	@echo "------------------------------------------------------------"
	cd $(BUILD_DIR) && cmake $(CMAKE_ARGS) .. && $(MAKE)

clean:
	cd $(BUILD_DIR) && $(MAKE) clean

purge:
	$(RMDIR) $(BUILD_DIR)

test: testprog testpy

testpy:
	@echo "------------------------------------------------------------"
	python WrapLib.py $(BUILD_DIR)/$(LIBPRE)purelib$(LIBEXT)
	@echo "------------------------------------------------------------"

testprog:
	@echo "------------------------------------------------------------"
	@$(BUILD_DIR)/test_matlib$(EXEEXT)
	@$(BUILD_DIR)/test_lapack$(EXEEXT)
	@$(BUILD_DIR)/test_lapack_min$(EXEEXT)



