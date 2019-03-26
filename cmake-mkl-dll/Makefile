MAKE=make
ifeq ($(OS),Windows_NT)
	OS=Windows
	suffix=-nmake
	CMAKE_ARGS=-G"NMake Makefiles"
	MAKE=nmake
	RMDIR=rmdir /S /Q
	LIBEXT='.dll'
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

test:
	@echo "------------------------------------------------------------"
	python Test.py $(BUILD_DIR)/$(LIBPRE)matlib$(LIBEXT)
	@echo "------------------------------------------------------------"



