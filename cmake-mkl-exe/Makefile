MAKE=make
ifeq ($(OS),Windows_NT)
	OS=Windows
	suffix=-nmake
	CMAKE_ARGS=-G"NMake Makefiles"
	MAKE=nmake
	RMDIR=rmdir /S /Q
	EXEEXT='.exe'
else
    OS=$(shell uname -s)
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
	@$(BUILD_DIR)/test_lapack_min$(EXEEXT)



