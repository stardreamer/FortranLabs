#Makefile
SRCDIR := source

OBJDIR := obj

BINDIR := bin

f90comp := gfortran
FLAGS :=  -Wall -pedantic -Wstrict-overflow=4  -Wextra  -Wshadow -O2  -c -std=f95 
BINDIR_REL := $(addprefix ../, $(BINDIR))

SRCDIR_REL := $(addprefix ../, $(SRCDIR))

SRCFILES := $(wildcard $(addsuffix /*.f90, $(SRCDIR_REL)))

OBJDIR_REL := $(addprefix ../, $(OBJDIR))

OBJFILES := $(patsubst $(SRCDIR_REL)/%.f90,$(OBJDIR_REL)/%.o,$(wildcard $(SRCFILES)))

default:
	@echo -e "\e[0;33mmake fortranlab\e[0;32m - компиляция лаборатоной"
	@echo -e "\e[0;33mmake clean\e[0;32m - чистка объектных файлов"
	@echo -e "\e[0;33mmake delete\e[0;32m - удаление исполняемых файлов" 
	
fortranlab:$(BINDIR_REL)/fortranlab $(OBJFILES)  
	@echo -e "Компиляция  программы успешна\n"
	

$(BINDIR_REL)/fortranlab: $(OBJFILES) 
	$(f90comp) -Wall $^ -o $@ -lm

	

$(OBJDIR_REL)/%.o: $(SRCDIR_REL)/%.f90
	$(f90comp) $<  $(FLAGS) $(addprefix -I, $(SRCDIR_REL)) -o $@ -pipe -lm
	
include  $(wildcard $(OBJDIR_REL)/*.d)
#Все пользователи имеют право читать копируется в каталог
clean:
	@rm -f $(OBJDIR_REL)/*.o $(OBJDIR_REL)/*.d
	@rm -f $(OBJDIR_REL_S)/*.o $(OBJDIR_REL_S)/*.d
	@rm -f $(OBJDIR_REL_D)/*.o $(OBJDIR_REL_D)/*.d
	@rm -f $(BINDIR_REL)/*
	@echo "Очистка завершена!"
delete:
	@echo "ПОТРАЧЕНО"
	@rm -f $(BINDIR_REL)$
.PHONY: clean delete fortranlab
