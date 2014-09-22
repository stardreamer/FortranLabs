#Makefile
SRCDIR := source

OBJDIR := obj

BINDIR := bin

MODDIR := mod

f90comp := gfortran

FLAGS :=  -Wall -pedantic -Wtabs  -Wstrict-overflow=4  -Wextra  -Wshadow -O2  -c -std=f95 
BINDIR_REL := $(addprefix ../, $(BINDIR))

MODDIR_REL := $(addprefix ../, $(MODDIR))

SRCDIR_REL := $(addprefix ../, $(SRCDIR))

SRCFILES := $(wildcard $(addsuffix /*.f90, $(SRCDIR_REL)))

MODSRCFILES := $(wildcard $(addsuffix /*.f90, $(MODDIR_REL)))

OBJDIR_REL := $(addprefix ../, $(OBJDIR))

OBJFILES := $(patsubst $(SRCDIR_REL)/%.f90,$(OBJDIR_REL)/%.o,$(wildcard $(SRCFILES)))

MODFILES := $(patsubst $(MODDIR_REL)/%.f90,$(OBJDIR_REL)/%.o,$(wildcard $(MODSRCFILES)))

default:
	@echo -e "\e[0;33mmake fortranlab\e[0;32m - компиляция лаборатоной"
	@echo -e "\e[0;33mmake clean\e[0;32m - чистка объектных файлов"
	@echo -e "\e[0;33mmake delete\e[0;32m - удаление исполняемых файлов" 
	
fortranlab:$(BINDIR_REL)/fortranlab $(MODFILES) $(OBJFILES)  
	@echo -e "Компиляция  программы успешна\n"
	

$(BINDIR_REL)/fortranlab: $(MODFILES) $(OBJFILES)
	$(f90comp) -Wall $^ -o $@ -lm

	
$(OBJDIR_REL)/%.o: $(MODDIR_REL)/%.f90
	$(f90comp) $<  $(FLAGS) $(addprefix -I, $(MODDIR_REL)) $(addprefix -J, $(OBJDIR_REL)) -o $@ -pipe -lm
	
$(OBJDIR_REL)/%.o: $(SRCDIR_REL)/%.f90 
	$(f90comp) $<  $(FLAGS) $(addprefix -I, $(OBJDIR_REL)) -o $@ -pipe -lm

	

#Все пользователи имеют право читать копируется в каталог
clean:
	@rm -f $(OBJDIR_REL)/*.o
	@rm -f $(OBJDIR_REL)/*.mod
	@find $(BINDIR_REL)/ -type f -not -name '*conf' | xargs rm -f

	@echo "Очистка завершена!"
delete:
	@echo "ПОТРАЧЕНО"
	@rm -f $(BINDIR_REL)$
.PHONY: clean delete fortranlab
