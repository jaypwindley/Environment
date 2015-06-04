SRC := $(wildcard ./*.el)
OBJ := $(SRC:%.el=%.elc)

.PHONY: all

all: $(OBJ)

.el.elc:
	emacs --batch --eval "(byte-compile-file \"$<\")"

clean:
	rm -f *.elc *~

