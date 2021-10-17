##
## EPITECH PROJECT, 2021
## B-CPP-300-STG-3-1-CPPD10-clement.muth
## File description:
## Makefile
##

CC	=	ghc

SRC	=	DoOp.hs

EXEC	=	doop

all:    $(EXEC)

$(EXEC):	$(OBJ)
	$(CC) -o $(EXEC) $(SRC)

clean:
	rm -rf *.hi *.o

fclean: clean
	rm -rf $(EXEC)

re:	fclean all

.PHONY: clean fclean re all