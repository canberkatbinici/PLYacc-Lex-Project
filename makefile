make: 
	lex leaf.l
	yacc -d leaf.y
	gcc lex.yy.c y.tab.c -ll
run:
	./a.out < test.lf

clear:
	rm lex.yy.c y.tab.c a.out y.tab.h 
