;<fractions> ::= <fraction><fractions>|""
;<fraction> ::= <spaces><digits-with-sign>/<digits>
;<spaces> ::= <space><spaces>|""
;<space> ::= " "|"\n"|"\t"
;<digits-with-sign> ::= <sign><ints> | <ints>
;<sign> ::= "+"|"-"
;<digits> ::= <int><ints>|""
;<digit> ::= 0|1|2|3|4|5|6|7|8|9

