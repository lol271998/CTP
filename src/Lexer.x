{
module Lexer where
}

%wrapper "basic"
$alpha = [_a-zA-Z]
$digit = [0-9]
$white = [\ \t\n\r]

tokens :-

$white+                    ;  -- skip whitespace
int                        { \_ -> TOK_INT }
string                     { \_ -> TOK_STRING }
intArray                   { \_ -> TOK_INTARRAY }
break                      { \_ -> TOK_BREAK }
do                         { \_ -> TOK_DO }
else                       { \_ -> TOK_ELSE }
end                        { \_ -> TOK_END }
for                        { \_ -> TOK_FOR }
function                   { \_ -> TOK_FUNCTION }
print                      { \_ -> TOK_PRINT }
printi                     { \_ -> TOK_PRINTI }
scani                      { \_ -> TOK_SCANI }
if                         { \_ -> TOK_IF }
in                         { \_ -> TOK_IN }
to                         { \_ -> TOK_TO }
let                        { \_ -> TOK_LET }
of                         { \_ -> TOK_OF }
then                       { \_ -> TOK_THEN }
var                        { \_ -> TOK_VAR }
while                      { \_ -> TOK_WHILE }
","                        { \_ -> TOK_COMMA }
":"                        { \_ -> TOK_COLON }
";"                        { \_ -> TOK_SMI_COLON }
"("                        { \_ -> TOK_LPAREN }
")"                        { \_ -> TOK_RPAREN }
"["                        { \_ -> TOK_LBRACKET }
"]"                        { \_ -> TOK_RBRACKET }
"+"                        { \_ -> TOK_PLUS }
"-"                        { \_ -> TOK_MINUS }
"*"                        { \_ -> TOK_MULT }
"/"                        { \_ -> TOK_DIV }
"%"                        { \_ -> TOK_PERC }
"="                        { \_ -> TOK_EQUAL }
"<>"                       { \_ -> TOK_UNEQUAL }
"<"                        { \_ -> TOK_LESS }
"<="                       { \_ -> TOK_LESS_EQUAL }
">"                        { \_ -> TOK_MORE }
">="                       { \_ -> TOK_MORE_EQUAL }
"&"                        { \_ -> TOK_AND }
"|"                        { \_ -> TOK_OR }
":="                       { \_ -> TOK_ASSIGN }
$alpha($alpha|$digit)*     { \s -> TOK_ID s }
$digit+                    { \s -> TOK_NUM (read s) }

{
data Token = TOK_INT
           | TOK_STRING
           | TOK_INTARRAY
           | TOK_BREAK
           | TOK_DO
           | TOK_ELSE
           | TOK_END
           | TOK_FOR
           | TOK_FUNCTION
           | TOK_PRINT
           | TOK_PRINTI
           | TOK_SCANI
           | TOK_IF
           | TOK_IN
           | TOK_TO
           | TOK_LET
           | TOK_OF
           | TOK_THEN
           | TOK_VAR
           | TOK_WHILE
           | TOK_COMMA
           | TOK_COLON
           | TOK_SMI_COLON
           | TOK_LPAREN
           | TOK_RPAREN
           | TOK_LBRACKET
           | TOK_RBRACKET
           | TOK_PLUS
           | TOK_MINUS
           | TOK_MULT
           | TOK_DIV
           | TOK_PERC
           | TOK_EQUAL
           | TOK_UNEQUAL
           | TOK_LESS
           | TOK_LESS_EQUAL
           | TOK_MORE
           | TOK_MORE_EQUAL
           | TOK_AND
           | TOK_OR
           | TOK_ASSIGN
           | TOK_ID String
           | TOK_NUM Int
           deriving (Eq, Show)
}
