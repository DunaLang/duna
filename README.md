## Duna
### Instalação 
- Make
- Lex (flex)
- Gcc ou clang
- Bison (yacc)

### Desenvolvimento
- Extensão Yash para VS Code

### Execução
`make run`

`./duna <arquivo.duna>`

Para testar o problema 1 e seus "should-fail":
`make --ignore-errors test1`

Para rodar o happy-path de qualquer problema (por exemplo, o problema 2):
`make runN n=2`