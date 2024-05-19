#lang eopl

;------------------- Integrantes y repositorio ------------------

;Laura Camila Betancourt Horta (2223435)
;Jhoan Felipe Leon Correa (2228527)
;Brayan Steven Narvaez Valdes (2226675)
;Repositorio : https://github.com/lauubetancourt/Pygraph-proyecto-FLP


;----------------- Especificacion lexica y sintactica ----------------

;----------Gramatica BNF

; 
; ; <programa> :=  <expresion>
; ;                un-programa (exp)
; ; 
; ; 
; ; <expresion> := <numero>
; ;                numero-lit (num) 
; ;
; ;		:= <texto> 
; ;		   caracter-lit (txy)
; ; 
; ;             := "\"" <texto> "\""
; ;                texto-lit (txt)
; ;
; ;		:= <false>
; ;                false-exp (false)
; ;
; ; 		:= <true>
; ;                true-exp (true)
; ; 
; ;             := <identificador>
; ;                var-exp (id)
; ;
; ; 		:= “var” “(“ (<identificador> “=” <expresion> “;”)+ “)” “{“ <expresion> “}”
; ;	            variableVar-exp (ids exps body)
; ;
; ; 		:= “set” <identificador> “=” <expresion>
; ;		   set-exp (id exp)
; ;
; ; 		:= “const” “(“ (<identificador> “=” <expresion> “;”)+ “)” “{“ <expresion> “}”
; ;	            variableConst-exp (ids exps body)
; ;
; ;             := "procedimiento" (<identificador>*',') "haga" <expresion> "finProc"
; ;                 procedimiento-exp (ids cuerpo)
; ;
; ;             := "declarar-rec" (<identificador> “(“ (<identificador> “,”)+ “)” “=” <expresion> “en” <expresion>
; ;                 rec-exp (ids exps cuerpo)
; ;
; ;             := "evaluar" expresion (expresion ",")* "finEval"
; ;                 app-exp (exp exps)  
; ; 
; ;		:= “list” ( <expresion>* “,”)
; ;                 list-exp (exps)
; ;
; ;		:= “vector” ( <expresion>* “,”)
; ;                 vector-exp (exps)
; ;
; ;		:= “reg” ( (<identificador> : <expresion>)* “,”)
; ;                 reg-exp (exps)
; ;
; ;		:= <primitiva-reg-bin> ( <expresion> , <identificador> ) 
; ;		   primaapp-bin-reg-exp (prim exp id)
; ;
; ; 	                <primitiva-reg-bin> := ref-reg (primitiva-bin-reg)
; ;
; ;		:= <primitiva-reg-ter> ( <expresion> , <identificador> , <expresion>) 
; ;		   primaapp-ter-reg-exp (prim exp1 id exp2)
; ;
; ; 	                <primitiva-reg-ter> := crear-reg (primitiva-crear-reg)
; ; 				            := set-reg (primitiva-set-reg)
; ;
; ;             := "Si" <expresion> "entonces" <expresion> "sino" <expresion> "finSI"
; ;                condicional-exp (test-exp true-exp false-exp)
; ; 
; ;             := “begin” <expresion>+ “,” “end”
; ;                 begin-exp (exps)              
; ;                
; ; 	        := “while” <expresion> “do” <expresion> “done”
; ;                while-exp (bool exp)
; ;
; ; 	        := “for” <identificador> “=” <expresion> “to” <expresion> “do” <expresion> “done” 
; ;	           for-exp (id exp1 exp2 exp3)
; ;
; ; 	        := “[“ <expresion> <pred-prim>  <expresion> “]”
; ;                 pred-prim-exp (op1 prim op2)
; ;
; ;		:= “not“ <expresion> oper-un-bool(exp)
; ; 
; ; 	                  <pred-prim>  := > (pred-bigger)
; ;		                       := <  pred-minor
; ;		                       := >=  pred-bigger-equal
; ;		                       := <=  pred-minor-equal
; ;		                       := ==  pred-equal
; ;		                       := !=  pred-not-equal
; ;		                       := and pred-and
; ;		                       := or pred-or
; ;
; ; 	        := <primitiva-sin-arg> primapp-sin-arg-exp (prim)
; ;	                 <primitiva-sin-arg> := vacio primitiva-vacio
; ;
; ; 	        := <primitiva-unaria> (<expresion>) primapp-un-exp (prim exp)
; ;	                 <primitiva-unaria> := longitud primitiva-longitud
; ;	                                    := add1 primitiva-add1
; ;	                                    := sub1 primitiva-sub1
; ;	                                    := vacio? primitiva-es-vacio
; ;	                                    := lista? primitiva-es-lista
; ;	                                    := cabeza primitiva-cabeza
; ;	                                    := cola primitiva-cola
; ;	                                    := vector? primitiva-es-vector
; ;	                                    := print primitiva-print
; ;	                                    := reg? primitiva-is-reg
; ;
; ; 	         := <primitiva-binaria> (<expresion> , <expresion>) primapp-bin-exp (prim exp1 exp2)
; ;	                 <primitiva-binaria> := + primitiva-suma
; ;	                                     := ~ primitiva-resta
; ;	                                     := / primitiva-div
; ;	                                     := * primitiva-multi
; ;	                                     := mod primitiva-mod
; ;	                                     := concat primitiva-concat
; ;	                                     := append primitiva-append
; ;	                                     := agregar-a-lista primitiva-agregar-a-lista
; ;	                                     := agregar-a-vector primitiva-agregar-a-vector
; ;	                                     := ref-vector primitiva-ref-vector
; ;
; ; 	         := <primitiva-ternaria> (<expresion> , <expresion>, <expresion>) primapp-ter-exp (prim exp1 exp2 exp3)
; ;	                 <primitiva-ternaria> := set-vector primitiva-set-vector
; ; 
; ; 	         := “grafo” <expresion> <expresion>
; ;                  grafo-exp (v a)
; ;
; ;	         := “vertices” (<expresion>*)
; ;                  vertices-exp (exps)
; ;
; ; 	         := “aristas” ( (<expresion> , <expresion>)* )
; ;                  aristas-exp (exps)
; ;
; ; 	         := (<expresion> . <atributo-grafo> (<expresion>*)) ( . <atributo-grafo> (<expresion>*)) )
; ;	            atributo-grafo-exp (exp1 atr exp2 atrs exps)
; ;
; ;	                 <atributo-grafo> := vertices atributo-vertices
; ;	                                  := aristas atributo-aristas
; ;	                                  := vecinos atributo-vecinos
; ;	                                  := agregar-arista atributo-agregar-arista
; ;	                                  := first atributo-first
; ;	                                  := rest atributo-rest
; ;	                                  := emptyG? atributo-emptyG?




;----------Especificacion lexica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identificador
   ("@" letter (arbno (or letter digit "?"))) symbol)
  (numero
   (digit (arbno digit)) number)
  (numero
   ("-" digit (arbno digit)) number)
  (numero
   (digit (arbno digit) "." digit (arbno digit)) number)
  (numero
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (texto
   (letter) string)
  (texto
   (letter (arbno (or letter digit "-" ":"))) string)
))

;----------Especificacion sintactica

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expresion (numero) numero-lit)

    (expresion (texto) caracter-lit)
    
    (expresion ("\"" texto "\"") cadena-lit)




    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IDENTIDICADORES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expresion (identificador) var-exp)


    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEFINICIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;; VAR ;;;;;;;;;;;;;;;;
    (expresion ("var" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}")
               variableVar-exp)

    ;;;;;;;;;;;;;;;; SET ;;;;;;;;;;;;;;;;
    (expresion ("set" identificador "=" expresion)
               set-exp)
    
    ;;;;;;;;;;;;;;;; CONST ;;;;;;;;;;;;;;;;
    (expresion ("const" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}")
               variableConst-exp)



    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEFINICIONS E INVOCACIONES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;; No recursivas ;;;;;;;;;;;;;;;;
    (expresion ("procedimiento" "("(separated-list identificador ",")")" "haga" expresion "finProc")
               procedimiento-exp(ids cuerpo))

    ;;;;;;;;;;;;;;;; recursivas ;;;;;;;;;;;;;;;;
    (expresion ("declarar-rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "en" expresion) 
                rec-exp)

    ;;;;;;;;;;;;;;;; Evaluar ;;;;;;;;;;;;;;;;
    (expresion ("evaluar" expresion "("(separated-list expresion ",")")" "finEval")
               app-exp(exp exps))

    

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATOS PREDEFINIDOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;; Listas ;;;;;;;;;;;;;;;;
    (expresion ("list" "("(separated-list expresion ",") ")")
               list-exp(exps))

    ;;;;;;;;;;;;;;;; Vectors ;;;;;;;;;;;;;;;;
    (expresion ("vector" "("(separated-list expresion ",") ")")
               vector-exp(exps))

    ;;;;;;;;;;;;;;;; Registros ;;;;;;;;;;;;;;;;
    (expresion ("reg" "("(separated-list identificador ":" expresion ",") ")")
               reg-exp(exps))


    ;;;;;;;;;;;;;;;; Expresion primitivas binarias ;;;;;;;;;;;;;;;;
    (expresion (primitiva-reg-bin "("  expresion ","identificador ")" )
               primaapp-bin-reg-exp)
    ;;;;;;;; Primitivas ;;;;;;;;
    (primitiva-reg-bin ("ref-reg") primitiva-bin-reg)

    ;;;;;;;;;;;;;;;; Expresion primitivas ternarias ;;;;;;;;;;;;;;;;
    (expresion (primitiva-reg-ter "(" expresion "," identificador "," expresion ")" )
               primaapp-ter-reg-exp)
    ;;;;;;;; Primitivas ;;;;;;;;
    (primitiva-reg-ter ("crear-reg") primitiva-crear-reg)
    (primitiva-reg-ter ("set-reg") primitiva-set-reg)

    



    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ESTRUCTURAS DE CONTROL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;; Condicional IF ;;;;;;;;;;;;;;;;
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSi")
               condicional-exp)
    
    ;;;;;;;;;;;;;;;; Begin ;;;;;;;;;;;;;;;;
    (expresion ("begin" expresion (arbno ";" expresion) "end")
                begin-exp)

    
    ;;;;;;;;;;;;;;;; While ;;;;;;;;;;;;;;;;
    (expresion ("while" expresion "do" expresion "done")
               while-exp)
    
    ;;;;;;;;;;;;;;;; For ;;;;;;;;;;;;;;;;
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "done")
               for-exp)




    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPRESIONES BOOLANEAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expresion ("false")
               false-exp)
    
    (expresion ("true")
               true-exp)
    
    (expresion ("[" expresion pred-prim expresion "]" )
               pred-prim-exp)

    (expresion ("not" expresion)
              oper-un-bool)

    ;;;;;;;;;;;;;;;; Primitivas boolaneas ;;;;;;;;;;;;;;;;
    (pred-prim (">")   pred-bigger)
    (pred-prim ("<")   pred-minor)
    (pred-prim (">=")  pred-bigger-equal)
    (pred-prim ("<=")  pred-minor-equal)
    (pred-prim ("==")  pred-equal)
    (pred-prim ("!=")  pred-not-equal)
    (pred-prim ("and") pred-and)
    (pred-prim ("or")  pred-or)






    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVAS GENERALES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SIN ARGUMENTO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expresion (primitiva-sin-arg)
               primapp-sin-arg-exp)
    
    ;;;;;;;;;;;;;;;; Primitivas ;;;;;;;;;;;;;;;;
    (primitiva-sin-arg ("vacio") primitiva-vacio)





    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UNARIAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expresion (primitiva-unaria "(" expresion ")" )
               primapp-un-exp)

    ;;;;;;;;;;;;;;;; Primitivas ;;;;;;;;;;;;;;;;
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("vacio?") primitiva-es-vacio)
    (primitiva-unaria ("lista?") primitiva-es-lista)
    (primitiva-unaria ("cabeza") primitiva-cabeza)
    (primitiva-unaria ("cola") primitiva-cola)
    (primitiva-unaria ("vector?") primitiva-es-vector)
    (primitiva-unaria ("print") primitiva-print)
    (primitiva-unaria ("reg?") primitiva-is-reg)





    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BINARIAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expresion (primitiva-binaria "(" expresion "," expresion ")" )
               primapp-bin-exp)
   
    ;;;;;;;;;;;;;;;; Primitivas ;;;;;;;;;;;;;;;;
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("mod") primitiva-mod)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria ("append") primitiva-append)
    (primitiva-binaria ("agregar-a-lista") primitiva-agregar-a-lista)
    (primitiva-binaria ("agregar-a-vector") primitiva-agregar-a-vector)
    (primitiva-binaria ("ref-vector") primitiva-ref-vector)




    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SIN TERNARIAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (expresion (primitiva-ternaria "(" expresion "," expresion "," expresion ")" )
               primapp-ter-exp)
    
    ;;;;;;;;;;;;;;;; Primitivas ;;;;;;;;;;;;;;;;
    (primitiva-ternaria ("set-vector") primitiva-set-vector)




    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAFOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;; Grafos ;;;;;;;;;;;;;;;;
    (expresion ("grafo" expresion expresion)
               grafo-exp)

    ;;;;;;;;;;;;;;;; Vertices ;;;;;;;;;;;;;;;;
    (expresion ("vertices" "("(separated-list expresion ",")")")
               vertices-exp)
    
    ;;;;;;;;;;;;;;;; Aristas ;;;;;;;;;;;;;;;;
    (expresion ("aristas" "(" (separated-list "(" expresion "," expresion ")" ",") ")" )
               aristas-exp)

    ;;;;;;;;;;;;;;;; Expresion Atributos ;;;;;;;;;;;;;;;;
    (expresion ("(" expresion "." atributo-grafo "("(arbno  expresion )")"(arbno "." atributo-grafo "("(arbno  expresion )")") ")")
               atributo-grafo-sin-enc-exp)

    ;;;;;;;;;;;;;;;; Atributos ;;;;;;;;;;;;;;;;
    (atributo-grafo ("vertices") atributo-vertices)
    (atributo-grafo ("aristas") atributo-aristas)
    (atributo-grafo ("vecinos") atributo-vecinos)
    (atributo-grafo ("agregar-arista") atributo-agregar-arista)
    (atributo-grafo ("first")  atributo-first)
    (atributo-grafo ("rest")   atributo-rest)
    (atributo-grafo ("emptyG?") atributo-emptyG?)
    ))


;----------------------- Implementacion ----------------------

;----------Tipos de datos

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;----------Funciones scan&parse, just-scan e interpretador

;FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;Interpretador (FrontEnd + Evaluación + señal para lectura)

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (evaluar-programa  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;----------Evaluar programa y expresiones

;evaluar-programa : <programa> -> numero
;Función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (expresion)
                   (evaluar-expresion expresion (ambiente-inicial))))))


;evaluar-expresion: <expresion> <ambiente> -> numero
;Evalua la expresión en el ambiente de entrada

(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DATOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (numero-lit (numero) numero)

      (caracter-lit (caracter) (string->symbol caracter))
      
      (cadena-lit (cadena) cadena)

      (false-exp () #f)

      (true-exp () #t)
      
      (oper-un-bool (pred) (not (evaluar-expresion pred amb)))





      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Identificadores ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
      (var-exp (id) (buscar-variable id amb))




      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Definiciones ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
      ;;;;;;;;;; Definir Var ;;;;;;;;;;
      (variableVar-exp(ids exps cuerpo)
                        (let ((args (evaluar-operandos exps amb)))
                          (evaluar-expresion cuerpo (ambiente-extendido ids args amb))
                        ))

      ;;;;;;;;;; Definir Const ;;;;;;;;;;
      (variableConst-exp(ids exps cuerpo)
                        (begin
                          (set! variables-constantes (append variables-constantes ids))
                          (let ((args (evaluar-operandos exps amb)))
                          (evaluar-expresion cuerpo (ambiente-extendido ids args amb)))
                        ))

      ;;;;;;;;;; Definir Var ;;;;;;;;;;
      (set-exp (id rhs-exp)
               (begin
                 (cond
                   [(buscar-variable-const id variables-constantes)
                    (eopl:error 'evaluar-expresion
                                 "No es posible modificar una variable constante" )]
                   [else (setref!
                          (buscar-variable-ref id amb)
                          (evaluar-expresion rhs-exp amb))
                   ])
                 1))



      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Definicion e invocacion de procedimientos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
      ;;;;;;;;;; no recursivos ;;;;;;;;;; 
      (procedimiento-exp (ids cuerpo)
                         (cerradura ids cuerpo amb))

      ;;;;;;;;;; recursivos  ;;;;;;;;;;
      (rec-exp (nombre-proc idss cuerpo letrec-cuerpo)
                  (evaluar-expresion letrec-cuerpo
                                   (ambiente-extendido-recursivo nombre-proc idss cuerpo amb)
                  ))

      ;;;;;;;;;; invocacion  ;;;;;;;;;;
      (app-exp (exp exps)
               (let ((proc (evaluar-expresion exp amb))
                     (args (evaluar-operandos exps amb)))
                 (if (procVal? proc)
                     (aplicar-procedimiento proc args)
                     (eopl:error 'evaluar-expresion
                                 "Attempt to apply non-procedure ~s" proc))
               ))





      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constructores de datos predefinidos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
      ;;;;;;;;;; listas ;;;;;;;;;;
      (list-exp (exps)
                    (evaluar-operandos exps amb))

      ;;;;;;;;;; Vecotres ;;;;;;;;;;
      (vector-exp (exps)
                    (let(( rand (evaluar-operandos exps amb)))
                        ( list->vector rand)
                    ))

      ;;;;;;;;;; Registros y sus primitivas ;;;;;;;;;;
      (reg-exp (ids exps)
                    (let(( rand (evaluar-operandos exps amb)))
                        (list (list->vector ids) (list->vector rand))
                    ))
      
      ;;;;;; Primitivas de registro binarias ;;;;;;
      (primaapp-bin-reg-exp (prim-bin-reg reg id)
                               (let((arg1 (evaluar-operando  reg amb)))
                                   (vector-ref (cadr arg1) (list-find-position id (vector->list (car arg1))))
                               ))

      ;;;;;; Primitivas de registro ternarias ;;;;;;
      (primaapp-ter-reg-exp (prim-ter-reg reg idc expc)
                               (let((arg1 (evaluar-operando reg amb))
                                    (arg2 (evaluar-operando expc amb)))
                                    (aplicar-primitiva-ternaria-reg prim-ter-reg (list arg1 idc arg2))
                               ))





      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constructores de datos predefinidos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
      ;;;;;;;;;; CONDICIONALES ;;;;;;;;;;
      (condicional-exp (test-exp true-exp false-exp)
                       (if (evaluar-expresion test-exp amb)
                           (evaluar-expresion true-exp amb)
                           (evaluar-expresion false-exp amb)
                       ))


      ;;;;;;;;;; BEGIN ;;;;;;;;;;
      (begin-exp (exp exps) 
                 (let loop 
                    ((acc (evaluar-expresion exp amb))
                     (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (evaluar-expresion (car exps) amb) (cdr exps)))
                 ))

      ;;;;;;;;;; WHILE ;;;;;;;;;;
      (while-exp (bool-exp exp)
                  (let loop ((i 0))
                   (when (evaluar-expresion bool-exp amb)
                         (evaluar-expresion exp amb)
                         (loop (+ 1 i)))
                  ))

      ;;;;;;;;;; FOR ;;;;;;;;;;
      (for-exp (id inicio final exp)
              (let ((ini (evaluar-expresion inicio amb))
                    (fin (evaluar-expresion final amb)))
                   (let loop ((i ini))
                     (when (<= i fin)
                           (evaluar-expresion exp (ambiente-extendido (list id) (list i) amb))
                           (loop (+ 1 i))
                     ))))      
      




      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Primitivas Generales ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;;;;;;;;;; Boolaneas ;;;;;;;;;;
      (pred-prim-exp (op1 pred-prim op2)
                (let ((args (evaluar-operandos (list op1 op2) amb)))
                       (aplicar-pred-prim pred-prim args)))
      
      ;;;;;;;;;; Sin argumentos ;;;;;;;;;;
      (primapp-sin-arg-exp (primitiva-sin-arg)
                           '())

      ;;;;;;;;;; Unarias ;;;;;;;;;;
      (primapp-un-exp (primitiva-unaria op1)
                      (let ((arg (evaluar-operando op1 amb)))
                      (aplicar-primitiva-unaria primitiva-unaria arg)))
      
      ;;;;;;;;;; Binarias ;;;;;;;;;;
      (primapp-bin-exp (primitiva-binaria op1 op2)
                       (let ((args (evaluar-operandos (list op1 op2) amb)))
                       (aplicar-primitiva-binaria primitiva-binaria args)))

      ;;;;;;;;;; Ternarias ;;;;;;;;;;
      (primapp-ter-exp (primitiva-ternaria op1 op2 op3)
                        (let ((args (evaluar-operandos (list op1 op2 op3) amb)))
                        (aplicar-primitiva-ternaria primitiva-ternaria args)))


      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAFOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
      ;;;;;;;;;; Grafo ;;;;;;;;;;
      (grafo-exp (v a)
                 (let ((vertices (evaluar-expresion v amb))
                       (aristas  (evaluar-expresion a amb)))
                      (list vertices aristas)
                 ))
      
      ;;;;;;;;;; Vertices ;;;;;;;;;;
      (vertices-exp (cars)
                    (let
                        ((arg (evaluar-operandos cars amb)))
                        arg
                    ))
      
      ;;;;;;;;;; Aristas ;;;;;;;;;;
      (aristas-exp (car1 car2)
                   (let
                       ((prim (evaluar-operandos car1 amb))
                        (seg (evaluar-operandos car2 amb)))
                       (crear-pares (mix prim seg))
                   ))

      ;;;;;;;;;; Atributo Grafo ;;;;;;;;;;
      (atributo-grafo-sin-enc-exp (exp1 atr exp2 atrs exps)
                          (if (null? exp2)
                              (funciongrafos
                                (let ((obj (evaluar-operando exp1 amb)))
                                     (aplicar-atributo obj atr '()))
                                atrs exps amb)

                              (funciongrafos
                                (let ((obj (evaluar-operando exp1 amb)))
                                     (let ((arg (evaluar-operando (car exp2) amb)))
                                          (aplicar-atributo obj atr arg)
                                     )
                                )
                                atrs exps amb)
                          ))


      )))









;Funciones auxiliares para aplicar evaluar-expresion a cada elemento de una lista de operandos (expresiones)
(define evaluar-operandos
  (lambda (rands env)
    (map (lambda (x) (evaluar-operando x env)) rands)))

(define evaluar-operando
  (lambda (rand env)
    (evaluar-expresion rand env)))



;applicar-primitiva-unaria: <primitiva-unaria> <list-of-expresion> -> numero
;Obtiene el resultado de aplicar la primitiva unaria al operando

(define aplicar-primitiva-unaria
  (lambda (prim arg)
    (cases primitiva-unaria prim
      (primitiva-longitud  () (string-length arg))
      (primitiva-add1      () (+ arg 1))
      (primitiva-sub1      () (- arg 1))
      (primitiva-es-vacio  () (null? arg))
      (primitiva-es-lista  () (list? arg))
      (primitiva-cabeza    () (car arg))
      (primitiva-cola      () (cdr arg))
      (primitiva-es-vector () (vector? arg))
      (primitiva-print     () (begin (display arg) (newline)))
      (primitiva-is-reg    ()  (reg?  arg))
    )))


      




;aplicar-primitiva-binaria: <primitiva-binaria> <list-of-expresion> -> numero
;Obtiene el resultado de aplicar la primitiva binaria a los operandos
(define aplicar-primitiva-binaria
  (lambda (prim args)
    (cases primitiva-binaria prim
      (primitiva-suma   () (+ (car args) (cadr args)))
      (primitiva-resta  () (- (car args) (cadr args)))
      (primitiva-div    () (/ (car args) (cadr args)))
      (primitiva-multi  () (* (car args) (cadr args)))
      (primitiva-mod    () (modulo (car args) (cadr args)))
      (primitiva-concat () (string-append (car args) (cadr args)))
      (primitiva-append () (append (car args) (cadr args)))
      (primitiva-agregar-a-lista () (append (car args) (list(cadr args))))
      (primitiva-agregar-a-vector() (list->vector (append (vector->list (car args)) (list (cadr args)))))
      (primitiva-ref-vector () (vector-ref (car args) (cadr args)))
)))

      




;aplicar-primitiva-ternaria: <primitiva-ternaria> <list-of-expresion> -> numero
;Obtiene el resultado de aplicar la primitiva ternaria a los operandos
(define aplicar-primitiva-ternaria
  (lambda (prim args)
    (cases primitiva-ternaria prim
      (primitiva-set-vector   () (vector-set! (car args) (cadr args) (caddr args))) 
)))


;aplicar-primitiva-ternaria-reg: <primitiva-reg-ter> <list-of-expresion> -> vector
;Obtiene el resultado de aplicar la primitiva ternaria de registro a los operandos
(define aplicar-primitiva-ternaria-reg
  (lambda (prim args)
    (cases primitiva-reg-ter prim
      (primitiva-crear-reg () (define v1 (list->vector (append  (vector->list (caar args)) (list (cadr args))))) (define v2 (list->vector (append  (vector->list (cadar args)) (list (caddr args)))))(list v1 v2))
      (primitiva-set-reg ()(vector-set! (cadar args)(list-find-position (cadr args) (vector->list (caar args)))(caddr args)))
      )))





;aplicar-atributo: <atributo-grafo> <exp>
;Obtiene el resultado de aplicar el atributo al grafo
;;;;;;;;;; define aplicar-atributo ;;;;;;;;;;
(define aplicar-atributo
  (lambda (exp1 atr exp2)
    (cases atributo-grafo atr
      (atributo-vertices       () (car exp1))
      (atributo-aristas        () (cadr exp1))
      (atributo-vecinos        () (vecinos (cadr exp1) exp2))
      (atributo-agregar-arista () (add-edge exp1 exp2))
      (atributo-first          () (car exp1))
      (atributo-rest           () (cdr exp1))
      (atributo-emptyG?        () (null? exp1))
      )))




;funciongrafos: <expresion> <list-of-expresion> <list-of-expresion> <environment> -> expresion
;Obtiene el resultado de aplicar todas las primitivas que se tienen que aplicar a un grafo en orden
(define funciongrafos
  ( lambda (obj atrs exps amb)
     (newline)
     (if (null? atrs) obj 
     (if (null? (car exps))
         (funciongrafos (aplicar-atributo obj (car atrs) '())
                         (cdr atrs) (cdr exps) amb)
         (let ((arg (evaluar-operando (caar exps) amb)))
              (funciongrafos (aplicar-atributo obj (car atrs) arg)
                             (cdr atrs) (cdr exps) amb))
     ))
  ))




;aplicar predicado booleano
(define aplicar-pred-prim
  (lambda (prim args)
    (cases pred-prim prim
      (pred-bigger       () (> (car args) (cadr args)))
      (pred-minor        () (< (car args) (cadr args)))
      (pred-bigger-equal () (>= (car args) (cadr args)))
      (pred-minor-equal  () (<= (car args) (cadr args)))
      (pred-equal        () (equal? (car args) (cadr args)))
      (pred-not-equal    () (not (equal? (car args) (cadr args))))
      (pred-and          () (and (car args) (cadr args)))
      (pred-or           () (or (car args) (cadr args)))
      )))




;----------Definiendo ambientes y un ambiente inicial
;Ambiente inicial

(define ambiente-inicial
  (lambda ()
    (ambiente-extendido
     '(@a @b @c @d @e)
      '(1 2 3 "Hola" "FLP" )
     (ambiente-vacio))))



;Definiendo ambientes

(define-datatype ambiente ambiente?
  (ambiente-vacio-ejecutado)
  (ambiente-extendido-ejecutado
   (syms (list-of symbol?))
   (vec vector?)
   (ambiente ambiente?)))

(define scheme-value? (lambda (v) #t))




;ambiente vacio:      -> ambiente
;Función que crea un ambiente vacío

(define ambiente-vacio 
  (lambda ()
    (ambiente-vacio-ejecutado)))





;ambiente-extendido: <list-of symbols> <list-of numbers> ambiente -> ambiente
;Función que crea un ambiente extendido

(define ambiente-extendido
  (lambda (syms vals env)
    (ambiente-extendido-ejecutado syms (list->vector vals) env)))





;----------Funcion buscar-variable

;buscar-variable: <identificador> <ambiente> -> scheme-value
;Funcion que busca una variable y retorna su valor (en caso de encontrarla) o un error (en caso de no encontrarla)

(define buscar-variable
  (lambda (id amb)
      (deref (buscar-variable-ref id amb))))

(define buscar-variable-ref
  (lambda (id amb)
    (cases ambiente amb
      (ambiente-vacio-ejecutado ()
                        (eopl:error 'apply-env-ref "No binding for ~s" id))
      (ambiente-extendido-ejecutado (ids vals amb)
                           (let ((pos (rib-find-position id ids)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (buscar-variable-ref id amb)))))))

(define buscar-variable-const
  (lambda (elemento lista)
    (cond
      [(null? lista) #f]
      [else
       (if(eqv? (car lista) elemento) #t
          (buscar-variable-const elemento (cdr lista)))])))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))


(define ambiente-extendido-recursivo
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (ambiente-extendido-ejecutado proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (cerradura ids body env)))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;Funciones auxiliares para encontrar la posición de un símbolo en la lista de símbolos de un ambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;----------Implementando procedimientos

(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb ambiente?)))
                     

(define aplicar-procedimiento
  (lambda (proc args)
    (cases procVal proc
      (cerradura (lista-ID exp amb)
               (evaluar-expresion exp (ambiente-extendido lista-ID args amb))))))

;----------Implementando variables mutables e inmutables

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

(define
  variables-constantes '())


;----------Implementando Registros
(define reg?
  (lambda (expresion)
  (cond ((null? expresion) #f)
        ((list? expresion)
         (let ((len (length expresion)))
               
          (and (= len 2) (vector? (car expresion)) 
               (vector? (cadr expresion)))))
          (else #f))))

;----------Implementando grafos
;;; procedimientos necesarios para los grafos

(define list-to-symbols
  (lambda (lst)
  (map string->symbol lst)))


(define mix
  (lambda (L1 L2)
    (if (or (null? L1) (null? L2))
        '()
        (cons (car L1) (cons (car L2) (mix (cdr L1) (cdr L2)))))))

(define crear-pares
  (lambda (lista)
  (cond
    [(null? lista) '()]
    [(null? (cdr lista)) (list (car lista))]
    [else (cons (list (car lista) (cadr lista))
                (crear-pares (cddr lista)))])))






;------------------------------Implementando atributos de grafos

;Vecinos
(define vecinos
  (lambda (aristas nodo)
        (if (null? aristas)
            '()
            (find-vecinos aristas nodo))
))

(define find-vecinos
  (lambda (l ele)
    (cond ((null? l) '())
          ((or (null? (car l)) (null? (cdr l))) 
           (if (or (equal? (caar l) ele) (equal? (cadar l) ele))
               (list (if (equal? (caar l) ele) (cadar l) (caar l)))
               '()
               ))
          ((equal? (caar l) ele) (cons (cadar l) (find-vecinos (cdr l) ele))) 
          ((equal? (cadar l) ele) (cons (caar l) (find-vecinos (cdr l) ele))) 
          (else (find-vecinos (cdr l) ele))))
)







;Función add-edge:
;Toma dos argumentos: grafo (el grafo) y arista (la arista a agregar).
;Llama a la función auxiliar add-edgeAux con el grafo, la arista y las aristas existentes ((cadr grafo)
;; grafo arista -> grafo

                                                                                     
;; add-edge:                                                                           
;  Proposito:                                                                            
;   grafo * aristas-> grafo ; toma un grafo y una arista, y devuelve el grafo resultante de agregar 
;   la arista en tal caso de que esta no estuviera, e igulamente con los vertices de la arista
;       
;  Args:                                                                                 
;    grafo: El grafo inicial
;    arista: la arista que se va a agregar
;                                                                                        
;  Returns:                                                                              
;    El grafo con con la arista agregada



;; add-vertice:                                                                           
;  Proposito:                                                                            
;   grafo * vertices * vertice-> grafo ; toma un grafo, sus vertices y un vertice a agregar, y devuelve el grafo resultante de agregar el vertice en tal caso de que esta no estuviera, o el mismo grafo si ya esta
;       
;  Args:                                                                                 
;    grafo: El grafo inicial
;    vertices: Lista de vertices del grafo
;    newVertice: Vertice a agregar al grafo
;                                                                                        
;  Returns:                                                                              
;    El grafo con con el vertice agregado


;; add-edgeAux:                                                                           
;  Proposito:                                                                            
;   grafo * aristas * arista-> grafo ; toma un grafo, sus aristas y una arista a agregar, y devuelve el grafo resultante de agregar la arista en tal caso de que esta no estuviera, o el mismo grafo si ya esta
;       
;  Args:                                                                                 
;    grafo: El grafo inicial
;    listaArista: Lista de aristas del grafo
;    arista: arista a agregar al grafo
;                                                                                        
;  Returns:                                                                              
;    El grafo con la arista agregada o el mismo grafo sin modificar



(define add-edge
  (lambda (grafo arista)

    (define add-vertice
      (lambda (grafo vertices newVertice)
        (cond
          [(null? vertices)
              (define verticesFinal (append (car grafo) (list newVertice)));la unificaicon toca verificarla
              (list verticesFinal (cadr grafo))]
          [(equal? newVertice (car vertices) ) 
              grafo]
          [else 
              (add-vertice grafo (cdr vertices) newVertice)]
        )))

    (define add-edgeAux
      (lambda (grafo arista listaArista)
        (cond
          [(null? listaArista)
             (define aristasFinal (append (cadr grafo) (list arista)))
             (define grafoVerticesFinal (list (car grafo) aristasFinal))
             (define grafo1 (add-vertice grafoVerticesFinal (car grafoVerticesFinal) (car arista)))
             (define grafo2 (add-vertice grafo1 (car grafo1) (cadr arista)))
             grafo2]
          [(equal? arista (car listaArista) ) 
              grafo]
          [(equal? arista (reverse (car listaArista))) 
              grafo]
          [else 
              (add-edgeAux grafo arista (cdr listaArista))]
        )))

    (add-edgeAux grafo arista (cadr grafo))
   )
 )






; reverse : list -> list
; usage : (reverse L) : la lista L ordenada de manera contraria
(define reverse
  (lambda (L)
    (if (null? L)
        '()
        (append (reverse (cdr L)) (list (car L))))))




;----------Ejecutando el interpretador

;(interpretador)


; =========== > Ejemplos de Scar&Parse < ==========

;Expresion- id-exp
(scan&parse "@valor")




;Numeros
(scan&parse "3")
(scan&parse "3.14")
(scan&parse "-2")
(scan&parse "-2.3")

;Primitivas Numeros
(scan&parse "+(1,1)")
(scan&parse "~(1,1)")
(scan&parse "/(1,1)")
(scan&parse "*(1,1)")
(scan&parse "mod(1,1)")
(scan&parse "add1(1)")
(scan&parse "sub1(1)")




;Caracter
(scan&parse "a")





;Primitivas Cadenas
(scan&parse "\"HolaMundo\"")

;Primitivas Cadenas
(scan&parse "concat(\"cadena\", \"hola\")")
(scan&parse "longitud(\"hola\")")




;Primitiva Print
(scan&parse "print(@a)")




;Expresiones boolaneas
(scan&parse "false")
(scan&parse "true")

;Expresiones pred boolaneas
(scan&parse "[10 >  2]")
(scan&parse "[10 <  2]")
(scan&parse "[10 >= 2]")
(scan&parse "[10 <= 2]")
(scan&parse "[10 == 2]")
(scan&parse "[10 != 2]")
(scan&parse "[[8 == 4] and [3 > 2]]")
(scan&parse "[[8 == 4] or  [3 > 2]]")
(scan&parse "not [10 >  2]")









;Listas
(scan&parse "list ()")
(scan&parse "list (1,2,3)")
(scan&parse "list (list (1,2,3) , list (2,2,2))")

;Primitivas lista
(scan&parse "vacio")
(scan&parse "vacio?(vacio)")
(scan&parse "vacio?(list(1,2,3))")
(scan&parse "agregar-a-lista(list(1,2,3), 5)")
(scan&parse "agregar-a-lista(list(1,2,3), list (5))")
(scan&parse "lista?(list (1,2,3,4,5))")
(scan&parse "lista?(2)")
(scan&parse "cabeza(list (1,2,3,4,5))")
(scan&parse "cola(list (1,2,3,4,5))")
(scan&parse "append(list (1,2,3,4,5) , list(6,7,8))")




;Listas
(scan&parse "vector ()")
(scan&parse "vector (1,2,3)")
(scan&parse "vector (vector (1,2,3) , list (2,2,2))")

;Primitivas lista
(scan&parse "vector?(vacio)")
(scan&parse "vector?(list(1,2,3))")
(scan&parse "vector?(vector(1,2,3))")
(scan&parse "agregar-a-vector(vector(1,2,3), 5)")
(scan&parse "agregar-a-vector(vector(1,2,3), vector (5))")
(scan&parse "ref-vector(vector (\"uno\",\"dos\",\"tres\"), 1)")
(scan&parse "set-vector(vector (\"uno\",\"dos\",\"tres\"), 1 , \"cuatro\")")



;Registros
(scan&parse "reg (@a: 1, @b: 2, @c: 3, @d: 4, @e: 5)")

;Primitivas registros
(scan&parse "reg?(reg (@a: 1, @b: 2, @c: 3, @d: 4, @e: 5))")
(scan&parse "crear-reg(reg (@a: 1, @b: 2, @b: 3), @c, 4 )")
(scan&parse "ref-reg(reg (@a: 1, @b: 2, @b: 3), @b )")
(scan&parse "set-reg(reg (@a: 1, @b: 2, @b: 3), @b, 4 )")


;Secuencia Begin
(scan&parse "begin
               @a;
               print(\"hola\");
               var (@x = 5) {add1(@x)};
               print(\"adios\");
               *(4,3) end")

;Condicional If
(scan&parse "Si [10 > 2] entonces *(20, 2) sino /(20, 2)  finSi")
(scan&parse "Si [10 == 2] entonces *(20, 2) sino /(20, 2)  finSi")

;Iteracion for
(scan&parse "for @i =1 to 10 do print(@i)done")

;Iteracion while
(scan&parse "var (@i = 1){while [@i <= 5] do begin print(@i);set @i = add1(@i) end done}")



;Definicion Var
(scan&parse "var (@x = 5) {add1(@x)}")
(scan&parse "const (@x = 5) {@x}")
(scan&parse "const (@x = 5) {begin set @x = 10; @x end}")


(scan&parse "declarar-rec
               @fact(@n) = Si [@n != 0]
                   entonces *(@n , evaluar @fact(sub1(@n)) finEval)
                    sino 1 finSi
               en
                 evaluar @fact(10) finEval")

(scan&parse "var(
               @sumar = procedimiento(@x, @y) haga
               +(@x, @y) finProc
            ){
              evaluar @sumar(2,3) finEval
            }")

;Grafos
;-vertices
(scan&parse "vertices (1,2)")

;-Aristas
(scan&parse "aristas((1,2),(2,3),(1,3))")

;-Grafo completo
(scan&parse "grafo vertices (1,2,3) aristas((1,2),(2,3),(1,3))")

;Primitivas grafos
;-Vertices
(scan&parse "(grafo vertices (1,2,3) aristas((1,2),(2,3),(1,3)).vertices())")

;-aristas
(scan&parse "(grafo vertices (1,2,3) aristas((1,2),(2,3),(1,3)).aristas())")

;-vecino
(scan&parse "(grafo vertices (1,2,3) aristas((1,2),(2,3),(1,3)).vecinos(1))")

;-fisrt
(scan&parse "(grafo vertices (1,2,3) aristas((1,2),(2,3),(1,3)).vecinos(1).first())")

;-rest
(scan&parse "(grafo vertices (1,2,3) aristas((1,2),(2,3),(1,3)).vecinos(1).rest())")

;-agregar-arista
(scan&parse "(grafo vertices (1,2,3,4) aristas((1,2),(2,4),(1,3)).agregar-arista(list(2,5)))")
(scan&parse "(grafo vertices (1,2,3,4) aristas((1,2),(2,4),(1,3)).agregar-arista(list(2,4)))")

;-Encadenamiento
(scan&parse "(grafo vertices (1,2,3) aristas((1,2),(2,3),(1,3)).vecinos(1).rest().first())")
(scan&parse "(grafo vertices (1,2,3,4) aristas((1,2),(2,4),(1,3)).aristas().rest().first().first())")

;-emptyG
(scan&parse "(grafo vertices (1,2,3) aristas((1,2),(2,3),(1,3)).aristas().emptyG?())")
(scan&parse "(grafo vertices (1,2,3) aristas ((1,2)).aristas().rest().emptyG?())")
