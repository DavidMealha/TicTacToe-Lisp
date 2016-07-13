;;;; projeto.lisp
;;;; Disciplina de IA - 2015 / 2016
;;;; vai conter a implementacao dos varios algoritmos de procura
;; Luis Serrano e David Mealha


(defparameter *no-teste* '((((NIL NIL NIL) (NIL NIL NIL) (X 0 NIL)) ((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))) 0 0 nil))
(defparameter *operadores* '(colocar-cruz colocar-circulo))
(defparameter *expandidos* 0) ;numero de nos expandidos
(defparameter *gerados* 0) ;numero de nos armazenados por explorar

;;; Construtores
;; cria
(defun cria-no (estado &optional(profundidade 0) (valor-heuristico 0) (no-pai NIL))
"Função que cria um novo no, recebendo 4 paramentros, onde 3 deles são opcionais, 
o estado representa o valor desse estado, a profundidade o nivel a que se encontra o no na arvore, 
o no pai do no que se pretende criar, caso seja um no inicial, o no pai é NIL e e por fim o seu valor
heuristico que e dado por uma funcao heuristica"
  (list estado profundidade valor-heuristico no-pai))

;;; Getters
;; no-estado
(defun no-estado (no)
  "Permite ler o estado, ou seja, o tabuleiro desse no"
  (first no)
)

;; no-profundidade
(defun no-profundidade (no)
  "Permite ler a profundidade do no"
  (second no)
)

;; no-valor-heuristico
(defun no-valor-heuristico (no)
  "Permite saber o valor heuristico do no passado como parametro"
  (third no)
)

;; no-pai
(defun no-pai (no)
  "Permite saber o no-pai do no passado como parametro"
  (fourth no)
)

;;; Procura-generica
;; (procura-generica *no2*  3  'solucaop 'sucessores 'dfs *operadores*)

; no-inicial constituido por um estado do problema, uma profundidade igual a zero e um valor NIL para o pai do problema.
; prof-max o valor do profundidade maxima da arvore
; f-solucao o nome do predicado que verifica se um no e solucao;
; f-sucessores o nome da funcao a utilizar para gerar os sucessores
; lista-operadores uma lista de operadores do problema que devem ser aplicados para resolver o problema
; f-algoritmo um algoritmo que permite ordenar a lista de sucessores com base na lista de abertos e nos sucessores.
;;; Procura-generica
(defun procura-generica (no_inicial prof-max f-solucao f-sucessores f-ordena lista-operadores f-heuristica)
  "Permite procurar a solucao de um problema usando a procura no espaÃ§o de estados com os algoritmos dfs, bfs e A*. De acordo com o algoritmo pode ser usada um limite 
   de profundidade, uma heuristica e uma funcao de ordenacao para os nos sucessores e os abertos.  
  "
  (let ( (abertos (list no_inicial)) (fechados nil) )
    (loop
     (cond
       ((null abertos) (return nil)); se a lista de abertos estiver vazia, nao existe solucao
       ((AND (equal f-ordena 'a-asterisco)(funcall f-solucao (first abertos))) (return (first abertos))); verifica se o no de menor custo e solucao e devolve-o se True
       
       (T   (let* ((sucessores  (funcall f-sucessores (first abertos) lista-operadores f-ordena prof-max f-heuristica abertos));gera os sucessores
              (solucao (sucessor-solucao-p sucessores f-solucao)); verifica se existe a solucao nos sucessores
              (numero-sucessores (length sucessores))); conta quantos sucessores foram gerados
              (cond 
               ((AND (OR (equal f-ordena 'bfs)(equal f-ordena 'dfs)) solucao) 
                (progn (setf *expandidos* (1+ *expandidos*)); apesar de ser encontrada solução, é preciso incrementar os expandidos
                  (setf *gerados* (+ *gerados* numero-sucessores));  incrementa o numero de nós gerados
                  (return solucao))) ; devolve solucao se bfs ou dfs e se solucao pertence aos sucessores 
               (T (progn ;senao
                    (setf *expandidos* (1+ *expandidos*));  incrementa os expandidos
                    (setf *gerados* (+ *gerados* numero-sucessores));  incrementa o numero de nós gerados
                    (setf fechados (cons (first abertos) fechados))  ; insere o no na lista de fechados
                    (setf abertos (funcall f-ordena (rest abertos) sucessores)))); redefine os abertos com a funcao ordena os sucessores
            )
          )
       )
      )
    )
  )
)

;; existep
(defun existep (no lista)
  "Permite verificar se um nó existe numa lista."
  (cond ((eq no (car lista)) T)  ;verifica se o no encontra-se na cabeça da lista, se for retorna true
        ((null lista) nil)  ;caso não tenha sido encontrado o no na lista retorna nil
        (t () (existep no (cdr lista))) ;condição recursiva para percorrer a lista 
   )
)

;;; Geracao dos sucessores
;; (sucessores *no-teste* *operadores* 3 nil)
;; sucessores
(defun sucessores (no operadores algoritmo prof-max f-heuristica abertos)
  "Permite gerar os sucessores de um no e recebe 4 parametros: um no, a lista de operadores,
   o nome do algoritmo de procura e a profundidade maxima para expansao da arvore.
   Retorna a lista de sucessores do no recebido como parametro. Se o algoritmo for a profundidade e
   o no recebido tiver uma profundidade igual a profundidade maxima nao e gerado nenhum sucessor.
   A função sucessores retorna somente os sucessores que não existem na lista de abertos.
"
  ;(load "/Users/luisserrano/Dropbox/Universidade/3º Ano/IA/projeto/puzzle.lisp")
  ;(load "C:\\Users\\David\\Desktop\\School\\2015-2016\\1º Semestre\\IA\\Projeto\\1Fase\\puzzle.lisp") ;faz load ao puzzle.lisp para que se possam verificar as coordenadas vazias
  (let ((lista-sucessores NIL) ;variavel onde serão guardados todos os sucessores
        (lista-xyz (incrementa-z));variavel onde serão guardadas todas as coordenadas do tabuleiro, começando pelo nivel mais acima
        )
    (cond ((and (eq algoritmo 'dfs) (eq (no-profundidade no) prof-max)) no) ;verifica se a profundidade do nó recebido equivale à profundidade max
          ((eq algoritmo 'a-asterisco) (gera-sucessores no operadores prof-max abertos lista-xyz lista-sucessores f-heuristica))
          (t (gera-sucessores no operadores prof-max abertos lista-xyz lista-sucessores))
          )
    )
)
 
(defun gera-sucessores (no operadores prof-max abertos lista-xyz lista-sucessores &optional(heuristica 1))
  "Função que irá gerar os sucessores usando a lista de coordenadas"
  (cond ((null (car lista-xyz)) lista-sucessores)
        ((eq (verifica-casa-vazia (first (car lista-xyz)) (second (car lista-xyz)) (third (car lista-xyz)) (no-estado no)) NIL) 
         (gera-sucessores no operadores prof-max abertos (cdr lista-xyz) lista-sucessores heuristica)) 
         ;se a casa xyz tiver ocupada, chama recursivamente a função gera-sucessores

        (t (gera-sucessores no operadores prof-max abertos (cdr lista-xyz) 
                            (append (list (sucessores-aux no (first operadores) prof-max abertos (first (car lista-xyz)) (second (car lista-xyz)) (third (car lista-xyz)) heuristica)
                                          (sucessores-aux no (second operadores) prof-max abertos (first (car lista-xyz)) (second (car lista-xyz)) (third (car lista-xyz)) heuristica))
                                  lista-sucessores) heuristica))
        ;caso contrário chamar recursivamente gera-sucessores, mas adiciona a lista de sucessores os nos gerados para essas coordenadad
        )
)

;;Funções de geração de coordenadas do tabuleiro
;incrementa-z
(defun incrementa-z (&optional (x 1) (y 1) (z 3) (lista NIL))
  "Função recursiva que incrementa a coordenada z, para que depois possa ser adiciona à lista de coordenadas xy, atraves da função add-z"
  (cond ((< z 1) lista)        
        (t (incrementa-z x y (1- z) (append (add-z z (incrementa-x x y)) lista))) ;append ou cons, com cons divide por tabuleiros
        ) 
)

;add-z
(defun add-z (z lista-xy &optional(lista-xyz NIL))
  "Função que adiciona a coordenada z a cada sub lista xy"
  (cond ((null (car lista-xy)) lista-xyz)
        (t (add-z z (cdr lista-xy) (cons (append (car lista-xy) (list z)) lista-xyz))) ;é necessário fazer (list z) senão tornar-se-a numa dotted list, devido ao cons
        )
)

;incrementa-x
(defun incrementa-x (x y &optional(lista NIL))
  "Função que gera uma lista de coordenadas xy"
  (cond ((> x 2) (append (list (list x (third (incrementa-y y))) (list x (second (incrementa-y y))) (list x (first (incrementa-y y)))) lista))
        (t (incrementa-x  (1+ x) y (append (list (list x (third (incrementa-y y))) (list x (second (incrementa-y y))) (list x (first (incrementa-y y)))) lista)))
        )
)

;incrementa-y
(defun incrementa-y (y &optional(lista NIL))
  "Função que gera uma lista de coordenadas y"
  (cond ((> y 2) (cons y lista))
        (t (incrementa-y (1+ y) (cons y lista))) 
  )
)
  
;; sucessores-aux
(defun sucessores-aux (no operador prof-max abertos x y z &optional(f-heuristica 1))
  "Permite gerar um sucessor de um no. A func recebe um no, o operador a aplicar e a profundidade-maxima
   da arvore de procura. A func retorna um no sucessor na qual o estado foi actualizado pela aplicacao do
   operador, a profundidade foi incrementada um valor e o no pai actualizado com a insercao de no recebido
   como parametro."
  (let* ((estado-1 (funcall operador x y z (no-estado no))) ;aplica o 1º operador ao estado do no recebido
         (no-gerado-1 (cria-no estado-1 (1+ (no-profundidade no)) (heuristica estado-1 f-heuristica) no))) ;cria o no já com o novo estado e calcula a heuristica
    (cond ((equal (existep no-gerado-1 abertos) nil) no-gerado-1)
          (t () NIL)
          )
    )
)
     
;;; Algoritmo Breadth-First
(defun bfs (abertos sucessores)
  "Função que aplica o algoritmo breadth-first(em largura),
   onde os  sucessores sao colocados no fim da lista de abertos."  
  (append abertos sucessores))

;;; Algoritmo Depth-First
(defun dfs (abertos sucessores)
  "Função que aplica o algoritmo depth-first(em profundidade), 
   onde os sucessores sao colocados no inicio da lista de abertos."  
  (append sucessores abertos))

;;; Algoritmo A*
(defun a-asterisco (abertos sucessores)
  "Função que aplica o algoritmo A*(procura ótima),
   onde a lista de sucessores e abertos é ordenada conforme o valor heuristico de cada nó."
  (ordenar-nos (append abertos sucessores))
)

;; ordena-nos
(defun ordenar-nos (lista-nos)
"Ordena os nos em ordem crescente de acordo com o custo"
  (sort lista-nos  #'< :key #'third) ;Vai ordenar a lista segundo o terceiro elemento de cada sub lista, ou seja, pelo valor heurisitco
)

;;; f-solucao
(defun solucao-p (no)
  "Função que verifica se um nó é solução"
  (cond ((= (calcula-simbolos-alinhados (no-estado no)) 3) T) ;caso estejam 3 simbolos alinhados é solução e retorna true
        (t () NIL) ;caso contrário retorna NIL
        )
)

;;; sucessor-solucao-p
(defun sucessor-solucao-p (sucessores f-solucao)
  (cond 
     ((null sucessores) nil) ; função de paragem, caso não tenha encontrado a solução na lista de sucessores
     ((funcall f-solucao (car sucessores)) (car sucessores)) ;caso o primeiro na lista de sucessores seja solução, retorna true
     (T (sucessor-solucao-p (cdr sucessores) f-solucao))) ;condição recursiva para percorrer lista de sucessores
)  

;;; Simbolos Alinhados
(defun calcula-simbolos-alinhados (estado &optional(heuristica 0))
  "Função que retorna o valor máximo de simbolos alinhos, em linha, coluna, ou diagonal"
  (get-max (list 
            (get-max (verifica-estado estado heuristica)) ;valor maximo alinhado em todas as linhas
            (get-max (verifica-linhas (inverte-linhas-para-linhas3D estado) heuristica)) ;valor maximo alinhado em todas as linhas 3D, como todas as linhas3D estão numa só lista, chama-se o verifica-linhas
            (get-max (verifica-linhas (inverte-linhas-para-colunas-diagonais estado) heuristica)) ;valor maximo alinhado em todas as colunas diagonais
            (get-max (verifica-linhas (inverte-linhas-para-colunas estado) heuristica)) ;todas as possibilidades de colunas num só tabuleiro. estão só numa lista com sublista, portanto chama-se o verifica-linhas 
            (get-max (verifica-linhas (inverte-linhas-para-colunas3D estado) heuristica)) ;valor maximo alinhado em colunas ao longo dos 3 tabuleiros(3D)
            (get-max (verifica-linhas (inverte-linhas-para-diagonais estado) heuristica)) ;valor maximo alinhado em diagonal por tabuleiro
            (get-max (verifica-linhas (inverte-linhas-para-diagonais3D estado) heuristica)) ;valor maximo de simbolos alinhados em diagonal ao longo dos tabuleiros
            )
           )
)

;;; Heuristica
(defun heuristica (estado f-heuristica)
  "Vai chamar a função que verifica todas as colunas, linhas e diagonais, para saber quantos simbolos   estao alinhados, de modo a determinar o valor heuristico, atraves da função heuristica 3-p(x)"
  (- 3 (calcula-simbolos-alinhados estado f-heuristica))
)

(defun verifica-estado (estado f-heuristica &optional(lista-valores-no nil))
  "Função que percorre o estado, e chama as funções para verificar as linhas, colunas e diagonais para cada tabuleiro"
  (cond ((null (car estado)) lista-valores-no)
         (t (verifica-estado (cdr estado) f-heuristica (append (verifica-linhas (car estado) f-heuristica) lista-valores-no)))
  )
)

;; Inverter linhas para linhas ao longo dos tabuleiros
(defun inverte-linhas-para-linhas3D (estado &optional(lista-linhas3D NIL) (lista-operadores-esq '(first second third)) (lista-operadores-dir '(third second first)))
  "Função que inverte o estado atual que esta por linhas, e cada sub lista passa a representar uma linha nos 3 tabuleiros(3D)"
  (let ((linha-1 (first lista-linhas3D))     
        (linha-2 (second lista-linhas3D))
        (linha-3 (third lista-linhas3D))
        (linha-4 (fourth lista-linhas3D))
        (linha-5 (fifth lista-linhas3D))
        (linha-6 (sixth lista-linhas3D)))
    (cond ((OR (null estado) (null lista-operadores-esq)) lista-linhas3D)
          (t (inverte-linhas-para-linhas3D (cdr estado) 
                                           (list (append  linha-1 (list (funcall (car lista-operadores-esq) (first (car estado))))) ;primeira linha 3D a partir da esquerda
                                                 (append  linha-2 (list (funcall (car lista-operadores-esq) (second (car estado))))) ;segunda linha 3D a partir da esquerda
                                                 (append  linha-3 (list (funcall (car lista-operadores-esq) (third (car estado)))))  ;terceira linha 3D a partir da esquerda
                                                 (append  linha-4 (list (funcall (car lista-operadores-dir) (first (car estado)))))  ;primeira linha 3D a partir da direita
                                                 (append  linha-5 (list (funcall (car lista-operadores-dir) (second (car estado)))))  ;terceira linha 3D a partir da direita
                                                 (append  linha-6 (list (funcall (car lista-operadores-dir) (third (car estado))))))  ;terceira linha 3D a partir da direita
                                           (cdr lista-operadores-esq)
                                           (cdr lista-operadores-dir)))
          )
    )
)

;; Inverter linhas para diagonais com y constante, onde só varia x e z, ou seja colunas diagonais
(defun inverte-linhas-para-colunas-diagonais (estado)
  "Função que interte o estado atual para colunas diagonais, estas diagonais são iguais as linhas 3D só que em vez de ser ao longo da linha é ao longo de uma coluna"
  (let* ((lista-colunas (inverte-linhas-para-colunas estado))
         (tabuleiro-colunas (list (list (first lista-colunas) (second lista-colunas) (third lista-colunas))
                                   (list (fourth lista-colunas) (fifth lista-colunas) (sixth lista-colunas))
                                   (list (seventh lista-colunas) (eighth lista-colunas) (ninth lista-colunas))))
         )
    (inverte-linhas-para-linhas3D tabuleiro-colunas) ;devolve lista com diagonais por coluna, usando a mesma função que devolve as linhas 3D
    )
)

;; Inverter linhas para colunas no mesmo tabuleiro
(defun inverte-linhas-para-colunas (estado)
  "Função que inverte o estado atual que esta por linhas, e cada sub lista passa a representar uma coluna"
  (cond ((null estado) estado)
        (t () (append (inverte-linhas-colunas-aux (car estado)) (inverte-linhas-para-colunas (cdr estado))))
  )
)

(defun inverte-linhas-colunas-aux (tabuleiro)
  (list (list (first (car tabuleiro)) (car (car (cdr tabuleiro))) (car (car (cdr (cdr tabuleiro)))))
        (list (second (car tabuleiro)) (second (car (cdr tabuleiro))) (second (car (cdr (cdr tabuleiro)))))
        (list (third (car tabuleiro)) (third (car (cdr tabuleiro))) (third (car (cdr (cdr tabuleiro)))))
  )
)

;; Inverter linhas para colunas ao longo dos tabuleiros
(defun inverte-linhas-para-colunas3D (estado &optional(list-invertida NIL))
   "Função que inverte o estado atual que esta por linhas, e cada sub lista passa a representar uma coluna nos 3 tabuleiros(3D)"
   (let ((nivel-1 (append (first (first estado)) (second (first estado)) (third (first estado))))
         (nivel-2 (append (first (second estado)) (second (second estado)) (third (second estado))))
         (nivel-3 (append (first (third estado)) (second (third estado)) (third (third estado))))
         )
     (append (inverte-linhas-para-colunas3D-aux nivel-1 nivel-2 nivel-3) list-invertida)
     )
)

(defun inverte-linhas-para-colunas3D-aux (nivel-1 nivel-2 nivel-3 &optional(lista-invertida NIL))
  (cond ((null nivel-1) lista-invertida)
        (t (inverte-linhas-para-colunas3D-aux (cdr nivel-1) (cdr nivel-2) (cdr nivel-3) (cons (list (car nivel-1) (car nivel-2) (car nivel-3)) lista-invertida)))
        )
)

;; Inverter linhas para diagonais no mesmo tabuleiro
(defun inverte-linhas-para-diagonais (estado &optional(lista-diagonais NIL))
  "Função que inverte o estado atual que esta por linhas, e cada sub lista passa a representar uma diagonal"
  (cond ((null estado) lista-diagonais) 
        (t (inverte-linhas-para-diagonais (cdr estado) (append (inverte-linhas-para-diagonais-aux (car estado)) lista-diagonais)))
        )
)

(defun inverte-linhas-para-diagonais-aux (tabuleiro)
  (list (list (first (first tabuleiro)) (second (second tabuleiro)) (third (third tabuleiro)))
          (list (third (first tabuleiro)) (second (second tabuleiro)) (first (third tabuleiro)))
          )
)

;; Inverter linhas para diagonais ao longo dos tabuleiros
(defun inverte-linhas-para-diagonais3D (estado)
  "Função que inverte o estado atual que esta por linhas, e cada sublista passa a representar uma diagonal nos 3 tabuleiros(3D)"
  (list (list (caaar estado) (second (second (second estado))) (third (third (third estado))))
        (list (third (caar estado)) (second (second (second estado))) (first (third (third estado))))
        (list (first (third (first estado))) (second (second (second estado))) (third (first (third estado))))
        (list (third (third (first estado))) (second (second (second estado))) (caar (third estado)))
        );cadr = second | caddr = third | caar = (first (first | caaar = (first (first (first |
)

;; Valor maximo de uma lista 
(defun get-max (lista-valores)
  "Função auxiliar que vai buscar o maior valor númerico de uma lista"
  (reduce #'max lista-valores))

;; Verificação de numeros de simbolos alinhados 
(defun verifica-linhas (tabuleiro f-heuristica &optional(lista-valores-tabuleiro nil))
  "Função que verifica a quantidade X's e 0's em todas as linhas de um tabuleiro "
  (cond ((null (car tabuleiro)) lista-valores-tabuleiro)
        ((AND (> (first (verifica-linha (car tabuleiro)))0) 
              (> (second (verifica-linha (car tabuleiro)))0) 
              (equal f-heuristica '2))
                    (verifica-linhas (cdr tabuleiro) f-heuristica lista-valores-tabuleiro)) ;caso seja a segunda heuristica e haja dois simbolos diferentes na mesma linha, não conta essa linha 
        (t (verifica-linhas (cdr tabuleiro) f-heuristica (append (verifica-linha (car tabuleiro)) lista-valores-tabuleiro)))
  )
)

(defun verifica-linha (linha &optional(nr-x 0) (nr-0 0) (lista nil) )
  "Função que verifica a quantidade X's e 0's em cada linha de um tabuleiro"
  (cond ((null linha) lista)
        ((equal (verifica-simbolo (car linha)) 'X) (verifica-linha (cdr linha) (1+ nr-x) nr-0 (list (1+ nr-x) nr-0)))
        ((equal (verifica-simbolo (car linha)) '0) (verifica-linha (cdr linha) nr-x (1+ nr-0) (list nr-x (1+ nr-0))))
        (t () (verifica-linha (cdr linha) nr-x nr-0 (list nr-x nr-0))))
)

(defun verifica-simbolo (elem)
  "Função que devolve o proprio valor de um elemento se for X ou 0, caso contrário retorna NIL"
  (cond ((equal elem 'X) 'X)
        ((equal elem '0) '0))
)

;;; Funções de analise da procura
(defun penetrancia (no)
  "Função que recebe o no-solucao e calcula a penetrancia"
  (cond ((NOT (= *gerados* 0))(/ (1+ (no-profundidade no)) *gerados*))
        )
)

(defun f-polinomial (L valor-t B)
  "funcao polinomial para calcular o valor de um dado B elevado a um valor L ate este ser 1"
  ; L = profundidade do no-solucao
  (cond ((= L 1) (- B valor-t)) ;condição de paragem
        (t (+ (expt B L) (f-polinomial (- L 1) valor-t B)))
  )
)

(defun fator-ramificacao (L valor-t valor-erro &optional (bmin 1) (bmax 10e11))
  "funcao para descobrir o fator de ramificacao do no solucao"
  (let ((bmedio (/ (+ bmin bmax) 2))) ; acha-se a metade da soma do bmin e do bmax
    (cond ((< (- bmax bmin) valor-erro) (/ (+ bmax bmin) 2)) ; se a diferenca entre o bmax e o bmin for menor que o erro estabelecido entao acha-se a metade da soma do bmin e do bmax outra vez
          ((< (f-polinomial L valor-t bmedio) 0) (fator-ramificacao L valor-t valor-erro bmedio bmax)) ; se a soma dos polinomios for menor que 0 chama-se outra vez a func com o bmin a ser o bmedio achado
          (t (fator-ramificacao L valor-t valor-erro bmin bmedio)) ; senao chama-se a func mas com o bmax a ser o bmedio
    )
  )
)
