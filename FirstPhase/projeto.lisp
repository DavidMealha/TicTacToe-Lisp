;;;; projeto.lisp
;;;; Disciplina de IA - 2015 / 2016
;;;; Este ficheiro carrega os outros ficheiros de codigo, escreve e le de ficheiros e trata da interacao com o utilizador.
;; Luis Serrano e David Mealha

;;--- TODO: interacao com o utilizador (menu-principal, onde colocar a cruz, etc)

;; Menu-principal
(defun menu-principal ()
  "Apresenta o menu principal com as opcoes do jogo. 
   Carrega os ficheiros restantes do programa(puzzle.lisp e procura.lisp)."
  (carregar-ficheiros)
  (loop
    (progn
      (format t "~% ------------------------------------------------------")
      (format t "~%|                  Jogo Do Galo 3D                     |")
      (format t "~%|                                                      |")
      (format t "~%|            1-Iniciar Procura                         |")
      (format t "~%|            2-Mostrar Tabuleiro                       |")      
      (format t "~%|            3-Sair                                    |")
      (format t "~%|                                                      |")
      (format t "~% ------------------------------------------------------")
      (format t "~%~%Escolha:")
      )
    (cond ((not (let ((escolha (read)))
               (cond 
                ((and (< escolha 4) (> escolha 0)) (case escolha
                                                    (1 (progn (menu-iniciar-procura)))
                                                    (2 (progn (imprimir-tabuleiro-3D (le-tabuleiro (tabuleiro))) nil))
                                                    (3 (progn (format t "PROGRAMA TERMINADO") nil)))
                )
                ( T (progn  (format t "~%ESCOLHA INVALIDA~%~%Escolha: ")
                            (setf escolha (read))
                            )))
               
               )) (return)))
    )
)

;; Menu-Iniciar-Procura
(defun menu-iniciar-procura ()
  "Apresenta o menu onde s�o feitas as op��es para a procura, como algoritom, profundidade max, etc.."
  (let* ((no-inicial(progn (format t "No inicial/Problema (a, b, c, d, e, f):~%") (read)))
         (algoritmo (progn (format t "Algoritmo(bfs, dfs, a-asterisco):~%") (read)))
         (prof-max (progn (format t "Profundidade M�xima:~%") (read)))
         (heuristica (progn (format t "Heuristicas(1,2): ~%") (read)))
         (no-solucao (iniciar-procura no-inicial algoritmo prof-max heuristica))
         (tempo (time (mostrar-resultado (iniciar-procura no-inicial algoritmo prof-max heuristica))))
         )
    (format t "~a ~%" tempo)
    (escrever-solucao no-solucao) 
    )
)


(defun iniciar-procura (no-inicial algoritmo prof-max heuristica)
  "Fun��o que inicia a procura de acordo com o no, algoritmo e profundidade, cujo s�o parametros inseridos pelo utilizador"
  (cond ((equal (procura-generica (funcall no-inicial) prof-max 'solucao-p 'sucessores algoritmo '(colocar-cruz colocar-circulo) heuristica) NIL) 
           (format t "N�o foi poss�vel encontrar solu��o"))
          (t (procura-generica (funcall no-inicial) prof-max 'solucao-p 'sucessores algoritmo '(colocar-cruz colocar-circulo) heuristica)) ; (funcall no-inicial) ou *no-teste*
          ;caso contr�rio imprimir o caminho at� � solu��o, ou seja percorrer at� chegar ao no-pai = nil
          )
)

;; Impress�o do jogo no ecr�
(defun mostrar-resultado (no-solucao)
  "Fun��o que mostra a informa��o da procura que foi feita"
  (progn (percorrer-nos no-solucao) (stats no-solucao))
)
(defun percorrer-nos (no-solucao)
  "Fun��o recursiva para percorrer desde o n� objetivo at� ao n� inicial"
  (let ((no-pai (no-pai no-solucao))
        )
    (cond ((null no-pai) (imprimir-no no-solucao))
          (t (percorrer-nos no-pai)(imprimir-no no-solucao))
          )
    )
)

(defun stats (no)
  "Fun��o que imprime as estatisticas da procura"
  (format t "N�s Gerados: ~a ~%" *gerados*)
  (format t "N�s Expandidos: ~a ~%" *expandidos*)
  (format t "Penetrancia: ~a ~%" (penetrancia no))
  (format t "Factor de Ramifica��o: ~a ~%" (fator-ramificacao (1+ (no-profundidade no)) *expandidos* 0.1))
  (format t "--------------------------------- ~%")
)

(defun imprimir-no (no)
  "Fun��o que imprime os valores de um n�"
  (progn 
    (format t "--------------------------------- ~%") 
    (format t "Estado: ~%") 
    (imprimir-tabuleiro-3D (no-estado no))
    (format t "Profundidade: ~a ~%" (no-profundidade no))
    (format t "Valor heuristico: ~a ~%" (no-valor-heuristico no))
    (format t "--------------------------------- ~%")
    )
)

(defun imprimir-tabuleiro-3D (board)
  "Fun��o que imprime no ecr� o tabuleiro de jogo sem ser em formato de lista.
   Representado em forma de tabelas"
  (let ((tabuleiro-1 (first board))
        (tabuleiro-2 (second board))
        (tabuleiro-3 (third board)))  
    (imprimir-tabuleiro tabuleiro-1 1)
    (imprimir-tabuleiro tabuleiro-2 2)
    (imprimir-tabuleiro tabuleiro-3 3)
    )
)

(defun imprimir-tabuleiro (tabuleiro nr)
  "Fun��o que permite imprimir um tabuleiro de cada vez"
  (format t "~%")
  (format t "TABULEIRO ~a: ~%~%" nr)
  (imprimir-linha
    (first (first tabuleiro)) (second (first tabuleiro)) (third (first tabuleiro)))
  (format t "~& --------------------------")
  (imprimir-linha
    (first (second tabuleiro)) (second (second tabuleiro)) (third (second tabuleiro)))
  (format t "~& --------------------------")
  (imprimir-linha
    (first (third tabuleiro)) (second (third tabuleiro)) (third (third tabuleiro)))
  (format t "~%~%"))

(defun imprimir-linha (first second third)
  "Fun��o que imprime cada linha de um tabuleiro"
  (format t "~&   ~6A|  ~6A|  ~6A ~%" first second third)
)

;;;--- TODO: escrever e ler ficheiros
;;Escrita de solu��o e estatisticas em ficheiro

; Estatisticas
(defun escrever-stats (no)
  "Fun��o que escreve as estatisticas num ficheiro"
  (let ((ficheiro (stats-dir)))
    (with-open-file (f ficheiro :direction :output :if-exists :overwrite :if-does-not-exist :create) 
      (format f " Gerados: ~a | Expandidos: ~a | Profundidade N� Solu��o: ~a | Fator de Ramifica��o: ~a | Penetrancia: ~a"  *gerados* *expandidos* (no-profundidade no) 0 (penetrancia no)))
  )
)

(defun stats-dir ()
  "Fun��o que define o caminho at� ao ficheiro onde se encontra o tabuleiro"
  (concatenate 'string (diretoria-atual) "stats.dat"))

; Caminho da solu��o
(defun escrever-solucao (no)
  (let ((ficheiro (solucao-dir)))
    (with-open-file (f ficheiro :direction :output :if-exists :overwrite :if-does-not-exist :create) (format f "~a" (lista-estados no))))
)

(defun solucao-dir ()
  "Fun��o que define o caminho at� ao ficheiro onde se encontra o tabuleiro"
  (concatenate 'string (diretoria-atual) "estadosSolucao.dat"))

(defun lista-estados (no &optional(lista NIL))
  "Fun��o que percorre recursivamente todos os n�s do caminho da solu��o e insere o estado de cada n�s numa lista"
  (cond ((null  no) lista)
        (t (lista-estados (no-pai no) (append (list (no-estado no)) lista)))
        )
)

;; Leitura do tabuleiro de jogo
(defun le-tabuleiro (ficheiro)
  "Fun��o que l� o tabuleiro de um ficheiro .dat"
  (with-open-file (f ficheiro :direction :input :if-does-not-exist :error) (read f)))

(defun tabuleiro ()
  "Fun��o que define o caminho at� ao ficheiro onde se encontra o tabuleiro"
  (concatenate 'string (diretoria-atual) "tabuleiro.dat"))

;; Leitura dos problemas do projecto
(defun problemas ()
  "Fun��o que retorna a lista de problemas fornecidos no enunciado do projeto"
  (concatenate 'string (diretoria-atual) "problemas.dat"))

(defun problema-a ()
"Devolver problema a)"
  (cria-no (first (le-tabuleiro (problemas))) 0 1 NIL)
)

(defun problema-b ()
"Devolve problema b)"
  (cria-no (second (le-tabuleiro (problemas))) 0 1 NIL)
)

(defun problema-c ()
"Devolve problema c)"
  (cria-no (third (le-tabuleiro (problemas))) 0 1 NIL)
)

(defun problema-d ()
"Devolve problema d)"
  (cria-no (fourth (le-tabuleiro (problemas))) 0 1 NIL)
)

(defun problema-e ()
"Devolver problema e)"
  (cria-no (fifth (le-tabuleiro (problemas))) 0 2 NIL)
)

(defun problema-f ()
"Devolver problema f)"
  (cria-no (sixth (le-tabuleiro (problemas))) 0 1 NIL)
)

;; Escrita do tabuleiro de jogo em ficheiro
(defun escrever-tabuleiro (tabuleiro ficheiro)
  "Fun��o que guarda tabuleiro no ficheiro .dat"
  (with-open-file (f ficheiro :direction :output :if-exists :overwrite :if-does-not-exist :create) (format f "~a" tabuleiro)) (list tabuleiro))

;;--- TODO: carregar os outros ficheiros, puzzle e procura
(defun carregar-ficheiros ()
  "Fun��o que carrega os ficheiros puzzle.lisp e procura.lisp"
  (compile-file (concatenate 'string (diretoria-atual) "puzzle.lisp") :load T)
  (compile-file (concatenate 'string (diretoria-atual) "procura.lisp") :load T)
  ;(load (concatenate 'string (diretoria-atual) "puzzle.lisp"))
  ;(load (concatenate 'string (diretoria-atual) "procura.lisp"))
)

(defun diretoria-atual ()
  "Fun��o que contem o caminho at� aos ficheiros do projeto"
  ;Luis
  ;(let ((caminho "/Users/luisserrano/Dropbox/Universidade/3� Ano/IA/projeto/")) caminho))
  ;David
  (let ((caminho "C:\\Users\\David\\Desktop\\School\\2015-2016\\1� Semestre\\IA\\Projeto\\1Fase\\")) caminho))

;;; Funcao para ver se um valor recebido faz parte do tabuleiro
;; validar-movimento
(defun validar-movimento (x y z tabuleiro)
	(cond
		((not (and (integerp x)
				   (<= 1 x 3))) ; se a pos x nao estiver entre 1 e 3 ou se nao for um inteiro
		(format t "~%Valor invalido.")) ; manda msg de erro
		((not (and (integerp y)
				   (<= 1 y 3))) ; se a pos y nao estiver entre 1 e 3 ou se nao for um inteiro
		(format t "~%Valor invalido.")) ; manda msg de erro
		((not (and (integerp z)
				   (<= 1 z 3))) ; se a pos z nao estiver entre 1 e 3 ou se nao for um inteiro
		(format t "~%Valor invalido.")) ; manda msg de erro
		;( (not (numberp (nth x tabuleiro) ) ) ) fazer funcao recursiva para percorrer lista

	)


)

