;;;; jogo.lisp
;;;; Disciplina de IA - 2015 / 2016
;;;; Este ficheiro carrega os outros ficheiros de codigo, escreve e le de ficheiros e trata da interacao com o utilizador.
;; Luis Serrano e David Mealha

;;--- TODO: interacao com o utilizador (menu-principal, onde colocar a cruz, etc)

;; Menu-principal
(defun menu-principal ()
  "Apresenta o menu principal com as opcoes do jogo. 
   Carrega os ficheiros restantes do programa(galo3d.lisp e alfabeta.lisp)."
  (carregar-ficheiros)
  (loop
    (progn
      (format t "~% ------------------------------------------------------")
      (format t "~%|                  Jogo Do Galo 3D                     |")
      (format t "~%|                                                      |")
      (format t "~%|            1-Jogar Humano-PC                         |")
      (format t "~%|            2-Jogar PC-PC                             |")      
      (format t "~%|            3-Sair                                    |")
      (format t "~%|                                                      |")
      (format t "~% ------------------------------------------------------")
      (format t "~%~%Escolha:")
      )
    (cond ((not (let ((escolha (read)))
               (cond 
                ((and (< escolha 4) (> escolha 0)) (case escolha
                                                    (1 (progn (menu-jogar-humano)))
                                                    (2 (progn ()))
                                                    (3 (progn (format t "PROGRAMA TERMINADO") nil)))
                )
                ( T (progn  (format t "~%ESCOLHA INVALIDA~%~%Escolha: ")
                            (setf escolha (read))
                            )))
               
               )) (return)))
     )
)

;; Menu-Iniciar-Procura
(defun menu-jogar-humano ()
  "Apresenta as perguntas onde são feitas as opções para o jogador humano"
  (let* ((peca-humano (progn (format t "Que peça pretende utilizar? (X/0)~%") (read)))
         (primeiro-jogador (progn (format t "Deseja ser o primeiro a jogar? (s/n) ~%") (read)))
         (tempo-max (progn (format t "Tempo máximo para o computador encontrar uma solução(em segundos): ~%") (read)))
         )
    (cond ((AND (OR (equal primeiro-jogador 's) (equal primeiro-jogador 'n)) (OR (equal peca-humano 'X) (equal peca-humano '0))) (iniciar-jogo peca-humano primeiro-jogador tempo-max)) ;caso os valores estejam bem começa a jogar
          (t (menu-jogar-humano))) ; c.c.
    )
)

;; Iniciar jogo
(defun iniciar-jogo (peca-humano primeiro-jogador tempo-max)
  "Função para realizar a primeira jogada do jogo"
  (let* ((tabuleiro-inicial '(((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))))
        )
    (cond ((equal primeiro-jogador 's) (humano-joga tabuleiro-inicial peca-humano tempo-max)) ;cria o no inicial, 
          (t (computador-joga tabuleiro-inicial (troca-peca peca-humano tempo-max)))
          )
    )
)

;; Escolher-posicao-tabuleiro(FUNCAO ADICIONADA PARA 2 FASE)
(defun escolher-posicao-tabuleiro (tabuleiro peca)
  "Função para escolher a posição onde efetuar, interação com o jogador humano"
  (let* ((coor-z (progn (imprimir-tabuleiro-3D tabuleiro) (format t "~%Escolha a posição onde deseja efetuar a sua jogada: ~% ~%")  (format t "Posição Z (Entre 1 e 3, onde 3 é tabuleiro mais acima): ~%") (read)))
         (coor-x (progn (format t "Posição X (Entre 1 e 3, onde 1 é a linha mais acima)~%" ) (read)))
         (coor-y (progn (format t "Posição Y (Entre 1 e 3, onde 1 é a coluna mais a esquerda)~%") (read)))         )
    (cond ((NOT (AND (<= 1 coor-z 3) (<= 1 coor-x 3) (<= 1 coor-y 3))) (escolher-posicao-tabuleiro tabuleiro peca))
          ((NOT (verifica-casa-vazia coor-x coor-y coor-z tabuleiro)) (progn (format t "Essa posição já se encontra preenchida!~%") (escolher-posicao-tabuleiro tabuleiro peca)))
          ((equal peca 'X) (realizar-jogada tabuleiro 'colocar-cruz coor-x coor-y coor-z))
          (t (realizar-jogada tabuleiro 'colocar-circulo coor-x coor-y coor-z))
          )
    )
)

;;;;
;;;; funções de jogo (humano e computador c/alfabeta) (FUNCAO ADICIONADA PARA 2 FASE)
;;;;
(defun humano-joga (tabuleiro peca tempo-max)
  "Retorna a jogada do humano sob a forma de um novo tabuleiro.
  Verifica se existe um vencedor ou um empate. Se nao for o caso, pede a jogada do PC"
  (let* ((novo-tabuleiro (escolher-posicao-tabuleiro tabuleiro peca))
         )
    (cond ((solucao-p (cria-no novo-tabuleiro) peca) (progn (imprimir-tabuleiro-3D novo-tabuleiro) (format t "~&Ganhou!") (menu-jogar-de-novo))) ;Verifica se o humano ganhou
          ((verifica-tabuleiro-preenchido-p novo-tabuleiro) (progn (imprimir-tabuleiro-3D novo-tabuleiro) (format t "~&Empatamos.") (menu-jogar-de-novo))) ;Verifica se o tabuleiro esta cheio, se true empataram
          (t (imprimir-tabuleiro-3D novo-tabuleiro) (computador-joga novo-tabuleiro (troca-peca peca) tempo-max)) ;c.c. é a vez do PC jogar
          )
    )
) 

(defun computador-joga (tabuleiro peca-pc tempo-max)
  "Retorna a jogada do PC sob a forma de um novo tabuleiro.
  Verifica se existe um vencedor ou um empate. Se nao for o caso, pede a jogada do humano"
  (let* ((tempo-inicial (get-universal-time)) ;vai buscar o tempo atual
         (valor-alfabeta (alfa-beta (cria-no tabuleiro 0 0 nil peca-pc) 1 -10e11 10e11 tempo-inicial tempo-max))
         (info-jogada (escrever-log valor-alfabeta))
         (novo-tabuleiro (no-estado *jogada*))
         )
    (cond ((solucao-p (cria-no novo-tabuleiro) peca-pc) (progn (imprimir-tabuleiro-3D novo-tabuleiro) (format t "~&Ganhei!") (menu-jogar-de-novo)))
          ((verifica-tabuleiro-preenchido-p novo-tabuleiro)(progn (imprimir-tabuleiro-3D novo-tabuleiro) (format t "~&Empatamos.") (menu-jogar-de-novo)))
          (t (humano-joga novo-tabuleiro (troca-peca peca-pc) tempo-max) )
          )
    )
)

;;;; Função para o campeonato
(defun jogar (estado simbolo tempo)
  "Função que servirá para jogar no campeonato.
   Recebe um tabuleiro, a propria peça e o tempo maximo de execução"
  (let* ((tempo-inicial (get-universal-time))
         (valor-alfabeta (alfa-beta (cria-no estado 0 0 nil simbolo) 1 -10e11 10e11 tempo-inicial tempo-max))
         )
    *jogada*
    )
)

;; jogar-de-novo
(defun menu-jogar-de-novo ()
  "Função chamada após um jogo terminar, para voltar a jogar"
  (let ((op-new-game (progn (format t "~%Quer jogar de novo? (s/n)") (read)))
        )
    (cond ((equal op-new-game 's) (menu-jogar-humano))
          (t nil)
          )
    )
)

;; Realizar-jogada (FUNCAO ADICIONADA PARA 2 FASE)
(defun realizar-jogada (tabuleiro operador x y z)
  "Função onde por fim é realizada a jogada do humano"
  (let ((novo-tabuleiro (funcall operador x y z tabuleiro)) ; Aplica o operador ao tabuleiro
        )
    novo-tabuleiro ;Imprime o tabuleiro no ecra, e devolve esse mesmo tabuleiro, para que se possa continuar a jogar
    )
)


(defun imprimir-tabuleiro-3D (board)
  "Função que imprime no ecrã o tabuleiro de jogo sem ser em formato de lista.
   Representado em forma de tabelas"
  (let ((tabuleiro-1 (first board))
        (tabuleiro-2 (second board))
        (tabuleiro-3 (third board)))  
    (imprimir-tabuleiro tabuleiro-1 1)
    (imprimir-tabuleiro tabuleiro-2 2)
    (imprimir-tabuleiro tabuleiro-3 3)
    (format t "---------------------------------------------")
    )
)

(defun imprimir-tabuleiro (tabuleiro nr)
  "Função que permite imprimir um tabuleiro de cada vez"
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
  "Função que imprime cada linha de um tabuleiro"
  (format t "~&   ~6A|  ~6A|  ~6A ~%" first second third)
)

;;;--- TODO: escrever e ler ficheiros
;;Escrita de solução e estatisticas em ficheiro

; Estatisticas
(defun escrever-log (valor-alfabeta)
  "Função que escreve as estatisticas num ficheiro"
  (let ((ficheiro (log-dir)))
    (with-open-file (f ficheiro :direction :output :if-exists :append :if-does-not-exist :create) 
      (format f "~%Jogada : ~a | Valor da Posição : ~a | Cortes Alfa: ~a | Cortes Beta: ~a | Nos Analisados: ~a | Tempo Gasto: ~a"  *jogada* valor-alfabeta *a-cut-off* *b-cut-off* *nos-analisados* *tempo-gasto*))
    (format t " ~%Valor da Posição : ~a | Cortes Alfa: ~a | Cortes Beta: ~a | Nos Analisados: ~a | Tempo Gasto: ~a ~%"  valor-alfabeta *a-cut-off* *b-cut-off* *nos-analisados* *tempo-gasto*)
    (setf *a-cut-off* 0)
    (setf *b-cut-off* 0)
    (setf *nos-analisados* 0)
    (setf *tempo-gasto* 0)
  )
)

(defun log-dir ()
  "Função que define o caminho até ao ficheiro onde se encontra o tabuleiro"
  (concatenate 'string (diretoria-atual) "log.dat"))


;; Escrita do tabuleiro de jogo em ficheiro
(defun escrever-tabuleiro (tabuleiro ficheiro)
  "Função que guarda tabuleiro no ficheiro .dat"
  (with-open-file (f ficheiro :direction :output :if-exists :overwrite :if-does-not-exist :create) (format f "~a" tabuleiro)) (list tabuleiro))

;;--- TODO: carregar os outros ficheiros, puzzle e procura
(defun carregar-ficheiros ()
  "Função que carrega os ficheiros puzzle.lisp e procura.lisp"
  (compile-file (concatenate 'string (diretoria-atual) "galo3d.lisp") :load T)
  (compile-file (concatenate 'string (diretoria-atual) "alfabeta.lisp") :load T)
)

(defun diretoria-atual ()
  "Função que contem o caminho até aos ficheiros do projeto"
  ;Luis
  ;(let ((caminho "/Users/luisserrano/Dropbox/Universidade/3º Ano/IA/projeto/")) caminho))
  ;David
  (let ((caminho "C:\\Users\\David\\Desktop\\School\\2015-2016\\1º Semestre\\IA\\Projeto\\1Fase\\2Fase\\")) caminho))


