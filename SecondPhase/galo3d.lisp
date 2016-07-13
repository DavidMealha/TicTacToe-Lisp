;;;; projeto.lisp
;;;; Disciplina de IA - 2015 / 2016
;;;; codigo relacionado com o problema
;; Luis Serrano e David Mealha


;;Funções para colocar uma cruz ou um circulo no tabuleiro
(defun colocar-cruz (x y z estado)
  (substituir-z x y z estado 'X))

(defun colocar-circulo (x y z estado)
  (substituir-z x y z estado '0))


;;Funções auxiliares que percorrem os tabuleiros de modo a jogar na casa pretendida
(defun substituir-y (y valor lista)
"Função que substitui na coordenada y, ou seja na casa onde se pretende jogar, pelo valor que foi escolhida, X ou 0"
  (let ((elem1 (first lista))
        (elem2 (second lista))
        (elem3 (third lista)))
    (cond ((and (= y 1) (null elem1)) (list valor elem2 elem3))
          ((and (= y 2) (null elem2)) (list elem1 valor elem3))
          ((and (= y 3) (null elem3)) (list elem1 elem2 valor)))))

(defun substituir-x (x y lista valor)
"Função que vai buscar a linha escolhida através da coordena x, em seguida vai à coluna selecionada, com
a coordenada y, e por fim irá alterar o valor da casa"
  (let ((elem1 (first lista))
        (elem2 (second lista))
        (elem3 (third lista)))
    (cond ((and (= x 1) (list (substituir-y y valor elem1) elem2 elem3)))
          ((and (= x 2) (list elem1 (substituir-y y valor elem2) elem3)))
          ((and (= x 3) (list elem1 elem2 (substituir-y y valor elem3)))))))

(defun substituir-z (x y z estado valor)
"Função que vai buscar um dos tres tabuleiros de acordo com a coordenada z, após dado o tabuleiro, avança 
para a linha escolhida, ou seja a coordenada x"
  (let  ((elem1 (first estado))
        (elem2 (second estado))
        (elem3 (third estado)))
    (cond ((and (= z 3) (list (substituir-x x y elem1 valor) elem2 elem3)))
          ((and (= z 2) (list elem1 (substituir-x x y elem2 valor) elem3)))
          ((and (= z 1) (list elem1 elem2 (substituir-x x y elem3 valor)))))))


;;Funções Auxiliares
(defun verifica-coordenada (coordenada)
"Função para verificar se uma coordenada tem o valor possivel, ou seja entre 1 e 3"
  (cond ((and (> (coordenada 0)) (< (coordenada 4))) T)
        (t () NIL)))

(defun verifica-casa-vazia (x y z estado)
"Função que verifica se uma casa estava vazia (se ainda é jogável), nas coordenadas recebidas.
Retorna T caso a casa esteja vazia, ou NIL caso contrário."
  (cond ((null (get-y y (get-x x (get-z z estado)))) T)
        (t () NIL)))

(defun get-casa (x y z estado)
  "Função que retorna o valor numa casa do tabuleiro, num conjunto de coordenadas especifico"
  (get-y y (get-x x (get-z z estado)))
)       

(defun get-z (z estado)
"Função que vai buscar o tabuleiro de acordo com a coordenada z recebida"
  (cond ((= z 3) (car estado))
          (t (get-z (1+ z) (cdr estado)))))

(defun get-x (x lista)
"Função que vai buscar a linha do tabuleiro de acordo com a coordenada x recebida"
  (cond ((= x 1) (car lista))
          (t (get-x (1- x)(cdr lista)))))

(defun get-y (y lista)
"Função que vai buscar a coluna do tabuleiro de acordo com a coordenada y recebida"
  (cond ((= y 1) (car lista))
          (t (get-y (1- y)(cdr lista)))))
