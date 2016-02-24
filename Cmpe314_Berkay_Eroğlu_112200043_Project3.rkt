
#lang plai-typed
;; Grammar:
;; S -> ± number
;; S -> + S S
;; S -> - S S
;; S -> * S S
;; S -> ^ S S
;; S -> S
;; S -> - S
;; Data Definition of msl expression
(define-type msl
  [msl-num (n : number)]
  [msl-add (lhs : msl) (rhs : msl)]
  [msl-sub (lhs : msl) (rhs : msl)]
  [msl-mul (lhs : msl) (rhs : msl)]
  [msl-div (lhs : msl) (rhs : msl)]
  [msl-expt (lhs : msl) (rhs : msl)]
  )
(define (expt x y)
  (cond
    ((= y 0) 1)
    (else
     (* x (expt x (- y 1))))))

;;Data definition of msl
(define (eval [expr : msl])
  (type-case msl expr
    [msl-num (n) n]
    [msl-add (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-sub (lhs rhs) (- (eval lhs) (eval rhs))]
    [msl-mul (lhs rhs) (* (eval lhs) (eval rhs))]
    [msl-div (lhs rhs) (/ (eval lhs) (eval rhs))]
    [msl-expt (lhs rhs) (expt (eval lhs) (eval rhs))]
    ))
(eval (msl-add (msl-num 40) (msl-num 10)))
(eval (msl-expt (msl-num 5) (msl-num 5)))



;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (msl-num 7)
;; '(+ 3 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(+ (+ 3 4) 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))
;;Definition of msl
(define (parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-add (parse (second sl)) (parse (third sl)))]
         [(*) (msl-mul (parse (second sl)) (parse (third sl)))]
         [(-) (msl-sub (parse (second sl)) (parse (third sl)))]
         [(/) (msl-div (parse (second sl)) (parse (third sl)))]
         [(**) (msl-expt (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(test (parse '7) (msl-num 7))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(+ (+ 3 4) 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))


;; Definition of Exponentiation
;; number -> number
;; examples
;; (^ 3 3) -> 27
;; (^ 2 4) -> 16
(define-type mslS
  [numS (n : number)]
  [plusS (l : mslS) (r : mslS)]
  [subS (l : mslS) (r : mslS)]
  [mulS (l : mslS) (r : mslS)]
  [expS (l : mslS) (r : mslS)]
  [uminusS (e : mslS)])

;;Data definition of desugar
(define (desugar [as : mslS]) : msl
  (type-case mslS as
    [numS (n)(msl-num n)] 
    [plusS (lhs rhs) (msl-add (desugar lhs) (desugar rhs))]
    [subS (lhs rhs) (msl-sub (desugar lhs) (desugar rhs))]
    [mulS (lhs rhs) (msl-mul (desugar lhs) (desugar rhs))]
    [expS (lhs rhs) (msl-expt (desugar lhs) (desugar rhs))]
    [uminusS (e) (msl-mul (msl-num -1) (desugar e))]))

(plusS (numS 5) (numS 7))
(desugar (plusS (numS 3) (numS 4)))

(test (desugar (numS 6)) (msl-num 6))
(test (desugar (plusS (numS 5) (numS 5))) (msl-add (msl-num 5) (msl-num 5)))
(test (desugar (subS (numS 7) (numS 7))) (msl-sub (msl-num 7) (msl-num 7)))
(test (desugar (mulS (numS 8) (numS 8))) (msl-mul (msl-num 8) (msl-num 8)))

;;Tests 

"Test1"
"TEST = (+ 2 5) -> 7"
(desugar (plusS (numS 2) (numS 5)))
(test (eval (desugar (plusS (numS 4) (numS 2)))) 6)
(eval (desugar (plusS (numS 4) (numS 2))))
"TEST = (+ 6 2) -> 8"
(desugar (plusS (numS 6) (numS 2)))
(test (eval (desugar (plusS (numS 6) (numS 2)))) 8)
(eval (desugar (plusS (numS 6) (numS 2))))
"TEST = (+ 16 23) -> 38"
(desugar (plusS (numS 16) (numS 23)))
(test (eval (desugar (plusS (numS 16) (numS 23)))) 39)
(eval (desugar (plusS (numS 16) (numS 23))))

;;Outputs of Test1
;"Test1"
;"TEST = (+ 2 5) -> 7"
;(msl-add (msl-num 2) (msl-num 5))
;(good (eval (desugar (plusS (numS 4) (numS 2)))) 6 6 "at line 102")
;6
;"TEST = (+ 6 2) -> 8"
;(msl-add (msl-num 6) (msl-num 2))
;(good (eval (desugar (plusS (numS 6) (numS 2)))) 8 8 "at line 106")
;8
;"TEST = (+ 16 23) -> 38"
;(msl-add (msl-num 16) (msl-num 23))
;(good (eval (desugar (plusS (numS 16) (numS 23)))) 39 39 "at line 110")
;39

"Test2"
"TEST = (* 2 3) -> 6"
(desugar (mulS (numS 2) (numS 3)))
(eval (desugar (mulS (numS 2) (numS 3))))
(test (eval (desugar (mulS (numS 2) (numS 3)))) 6)
"TEST = (* 8 4) -> 32"
(desugar (mulS (numS 8) (numS 4)))
(eval (desugar (mulS (numS 8) (numS 4))))
(test (eval (desugar (mulS (numS 8) (numS 4)))) 32)
"TEST = (* 9 7) -> 63"
(desugar (mulS (numS 9) (numS 7)))
(eval (desugar (mulS (numS 9) (numS 7))))
(test (eval (desugar (mulS (numS 9) (numS 7)))) 63)

;;Outputs of Test2
;"TEST = (* 2 3) -> 6"
;(msl-mul (msl-num 2) (msl-num 3))
;6
;(good (eval (desugar (mulS (numS 2) (numS 3)))) 6 6 "at line 117")
;"TEST = (* 8 4) -> 32"
;(msl-mul (msl-num 8) (msl-num 4))
;32
;(good (eval (desugar (mulS (numS 8) (numS 4)))) 32 32 "at line 121")
;"TEST = (* 9 7) -> 63"
;(msl-mul (msl-num 9) (msl-num 7))
;63
;(good (eval (desugar (mulS (numS 9) (numS 7)))) 63 63 "at line 125")

"Test3"
"TEST = (+ (- 8) 4) -> -4"
(desugar (plusS (uminusS (numS 8)) (numS -4)))
(eval (desugar (plusS (uminusS (numS 8)) (numS 4))))
(test (eval (desugar (plusS (uminusS (numS 8)) (numS 4)))) -4)
"TEST = (- (- 6) 4) -> -10"
(desugar (plusS (uminusS (numS 6)) (numS 4 )))
(eval (desugar (subS (uminusS (numS 6)) (numS 4))))
(test (eval (desugar (subS (uminusS (numS 6)) (numS 4)))) -10)
"TEST = (* (- 1) 8) -> 8"
(desugar (plusS (uminusS (numS 1)) (numS 8)))
(eval (desugar (mulS (uminusS (numS 1)) (numS 8))))
(test (eval (desugar (mulS (uminusS (numS 1)) (numS 8)))) -8)

;;Outputs of Test3
;"TEST = (+ (- 8) 4) -> -4"
;(msl-add (msl-mul (msl-num -1) (msl-num 8)) (msl-num -4))
;-4
;(good (eval (desugar (plusS (uminusS (numS 8)) (numS 4)))) -4 -4 "at line 131")
;"TEST = (- (- 6) 4) -> -10"
;(msl-add (msl-mul (msl-num -1) (msl-num 6)) (msl-num 4))
;-10
;(good (eval (desugar (subS (uminusS (numS 6)) (numS 4)))) -10 -10 "at line 135")
;"TEST = (* (- 1) 8) -> 8"
;(msl-add (msl-mul (msl-num -1) (msl-num 1)) (msl-num 8))
;-8
;(good (eval (desugar (mulS (uminusS (numS 1)) (numS 8)))) -8 -8 "at line 139")

"Test4"
"TEST = (^ 2 3) -> 8 "
(desugar (expS (numS 2) (numS 3)))
(eval (desugar (expS (numS 2) (numS 3))))
(test (eval (desugar (expS (numS 2) (numS 3)))) 8)
"TEST = (^ 4 4) -> 256"
(desugar (expS (numS 4) (numS 4)))
(eval (desugar (expS (numS 4) (numS 4))))
(test (eval (desugar (expS (numS 4) (numS 4)))) 256)
"TEST = (^ 3 3) -> 27"
(desugar (expS (numS 3) (numS 3)))
(eval (desugar (expS (numS 3) (numS 3))))
(test (eval (desugar (expS (numS 3) (numS 3)))) 27)

;;Outputs of Test4
;"TEST = (^ 2 3) -> 8 "
;(msl-expt (msl-num 2) (msl-num 3))
;8
;(good (eval (desugar (expS (numS 2) (numS 3)))) 8 8 "at line 145")
;"TEST = (^ 4 4) -> 256"
;(msl-expt (msl-num 4) (msl-num 4))
;256
;(good (eval (desugar (expS (numS 4) (numS 4)))) 256 256 "at line 149")
;"TEST = (^ 3 3) -> 27"
;(msl-expt (msl-num 3) (msl-num 3))
;27






;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (msl-num 7)
;; '(+ 3 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(+ (+ 6 8) 40) -> (msl-add (msl-add (msl-num 6) (msl-num 8)) (msl-num 40))



(test (parse '7) (msl-num 7))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(+ (+ 6 8) 40)) (msl-add (msl-add (msl-num 6) (msl-num 8)) (msl-num 40)))





 (msl-add (msl-num 3) (msl-num 4)) 
 (msl-mul (msl-num 3) (msl-num 4)) 
 (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9)) 
 (msl-mul (msl-num 3) (msl-add (msl-num 4) (msl-num 9)))


;; eval msl -> number
;; evaluate an msl expression
;; examples
;; (msl-num 9) -> 9
;; (msl-add (msl-num 6) (msl-num 7)) -> 13
;; (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)) -> 42



(test (eval (msl-num 9))  9)
(test (eval (msl-add (msl-num 6) (msl-num 7)))  13)
(test (eval (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))  42)
(test (parse '7) (msl-num 7))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(+ (+ 6 8) 40)) (msl-add (msl-add (msl-num 6) (msl-num 8)) (msl-num 40)))
(test (parse '3) (msl-num 3))
(test (parse '(+ 2 5)) (msl-add (msl-num 2) (msl-num 5)))




;"Parser -> reverse polish output" 
;(output-reverse-polish (parse '(+ 99 (* 5 8))))
;(output-reverse-polish (parse '(- 10 (+ 4 5))))
;(output-reverse-polish (parse '(^ 3 (- 5 5))))
;(output-reverse-polish (parse '(+ (- 99 98) (* 5 (+ 3 8)))))
;"Parser -> evaluation" 
;"(+ 99 (* 5 8)) "(eval (parse '(+ 99 (* 5 8))))
;"(* (+ 4 5) (* 3 5))" (eval (parse '(* (+ 4 5) (* 3 5))))
;"Parser-infix -> valuation"
;"(99 + (5 * 8)) "(eval (parse-infix '(99 + (5 * 8))))
;"((4 + 5) * (3 * 5))" (eval (parse '((4 + 5) * (3 * 5))))
;;Definition of funcF
(define-type funcF
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : funcF)]
  [plusC (lhs : funcF) (rhs : funcF)]
  [multC (lhs : funcF) (rhs : funcF)])



;(define (parse [s : s-expression]) : msl
;  (cond
;    [(s-exp-number? s) (msl-num (s-exp->number s))]
;    [(s-exp-list? s)
;     (let ([sl (s-exp->list s)])
;       (case (s-exp->symbol (first sl))
;         [(+) (msl-add (parse (second sl)) (parse (third sl)))]
;         [(*) (msl-mul (parse (second sl)) (parse (third sl)))]
;         [(-) (msl-sub (parse (second sl)) (parse (third sl)))]
;         [(^) (msl-exp (parse (second sl)) (parse (third sl)))]
;         [else (error 'parse "invalid list input")]))]
;    [else (error 'parse "invalid input")]))

(define (newpars [s : s-expression]) : funcF
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(= (length sl) 3)
          (case (s-exp->symbol (first sl))
            [(+) (plusC (newpars (second sl)) (newpars (third sl)))]
            [(*) (multC (newpars (second sl)) (newpars (third sl)))]
            [else (error 'newpars "invalid list input!")]
            )]
         [(= (length sl) 2)
          (appC (s-exp->symbol (first sl)) (newpars (second sl)))]
         [else (error 'newpars "invalid number of inputs")]
         ))]
    [else (error 'newpars "invalid input!")]))


(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : funcF)])

;; Grammar:
;; x -> ± number
;; x -> - x 1
;; x -> * x x
;; x -> - x

(define FuncDefName
  (list
   (fdC 'square 'x (newpars '(* x x)))
   (fdC 'subtract1 'x (newpars '(+ x -1)))
   (fdC 'negative 'x (newpars '(* x -1)))))

;symbol -> FunDefC
;;Definition of FunDefC

(define (get-funcDef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-funcDef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-funcDef n (rest fds))])]))


;; Grammar:
;; C -> ± number
;; C -> + C C
;; C -> * C C

(define (substitute [what : funcF] [for : symbol] [in : funcF]) : funcF
  (type-case funcF in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (substitute what for a))]
    [plusC (l r) (plusC (substitute what for l)
                        (substitute what for r))]
    [multC (l r) (multC (substitute what for l)
                       (substitute what for r))]))


;Tests:
"Test1"

(test (substitute (numC 2) 'x (numC 3))(numC 3))
(test (substitute (numC 3) 'x (newpars '(f (* x x))))
      (appC 'f (multC (numC 3) (numC 3))))
(test (substitute (numC 5) 'y (newpars '(f (* x x))))
      (appC 'f (multC (idC 'x) (idC 'x))))
(test (substitute (numC 5) 'x (newpars '(* x x)))
      (multC (numC 5) (numC 5)))
(test (substitute (numC 4) 'x (newpars '(+ x x)))
      (plusC (numC 4) (numC 4)))
"Test2"

(test (substitute (numC 2) 'x (numC 3))(numC 3))
(test (substitute (numC 3) 'x (newpars '(f (* x x))))
      (appC 'f (multC (numC 3) (numC 3))))
(test (substitute (numC 6) 'y (newpars '(f (* x x))))
      (appC 'f (multC (idC 'x) (idC 'x))))
(test (substitute (numC 6) 'x (newpars '(* x x)))
      (multC (numC 6) (numC 6)))
(test (substitute (numC 3) 'x (newpars '(+ x x)))
      (plusC (numC 3) (numC 3)))

"Test3"

(test (substitute (numC 1) 'x (numC 2))(numC 2))
(test (substitute (numC 2) 'x (newpars '(f (* x x))))
      (appC 'f (multC (numC 2) (numC 2))))
(test (substitute (numC 2) 'y (newpars '(f (* x x))))
      (appC 'f (multC (idC 'x) (idC 'x))))
(test (substitute (numC 2) 'x (newpars '(* x x)))
      (multC (numC 2) (numC 2)))
(test (substitute (numC 3) 'x (newpars '(+ x x)))
      (plusC (numC 3) (numC 3)))




(define (interpre [e : funcF] [fds : (listof FunDefC)]) : number
  (type-case funcF e
    [numC (n) n]
    [idC (_) (error 'interpre "error")]
    [appC (f a) (local ([define fd (get-funcDef f fds)])
                  (interpre (substitute
                           (numC (interpre a fds)) 
                           (fdC-arg fd)
                           (fdC-body fd))
                          fds))]
    [plusC (l r) (+ (interpre l fds) (interpre r fds))]
    [multC (l r) (* (interpre l fds) (interpre r fds))]))

;Tests of interpre:
"Test1"
"Test = ((+ 4 5) -> 9"
(test (interpre (newpars '(+ 4 5)) empty) 9)
(interpre (newpars '(+ 4 5)) empty)
"Test = (( * 2 3) -> 6)"
(test (interpre (newpars '(* 2 3)) empty) 6)
(interpre (newpars '(* 2 3)) empty)
"Test = ((* 5 5) -> 25)"
(test (interpre (newpars '(square 5)) FuncDefName) 25)
(interpre (newpars '(square 5)) FuncDefName)
(test (interpre (newpars '(negative 3)) FuncDefName) -3)
(interpre (newpars '(negative 3)) FuncDefName)

;;Test1 Outputs;
;"Test = ((+ 4 5) -> 9"
;(good (interpre (newpars '(+ 4 5)) empty) 9 9 "at line 284")
;9
;"Test = (( * 2 3) -> 6)"
;(good (interpre (newpars '(* 2 3)) empty) 6 6 "at line 287")
;6
;"Test = ((* 5 5) -> 25)"
;(good (interpre (newpars '(square 5)) FuncDefName) 25 25 "at line 290")
;25
;(good (interpre (newpars '(negative 3)) FuncDefName) -3 -3 "at line 292")
;-3



"Test2"
"Test = ((+ 3 5) -> 8)"
(test (interpre (newpars '(+ 3 5)) empty) 8)
(interpre (newpars '(+ 2 4)) empty)
"Test = ((* 5 2 ) -> 10)"
(test (interpre (newpars '(* 5 2)) empty) 10)
(interpre (newpars '(* 5 2)) empty)
"Test = ((* 8 8) -> 64)"
(test (interpre (newpars '(square 8)) FuncDefName) 64)
(interpre (newpars '(square 5)) FuncDefName)
(test (interpre (newpars '(negative 1)) FuncDefName) -1)
(interpre (newpars '(negative 1)) FuncDefName)

;;Test2 Outputs;
;"Test = ((+ 3 5) -> 8)"
;(good (interpre (newpars '(+ 3 5)) empty) 8 8 "at line 297")
;6
;"Test = ((* 5 2 ) -> 10)"
;(good (interpre (newpars '(* 5 2)) empty) 10 10 "at line 300")
;10
;"Test = ((* 8 8) -> 64)"
;(good (interpre (newpars '(square 8)) FuncDefName) 64 64 "at line 303")
;25
;(good (interpre (newpars '(negative 1)) FuncDefName) -1 -1 "at line 305")
;-1

"Test3"
"Test = ((+ 1 2) -> 3)"
(test (interpre (newpars '(+ 1 2)) empty) 3)
(interpre (newpars '(+ 3 5)) empty)
"Test = ((* 3 4 ) -> 12)"
(test (interpre (newpars '(* 3 4)) empty) 12)
(interpre (newpars '(* 3 4)) empty)
"Test = ((* 7 7) -> 49)"
(test (interpre (newpars '(square 7)) FuncDefName) 49)
(interpre (newpars '(square 5)) FuncDefName)
(test (interpre (newpars '(negative 6)) FuncDefName) -6)
(interpre (newpars '(negative 6)) FuncDefName)

;;Test3 Outputs;
;"Test = ((+ 1 2) -> 3)"
;(good (interpre (newpars '(+ 1 2)) empty) 3 3 "at line 308")
;8
;"Test = ((* 3 4 ) -> 12)"
;(good (interpre (newpars '(* 3 4)) empty) 12 12 "at line 311")
;12
;"Test = ((* 7 7) -> 49)"
;(good (interpre (newpars '(square 7)) FuncDefName) 49 49 "at line 314")
;25
;(good (interpre (newpars '(negative 6)) FuncDefName) -6 -6 "at line 316")
;-6

"Test4"
"Test = ((+ 2 3) -> 5)"
(test (interpre (newpars '(+ 2 3)) empty) 5)
(interpre (newpars '(+ 2 4)) empty)
"Test = ((* 1 5 ) -> 5)"
(test (interpre (newpars '(* 1 5)) empty) 5)
(interpre (newpars '(* 1 5)) empty)
"Test = ((* 9 9) -> 81)"
(test (interpre (newpars '(square 9)) FuncDefName) 81)
(interpre (newpars '(square 2)) FuncDefName)
(test (interpre (newpars '(negative 9)) FuncDefName) -9)
(interpre (newpars '(negative 9)) FuncDefName)

;;Test4 Outputs;
;"Test = ((+ 2 3) -> 5)"
;(good (interpre (newpars '(+ 2 3)) empty) 5 5 "at line 321")
;6
;"Test = ((* 1 5 ) -> 5)"
;(good (interpre (newpars '(* 1 5)) empty) 5 5 "at line 324")
;5
;"Test = ((* 9 9) -> 81)"
;(good (interpre (newpars '(square 9)) FuncDefName) 81 81 "at line 327")
;4
;(good (interpre (newpars '(negative 6)) FuncDefName) -6 -6 "at line 329")
;-6













         
            
  
  








