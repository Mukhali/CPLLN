#lang slideshow
(circle 10)
(rectangle 10 20)
(hc-append (circle 10) (rectangle 10 20))

;; 定义变量

(define c (circle 10))
(define r (rectangle 10 20))
(hc-append c r)
(hc-append 20 c r c) ;给予20个空格

;; definition function

(define (square n)
  ; A semi-colon starts a line comment.
  ; The expression below is the function body.
  (filled-rectangle n n))

;; Local Binding

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(four (circle 10)) ;; test

;; 显示Local Binding 使用let(本地) or let*
(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

 (checker (colorize (square 10) "red")
           (colorize (square 10) "black"))   ;test

;; the difference  let and let*. let* allow later bindings to use earlier binding.
(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))

(checkerboard (square 10))                   ;test 


;; Functions are Values  函数是可以运算的，R语言的高阶函数就是这样。

circle                               ; #<procedure:circle>

;(define (series mk)
;  (hc-append 4 (mk 5) (mk 10) (mk 20)))


;; 使用lambda这样能使用匿名函数
;; (series (lambda (size) (checkerboard (square size))))

(define series
  (lambda (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))  ;研究一下以上三个代码片段

;; Lexical Scope

(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))

(rgb-series circle)
(rgb-series square)

(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))

(series (rgb-maker circle))
(series (rgb-maker square))


;; Lists

(list "red" "green" "blue")
(list (circle 10) (square 10))

(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "green" "blue" "purple")))

(rainbow (square 5))

(apply vc-append (rainbow (square 5)))

;; Modules and require

(require pict/flash)

(filled-flash 40 30)

;; (require (planet schematics/random:1:0/random))
;; 系统会自动从PLaneT server 下载包
;; (random-gaussian)

;; (provide rainbow square) 可以在Module中提供这两个函数

;; Macros

(require slideshow/code)  ;code is a new syntactic form 
(code (circle 10))

(define-syntax pict+code   ;新定义的一种语法
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))

(pict+code (circle 10))

;; Objects

;; Racket 的类是通过racket/class 包来实现的,
;; racket/gui/base 包提供了GUI和画图类
;; Racket 中类的定义是以%结尾的

(require racket/class
         racket/gui/base)
(define f (new frame% [label "My Art"]
                      [width 300]
                      [height 300]
                      [alignment '(center center)]))

(send f show #t)

(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent f]
                 [style '(border)]
                 [paint-callback (lambda (self dc)
                                   (drawer dc 0 0))])))
(add-drawing (pict+code (circle 10)))
(add-drawing (colorize (filled-flash 50 30) "yellow"))


