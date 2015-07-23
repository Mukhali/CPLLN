#lang racket ;声明我们使用的语言

;; Racket 语言不是一种 minimalist language 而是一种 rich language，
;; 如果你喜欢minimalist language 你应该去学习Scheme，
;; 或者只去学习 Racket 语言中 Scheme的那一部分，
;; Racket 和 Python 一样都是返回一个新变量，而不是修改，
;; 这个因果好像不是这样，但我学习 Python 在前，
;; 还记得当年看 《黑客与画家》是的感觉吗？
;; 看完后去追寻那种新奇
;; 还有记得《The Little Schemer》？

#|
    Racket 是一种教学语言，所以他本身可能性能或安全性方面不是很强大，
    但它在整个技术堆栈方面是很齐全的，一个程序员这一辈子所要用到的
    知识，或者涉及到的方面，它都有。
|#

#|
Writing definitions outside of a module leads to bad error messages,
bad performance, and awkward scripting to combine and run programs. 
The problems are not specific to racket; they’re fundamental 
limitations of the traditional top-level environment, which Scheme 
and Lisp implementations have historically fought with ad hoc 
command-line flags, compiler directives, and build tools. The module
system is designed to avoid these problems, so start with #lang, and 
you’ll be happier with Racket in the long run.
|#
;;; 注释
;; 单行注释

#| 块注释

    #| 
        可以嵌套
    |#
|#

#; (被忽略的表达式)

;; 绿色的是数字
;; gray 代表文本
;;

;;; Module
;; #lang <langname> <topform>*

;;; 在 REPL 中执行文件
;; (enter! "filename.rkt")
;; (load "filename.rkt")

;;; Identifiers 
#|
+
Hfuhruhurr
integer?
pass/fail
john-jacob-jingleheimer-schmidt
a-b-c+1-2-3
|#

;;; 保留字符
;;  ( ) [ ] { } " , ' ` ; # | \


;;; 数字   
1111111111111111111111  ;
#b111                   ;二进制数
#o111                   ;
#x111
3.14
6.02e+23
1/2
1+2i

;;; 函数调用格式(f x y z ……)
(define (bake flavor)
  (printf "pre-heating oven...\n")
  (string-append flavor " pie"))  
(bake "apple")
;; 多表达式，只返回最后一个表达式的值
(define (nobake flavor)
  string-append flavor "jello")
(nobake "green")
;; Racket 不建议使用多表达式，因为有副作用，以上就是一个具体的例子，只是返回“jello”

;; 在创建列表数据的字面量，使用`来阻止它们被求值。
`(+ 1 2)

;;; 常用的数字运算
(+ 1 2)
(- 1 2)
(* 2 1)
(/ 35 5)
(/ 1 3)          ; 1/3
(expt 2 3)
(quotient 5 2)   ; => 2
(remainder 5 2)  ; => 1
(exact->inexact 1/3)  ; => 0.333……
(+ 1+2i 2-3i)         ; => 3-1i

;;; 布尔类型
#t
#f  ; 除了#f外，其它任何类型都为真
(not #t)
(and 0 #f (error "doesn't get here")) ; => #f
(or #f 6 (error "doesn't get here"))  ; => 6

;;; 字符
#\A     ; => #\A
#\λ     ; => #\λ
#\u03BB ; => #\λ

;;; 定长字符串
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; \是转义字符
"Foo\tbar\41\x21\u0021\a\r\n" ; 包含C语言的转义字符,和Unicode
"λx:(μα.α→α).xx"              ; 字符串可以包含Unicode字符
(string-append "Hello " "world!")
(string-ref "Apple" 0)
(format "~a can be ~a" "strings" "formatted")  ; 记着~a
(printf "I'm Racket. Nice to meet you!\n")

;;; 变量
;; 使用 define 来定义
(define some-var 5)
some-var

(define ⊆ subset?)
(⊆ (set 3 2) (set 1 2 3))


;;; 本地绑定
;; 本地绑定: `me' 被绑定到 "Bob",并且只在 let 中生效
(let ([me "Bob"])
  "Alice"
  me) 

(let* ([me "Alice"])
  "Bob"
  me) 

;;; 结构体
(struct dog (name breed age))
(define my-pet
  (dog "lassie" "collie" 5))
my-pet ; => #<dog>
(dog? my-pet) ; => #t
(dog-name my-pet) ; => "lassie"  
;; 注意这种表达式

;;; 对
;; 对是一种不可变得，`cons' 返回对, `car' 和 `cdr' 从对中提取第1个
;; 和第2个元素
(cons 1 2)                         ; => '(1 . 2)
(car (cons 1 (cons 4 2)))          ; => 1
(cons 3 (cdr (cons 1 (cons 4 2)))) ; => '(3 4 . 2)

;;; 列表
;; 列表是一种以 'null' or '()' 结尾的'cons'
(cons 1 (cons 2 (cons 3 null))) ; => '(1 2 3)
;; `list' 给列表提供了一个非常方便的可变参数的生成器
(list 1 2 3) ; => '(1 2 3)
;; 一个单引号也可以用来表示一个列表字面量
'(1 2 3) ; => '(1 2 3)
(cons 4 '(1 2 3)) ; => '(4 1 2 3)
(append '(1 2) '(3 4)) ; => '(1 2 3 4)
(map add1 '(1 2 3))          ; => '(2 3 4)
(map + '(1 2 3) '(10 20 30)) ; => '(11 22 33)
(filter even? '(1 2 3 4))    ; => '(2 4)
(count even? '(1 2 3 4))     ; => 2
(take '(1 2 3 4) 2)          ; => '(1 2)
(take '(1 2 3 4) 2)          ; => '(1 2)
(take '(1 9 3 9 9 4) 4)      ; => '(1 9 3 9)
(drop '(1 2 3 4) 2)          ; => '(3 4)

;;; 向量
#(1 2 3) ; => '#(1 2 3)
(vector-append #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)

;;; Set
;; 从一个列表创建一个Set
(list->set '(1 2 3 1 2 3 3 2 1 3 2 1)) ; => (set 1 2 3)

;; 使用 `set-add' 增加一个成员
;; (函数式特性: 这里会返回一个扩展后的Set,而不是修改输入的值)
(set-add (set 1 2 3) 4) ; => (set 1 2 3 4)

;; 使用 `set-remove' 移除一个成员
(set-remove (set 1 2 3) 1) ; => (set 2 3)

;; 使用 `set-member?' 测试成员是否存在
(set-member? (set 1 2 3) 1) ; => #t
(set-member? (set 1 2 3) 4) ; => #f

;;; 散列表
;; 创建一个不变的散列表 (可变散列表的例子在下面)
(define m (hash 'a 1 'b 2 'c 3))

;; 根据键取得值
(hash-ref m 'a) ; => 1

;; 你可以给不存在的键提供一个默认值
(hash-ref m 'd 0) ; => 0

;; 获取一个不存在的键是一个异常
; (hash-ref m 'd) => 没有找到元素

;; 使用 `hash-set' 来扩展一个不可变的散列表
;; (返回的是扩展后的散列表而不是修改它)
(define m2 (hash-set m 'd 4))
m2 ; => '#hash((a . 1) (b . 2) (d . 4) (c . 3))

;; 使用 `hash-remove' 移除一个键值对 (函数式特性,m并不变)
(hash-remove m 'a) ; => '#hash((b . 2) (c . 3))

;;; 函数

(define (twice f v)
  (f (f v)))
(twice sqrt 16)

;; 使用 `lambda' 创建lambda 函数, 函数总是返回它最后一个表达式的值
(lambda () "Hello World") ; => #<procedure>
;; 也可以使用 Unicode 字符 `λ' 最开始的时候就是使用这个符号
(λ () "Hello World")     ; => 同样的函数

;; 使用括号调用一个函数,也可以直接调用一个 lambda 表达式
((lambda () "Hello World")) ; => "Hello World"
((λ () "Hello World"))      ; => "Hello World"

;; 将函数赋值为一个变量
(define hello-world (lambda () "Hello World"))
(hello-world)               ; => "Hello World"

;; 你可以使用函数定义的语法糖来简化代码  
;; 这个直接和定义变量的形式是一样的。
(define (hello-world2) "Hello World")
(hello-world2)              ; => Hello World

;; 带参数列表的函数
(define hello
  (lambda (name)
    (string-append "Hello " name)))
(hello "Steve")             ; => "Hello Steve"
;; 同样的,可以使用语法糖来定义:
(define (hello2 name)
  (string-append "Hello " name))

;; 可变参数, `case-lambda'
(define hello3
  (case-lambda
    [() "Hello World"]
    [(name) (string-append "Hello " name)]))
(hello3 "Jake")             ; => "Hello Jake"
(hello3)                    ; => "Hello World"
;; 给参数指定一个可选的默认值
(define (hello4 [name "World"])
  (string-append "Hello " name))

;; 函数可以将多余的参数放到一个列表里
(define (count-args . args)
  (format "You passed ~a args: ~a" (length args) args))
(count-args 1 2 3)          ; => "You passed 3 args: (1 2 3)"
;; ... 也可以使用不带语法糖的 `lambda' 形式:
(define count-args2
  (lambda args
    (format "You passed ~a args: ~a" (length args) args)))

;; 你可以混用两种用法
(define (hello-count name . args)
  (format "Hello ~a, you passed ~a extra args" name (length args)))
(hello-count "Finn" 1 2 3)   ; => "Hello Finn, you passed 3 extra args"
;; ... 不带语法糖的形式:
(define hello-count2
  (lambda (name . args)
    (format "Hello ~a, you passed ~a extra args" name (length args))))

;; 使用关键字
(define (hello-k #:name [name "World"] #:greeting [g "Hello"] . args)
  (format "~a ~a, ~a extra args" g name (length args)))

(hello-k)                 ; => "Hello World, 0 extra args"
(hello-k 1 2 3)           ; => "Hello World, 3 extra args"
(hello-k #:greeting "Hi") ; => "Hi World, 0 extra args"
(hello-k #:name "Finn" #:greeting "Hey") ; => "Hey Finn, 0 extra args"
(hello-k 1 2 3 #:greeting "Hi" #:name "Finn" 4 5 6)
                                         ; => "Hi Finn, 6 extra args"

;;; 逻辑运算

;; 判断数字使用 `='
(= 3 3.0)       ; => #t
(= 2 1)         ; => #f

;; 判断对象使用 `eq?'
(eq? 3 3)      ; => #t
(eq? 3 3.0)    ; => #f
(eq? (list 3) (list 3)) ; => #f

;; 判断集合使用 `equal?'
(equal? (list 'a 'b) (list 'a 'b)) ; => #t
(equal? (list 'a 'b) (list 'b 'a)) ; => #f

;;; 控制结构

;; 条件判断 if 、 cond and 模式匹配

(if #t               ; 测试表达式
    "this is true"   ; 为真的表达式
    "this is false") ; 为假的表达式
; => "this is true"

;; 注意, 除 `#f` 之外的所有值都认为是真
(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(Groucho Zeppo)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'yep

;; nested if (不提出，可以用 and or or 来替代)
(define (reply s)
  (if (if (string? s)
          (equal? "hello" (substring s 0 5))
          #f)
      "hi!"
      "huh?"))
(reply "hellee")        ; => "huh?"

(define (reply1 s)
  (if (and (string? s)
           (>= (string-length s) 5)
           (equal? "hello" (substring s 0 5)))
      "hi!"
      "huh?"))

;; `cond' 会进行一系列的判断来选择一个结果
(cond [(> 2 2) (error "wrong!")]  ; error 是 racket 提供的一个函数
      [(< 2 2) (error "wrong again!")]
      [else 'ok]) ; => 'ok

(define (reply-more s)
  (cond
   [(equal? "hello" (substring s 0 5))
    "hi!"]
   [(equal? "goodbye" (substring s 0 7))
    "bye!"]
   [(equal? "?" (substring s (- (string-length s) 1)))
    "I don't know"]
   [else "huh?"]))

(reply-more "hello racket")

;; 模式匹配

(define (fizzbuzz? n)
  (match (list (remainder n 3) (remainder n 5))
    [(list 0 0) 'fizzbuzz]
    [(list 0 _) 'fizz]
    [(list _ 0) 'buzz]
    [_          #f]))

(fizzbuzz? 15) ; => 'fizzbuzz
(fizzbuzz? 37) ; => #f

;; 循环

;; 循环可以使用递归(尾递归)
(define (loop i)
  (when (< i 10)
    (printf "i=~a\n" i)
    (loop (add1 i))))      ; add1 是 Racket 内置
(loop 5)            ;

;; 类似的,可以使用 `let` 定义
(let loop ((i 0))
  (when (< i 10)
    (printf "i=~a\n" i)
    (loop (add1 i)))) ; => i=0, i=1, ...

;; loop 不是 Racket 内置的， for 是
(for ([i 10])
  (printf "i=~a\n" i)) ; => i=0, i=1, ...
(for ([i (in-range 5 10)])
  (printf "i=~a\n" i)) ; => i=5, i=6, ...

;; for 在很多数据结构中是内置的
(for ([i (in-list '(l i s t))])  ; 列表
  (displayln i))

(for ([i (in-vector #(v e c t o r))]) ;向量
  (displayln i))

(for ([i (in-string "string")])    ;字符串
  (displayln i))

(for ([i (in-set (set 'x 'y 'z))])
  (displayln i))

(for ([(k v) (in-hash (hash 'a 1 'b 2 'c 3 ))])
  (printf "key:~a value:~a\n" k v))

;; 并行扫描多个序列 (遇到长度小的就停止)
;; 当 for 后面有 * 时，是嵌套循环
(for ([i 10] [j '(x y z)]) (printf "~a:~a\n" i j))
; => 0:x 1:y 2:z

;; 嵌套循环
(for* ([i 2] [j '(x y z)]) (printf "~a:~a\n" i j))
; => 0:x, 0:y, 0:z, 1:x, 1:y, 1:z

;; 带有条件判断的 `for`
(for ([i 1000]
      #:when (> i 5)
      #:unless (odd? i)
      #:break (> i 10))
  (printf "i=~a\n" i))
; => i=6, i=8, i=10

;;; 更多的例子帮助你加深理解..
;; 和 `for' 循环非常像 -- 收集结果

(for/list ([i '(1 2 3)])
  (add1 i))    ; => '(2 3 4)

(for/list ([i '(1 2 3)] #:when (even? i))
  i) ; => '(2)

(for/list ([i 10] [j '(x y z)])
  (list i j)) ; => '((0 x) (1 y) (2 z))

(for/list ([i 1000] #:when (> i 5) #:unless (odd? i) #:break (> i 10))
  i) ; => '(6 8 10)

(for/hash ([i '(1 2 3)])
  (values i (number->string i)))
; => '#hash((1 . "1") (2 . "2") (3 . "3"))

;; 也有很多其他的内置方法来收集循环中的值:
(for/sum ([i 10]) (* i i)) ; => 285
(for/product ([i (in-range 1 11)]) (* i i)) ; => 13168189440000
(for/and ([i 10] [j (in-range 10 20)]) (< i j)) ; => #t
(for/or ([i 10] [j (in-range 0 20 2)]) (= i j)) ; => #t
;; 如果需要合并计算结果, 使用 `for/fold'
(for/fold ([sum 0]) ([i '(1 2 3 4)]) (+ sum i)) ; => 10
;; (这个函数可以在大部分情况下替代普通的命令式循环)

;; 异常

;; 要捕获一个异常,使用 `with-handlers' 形式
(with-handlers ([exn:fail? (lambda (exn) 999)])
  (+ 1 "2")) ; => 999
(with-handlers ([exn:break? (lambda (exn) "no time")])
  (sleep 3)
  "phew") ; => "phew", 如果你打断了它，那么结果 => "no time"

;; 使用 `raise' 抛出一个异常后者其他任何值
(with-handlers ([number?    ; 捕获抛出的数字类型的值
                 identity]) ; 将它们作为普通值
  (+ 1 (raise 2))) ; => 2

;; 可变的值

;; 很多 Racket 诗句类型是不可变的 (对,列表,等)
;; 有一些既是可变的又是不可变的 (字符串，向量，散列表等...)

;; 使用 `set!' 给一个已经存在的变量赋一个新值
(define n 5)
(set! n (add1 n))
n                   ; => 6

;; 给那些明确地需要变化的值使用 `boxes` (在其他语言里类似指针
;; 或者引用)
(define n* (box 5))
(set-box! n* (add1 (unbox n*)))
(unbox n*)       ; => 6

;; 使用 `vector' 或者 `make-vector' 创建一个可变的向量
(define vec (vector 2 2 3 4))
(define wall (make-vector 100 'bottle-of-beer))
;; 使用 `vector-set!` 更新一项
(vector-set! vec 2 1)      
vec                    ; => '#(1 2 1 4)
(vector-set! wall 99 'down)
wall                   ; => #(1 2 3 4)

;; 创建一个空的可变散列表,然后操作它
(define m3 (make-hash))
(hash-set! m3 'a 1)
(hash-set! m3 'b 2)
(hash-set! m3 'c 3)
(hash-ref m3 'a)   ; => 1
(hash-ref m3 'd 0) ; => 0
(hash-remove! m3 'a)


;; 模块

;; 模块让你将你的代码组织为多个文件,成为可重用的模块,
;; 在这里，我们使用嵌套在本文的整个大模 里的子模块(从 "#lang" 这一行开始)
;; Scheme 中也引入了模块， 

(module cake racket/base ; 基于 racket/base 定义一个 `cake` 模块

  (provide print-cake) ; 这个模块导出的函数

  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))

  (define (show fmt n ch) ; 内部函数
    (printf fmt (make-string n ch))
    (newline)))

;; 使用 `require` 从模块中得到所有 `provide` 的函数
(require 'cake) ; 这里的 `'`表示是本地的子模块
(print-cake 3)
; (show "~a" 1 #\A) ; => 报错, `show' 没有被导出，不存在

;; 类和对象

;; 创建一个 fish% 类(%是给类绑定用的)
(define fish%
  (class object%
    (init size) ; 初始化的参数
    (super-new) ; 父类的初始化
    ;; 域
    (define current-size size)
    ;; 公共方法
    (define/public (get-size)
      current-size)
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))

;; 创建一个 fish% 类的示例
(define charlie
  (new fish% [size 10]))

;; 使用 `send' 调用一个对象的方法
(send charlie get-size) ; => 10
(send charlie grow 6)
(send charlie get-size) ; => 16

;; `fish%' 是一个普通的值,我们可以用它来混入
(define (add-color c%)
  (class c%
    (init color)
    (super-new)
    (define my-color color)
    (define/public (get-color) my-color)))
(define colored-fish% (add-color fish%))
(define charlie2 (new colored-fish% [size 10] [color 'red]))
(send charlie2 get-color)
;; 或者，不带名字
(send (new (add-color fish%) [size 10] [color 'red]) get-color)

;; 宏

;; 宏让你扩展这门语言的语法

;; 让我们定义一个while循环
(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(let ([i 0])
  (while (< i  10)
    (displayln i)
    (set! i (add1 i))))

;; 宏是安全的,你不能修改现有的变量
(define-syntax-rule (swap! x y) ; !表示会修改
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define tmp 2)
(define other 3)
(swap! tmp other)
(printf "tmp = ~a; other = ~a\n" tmp other)
;; 变量 `tmp` 被重命名为 `tmp_1`
;; 避免名字冲突
;; (let ([tmp_1 tmp])
;;   (set! tmp other)
;;   (set! other tmp_1))

;; 但它们仍然会导致错误代码，比如:
(define-syntax-rule (bad-while condition body ...)
  (when condition
    body ...
    (bad-while condition body ...)))
;; 这个宏会挂掉，它产生了一个无限循环，如果你试图去使用它
;; 编译器会进入死循环

;; 契约

;; 契约限制变量从模块中导入

(module bank-account racket
  (provide (contract-out
            [deposit (-> positive? any)] ; 数量一直是正值
            [balance (-> positive?)]))

  (define amount 0)
  (define (deposit a) (set! amount (+ amount a)))
  (define (balance) amount)
  )

(require 'bank-account)
(deposit 5)

(balance) ; => 5

;; 客户端尝试存储一个负值时会出错
;(deposit -5) ; => deposit: contract violation


