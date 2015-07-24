![Lambda](https://github.com/Mukhali/MPLLN/blob/master/Scheme/lambda_logo.png)

## 0x00 tips
    
    #lang Scheme
    当你学会了编程之后，了解了程序世界之后，学习一些各种语言很好玩。
    Scheme 大多数操作是函数
    Scheme 的求值顺序是从内括号到外括号
    Scheme 有一种默认的括号返回值
    define 定义的是全局类型？
    Scheme 是基于解释器的语言，和 C 不同。
    Scheme 解释器通过内存空间中的数据地址操作所有的数据，所有存在于内存空间中的
    对象都以同样的方式处理。
    Scheme 语言不去分过程和其他数据结构
    常用的赋值语句在 Scheme 中地位如此之低。
    Scheme 为什么难用，因为语法以简洁为美，没有添加语法糖。
    Scheme 为什么难用，因为很多东西没有标准化，为了保持简洁。
    以上两点都不适合人类在建造事物的思路。



## 0x01 S 表达式

由括号、标记和分隔符组成的式子

## 0x02 原子

除了表之外的数据都叫原子：数字，字符，字符串，向量和空表’()。

’()既是原子，又是表

## 0x03 数据结构

表、数、（Character）、字符串（String）、符号（Symbol）、向量（Vector）

###  列表

Lisp 语言本身就是称为列表操作语言，所以列表操作很重要，

表的递归定义：

    1、‘()是一个表

    2、如果ls是一个表且obj是某种类型的数据，那么(cons obj ls)也是一个表。表本身就是递归的，所以在递归中很有用，就像计算机本身就是离散的。关于表注意输出中的.。

list 函数可以用来构件表。

基本的表操作：cons、car、cdr、list和quote

Cons单元是一个存放了两个地址的内存空间。第一给地址称作car，第二个地址称作
cdr。

函数car：寄存器地址部分（Contents of the Address part of the Register）

函数cdr：寄存器减量部分（Contents of the Decrement part of the Register）

函数cons：构造（construction）

以上这些称呼都是历史遗留称呼。

’()：空表

### 字符与字符串

在字符前添加#\表示字符。如 #\a、#\Space、#\Tab、#\Linefeed和#\Return
```
(char? obj);;如果obj是一个字符则返回#t。
(char=? c1 c3);;如果c1和c2是同一个字符的话则返回#t。
(char->integer c);;将c转化为对应的整数（字符代码，character code）。示例
(char->integer #\a) => 97 
(integer->char n);;该函数将一个整数转化为对应的字符。
(char<? c1 c2)
(char<= c1 c2)
(char> c1 c2)
(char>= c1 c2) 
```

这些比较函数对大小写不敏感

```
(char-ci=? c1 c2)
(char-ci<? c1 c2)
(char-ci<=? c1 c2)
(char-ci>? c1 c2)
(char-ci>=? c1 c2)
```
这些函数分别用于检测字符c是否为字母、数字、空白符、大写字母或小写字母。

```
(char-alphabetic? c)
(char-numeric? c)
(char-whitespace? c)
(char-upper-case? c)
(char-lower-case? c)
```
这些函数分别返回字符C对应的大写或小写。  
```  
(char-upcase c)
(char-downcase c)：
```
字符串是通过两个闭合的双引号来识别，“abc”

```
(string? s);; 如果s是一个字符则返回#t。
(make-string n c);; 返回由n个字符c组成的字符串。参数c可选。
(string-length s);; 返回字符串s的长度。
(string=? s1 s2);; 如果字符串s1和s2相同的话则返回#t。
(string-ref s idx);; 返回字符串s中索引为idx的字符（索引从0开始计数）。
(string-set! s idx c);; 将字符串s中索引为idx的字符设置为c。
(substring s start end); 返回字符串s从start开始到end-1处的子串。
例如(substring "abcdefg" 1 4) => "b c d" 
(string-append s1 s2 ...);; 连接两个字符串s1和s2 
(string->list s);; 将字符串s转换为由字符构成的表。
(list->string ls);; 将一个由字符构成的表转换为字符串。
(string-copy s);; 复制字符串s。 
```

### symbol
`(symbol? x)` ：如果`x`是一个符号则返回`#t`。

`(string->symbol str)`：将`str`转换为符号。`str`应该都是小写的，否则地址系统可能无法正常工作。

`(symbol->string sym)`：将sym转换为字符。

### 关联表
关联表是一种键值对，作为键值的类型只能是：Symbols, characters, numbers和
Strings（需要转换为符合） 

常用的函数 assq（速度最快）, assv, assoc（速度最慢）

```
(define wc '((hi . 3) (everybody . 5) (nice . 3) (to . 10) (meet . 4) (you . 8)))
(assq 'hi wc)
(assq 'you wc)
(assq 'i wc)
```

### Hash 表
```
(make-eq-hash-table size),  ;; 创建 Hash 表，size 是可选的
(make-eqv-hash-table size),
(make-equal-hash-table size),
(make-string-hash-table size)   
(hash-table/put! hash-table key datum) 
(hash-table/get hash-table key default) ;; 获取值
(hash-table->alist hash-table) ;;将 Hash 表 转换为 关联表
```

### 向量（不是 C 中的数组）
向量有索引，可以储存不同类型的数据，

```
'#(1 2 3)             ; 整数向量
'#(a 0 #\a)           ; 由符号、整数和字符构成的向量
```

### 结构体
Scheme中的结构体与C语言中的结构体类似。

Scheme中的结构体比C语言中的更容易使用。

```
(define-structure book title authors publisher year isbn)
(define bazaar 
  (make-book 
   "The Cathedral and the Bazaar"
   "Eric S. Raymond"
   "O'Reilly"
   1999
   0596001088))

(define-structure (book keyword-constructor copier) 
  title authors publisher year isbn)

(define bazaar 
  (make-book 
   'title "The Cathedral and the Bazaar"
   'authors "Eric S. Raymond"
   'publisher "O'Reilly"
   'year 1999 
   'isbn 0596001088))


```


## 0x04 引用

quote 或者 ` ：是一种用来阻止求值的符号，组织内部所以括号的求值。

## 0x05 目录操作

```
(cd "C:\\doc\\scheme")
(load "hello.scm")
```
## 0x06 变量

### define
define 运算符会使用第一个参数作为全局参数，并将其与第二个参数绑定起来。

用 define 定义的是全局变量

### let 和 letrec
let 定义的变量是局部变量，格式 (let binds body)。
letrec类似于let，但它允许一个名字递归地调用它自己。
```
(let ((i 1) (j 2))
  (+ i j))
```
解读： 声明局部变量 i 和 j，将他们与 1，2 绑定，然后求和。

       变量的范围就在 body 里。

let 可以嵌套使用
```
(let ((i 1))
  (let ((j (+ i 2)))
    (* i j)))
```
```
(let* ((i 1) (j (+ i 2)))
  (* i j))
```
这个会提示错误，应为在 j 的作用域中没有变量 i 的定义，

但如果是用 let*，就可以。

let*只是嵌套的let表达式的语法糖而已。

let表达式只是lambda表达式的一个语法糖：
```
(let ((p1 v1) (p2 v2) ...) exp1 exp2 ...)
;⇒
((lambda (p1 p2 ...)
    exp1 exp2 ...) v1 v2)
```

let表达式是lambda表达式的一个语法糖。变量的作用域通过使用let表达式或
lambda

表达式来确定。在Scheme中，这个有效域由源代码的编写决定，这叫做词法闭
包（lexical closure）。

```
(define (fact-letrec n)
  (letrec ((iter (lambda (n1 p)
       (if (= n1 1)
           p
           (let ((m (- n1 1)))
       (iter m (* p m)))))))     ; *
    (iter n n)))
```

## 0x07 控制

### if 
(if predicate then_value else_value)

then_value和else_value都应该是S-表达式。

如果你需要副作用，那么就应该使用begin表达式。

### and

返回 #f 或者 最后一个 S 表达式。

### or

返回 #t 或者 最后一个 S 表达式。

### cond

代替嵌套的 if
```
(cond
  (predicate_1 clauses_1)
  (predicate_2 clauses_2)
    ......
  (predicate_n clauses_n)
  (else        clauses_else))
```

### do
```
(do binds (predicate value)
    body)

[binds] → ((p1 i1 u1) (p2 i2 u2) ... )
```
变量p1，p2，…被分别初始化为i1，i2，…并在循环后分别被更新为u1，u2，…。
```
(define (fact-do n)
  (do ((n1 n (- n1 1)) (p n (* p (- n1 1)))) ((= n1 1) p)))
```

## 0x08 函数

### define
define 定义函数

### lambda
lambda 需要至少一个的参数，第一个参数是由定义的过程所需的参数组成的表。

### 函数的两种定义形式

```
(define hello
  (lambda (name)
    (string-append "Hello " name "!")))
```
```
(define (hello name)
  (string-append "Hello " name "!"))
```

### 做出判断的函数(eq?, eqv? and equal?)

用于做判断的函数都以'?'结尾。

```
(define str "hello")

(eq? str str)

(eq? "hello" "hello")

(eq? 1 1)

(eq? 1.0 1.0)

(eqv? 1.0 1.0)
;Value: #t

(eqv? 1 1.0)

(eqv? (list 1 2 3) (list 1 2 3))

(eqv? "hello" "hello")

(eqv? (lambda(x) x) (lambda (x) x))

(equal? (list 1 2 3) (list 1 2 3))

(equal? "hello" "hello")

```

### 检测数据类型函数
```
pair? 如果对象为序对则返回#t；
list? 如果对象是一个表则返回#t。
      要小心的是空表’()是一个表但是不是一个序对。
null? 如果对象是空表’()的话就返回#t。
symbol? 如果对象是一个符号则返回#t。
char? 如果对象是一个字符则返回#t。
string? 如果对象是一个字符串则返回#t。
number? 如果对象是一个数字则返回#t。
complex? 如果对象是一个复数则返回#t。
real? 如果对象是一个实数则返回#t。
rational? 如果对象是一个有理数则返回#t。
integer? 如果对象是一个整数则返回#t。
exact? 如果对象不是一个浮点数的话则返回#t。
inexact? 如果对象是一个浮点数的话则返回#t。
```
### 比较函数
```
=、>、<、<=、>=
odd?、even?、postitive?、negative?、zero?
```
### 高阶函数

高阶函数（Higher Order Function）是一种以函数为参数的函数。高阶函数
有利于程序的模块化和提高程序的可读性。

map
```
(map procedure list1 list2 ...)

(map + '(1 2 3) '(4 5 6))
(map (lambda (x) (* x x)) '(1 2 3))
```

apply
```
(apply max '(1 3 2))      ;⇒   3
(apply + 1 2 '(3 4 5))    ;⇒  15
(apply - 100 '(5 12 17))  ;⇒  66
```

### 向量函数

## 0x09 IO

默认是输出和输入。

### 从文件输入

(open-input-file filename)，(read-char port) 和 eof-object

(close-input-port port)：关闭输入端口

```
(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-char p)))
      (if (eof-object? c)
    (begin
      (close-input-port p)
      (list->string (reverse ls1)))
    (loop (cons c ls1) (read-char p))))))
(cd "C:\\doc")
(read-file "hello.txt")
(display (read-file "hello.txt"))
```
读取文件时如果遇到 EOF，会返回 eof-object，可以通过 eof-object? 来检测。

call-with-input-file和with-input-from-file

```
(define (read-file file-name)
  (call-with-input-file file-name
    (lambda (p)
      (let loop((ls1 '()) (c (read-char p)))
  (if (eof-object? c)
      (begin
        (close-input-port p)
        (list->string (reverse ls1)))
      (loop (cons c ls1) (read-char p)))))))

(define (read-file file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop((ls1 '()) (c (read-char)))
  (if (eof-object? c)
      (list->string (reverse ls1))
      (loop (cons c ls1) (read-char)))))))
```
(read port)

### 输出

(open-output-file filename)：该函数打开一个文件用作输出，放回该输出端口。

(close-output-port port)：关闭用于输出的端口。

(call-with-output-file filename procedure)：
打开文件filename用于输出，并调用过程procedure。该函数以输出端口为参数。

(with-output-to-file filename procedure)：
打开文件filename作为标准输出，并调用过程procedure。该过程没有参数。当控制
权从过程procedure中返回时，文件被关闭。 

(write obj port)
：函数将obj输出至port。字符串被双引号括起而字符具有前缀#\。
(display obj port)：
该函数将obj输出至port。字符串*不被*双引号括起而字符*不*具有前缀#\。
(newline port)：以新行起始。
(write-char char port)：该函数向port写入一个字符。 

## 0x10 递归

这个世界是你的，也是他的，但终归是递归的。

```
(define (fact n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))
```
### 尾递归

尾递归相对来说在调用开销和内存空间方面有优势。
```
(define (fact-tail n)
  (fact-rec n n))

(define (fact-rec n p)
  (if (= n 1)
      p
      (let ((m (- n 1)))
  (fact-rec m (* p m)))))
```

## 0x11 作用域和闭包的关系
Scheme中变量的作用域被限定在了源码中定义其的那个括号里。作用域与源代码书写
方式一致的作用域称为“词法闭包（Lexical closure）”或“静态作用域（Static 
scope）”。

动态作用域（Dynamic scope）：这种作用域仅在程序运行时确定。由于会在调试时
带来种种问题，这种作用域现在已经不再使用。

## 0x12 副作用
Scheme过程的主要目的是返回一个值，而另一个目的则称为副作用（Side Effect）。赋值和IO操作就是副作用。

## 0x13 赋值

set!、set-car!、set-cdr!、string-set!、vector-set!
Scheme中，具有破坏性的方法都以！结尾。

### set!
```
(define var 1)
(set! var (* var 10))
```
### set-car!，set-cdr!
为一个cons单元的car部分和cdr部分赋新值。

可以用来实现队列功能
```
(define (make-queue)
  (cons '() '()))

(define (enqueue! queue obj)
  (let ((lobj (cons obj '())))
    (if (null? (car queue))
  (begin
    (set-car! queue lobj)
    (set-cdr! queue lobj))
  (begin
    (set-cdr! (cdr queue) lobj)
    (set-cdr! queue lobj)))
    (car queue)))

(define (dequeue! queue)
  (let ((obj (car (car queue))))
    (set-car! queue (cdr (car queue)))
    obj))
(define q (make-queue))

(enqueue! q 'a)

(enqueue! q 'b)

(enqueue! q 'c)

(dequeue! q)
```

## 宏 
用户定义语法也称作宏（Macro）。Lisp/Scheme中的宏比C语言中的宏更加强大。
宏可以使你的程序优美而紧凑。

变量捕获

表达式(nil! x)会变换为(set! x '())
syntax-rules的第二个参数由是变换前表达式构成的表。
_代表宏的名字
```
(define-syntax nil!
  (syntax-rules ()
    ((_ x)
     (set! x '()))))

```

## Currying
有些参数

## Lazy evaluation
可以在需要的时候在计算，可以进行数学公式约简

## Continuation
CPS、没有栈
