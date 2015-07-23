## 0x00 tips

    变量必须大写字母开头，这个和 Haskell 不一样。
    小些字母是常量还是关键字？
    Message passing concurrency
    热代码升级
    项式、模式和模式匹配
## 数据类型

常量数据类型：数值、Atom

复合数据类型：元组、列表

变量是以大写字母开头：

	X = {book, preface, acknowledgements, contents,
	     {chapters, [
	        {chapter, 1, 'An Erlang Tutorial'},
	        {chapter, 2, ...}
	        ]
	     }},   

“=”是右结合操作符，因此A = B = C = D被解析为A = (B = (C = D))。

Erlang提供了多进程原语：spawn用于启动一个并行计算（称为进程）；send向一个进程发送一条消息；而receive从一个进程中接收一条消息。	      


## Erlang内链驱动


[1] http://www.imop.us/v/MTgzMg==.html