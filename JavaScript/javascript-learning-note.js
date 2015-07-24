// JavaScript 中的函数也是对象，JavaScript 允许函数在包含可执行代码的同时，
//能像其他对象一样被传递。
// JavaScript 不仅仅可以用于前端编程，还可以用于后端。
// 不加 var 会成为全局作用域
// javascript 中的对象相当于其他语言中的字典或映射：是键-值的集合
// javascript 语言是一种原型语言，所以任何对象都有一个原型在背后
// javascript 语言只存在函数作用域，那么 lambda 函数就有新用法了
// javascript 语言构造函数有一个属性prototype
// javascript 语言包装类型的妙用

/* 类型
   Number（数字）
   String（字符串）
   Boolean（布尔）
   Symbol（符号）
   Object（对象）
     Function（函数）
     Array（数组）
     Date（日期）
     RegExp（正则表达式）
   Null（空）
   Undefined（未定义）
   Error（错误）
*/


/*多行
  注释*/

// 语句可以以分号作为结束符，也可以以换行符
var testVar = 0;
var testVar = 0

//// 数字（都用浮点数表示）、字符串与操作符

// Javascript 只有一种数字类型 (即 64位 IEEE 754 双精度浮点).
0.1 + 0.2               // = 0.30000000000000004
3                       // = 3
1.5                     // = 1.5

// 所有基本的算数运算
1 + 1                   // = 2
8 - 1                   // = 7
10 * 2                  // = 20
35 / 5                  // = 7

// 包括无法整除的除法
5 / 2                   // = 2.5

// 位运算也和其他语言一样。当你对浮点数进行位运算时，
// 浮点数会转换为至多 32 位的无符号整数
1 << 2                  // = 4

// 括号可以决定优先级
(1 + 3) * 2             // = 8

// 内置对象
Math.sin(3.5)
parseInt("123", 10)
parseFloat()
isNaN(NaN)
isFinite(1/0)

// 有三种非数字的数字类型
Infinity                //  1/0 的结果
-Infinity               // -1/0 的结果
NaN                     // 0/0 的结果

// 也有布尔值
true
false

// 可以通过单引号或双引号来构造字符串
'abc'
"Hello, world"

// 用！来取非
!true                  // = false
!false                 // = true

// 相等 ==
1 == 1                 // = true
2 == 1                 // = false

// 不等 !=
1 != 1                 // = false
2 != 1                 // = true

// 更多的比较操作符 
1 < 10                 // = true
1 > 10                 // = false
2 <= 2                 // = true
2 >= 2                 // = true

// 字符串用+连接
"Hello " + "world!"    // = "Hello world!"

// 字符串也可以用 < 、> 来比较
"a" < "b"              // = true

// 比较时会进行类型转换...
"5" == 5               // = true

// ...除非你是用 ===
"5" === 5              // = false

// 你可以用charAt来得到字符串中的字符
"This is a string".charAt(0)

// 还有两个特殊的值：null和undefined
null                  // 用来表示刻意设置成的空值
undefined             // 用来表示还没有设置的值

// null, undefined, NaN, 0 和 "" 都是假的（false），其他的都视作
// 辑真
// 注意 0 是逻辑假而  "0"是逻辑真, 尽管 0 == "0".  => true

//// 变量、数组和对象

// 变量需要用 var 这个关键字声明. Javascript是动态类型语言，但是静态
// 作用域语言，所以你在声明时无需指定类型。 赋值需要用 =
var someVar = 5

// 如果你在声明时没有加var关键字，你也不会得到错误
// 但是此时这个变量就会拥有全局的作用域，而非当前作用域
someOtherVar = 10

// 没有被赋值的变量都会返回undefined这个值
var someThirdVar                        // = undefined

// 对变量进行数学运算有一些简写法
someVar += 5        // 等价于 someVar = someVar + 5; someVar 现在是 10
someVar *= 10       // 现在 someVar 是 100

// 自增和自减也有简写
someVar++           // someVar 是 101
someVar--           // 回到 100

"hello".length
"hello".charAt(0)
"hello".toUpperCase()

// 数组是任意类型组成的有序列表
var myArray = ["Hello", 45, true]  //数组字面量法（array literal）

// 数组的元素可以用方括号下标来访问
// 数组的索引从0开始
myArray[1] // = 45

// 数组还有一个特殊的length（长度）属性（数组的长度是比数组最高索引值大 1 的数）
var a = new Array();
a[0] = "dog";
a[1] = "cat";
a[2] = "hen";
a.length

//使用字面量法创建对象或数组时在括号末尾加上逗号(比如 var a = [1, 2, 3, ] )在不同浏览器中可能导致不同的结果，所以暂时不推荐这么做。
var a = ["dog", "cat", "hen"];

// 对象
// JavaScript 中的对象可以简单理解成“名称-值”对
{key1: "Hello", key2: "World"}

// 创建一个空对象
var obj = new Object();
var obj = {};    //对象字面量（object literal）法。这种语法在 JSON 格式中被广
//泛采用，一般我们优先选择第二种方法。

// 对象赋值方法
obj.name = "Simon"
var name = obj.name;

obj["name"] = "Simon";
var name = obj["name"];

// 对象字面量可以用来定义对象
var obj = {
    name: "Carrot",
    "for": "Max",
    details: {
        color: "orange",
        size: 12
    }
}

// 链式（chain）访问
obj.details.color
obj["details"]["size"]

// 键是字符串，但是引号也并非是必须的，如果键本身是合法的js标识符
// 而值则可以是任意类型的值
var myObj = {myKey: "myValue", "my other key": 4}

// 对象的访问可以通过下标
myObj["my other key"]          // = 4

// 或者也可以用 . 
myObj.myKey                    // = "myValue"

// 对象是可变的，键和值也可以被更改或增加
myObj.myThirdKey = true

// 如果你想要访问一个还没有被定义的属性，那么会返回undefined
myObj.myFourthKey              // = undefined

//// 控制

// if语句和其他语言中一样
var count = 1
if (count == 3){
    // count 是 3 时执行
} else if (count == 4) {
    // count 是 4 时执行
} else {
    // 其他情况下执行 
}

// while循环
while (true) {
    // 无限循环
}

// Do-while 和 While 循环很像 ，但前者会至少执行一次
var input
do {
    input = getInput()
} while (!isValid(input))

// for循环和C、Java中的一样
for (var i = 0; i < 5; i++){
    // 遍历5次
}

// && 是逻辑与, || 是逻辑或
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear"
}
if (colour == "red" || colour == "blue"){
    // colour是red或者blue时执行
}

// && 和 || 是“短路”语句，在初始化值时会变得有用    
var name = otherName || "default"             // 这个很有特点

//// 函数、递归、作用域和闭包

// JavaScript 函数由function关键字定义
// 没有使用 return 语句，或者一个没有值的 return 语句，JavaScript 会返回 undefined。
// 参数是通过arguments对象传进去的
function add(x, y) {
    var total = x + y;
    return total;
}
add(2, 3, 4)                               // = 5

function add() {
    var sum = 0;
    for (var i = 0, j = arguments.length; i < j; i++) {
        sum += arguments[i];
    }
    return sum;
}
add(2, 3, 4)                              // = 9


function myFunction(thing){
    return thing.toUpperCase()
}
myFunction("foo")                             // = "FOO"

// 函数也可以是匿名的，是不是相当于 lambda 函数
function(thing){
    return thing.toLowerCase()
}

// javascript中的函数也是对象，所以函数也能够赋给一个变量，并且被传递
// 比如一个事件处理函数
function myFunction(){
    // this code will be called in 5 seconds' time
}
setTimeout(myFunction, 5000)

// 你甚至可以直接把一个函数写到另一个函数的参数中
setTimeout(function myFunction(){
    // 5秒之后会执行这里的代码
}, 5000)

// JavaScript 仅有函数作用域，而其他的语句则没有作用域
if (true){                           // 这个不是函数
    var i = 5
}
i // = 5 - 并非我们在其他语言中所得到的undefined

// 匿名函数
var avg = function() {
    var sum = 0;
    for (var i = 0, j = arguments.length; i < j; i++) {
        sum += arguments[i];
    }
    return sum / arguments.length;
}

// 通过执行匿名函数，可以避免一些临时变量扩散到外边去
function(){
    var temporary = 5       // 函数变量
    window.permanent = 10   // 全局变量
    permanent2 = 15         // 全局变量
}()
temporary                   // 抛出引用异常
permanent                   // = 10
permanent2                  // = 15

// 函数的递归调用
function countChars(elm) {
    if (elm.nodeType == 3) { // TEXT_NODE
        return elm.nodeValue.length;
    }
    var count = 0;
    for (var i = 0, child; child = elm.childNodes[i]; i++) {
        count += countChars(child);
    }
    return count;
}

// 匿名函数的递归调用 arguments.callee 
var charsInBody = (function(elm) {
    if (elm.nodeType == 3) { // TEXT_NODE
        return elm.nodeValue.length;
    }
    var count = 0;
    for (var i = 0, child; child = elm.childNodes[i]; i++) {
        count += arguments.callee(child);
    }
    return count;
})(document.body);

// 闭包   作用域链（scope chain）
function makeAdder(a) {
    return function(b) {
        return a + b;
    }
}
x = makeAdder(5);
y = makeAdder(20);
x(6)    // = 11
y(7)    // = 27

// 内部函数可以拥有外部函数的所有访问权
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!"
    function inner(){
        alert(prompt)
    }
    setTimeout(inner, 5000)
    // setTimeout 是异步的，所以这个函数会马上终止不会等待。
    // 然而，在5秒结束后，inner函数仍然会弹出prompt信息。
}
sayHelloInFiveSeconds("Adam")     // 会在5秒后弹出 "Hello, Adam!" 

//// 对象、构造函数和原型

//  对象包含方法
var myObj = {
    myFunc: function(){
        return "Hello world!"
    }
}
myObj.myFunc()                   // = "Hello world!"

// 当对象中的函数被调用时，这个函数就可以通过this关键字访问这个对象
myObj = {
    myString: "Hello world!",
    myFunc: function(){
        return this.myString
    }
}
myObj.myFunc()                   // = "Hello world!"

// 但这个函数访问的其实是其运行时环境，而非定义时环境
// 所以如果函数所在的环境不在当前对象的环境中运行时，就运行不成功了
var myFunc = myObj.myFunc
myFunc() // = undefined

// 相应的，一个函数也可以被指定为一个对象的方法，并且用过this可以访问
// 这个对象的成员，即使在定义时并没有绑定任何值
var myOtherFunc = function(){
    return this.myString.toUpperCase()
}
myObj.myOtherFunc = myOtherFunc
myObj.myOtherFunc()  // = "HELLO WORLD!"

// 当你通过new关键字调用一个函数时，就会生成一个对象
// 而对象的成员需要通过this来定义。
// 这样的函数就叫做构造函数

var MyConstructor = function(){
    this.myNumber = 5
}
myNewObj = new MyConstructor() // = {myNumber: 5}
myNewObj.myNumber // = 5

// 将JavaScript改造成你熟悉的面向对象方法
// first edition
function makePerson(first, last) {
    return {
        first: first,
        last: last
    }
}
function personFullName(person) {
    return person.first + ' ' + person.last;
}
function personFullNameReversed(person) {
    return person.last + ', ' + person.first
}
s = makePerson("Simon", "Willison");
personFullName(s)    // = Simon Willison
personFullNameReversed(s)  // = Willison, Simon

// second edition   // 使用点或花括号访问属性或方法时是this
function makePerson(first, last) {
    return {
        first: first,
        last: last,
        fullName: function() {
            return this.first + ' ' + this.last;
        },
        fullNameReversed: function() {
            return this.last + ', ' + this.first;
        }
    }
}
s = makePerson("Simon", "Willison")
s.fullName()    // = Simon Willison
s.fullNameReversed()  // = Willison, Simon

// third edition
function Person(first, last) {
    this.first = first;
    this.last = last;
    this.fullName = function() {
        return this.first + ' ' + this.last;
    }
    this.fullNameReversed = function() {
        return this.last + ', ' + this.first;
    }
}
var s = new Person("Simon", "Willison");

// fouth edition
function personFullName() {
    return this.first + ' ' + this.last;
}
function personFullNameReversed() {
    return this.last + ', ' + this.first;
}
function Person(first, last) {
    this.first = first;
    this.last = last;
    this.fullName = personFullName;
    this.fullNameReversed = personFullNameReversed;
}

// fifth edition  使用原型
function Person(first, last) {
    this.first = first;
    this.last = last;
}
Person.prototype.fullName = function() {
    return this.first + ' ' + this.last;
}
Person.prototype.fullNameReversed = function() {
    return this.last + ', ' + this.first;
}

// 每一个js对象都有一个原型，当你要访问一个没有定义过的成员时，
// 解释器就回去找这个对象的原型

// 有一些JS实现会让你通过一个对象的__proto__方法访问这个原型。
// 这虽然对理解这个对象很有用，但是这并不是标准的一部分
// 我们之后会通过标准方式来访问原型。
var myObj = {
    myString: "Hello world!",
}
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase()
    }
}
myObj.__proto__ = myPrototype
myObj.meaningOfLife // = 42

// This works for functions, too.
myObj.myFunc() // = "hello world!"

// 当然，如果你要访问的成员在原型当中也没有定义的话，
// 解释器就会去找原型的原型。
myPrototype.__proto__ = {
    myBoolean: true
}
myObj.myBoolean // = true

// 这其中并没有对象的拷贝。每个对象的原型实际上是持有原型对象的引用
// 这说明当我们改变对象的原型时，会影响到其他以这个原型为原型的对象
myPrototype.meaningOfLife = 43
myObj.meaningOfLife // = 43

// 我们知道 __proto__ 并非标准规定，实际上也没有办法更改已经指定好的原型。
// 但是，我们有两种方式可以为新的对象指定原型。

// 第一种方式是 Object.create，这个方法是在最近才被添加到Js中的
// 也因此并不是所有的JS实现都有这个啊
var myObj = Object.create(myPrototype)
myObj.meaningOfLife // = 43

// 第二种方式可以在任意版本中使用，不过需要通过构造函数。
// 构造函数有一个属性prototype。但是这 *不是* 构造函数本身的函数
// 而是通过构造函数和new关键字生成新对象时自动生成的。
myConstructor.prototype = {
    getMyNumber: function(){
        return this.myNumber
    }
}
var myNewObj2 = new myConstructor()
myNewObj2.getMyNumber() // = 5

// 字符串和数字等内置类型也有通过构造函数来创建的包装类型
var myNumber = 12
var myNumberObj = new Number(12)
myNumber == myNumberObj // = true

// 但是它们并非严格等价
typeof myNumber // = 'number'
typeof myNumberObj // = 'object'
myNumber === myNumberObj // = false
if (0){
    // 这段代码不会执行，因为0代表假
}

if (Number(0)){
    // 这段代码会执行，因为Number(0)代表真
}

// 但是，包装类型和内置类型共享一个原型
// 这样你就可以给内置类型也增加一些功能
String.prototype.firstCharacter = function(){
    return this.charAt(0)
}
"abc".firstCharacter() // = "a"

// 这个技巧可以用来用老版本的javascript子集来是实现新版本js的功能
// 这样就可以在老的浏览器中使用新功能了。

// 比如，我们知道Object.create并没有在所有的版本中都实现
// 但是我们仍然可以通过这个技巧来使用
if (Object.create === undefined){ // 如果存在则不覆盖
    Object.create = function(proto){
        // 用正确的原型来创建一个临时构造函数
        var Constructor = function(){}
        Constructor.prototype = proto
        // 之后用它来创建一个新的对象
        return new Constructor()
    }
}


// 一些小技巧
var name = o && o.getName();
var name = otherName || "default";

for (var i = 0; i < a.length; i++) {
    // Do something with a[i]
}

// better
for (var i = 0, len = a.length; i < len; i++) {
    // Do something with a[i]
}

// best
for (var i = 0, item; item = a[i++];) { //数组中不含假值
    // Do something with item
}

for (var i in a) {
    // Do something with a[i]
}

// arguments.callee
function counter() {
    if (!arguments.callee.count) {
        arguments.callee.count = 0;
    }
    return arguments.callee.count++;
}

counter()        // = 0
counter()        // = 1

// 原型的toString()方法打印函数内容
var s = new Person("Simon", "Willison");
s           // =[object Object]
Person.prototype.toString = function() {
    return '<Person: ' + this.fullName() + '>';
}
s          // = <Person: Simon Willison>


