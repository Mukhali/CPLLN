## 0x00 tips

    只有函数作用域。
    JavaScript 中所有变量都是对象，除了null和undefined 除外
    JavaScript 内部，字符以UTF-16的格式储存，每个字符固定为2个字节。
    JavaScript 语言会减少全局函数
    只有构造函数才有 new
    遍历器（Iterator）是一种协议，只要部署这个协议，就可以完成遍历操作。
    ES6的遍历器协议规定，部署了next方法的对象，就具备了遍历器功能
    怎么觉得 JavaScript、Lisp、Python 和 Ruby 这么像勒。
    JavaScript 不包含传统的类继承模型，而是使用 prototype 原型模型。
    基于原型的继承模型比传统的类继承还要强大。 
    执行上下文为全局范围时，定义的属性和方法将被添加到window对象中。
    先在类型的实例上查找，如果没有则继续在类型原型上查找。
    查找路径采用短路算法，即找到首个后即返回。
    （monkey patching）扩展内置类型的原型不推荐
    在 JavaScript 中才能体现作用域和命名空间的重要性？
    

## 0x01 变量

## 字符串

## 数值

JavaScript 是弱类型语言，这就意味着，等于操作符会为了比较两个值而进行强制类型转换。

===：不像普通的等于操作符，严格等于操作符不会进行强制类型转换

## 数组

ES6：数组推导（array comprehension）直接通过数组生成新数组，和 Python 像

new Array(3)

new Array('3')

## 自动分号插入
尽管 JavaScript 有 C 的代码风格，但是它不强制要求在代码中使用分号，实际上可以省略它们。

## 对象

```
false.toString(); // 'false'
[1, 2, 3].toString(); // '1,2,3'

function Foo(){}
Foo.bar = 1;
Foo.bar; // 1
```
数字的字面量也是对象
```
2..toString(); // 第二个点号可以正常解析
2 .toString(); // 注意点号前面的空格
(2).toString(); // 2先被计算
```

JavaScript 的对象可以作为哈希表使用，主要用来保存命名的键与值的对应关系。
使用对象的字面语法 - {} - 可以创建一个简单对象。这个新创建的对象从 Object.
prototype 继承下面，没有任何自定义属性。

### 在对象中添加属性
```
var foo = {};             // 一个空对象
var bar = {test: 12};     // 一个有属性 test 的对象
bar.test;                 // 访问属性
bar['test'];
var get = 'test';
bar[get]; // kitten
```
中括号的应用范围比'.'范围广：

    动态设置属性
    属性名不是一个有效的变量名（译者注：比如属性名中包含空格，或者属性名是 JS 的关键词）

### 在对象中添加属性

删除属性的唯一方法是使用 delete 操作符；设置属性为 undefined 或者 null
并不能真正的删除属性， 而仅仅是移除了属性和值的关联。

```
var obj = {
    bar: 1,
    foo: 2,
    baz: 3
};
obj.bar = undefined;
obj.foo = null;
delete obj.baz;

for(var i in obj) {
    if (obj.hasOwnProperty(i)) {
        console.log(i, '' + obj[i]);
    }
}
```
## 原型

原型链的继承
```
function Foo() {
    this.value = 42;
}
Foo.prototype = {
    method: function() {}
};

function Bar() {}

// 设置Bar的prototype属性为Foo的实例对象
Bar.prototype = new Foo();
Bar.prototype.foo = 'Hello World';

// 修正Bar.prototype.constructor为Bar本身
Bar.prototype.constructor = Bar;

var test = new Bar() // 创建Bar的一个新实例

// 原型链
test [Bar的实例]
    Bar.prototype [Foo的实例] 
        { foo: 'Hello World' }
        Foo.prototype
            {method: ...};
            Object.prototype
                {toString: ... /* etc. */};
```

当查找一个对象的属性时，JavaScript 会向上遍历原型链，直到找到给定名称的属性为止

为了判断一个对象是否包含自定义属性而不是原型链上的属性， 我们需要使用继承自 Object.prototype 的 hasOwnProperty 方法。

## Hoisting
```
bar();
var bar = function() {};
var someValue = 42;

test();
function test(data) {
    if (false) {
        goo = 1;

    } else {
        var goo = 2;
    }
    for(var i = 0; i < 100; i++) {
        var e = data[i];
    }
}
```

提升后

```
// var 表达式被移动到这里
var bar, someValue; // 缺省值是 'undefined'

// 函数声明也会提升
function test(data) {
    var goo, i, e; // 没有块级作用域，这些变量被移动到函数顶部
    if (false) {
        goo = 1;

    } else {
        goo = 2;
    }
    for(i = 0; i < 100; i++) {
        e = data[i];
    }
}

bar(); // 出错：TypeError，因为 bar 依然是 'undefined'
someValue = 42; // 赋值语句不会被提升规则（hoisting）影响
bar = function() {};

test();
```


## 函数


let 立即执行匿名函数（IIFE）

这个命令和 Scheme 语言中的效果是一样的。

但作用域不一样，这个是基于代码块的。

let不像var那样，会发生“变量提升”现象。

const 用于声明常量

解构（Destructuring）：var [a, b, c] = [1, 2, 3];

### 模拟私有变量

```
function Counter(start) {
    var count = start;
    return {
        increment: function() {
            count++;
        },

        get: function() {
            return count;
        }
    }
}

var foo = Counter(4);
foo.increment();
foo.get(); // 5
```
Counter 函数返回两个闭包，函数 increment 和函数 get。


### 构造函数
 Javascript 中的构造函数与一般函数没有任何区别，在创建实例时，如果我们使用
 了new关键字，那么这个函数就具有构造函数的特性，否则就是一般函数。（这个和
 以前学习基于 C 的面向对象语言不一样）


