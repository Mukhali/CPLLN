![Haskell](https://github.com/Mukhali/MPLLN/tree/master/Haskell/Haskell_logo.png)

## 0x00 tips
    
    只有 final 变量
    语法像数学表达式
    list的Range特性仍然是来源于数学，比如常数集{x}。
    list comprehension来源于数学中的集合表达式。
    :: 符号表示类型约束，在声明函数时也可使用该符号来明确定义域和值域。
    order-independent：haskell中的函数并没有顺序。
    Haskell拥有一套强大的类型系统，支持自动类型推导。
    在 Haskell 中第一次发现了群论的痕迹。
    接触了一些 Haskell 设计概念，发现很特别，它把数学的研究成果搬到了编程领域。
    很多事物每个人最求的东西，不一样，就像 Golang 的哲学。
    类型构造子、值构造子。


:set prompt "ghci>"

successor（后继）
succ 8

负数尽量用（）包罗。

Haskell 中的基本对象：函数、类型和类型类。

## 类型

:t 来解析表达式或者函数，返回形如“表达式::类型”的结构。

x :: Integer 表示 x 是 Integer 类型。

Char，Int（有界），Integer（无界），Float，Double，Bool。

类型的首字母都是大写，所以函数的首字母都不能大写。

## 类型类

可以看作java中接口（interface）的类似物。

Eq：这一类型类提供了判断相等性的接口，凡是可比较相等性的类型必属于Eq类。

Ord（可比较性，返回LT、GT、EQ等）

Show（可显示性，对应函数show）

Read（与Show相反，对应函数read可以将字符串转为需要的类型，形式是read x ::type或者让read根据所在的表达式自己推断需要的类型如 read "3.5"+4）

Enum（可枚举性，即可以用succ和pred得到后继或者前驱值）

Bounded（有界性，可以使用minBound和maxBound函数获得上下界）

Num（数字类型）

Integral（整数类型）

Floating（浮点类型）。

=>符号来声明类型类的约束。
如fromIntegral :: (Integral a, Num b) => a -> b（将整数类转换成更通用的Num类型） 

## 函数

addThree :: Int->Int->Int->Int
->符号分隔参数，返回值类型在最后（显然每个函数只能返回一个值）。

## 模式匹配 

## 门卫
guard是与模式匹配配合使用的，它的作用是增加过滤条件。门卫由跟在函数名及参数后面的竖线标志。

    bmiTell :: (RealFloat a) => a -> a -> String   
    bmiTell weight height   
        | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"   
        | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"   
        | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"   
        | otherwise                   = "You're a whale, congratulations!"

## Where 

把上面门卫例子中的重复部分抽取出来后，可以如下：

    bmiTell :: (RealFloat a) => a -> a -> String   
    bmiTell weight height   
        | bmi <= skinny = "You're underweight, you emo, you!"   
        | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"   
        | bmi <= fat    = "You're fat! Lose some weight, fatty!"   
        | otherwise     = "You're a whale, congratulations!"   
        where bmi = weight / height ^ 2   
              skinny = 18.5   
              normal = 25.0   
              fat = 30.0

函数在where绑定中定义的名字只对本函数可见，因此我们不必担心它会污染其他函数的命名空间。              

## 定义模块

    module Geometry   
    ( sphereVolume   
    ，sphereArea   
    ，cubeVolume   
    ，cubeArea   
    ，cuboidArea   
    ，cuboidVolume   
    ) where   
     
    sphereVolume :: Float -> Float   
    sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)   
     
    sphereArea :: Float -> Float   
    sphereArea radius = 4 * pi * (radius ^ 2)   
     
    cubeVolume :: Float -> Float   
    cubeVolume side = cuboidVolume side side side   
     
    cubeArea :: Float -> Float   
    cubeArea side = cuboidArea side side side   
     
    cuboidVolume :: Float -> Float -> Float -> Float   
    cuboidVolume a b c = rectangleArea a b * c   
     
    cuboidArea :: Float -> Float -> Float -> Float   
    cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2   
     
    rectangleArea :: Float -> Float -> Float   
    rectangleArea a b = a * b

## 编译

hello.hs 文件内容如下

    main = putStrLn "Hello, world!"

通过命令行进入文件目录

    ghc --make hello

这样之后就能

    ./hello

## 解释

hello.hs 文件内容如下

    main = putStrLn "Hello, world!"

通过命令行进入文件目录

    ghc hello.hs


## 加载
进入 REPL 环境下输入以下内容

    :l hello.hs

这样我们的函数就装载成功（hello.hs 定义函数 double x = x + x ）。修改后

	:l hello.hs or :r

在 hs 文件中装载模块：

    import Data.List

在 GHCI 中装载模块：

    :m Data.List


## Monoid
Monoid是一个满足结合律的特殊集合，我们将一个范畴有元素对象和态射箭头，态射箭头有组合和幺元两种，且满足结合律，这种范畴称为Monoid。

元/幺元/么元

    add :: Integer -> (Integer -> Integer)
    add arg1 arg2 = arg1 + arg2

## lambda
符合：\

## 项目
需要使用cabal初始化一个项目目录，配置project.cabal文件中一些设置，然后再重新配置 cabal。如果你想有单元测试设置了自动发现的测试（类似Java或C＃测试属性），你必须再进一步，创建一个单独的测试运行器，并添加了一堆神奇的Haskell预处理标签。

## 范畴
范畴是由元素对象和态射箭头组成的，这个箭头开始端是一个元素对象，目的地也是一个元素对象。


## Links

[hoogle](https://www.haskell.org/hoogle/)
