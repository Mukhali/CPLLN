![Ruby](https://github.com/Mukhali/MPLLN/tree/master/Ruby/Ruby_logo.png)

## 0x00 tips

    Ruby中的元编程，是可以在运行时动态地操作语言结构（如类、模块、实例变量等）的技术。你甚至于可以在不用重启的情况下，在运行时直接键入一段新的Ruby代码，并执行它。
    可以直接操作抽象语法树很特别奥。
    Ruby中每个对象都有其自己的匿名类，一个类能拥有方法，但是只能对该对象本身其作用：当我们对一个具体的对象添加方法时，Ruby会插入一个新的匿名类于父类之间，来容纳这个新建立的方法。值得注意的是，匿名类通常是不可见（Hidden）的。它没有名字因此不能像其他类一样，通过一个常量来访问。你不能为这个匿名类实例化一个新的对象。
    原来是语言交互的模式决定了他的功能。
    Ruby的Binding对象的概念和 Continuation有共通之处。
    Continuation主要用于实际堆、栈内存的环境跳转，而Binding则比较高层。
    SASS 是用 Ruby 实现的
    所有东西都是对象


## 0x01 实例变量、方法、类

实例变量（Instance Variables）是当你使用它们时，才会被建立的对象。因此，即
使是同一个类的实例，也可以有不同的实例变量。

万物皆对象，类也是对象

Ruby 中类与对象的关系
```
# 对象的方法即为其所属类的实例方法
1.methods == 1.class.instance_methods
#=> true

# 类的“溯源”
N = Class.new
N.ancestors
#=> [N, Object, Kernel, BasicObject]
N.class
#=> Class
N.superclass
#=> Object
N.superclass.superclass
#=> BasicObject
N.superclass.superclass.superclass
#=> nil
```

类是开放的
```
class String
  def writesize
    self.size
  end
end

puts "Tell me my size!".writesize
```
在 Ruby 中还存在匿名类，好特别


## 模块
当你在一个类中包含一个模块时，Ruby创建了一个匿名类来封装这个模块，并将这个
匿名类插入到祖先链中，也在这个类的上方。



## 元编程（自反、内省、反射）

class、instance_methods、intance_variables、instance_exec、

### respond_to?
respond_to?方法是反射机制中另一个有用的方法。使用respond_to?方法，
可以提前知道对象是否能够处理你想要交予它执行的信息。
```
obj = Object.new
if obj.respond_to?(:program)
  obj.program
else
  puts "Sorry, the object doesn't understand the 'program' message."
end
```

### send
send是Object类的实例方法。
send方法的第一个参数是你期望对象执行的方法的名称,包括私有方法。

```
class Rubyist
  private
    def say_hello(name)
      "#{name} rocks!!"
    end
end

obj = Rubyist.new
puts obj.send(:say_hello, 'Matz')
```
### define_method
Module#define_method是Module类实例的私有方法。
因此define_method方法仅能由类或者模块使用。


### method_missing
当Ruby使用look-up机制找寻方法时，如果方法不存在，那么Ruby将会在原receiver中自行调用一个叫做method_missing的方法。
method_missing是由Kernel模块提供的方法，因此任意对象都有此方法。
重载method_missing方法允许你对不存在的方法进行处理。
```
class Rubyist
  def method_missing(m, *args, &block)
    puts "Called #{m} with #{args.inspect} and #{block}"
  end
end

Rubyist.new.anything # => Called anything with [] and
Rubyist.new.anything(3, 4) { something } 
# => Called anything with [3, 4] and #<Proc:0x02efd664@tmp2.rb:7>
```
### remove_method和undef_method
想要移除已存在的方法，在类的作用域（Scope）内使用remove_method方法。
undef_method会阻止任何对指定方法的访问，无论该方法是在对象所属的类中被定义，还是其父类及其先祖类。

```
class Rubyist
  def method_missing(m, *args, &block)
    puts "Method Missing: Called #{m} with #{args.inspect} and #{block}"
  end

  def hello
    puts "Hello from class Rubyist"
  end
end

class IndianRubyist < Rubyist
  def hello
    puts "Hello from class IndianRubyist"
  end
end

obj = IndianRubyist.new
obj.hello # => Hello from class IndianRubyist

class IndianRubyist
  remove_method :hello  # removed from IndianRubyist, but still in Rubyist
end
obj.hello # => Hello from class Rubyist

class IndianRubyist
  undef_method :hello   # prevent any calls to 'hello'
end
obj.hello # => Method Missing: Called hello with [] and
```
### eval
Kernel模块提供了一个叫做eval的方法，该方法用于执行一个用字符串表示的代码。
效率不高，存在安全漏洞，eval方法是在万般无奈的情况下才被选择的。

### instance_eval, module_eval, class_eval
instance_eval，module_eval和class_eval是eval方法的特殊形式。

Object类提供了一个名为instance_eval的公开方法，该方法可被一个实例调用。

module_eval和class_eval方法用于模块和类，而不是对象。class_eval是module_eval的一个别名。module_eval和class_eval可用于从外部检索类变量。

module_eval和class_eval方法亦可用于添加类或模块的实例方法。

通过instance_eval传递的代码块使得你可以在对象内部操作。你可以在对象内部肆
意操纵，不再会有任何数据是私有的！instance_eval亦可用于添加类方法。


```
class Rubyist
  def initialize
    @geek = "Matz"
  end
end
obj = Rubyist.new

# instance_eval可以操纵obj的私有方法以及实例变量

obj.instance_eval do
  puts self # => #<Rubyist:0x2ef83d0>
  puts @geek # => Matz
end

class Rubyist
end

Rubyist.instance_eval do
  def who
    "Geek"
  end
end

puts Rubyist.who # => Geek

class Rubyist
  @@geek = "Ruby's Matz"
end
puts Rubyist.class_eval("@@geek") # => Ruby's Matz

class Rubyist
end
Rubyist.class_eval do
  def who
    "Geek"
  end
end
obj = Rubyist.new
puts obj.who # => Geek
```

### class_variables

如果你想知道一个类中有哪些类变量，我们可以使用class_varibles方法。
他返回一个数组（Array），以符号（Symbol）的形式返回类变量的名称。

```
class Rubyist
  @@geek = "Ruby's Matz"
  @@country = "USA"
end

class Child < Rubyist
  @@city = "Nashville"
end
print Rubyist.class_variables # => [:@@geek, :@@country]
puts
p Child.class_variables # => [:@@city]
```

### class_variable_get, class_variable_set

class_variable_get方法需要一个代表变量名称的符号作为参数，并返回变量的值。
class_variable_set方法也需要一个代表变量名称的符号作为参数，同时也要求传递一个值，作为欲设定的值。

```
class Rubyist
  @@geek = "Ruby's Matz"
end

Rubyist.class_variable_set(:@@geek, 'Matz rocks!')
puts Rubyist.class_variable_get(:@@geek) # => Matz rocks!
```

### instance_variable_get, instance_variable_set

```
class Rubyist
  def initialize(p1, p2)
    @geek, @country = p1, p2
  end
end
obj = Rubyist.new('Matz', 'USA')
puts obj.instance_variable_get(:@geek) # => Matz
puts obj.instance_variable_get(:@country) # => USA

class Rubyist
  def initialize(p1, p2)
    @geek, @country = p1, p2
  end
end

obj = Rubyist.new('Matz', 'USA')
puts obj.instance_variable_get(:@geek) # => Matz
puts obj.instance_variable_get(:@country) # => USA
obj.instance_variable_set(:@country, 'Japan')
puts obj.inspect
```

### const_get, const_set
  
const_get和const_set用于操作常量。const_get返回指定常量的值。

const_set为指定的常量设置指定的值，并返回该对象。如果常量不存在，那么他会创建该常量。
```
class Rubyist
end
puts Rubyist.const_set("PI", 22.0/7.0) # => 3.14285714285714
```
因为const_get返回常量的值，因此，你可以使用此方法获得一个类的名字并为这个类添加一个新的实例化对象的方法。这样使得我们有能力在运行时创建类并实例化其实例。

```
# Let us call our new class 'Rubyist'
# (we could have prompted the user for a class name)
class_name = "rubyist".capitalize
Object.const_set(class_name, Class.new)
# Let us create a method 'who'
# (we could have prompted the user for a method name)
class_name = Object.const_get(class_name)
puts class_name # => Rubyist
class_name.class_eval do
  define_method :who do |my_arg|
    my_arg
  end
end
obj = class_name.new
puts obj.who('Matz') # => Matz
```

