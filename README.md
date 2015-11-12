# swirl

swirl is the Erlang port of whiskers.js template library. Its primary focuses on template readability by limiting template logic. The library uses partial application to express template as functions and compile them to beam code.


## concept

The template is blob, it defines a data binding rules and responsible to format and visualize it. The data is applied separately, and it is modeled as nested native / json structure. The library support both native Erlang maps and key-value pairs. The library transforms template to partial application. 

The template uses dot-bind notation to declare data mapping (e.g. `a.b.c`). The renderer engine uses this notation to iterate data structure along given path to required value. 

The library provides api `swirl:f()`. It take template as input and returns partial application. The partial application takes two parameters using curring: scope and data structure. The scope is a binding prefix pre-pended to each mapping declared in template. The data structure is nested map key-value pairs. There is an utility function `swirl:apply()` that does curring-and-apply automatically for any given template... 

```
Fun0 = swirl:f("I am {name}!").
Fun1 = Fun0(undefined).
Fun1(#{name => "Erlang"}).

swirl:apply("I am {name}!", #{name => "Erlang"}).
```

### scope

The scope allow to re-use template and data structures by switching context at runtime. This feature is required to apply single template multiple data instances...  

```
Fun0 = swirl:f("I am {name}!").
Fun1 = Fun0(lang).
Fun1(#{lang => #{name => "Erlang"}, host => #{name => "127.0.0.1"}}).

swirl:apply("I am {name}!", lang, #{lang => #{name => "Erlang"}, host => #{name => "127.0.0.1"}}).
swirl:apply("I am {name}!", host, #{lang => #{name => "Erlang"}, host => #{name => "127.0.0.1"}}).
```


## language

The language is the smallest mustachioed template system limited to 4 statements:
* variable `{a}`
* condition `{if a} ... {else} ... {/if}`
* loop `{for x in a} ... {/for}`
* partial `{>a}`

[template example](test/article.swirl)

[data example](test/article.json)


### variable

The variable statement binds data from context using dot notation. The given path shall be evaluated to scalar data type. 

```
   {a.b.c}
```


### condition

The `if` statements displays section only if variable is truth. The statements supports `if not` flavor as well.

```
   {if a.b.c}
      <p>{a.b.c}</p>
   {else}
      <p>No variable!</p>
   {/if}
```

### loop

The `for` statements iterates over variables in array. 

```
   {for x in a.b.c}
      <p>{x}</p>
   {/for}
```

### partial

The partial tag renders any template assigned to that variable using current context.

```
   <div>{>a}</div>
```

```
   swirl:apply("I am {>title}", #{title => "{name}!", name => "Erlang"}).
```

## advanced

### modules

The collection of independent templates can be grouped into modules using maps. The map defines template name (key) and partial application (value). The partial application is automatically scoped to the name. 

```
   Fun = swirl:f(#{a => swirl:f("I am {name}!"), b => swirl:f("I am from {country}!")}).
   swirl:apply(Fun, a, #{a => #{name => "Erlang"}, b => #{country => "Sweden"}}).
   swirl:apply(Fun, b, #{a => #{name => "Erlang"}, b => #{country => "Sweden"}}).
```

Modules helps to build complex data-driven rendering using reusable templates via _partial_.

```
   Fun = swirl:f(#{
      main => swirl:f("{>title} {>country}"), 
      a    => swirl:f("I am {name}!"), 
      b    => swirl:f("I am from {name}!")
   }).

   swirl:apply(Fun, main, #{
      main => #{title => a, country => b},
      a    => #{name => "Erlang"},
      b    => #{name => "Sweden"}
   }).
```


### byte code compilation

The library implements a template compilation to byte code using module grouping. 
See example below.

```
   {ok, _, Code} = swirl:c(hw, #{a => "I am {name}!", b => "I am from {country}!"}).
   code:load_binary(hw, [], Code).

   hw:a(#{a => #{name => "Erlang"}, b => #{country => "Sweden"}}).
   hw:b(#{a => #{name => "Erlang"}, b => #{country => "Sweden"}}).
```

Compiled code support _partial_ for dynamic template rendering.

```
   {ok, _, Code} = swirl:c(hw, #{
      main => "{>title} {>country}", 
      a    => "I am {name}!", 
      b    => "I am from {name}!"
   }).
   code:load_binary(hw, [], Code).

   hw:main(#{
      main => #{title => {hw, a}, country => {hw, b}},
      a    => #{name => "Erlang"},
      b    => #{name => "Sweden"}
   }).
```

The library implement a command line tool for beam-file compilation

```
   ./swirl -o ebin/ test/article.swirl

   article:f(#{tags => ["a", "b", "c"], content => "Lorem ipsum dolor sit amet"}).
```




