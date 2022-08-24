---
# https://sli.dev/builtin/layouts.html#statement

theme: default
# background: images/pawel-czerwinski-vjuSjU3Dfm0-unsplash.jpg
background: /ernest-brillo-Qi8CvonsYnM-unsplash.jpg
# apply any windi css classes to the current slide
class: 'text-center'
# https://sli.dev/custom/highlighters.html
highlighter: shiki
# show line numbers in code blocks
lineNumbers: false
# some information about the slides, markdown enabled
info: |
  lorem ipsum
routerMode: hash
colorSchema: light
# persist drawings in exports and build
drawings:
  persist: true

# Change the scale to zoom in the slides more or less
canvasWidth: 775 # default is 980
---

# Building a Type Safe OTP

twitter: @louispilfold

<!--
The last comment block of each slide will be treated as slide notes. It will be visible and editable in Presenter Mode along with the slide. [Read more in the docs](https://sli.dev/guide/syntax.html#notes)
-->

---
layout: two-cols
---

# Hi, I'm Louis!

- 🐙 I love Erlang
- ⚗️ I love Elixir
- 🔮 I love types
- ✨ I made Gleam


twitter: @louispilfold <br>
github: @lpil

::right::

<img src="/lucy-circle-pride.svg">

<!--
-->

---
layout: quote
class: text-center
---

# "It is not possible to type OTP"

<h3 v-click>
  ✨ OK, let's type all the other bits ✨
</h3>
<br>
<br>
<br>
<br>
<br>

<!--
I wanted to add types to Erlang

People would say not possible

Disappointing

But most Erlang code isn't OTP

We know FP can be typed

Let's type that
-->
---
layout: image-right
image: /daniele-franchi-S4jPaP071KI-unsplash.jpg
---


<img src="/gleam.svg" style="max-width: 90%; margin: 2rem 0 5rem 0">

A friendly programming language

- Runs on the BEAM
- Has a strong static type system

<!--
Resulted in Gleam

BEAM: actors, distributed, multi-core, fault tolerant

Types: like Elm. Compiler trying to help

I am not going to sell types to you
-->
---

# A brief history of Gleam

- 2018: Development begins
- 2019: v0.1 released
- 2020: Language refinement, formatter, html docs, OTP
- 2021: Build tool, new parser, JavaScript, typespecs
- 2022: LSP, JSON, WASM, .d.ts

<style>
  li {
    margin-top: 0.6rem;
  }
</style>

<!--
Gleam history

Notice OTP in 2020

original assumption was wrong

maybe we can make a type safe OTP

useful, enjoyable, compatible
-->
---
class: text-center
---

# What do you mean by type safe?

<!--
Pit of success

The most obvious solution should be optimal

Invalid solutions should not be possible

Compiler helps you

That's the goal
-->

---
---
```rust {all|1-3|5-11|8}
pub type Language {
  Language(name: String, magic: String)
}

pub fn add_beamers(languages) {
  [ Language(name: "Erlang", magic: "Actors"),
    Language(name: "Elixir", magic: "Macros"),
    Language(name: "Gleam"),
    ..languages,
  ]
}
```

<v-click>

```
  ┌─ ./src/main.gleam:9:5
  │
9 │     Language(name: "Gleam"),
  │     ^^^^^^^^^^^^^^^^^^^^^^^ expected 2 arguments, got 1

This call accepts these additional labelled arguments:
  - magic
```

</v-click>
<!--
A quick intro to Gleam

**CLICK**  Define a record

**CLICK**  Use it in a function

No annotations needed

**CLICK**  Mistake: missing argument

**CLICK** The compiler lets us know
-->
---
layout: center
class: text-center
---

# No send or receive in Gleam!
We gotta import them

<!-- 
No send/receive

To work with processes you must use Erlang FFI
-->

---
---

# External types

<div grid="~ cols-2 gap-4">
<div>

In Erlang
```erlang
-module(my_erlang_module).

% Define a type
@opaque my_type() :: term().
```

</div>
<div>

In Gleam

```rust
// Import it into Gleam
external type MyType
```

</div>
</div>

<!--
Permits referencing Erlang types in Gleam
-->
---
---

# External functions

<div grid="~ cols-2 gap-4">
<div>

In Erlang
```erlang {all|0}
-module(my_erlang_module).
-export([add/2]).

% Define a function
-spec add(float(), float()) -> float().
add(A, B) ->
    A + B.
```

</div>
<div>

In Gleam

```rust {all|2}
// Import it into Gleam
external fn add(Float, Float) -> Float =
  "my_erlang_module" "add"

pub fn main() {
  add(1.0, 2.2) // <- Use it!
}
```

</div>
</div>

<!--
Permits calling Erlang functions in Gleam

**CLICK CLICK**

Have to give the type of arguments + return

Enough Gleam. Let's make type safe OTP!
-->
---
---
# Let's wrap gen_server

<div grid="~ cols-2 gap-4">
<div>

```erlang {all|0}
init(_Args) ->
    {ok, 0}.


handle_call(increment, _From, X) ->
    {reply, X, X + 1};
handle_call(is_zero, _From, X) ->
    {reply, X, X =:= 0}.



handle_cast(reset, _) ->
    {noreply, 0}.
```

</div>
<div>

```rust {all|7-8}
pub fn init(_args) {
  Ok(0)
}

pub fn handle_call(msg, _from, x) {
  case msg {
    Increment -> Reply(x, x + 1)
    IsZero -> Reply(x, x == 0)
  }
}

pub fn handle_cast(msg, _x) {
  case msg {
    Reset -> Noreply(0)
  }
}
```

</div>
</div>

<!--
One to one wrapping

Can you see the problem?

**CLICK CLICK**

Increment returns reply of int  
IsZero returns reply of bool

Gleam won't permit it

Also: Unsatisfying. High level. Limited

Not powerful enough to represent OTP
-->
---
layout: center
class: text-center
---
# OTP is implemented in Erlang

Gleam should be as powerful

<!--
OTP is just Erlang

We don't want to be limited

Gleam should be as powerful

Right primitives -> we can implement OTP
-->
---
layout: image-left
image: /jake-weirick-dWUPJdXiC-M-unsplash.jpg
---
# The primitives

- send
- receive
- spawn
- link
- monitor
- trap_exits

---
---
- Wrapping OTP is not enough
	- If we wrap gen_server we can't do everything
	- Elixir's task, gen_statem, etc
	- We want type safe versions of the primitives
		- send
		- receive
		- spawn
		- link
		- monitor
		- trap_exits
- Type safe primitives
	- define Pid type (no parameter)
	- link (easy, just a fn)
	- monitor (Monitor type + fn)
	- trap_exits (Erlang wrapper + fn)
	- send
		- How do we know what type of message a pid accepts?
		- Parameterise pids with the message type
	- spawn (fn)
	- receive
		- gen_server style eager receive fn
		- need to have a reference to self in order to know own message type
- Show spawning a process and sending a message to it
- Let's implement `call`
	- Sending a message and getting a response.
	- Show the code
	- What if the response isn't the first message in the inbox?
	- How does OTP do it?
		- send `{call, {Pid, Ref}, Msg}`, reply with `{Ref, Reply}`
		- the reference is used to add some semantic information to messages being received
		- We need selective receive!
	- Selecting for one specific tag tuple
	- `receive` can select from multiple expected messages at once
		- Selecting multiple tags at once with Selector
	- implement call with it
	- Wait! There's a problem!
	- We can't reply because the caller pid might not accept that message type
	- Having a pid accept only one type of message is not enough
	- We need to add semantic information to messages being sent
	- Use subjects again!
---
layout: image-right
image: https://source.unsplash.com/collection/94734566/1920x1080
---

# Code

Use code snippets and get the highlighting directly![^1]

```ts {all|2|1-6|9|all}
interface User {
  id: number
  firstName: string
  lastName: string
  role: string
}

function updateUser(id: number, update: User) {
  const user = getUser(id)
  const newUser = { ...user, ...update }
  saveUser(id, newUser)
}
```

<arrow v-click="3" x1="400" y1="420" x2="230" y2="330" color="#564" width="3" arrowSize="1" />

[^1]: [Learn More](https://sli.dev/guide/syntax.html#line-highlighting)

<style>
.footnotes-sep {
  @apply mt-20 opacity-10;
}
.footnotes {
  @apply text-sm opacity-75;
}
.footnote-backref {
  display: none;
}
</style>

---

# Components

<div grid="~ cols-2 gap-4">
<div>

You can use Vue components directly inside your slides.

We have provided a few built-in components like `<Tweet/>` and `<Youtube/>` that you can use directly. And adding your custom components is also super easy.

```html
<Counter :count="10" />
```

<!-- ./components/Counter.vue -->
<Counter :count="10" m="t-4" />

Check out [the guides](https://sli.dev/builtin/components.html) for more.

</div>
<div>

```html
<Tweet id="1390115482657726468" />
```

<Tweet id="1390115482657726468" scale="0.65" />

</div>
</div>


---
class: px-20
---

# Themes

Slidev comes with powerful theming support. Themes can provide styles, layouts, components, or even configurations for tools. Switching between themes by just **one edit** in your frontmatter:

<div grid="~ cols-2 gap-2" m="-t-2">

```yaml
---
theme: default
---
```

```yaml
---
theme: seriph
---
```

<img border="rounded" src="https://github.com/slidevjs/themes/blob/main/screenshots/theme-default/01.png?raw=true">

<img border="rounded" src="https://github.com/slidevjs/themes/blob/main/screenshots/theme-seriph/01.png?raw=true">

</div>

Read more about [How to use a theme](https://sli.dev/themes/use.html) and
check out the [Awesome Themes Gallery](https://sli.dev/themes/gallery.html).

---
preload: false
---

# Animations

Animations are powered by [@vueuse/motion](https://motion.vueuse.org/).

```html
<div
  v-motion
  :initial="{ x: -80 }"
  :enter="{ x: 0 }">
  Slidev
</div>
```

<div class="w-60 relative mt-6">
  <div class="relative w-40 h-40">
    <img
      v-motion
      :initial="{ x: 800, y: -100, scale: 1.5, rotate: -50 }"
      :enter="final"
      class="absolute top-0 left-0 right-0 bottom-0"
      src="https://sli.dev/logo-square.png"
    />
    <img
      v-motion
      :initial="{ y: 500, x: -100, scale: 2 }"
      :enter="final"
      class="absolute top-0 left-0 right-0 bottom-0"
      src="https://sli.dev/logo-circle.png"
    />
    <img
      v-motion
      :initial="{ x: 600, y: 400, scale: 2, rotate: 100 }"
      :enter="final"
      class="absolute top-0 left-0 right-0 bottom-0"
      src="https://sli.dev/logo-triangle.png"
    />
  </div>

  <div
    class="text-5xl absolute top-14 left-40 text-[#2B90B6] -z-1"
    v-motion
    :initial="{ x: -80, opacity: 0}"
    :enter="{ x: 0, opacity: 1, transition: { delay: 2000, duration: 1000 } }">
    Slidev
  </div>
</div>

<!-- vue script setup scripts can be directly used in markdown, and will only affects current page -->
<script setup lang="ts">
const final = {
  x: 0,
  y: 0,
  rotate: 0,
  scale: 1,
  transition: {
    type: 'spring',
    damping: 10,
    stiffness: 20,
    mass: 2
  }
}
</script>

<div
  v-motion
  :initial="{ x:35, y: 40, opacity: 0}"
  :enter="{ y: 0, opacity: 1, transition: { delay: 3500 } }">

[Learn More](https://sli.dev/guide/animations.html#motion)

</div>

---

# LaTeX

LaTeX is supported out-of-box powered by [KaTeX](https://katex.org/).

<br>

Inline $\sqrt{3x-1}+(1+x)^2$

Block
$$
\begin{array}{c}

\nabla \times \vec{\mathbf{B}} -\, \frac1c\, \frac{\partial\vec{\mathbf{E}}}{\partial t} &
= \frac{4\pi}{c}\vec{\mathbf{j}}    \nabla \cdot \vec{\mathbf{E}} & = 4 \pi \rho \\

\nabla \times \vec{\mathbf{E}}\, +\, \frac1c\, \frac{\partial\vec{\mathbf{B}}}{\partial t} & = \vec{\mathbf{0}} \\

\nabla \cdot \vec{\mathbf{B}} & = 0

\end{array}
$$

<br>

[Learn more](https://sli.dev/guide/syntax#latex)

---

# Diagrams

You can create diagrams / graphs from textual descriptions, directly in your Markdown.

<div class="grid grid-cols-3 gap-10 pt-4 -mb-6">

```mermaid {scale: 0.5}
sequenceDiagram
    Alice->John: Hello John, how are you?
    Note over Alice,John: A typical interaction
```

```mermaid {theme: 'neutral', scale: 0.8}
graph TD
B[Text] --> C{Decision}
C -->|One| D[Result 1]
C -->|Two| E[Result 2]
```

```plantuml {scale: 0.7}
@startuml

package "Some Group" {
  HTTP - [First Component]
  [Another Component]
}

node "Other Groups" {
  FTP - [Second Component]
  [First Component] --> FTP
}

cloud {
  [Example 1]
}


database "MySql" {
  folder "This is my folder" {
    [Folder 3]
  }
  frame "Foo" {
    [Frame 4]
  }
}


[Another Component] --> [Example 1]
[Example 1] --> [Folder 3]
[Folder 3] --> [Frame 4]

@enduml
```

</div>

[Learn More](https://sli.dev/guide/syntax.html#diagrams)


---
layout: center
class: text-center
---

# Learn More

[Documentations](https://sli.dev) · [GitHub](https://github.com/slidevjs/slidev) · [Showcases](https://sli.dev/showcases.html)
