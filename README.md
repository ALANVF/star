# Star

Star is an experimental programming language that's made to be powerful, productive, and predictable:
- Powerful: Get stuff done with less boilerplate
- Productive: Get stuff done quickly without working against the language
- Predictable: Get stuff done without having to think about it twice

One of the most important goals of Star is that it's designed to be completely consistent. In other words, Star has been designed to not have edge cases, footguns, or any sort of "magic" that's exclusive to language built-ins.


## Primary features

- A vast and unlimited type system and type hierarchy
	- Multiple inheritance allows for more code reuse
	- Kinds (variants) can be used to represent a fixed selection of values as well as specific arrangements of data
		- Kinds may inherit from classes, protocols, and even other kinds
		- Kinds may have instance members like classes do, which are accessible from any kind value
		- Multi-kinds can be used to represent a set of various flags, options, and choices without needing sets or hashmaps
	- Generics allow types and methods to be compatible with different types without code duplication
		- Generic constraints can require specific types, supertypes, subtypes, and shapes (members / methods), all of which can reference other generics
		- Multiple constraints are allowed on a generic
		- Generics can require a specific number of type parameters, which allows for powerful constructs like higher-kinded types
	- Type specialization and type refinement can allow for multi-dispatch, and enable certain functionality depending on the types given
	- Categories are named type extensions that can add new functionality to existing types at compile-time
- Rich standard library comes with the essentials, many data structures, and other common functionality
	- ...and some nuclear batteries (in case you needed them)
- Low-level *and* high-level
	- Writing low-level code shouldn't be a pain
	- Writing high-level code shouldn't require five layers of abstraction


## Other notable features

- Unified type conversion system makes magical methods like `toString` a thing of the past
- Cascades and nested cascades completely eliminate the need for temporary variables and builder types
- Pattern matching on classes, kinds, and other data structures allows for efficient and concise decision-making code
- Block expressions allow for statements to be used within an expression
- Structured loop constructs reduce off-by-one errors


### Syntax

Before you click away because of how weird the syntax might look, please just give Star a chance. It may seem weird at first,
but every aspect of Star's syntax has a purpose. None of it exists "just for decoration" or anything ridiculous like that.
Rather, the syntax is made to be completely unambiguous, which means that some things have to look different (within reason)
in order to be distinguishable.

For example, Star uses `#{...}` for tuples instead of `(...)`. This is because `(...)` is also used for grouping expressions.
The fact that `(1)` produces an integer might be surprising for those who were expecting a single-element tuple.
Most languages use the syntax `(1,)` for single-element tuples in order to solve the ambiguity issue, however this results in
an edge case, which then creates more issues and inconsistency. By explicitly distinguishing tuple syntax from grouping syntax,
all issues caused by the inconsistency and edge case have been resolved.

"Wow that's dumb", right? Well maybe you don't care about these kinds of issues. However these issues can add up, and make
languages increasingly difficult to learn and incredibly annoying to use. This ends up benefiting nobody and only frustrates
the programmer even more.


### Example


```swift
module Main {
	on [main] {
		Core[say: "Welcome to Star!"]
	}
}
```


### Running Star

Things you'll need in order to run Star (or rather, the parser. the compiler and runtime don't exist yet):
- Haxe (4.1.4 or later)
- [My fork of HashLink](https://github.com/ALANVF/hashlink/tree/star)


### Thinking ahead

I've always loved the tooling and features provided by Smalltalk languages like Pharo and Self, but I've never liked using them
because it's all restricted to the VM. I'd like to eventually have the same kind of tooling for Star, but make it all "pluggable"
through an API so that it can be used from other editors, IDEs, languages, build tools, etc. It sounds like wishful thinking, but I'm
sure it could somehow be possible once Star gains more traction.


### Resources

- Language specification (most of it): https://github.com/ALANVF/Star-lang-specification