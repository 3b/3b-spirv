SPIR-V assembler with lispy syntax.

### current usage (subject to change):

```lisp

(ll-assemble
 '((spirv-core:capability :shader)
   (spirv-core:ext-inst-import :glsl "GLSL.std.450")
   (spirv-core:memory-model :logical :glsl-450)
   (spirv-core:entry-point :fragment main main input1 input2 output1 ...)
   (spirv-core:execution-mode main :origin-lower-left)
   (spirv-core:source :glsl 450)
   ...
   (spirv-core:type-void :void)
   (spirv-core:type-function |void()| :void)
   ...
   (spirv-core:function main :void :none |void()|)
   ...
   (spirv-core:function-end)
   ))

;; -> (unsigned-byte 32) vector containing assembled output

```

Opcodes are in package `spirv-core` with `Op` removed and `-`
separator instead of capital letters, for example `OpExtInstImport` ->
`spirv-core:ext-inst-import`.

Result `<id>` is passed as first argument when applicable, followed by corresponding Result Type `<id>` if applicable, followed by any remaining arguments.

Currently `"GLSL.std.450"` is the only extension supported, symbols are in package `spirv-glsl` and renamed similarly.

Spirv `<id>`s are symbols, for example `:glsl`,`main`,`input1`, etc. above.

Enumerated constants are keywords, as in `:shader`,`:logical`,`:glsl-450` above. (Note that `:glsl` is just a symbol being used to name an `<id>`, not an enumerant).

Bit mask constants are either a keyword or list of keywords, for example `:none` above, or something like `(:inline :pure)`. (`:none` or `()`/`NIL` can be used to represent no bits set).

Extension opcodes look like

```lisp
   (spirv-core:ext-inst <dest> <type> :glsl spirv-glsl450:sqrt <arg...>)
```

which would call the opcode `sqrt` in the imported extension named by the `<id>` `:glsl`


### Current status

Assembles the [example](https://www.khronos.org/registry/spir-v/specs/1.1/SPIRV.html#_example) from the spec correctly as far as I can tell. The code to assemble opcodes is generated automatically, so there are probably still some instructions it doesn't get right. (There are probably some more it gets wrong due to problems with the json spec).

Some attempt at validation/error checking is made, but is far from complete.