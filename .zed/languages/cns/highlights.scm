; CNS Syntax Highlighting for Tree-sitter
; Sections (Story, Given, Step, End)
(section_story) @keyword
(section_given) @keyword
(section_step) @keyword
(section_end) @keyword

; Keywords and narrative markers
(narrative_keyword) @keyword.control
"Story:" @keyword
"Given:" @keyword
"Step" @keyword
"End:" @keyword
"Because:" @keyword.control
"Then:" @keyword.control
"Effect:" @keyword.control
"When" @keyword.control
"If" @keyword.control
"Otherwise:" @keyword.control
"Return" @keyword.control

; Causal arrows
"→" @operator
"←" @operator
"↔" @operator

; Operators
"=" @operator
"+" @operator
"-" @operator
"*" @operator
"/" @operator
">" @operator
"<" @operator
">=" @operator
"<=" @operator
"!=" @operator

; Numbers
(number) @number

; Strings
(string) @string
(escape_sequence) @string.escape

; Variables and identifiers
(variable_name) @variable
(type_annotation) @type

; Semantic tags
(semantic_tag) @attribute

; Variable substitution
"{" @punctuation.special
"}" @punctuation.special

; Comments
(comment) @comment

; Functions and builtins
"socket-listen" @function.builtin
"socket-accept" @function.builtin
"socket-receive" @function.builtin
"socket-send" @function.builtin
"socket-close" @function.builtin
"print" @function.builtin
"log" @function.builtin

; Step numbers
(step_number) @number.literal

; Punctuation
":" @punctuation.delimiter
"," @punctuation.delimiter
"." @punctuation.delimiter
