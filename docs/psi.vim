" Psi vim/neovim syntax highlighting




syntax clear



" Declares and primitive types
syn keyword declares val var pro sub co fn op package obj class trait type mod
syn keyword types Byte Short Int Long Float Double Char String Bit Null
syn keyword conditional if else match case
syn keyword repeat for while 
syn keyword exception try catch finally throw
syn keyword modifier abstract final private inherited override extends as sealed
syn keyword classes new this super
syn keyword misc ret import
highlight link declares Statement
highlight link types Type
highlight link conditional Conditional 
highlight link repeat Repeat
highlight link exception Exception
highlight link modifier Keyword
highlight link classes Macro
highlight link misc Macro

" Highlighting the declares
syn match valdef "\(^val\s\)\@<=\w\+"
syn match vardef "\(^var\s\)\@<=\w\+"
syn match prodef "\(^pro\s\)\@<=\w\+"
syn match subdef "\(^sub\s\)\@<=\w\+"
syn match codef "\(^co\s\)\@<=\w\+"
syn match fndef "\(^fn\s\)\@<=\w\+"
syn match opdef "\(^op\s\)\@<=\w\+"
syn match objdef "\(^obj\s\)\@<=\w\+"
syn match classdef "\(^class\s\)\@<=\w\+"
syn match traitdef "\(^trait\s\)\@<=\w\+"
syn match typedef "\(^type\s\)\@<=\w\+"
syn match moddef "\(^mod\s\)\@<=\w\+"
highlight link valdef Function
highlight link vardef Function
highlight link prodef Function
highlight link subdef Function
highlight link codef Function
highlight link fndef Function
highlight link opdef Function
highlight link objdef Function
highlight link classdef Function
highlight link traitdef Function
highlight link typedef Function
highlight link moddef Function


" Punctuation
syn match dot			"\."
syn match comma		","
syn match semicolon	";"
syn match uScore		"_"
syn match tilde		"\~"
syn match equals		"="
syn match colon		":"
syn match optLine "^>.*"
highlight link dot			Operator
highlight link comma			Operator
highlight link semicolon	Operator
highlight link uScore		Operator
highlight link tilde			Operator
highlight link optLine		Operator
highlight link equals	Macro
highlight link colon		Macro
highlight link asterisk	Macro


" Comments
syn match single_comment "//.*$"
syn region multi_comment start="/\/\*/" end="/\*\//"
highlight link single_comment Comment
highlight link multi_comment Comment


" Literals
syn region string start="\"" end="\"" skip="\\\""
syn keyword boolean true false
syn keyword null null
syn match int "\v<\d+>"
syn match float "\v<\d+\.\d+>"
syn match hex "\v<0x\x+([Pp]-?)?\x+>"
syn match bin "\v<0b[01]+>"
highlight link string String
highlight link boolean Define
highlight link null Define
highlight link int Number
highlight link hex Number
highlight link bin Number
highlight link float Float
