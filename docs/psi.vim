" Vim syntax file
" Language: Psi
" ----------------------------------------------------------------------------

if !exists('main_syntx')
	if exists("b:current_syntax")
		finish
	endif
	let main_syntax = 'psi'
endif

scriptencoding utf-8

let b:current_syntax = "psi"


function! s:ContainedGroup()
	try
		silent syn list @psi
		return '@psi,@NoSpell'
	catch /E392/
		return 'TOP,@Spell'
	endtry
endfunction

unlet! b:current_syntax

syn case match
syn sync minlines=200 maxlines=1000

" ---------------------------------------------------------------------------- "
"												KEYWORDS												 "
" abs			abstract																				 "
" case																								 "
" catch																								 "
" cls			class																					 "
" else																								 "
" enum																								 "
" false																								 "
" fin			final																					 "
" finally																							 "
" fn			function																				 "
" if																									 "
" let																									 "
" match																								 "
" new																									 "
" null																								 "
" obj			object																				 "
" op			operation																			 "
" ovr			override																				 "
" pkg			package																				 "
" pro			program																				 "
" pro			protected																			 "
" pub			public																				 "
" ret			return																				 "
" seal		sealed																				 "
" super																								 "
" this																								 "
" throw																								 "
" trait																								 "
" true																								 "
" try																									 "
" use																									 "
" val																									 "
" var																									 "
" while																								 "
" yield																								 "
"												WILDCARDS											 "
" ?																									 "
" ..																									 "
"												COMMENTS												 "
" //																									 "
" /*  */																								 "
" ---------------------------------------------------------------------------- "

syn keyword psiKeyword catch else finally match throw try while yield if
syn keyword psiKeyword cls trait obj enum					nextgroup=psiInstanceDeclare 		  skipwhite
syn keyword psiKeyword case									nextgroup=psiCaseFollowing 		  skipwhite
syn keyword psiKeyword val var								nextgroup=psiNameDefine				  skipwhite
syn keyword psiKeyword op let ret 							nextgroup=psiKeyword,psiNameDefine skipwhite
syn keyword psiKeyword fn										nextgroup=psiSpecial2,psiNameDefine skipwhite
hi def link psiKeyword Keyword

exe 'syn region psiBlock start=/{/ end=/}/ contains=' . s:ContainedGroup() . ' fold'

syn keyword psiUnimportant Void
syn match psiUnimportant "\."
syn match psiUnimportant ","
syn match psiUnimportant ";"
syn match psiUnimportant ":"
syn match psiUnimportant "!"
syn match psiUnimportant "@"
syn match psiUnimportant "#"
syn match psiUnimportant "%"
syn match psiUnimportant "\^"
syn match psiUnimportant "&"
syn match psiUnimportant "\*"
syn match psiUnimportant "-"
syn match psiUnimportant "+"
syn match psiUnimportant "="
syn match psiUnimportant "/"
syn match psiUnimportant "\\"
syn match psiUnimportant "|"
syn match psiUnimportant "<"
syn match psiUnimportant ">"
syn match psiWildcard "\.\."
syn match psiWildcard "?"
syn match psiError "\*\/"
hi def link psiUnimportant Comment
hi def link psiWildcard Macro


syn match psiChar /'.'/
syn match psiChar /'\\[\\"'ntbrf]'/ 		contains=psiEscapedCh
syn match psiChar /'\\u[a-fA-F0-9]\{4}'/	contains=psiUnicodeCh
syn match psiEscapedCh /\\[\\"'ntbrf]/
syn match psiUnicodeCh /\\u[a-fA-F0-9]\{4}/
hi def link psiChar Character
hi def link psiEscapedCh Special
hi def link psiUnicodeCh Special


syn match psiNameDefine /\<[a-zA-Z0-9_]\+\>/ contained nextgroup=psiPostNameDef,psiVarDeclareList
syn match psiNameDefine /`[^`]\+`/				contained nextgroup=psiPostNameDef
syn match psiVarDeclareList /\s*,\s*/			contained nextgroup=psiNameDefine  contains=psiUnimportant
syn match psiPostNameDef /\_s*:\_s*/ 			contained nextgroup=psiTypeDeclare contains=psiUnimportant
syn match psiExtendedNameDef /[a-zA-Z0-9_]*\./ contained nextgroup=psiExtendedNameDef contains=psiUScoreWord,psiCapitalizedWord
hi def link psiNameDefine Function
hi def link psiExtendedNameDef psiUnimportant


syn match psiInstanceDeclare /\<[_\.a-zA-Z0-9]\+\>/ contained nextgroup=psiInstancehash,psiPostInstanceDeclare
syn match psiInstanceDeclare /`[^`]\+`/				 contained
syn match psiInstanceInherit /\<[_\.a-zA-Z0-9]\+\>/ contained nextgroup=psiIInstancehash,psiPostInstanceInherit
syn match psiInstanceHash /#/								 contained nextgroup=psiInstanceDeclare
syn match psiIInstanceHash /#/							 contained nextgroup=psiInstanceInherit
syn match psiPostInstanceDeclare /\_s*:\_s*/			 contained nextgroup=psiInstanceInherit contains=psiUnimportant
syn match psiPostInstanceInherit /\s*,\s*/			 contained nextgroup=psiInstanceInherit contains=psiunimportant
hi def link psiInstanceDeclare Special
hi def link psiInstanceInherit Special
hi def link psiIInstanceHash psiInstanceHash
hi def link psiInstanceHash psiUnimportant


syn match psiUScoreWord /\<[A-Z_][A-Z0-9_]*\>/
syn match psiCapitalizedWord /\<[A-Z][a-zA-Z0-9_]*\>/
syn match psiStatement /[A-Z0-9_]*?/
syn match psiStatement /[A-Z0-9_]*!/
hi def link psiStatement Statement
hi def link psiUScoreWord PreProc
hi def link psiCapitalizedWord Special


syn region psiTypeState	matchgroup=Keyword start=/\<type\_s\+\ze/ end=/$/
			\ contains=psiTTDeclare,psiBracket,psiTTEquals,psiTState
syn match psiTTDeclare /(/ 													contained nextgroup=psiTTExtension,psiTTEquals contains=psiParen			skipwhite
syn match psiTTDeclare /\%(=>\)\ze/											contained nextgroup=psiTTDeclare					  contains=psiTTExtension	skipwhite
syn match psiTTDeclare /\<[_\.a-zA-z0-9]\+\>/							contained nextgroup=psiTTExtension,psiTTEquals 									skipwhite
syn match psiTTEquals /=\ze[^>]/												contained nextgroup=psiTTPostDeclare 												skipwhite
syn match psiTTExtension /)\?\_s*\zs\%(=>\|<:\|>:\|=:=\|::\|#\)/	contained nextgroup=psiTTDeclare					  contains=psiTypeOperator skipwhite
syn match psiTTPostDeclare /\<[_\.a-zA-Z0-9]\+\>/						contained nextgroup=psiTTPostExtension												skipwhite
syn match psiTTPostExtension /\%(=>\|<:\|>:\|=:=\|::\)/				contained nextgroup=psiTTPostDeclare			  contains=psiTypeOperator skipwhite
hi def link psiTTDeclare Type
hi def link psiTTExtension Keyword
hi def link psiTTPostDeclare Special
hi def link psiTTPostExtension Keyword


syn match psiTAnnotation /\%([_a-zA-Z0-9\s]:\_s*\)\ze[_=(\.a-zA-Z0-9]\+/ nextgroup=psiTypeDeclare contains=psiParen skipwhite
syn match psiTAnnotation /)\_s*:\_s*\ze[_=(\.a-zA-Z0-9]\+/					 nextgroup=psiTypeDeclare skipwhite
hi clear psiTAnnotation

syn match psiCaseFollowing  /\<[_\.a-zA-Z0-9]\+\>/ contained contains=psiCapitalizedWord
syn match psiCaseFollowing /`[^`]\+`/					contained contains=psiCapitalizedWord
hi def link psiCaseFollowing Special


syn keyword psiKeyModifier abs ovr fin pub seal super
hi def link psiKeyModifier Function


syn match psiKeyModifier /\<pro\>\(\s\+\(cls\|trait\|obj\|enum\|val\|var\|op\|let\|fn\|abs\|ovr\|fin\|pub\|seal\|super\)\)\@=/
syn match psiKeyword 	 /\<pro\>\(\s\+\(cls\|trait\|obj\|enum\|val\|var\|op\|let\|fn\|abs\|ovr\|fin\|pub\|seal\|super\)\)\@!/	nextgroup=psiNameDefine skipwhite


syn keyword psiSpecial this true false null
syn keyword psiSpecial2 new nextgroup=psiNameDefine skipwhite
syn match psiSpecial "\%(=>\|<-\|->\)"
syn match psiSpecial /`[^`]\+`/
hi def link psiSpecial PreProc
hi def link psiSpecial2 psiSpecial


syn keyword psiExternal pkg use nextgroup=psiExtendedNameDef skipwhite
hi def link psiExternal Include


syn match psiStrEmbedded /\\"/ contained
syn region psiString start=/"/ end=/"/ contains=psiStrEmbedded,psiEscapedCh,psiUnicodeCh
hi def link psiString String
hi def link psiStrEmbedded String


syn region psiInStr matchgroup=psiInBrack start=/\<[a-zA-Z][a-zA-Z0-9_]*"/
			\ skip=/\\"/ end=/"/ contains=psiInterp,psiEscapedCh,psiUnicodeCh
hi def link psiInStr String


hi def link psiInBrack Special
hi def link psiInBound Function


syn match psiNumber /\<0[dDfFlL]\?\>/
syn match psiNumber /\<[1-9]\d*[dDfFlL]\?\>/
syn match psiNumber /\<0[xX][0-9a-fA-F]\+[dDfFlL]\?\>/
syn match psiNumber /\<0[bB][01]\+[dDfFlL]\?\>/
hi def link psiNumber Number


syn region psiParen start="(" end=")" skipwhite contained contains=psiTypeDeclare,psiBrack,psiParen,psiUnimportant
syn region psiBrack matchgroup=psiBrackB start="\[" end="\]" skipwhite nextgroup=psiTExtension
			\ contains=psiTypeDeclare,psiBrack,psiTypeOp,psiTAnnotationParam,psiString
syn match psiTypeOp /[-+=:<>]\+/ contained
syn match psiTAnnotationParam /@\<[`_a-zA-Z0-9]\+\>/ contained
syn match psiParenM /[(){}]/
hi def link psiParenM Comment
hi def link psiBrackB Type
hi def link psiTypeOp Keyword
hi def link phiTAnnotationParam Function


syn match psiMetadata "\%^#!.*" display
syn match psiMetadata "^#.*" display
syn region psiMultilineComment start="/\*" end="\*/" keepend fold
			\ contains=psiMultilineComment,psiTodo,@Spell
syn match psiTodo "\vTODO|FIXME|XXX|AAAA" contained
hi def link psiMetadata Comment
hi def link psiMultilineComment Comment
hi def link psiTodo Todo



syn match psiAnnotation /@\<[`_a-zA-Z0-9.]\+\>/
hi def link psiAnnotation PreProc

syn match psiInlineComment "//.*$" contains=psiTodo,@Spell
hi def link psiInlineComment Comment

" ----------------------------------------------------------------------------

syn keyword psiBuiltinFunction println print prinf assert
syn keyword psiImportant System Thread Qubit
hi def link psiBuiltinFunction Function
hi def link psiImportant PreProc

" ----------------------------------------------------------------------------
let b:current_syntax = 'psi'
if main_syntax ==# 'psi'
	unlet main_syntax
endif
