" Vim syntax file
" Language:   Star
" Maintainer: theangryepicbanana
" Filenames:  *.star

if exists("b:current_syntax")
	finish
endif

syn match     starStrEscape   contained ?\\\(\\\|"\|[0nrtveab]\|x[a-fA-F0-9]\+\|o[0-7]\{1,3}\|u\d\{4}\)?
syn region    starStrEscNest  matchgroup=starStrEscOp start="\\(" matchgroup=starStrEscOp end=")" contained contains=@starRules

syn match     starSep         ","

" Comments
syn match     starLineComment ";\([^[].*\)\?$"
syn region    starCommentNest start="\(;\)\@<!\["  end="\]" transparent contained
syn region    starComment     start=";\[" end="\]" fold contains=starCommentNest

" Other stuff
syn keyword   starConstant    true false this

syn match     starLabel       /\v<[a-z_][a-zA-Z0-9_]*(:)@=/
syn match     starName        /\v<([a-z][a-zA-Z0-9_]*|_[a-zA-Z0-9_]+)(:)@!>/
syn match     starType        "\<[A-Z][a-zA-Z0-9_]*\>\|\<_\>"
syn match     starTag         "\#[a-z_][a-zA-Z0-9_]*\>"
syn match     starMacroName   "@[a-z_][a-zA-Z0-9_]*\>"
syn region    starLitSym      start="`" end="`"

syn match     starDec         "[+-]\?\d\+\.\d\+\([eE][+-]\?\d\+\(\.\d\+\)\?\)\?"
syn match     starInt         "[+-]\?\d\+\(\.\)\@!\([eE][+-]\?\d\+\(\.\d\+\)\?\)\?"
syn match     starInt         "\<0[xX][a-fA-F0-9]\+"
syn region    starStr         start=+"+ skip=+\\"+ end=+"+ contains=starStrEscape,starStrEscNest

" Wrappers. Add custom thingy for Funcs once I figure out how to do it
syn region    starArray       start="#\[" end="\]" transparent fold
syn region    starHash        start="#("  end=")"  transparent fold
syn region    starParen       start="("   end=")"  transparent fold
syn region    starGroup       start="\["  end="\]" transparent fold
syn region    starBlock       start="{"   end="}"  transparent fold

syn keyword   starAttribute   contained static hidden readonly friend sealed
syn keyword   starAttribute   contained unordered getter setter main inline noinherit
syn keyword   starAttribute   contained asm native macro
syn keyword   starAttribute   contained flags uncounted strong

syn keyword   starKeyword     module my on return init deinit operator
syn keyword   starKeyword     class alias type kind category protocol
syn keyword   starKeyword     is of use has

" might work?
syn region    starIsAttribute start="\(\<is\s\+\)\@<=" end="\s\+\|$" contains=starAttribute

syn match     starCoreword    /\v<(if|else|while|for|recurse|do|case|match|at|break|next|throw|try|catch|new)(:)@!>/

syn cluster   starRules       contains=starSep,starComment,starConstant,starLabel,starName,starType,starTag,starMacroName,starLitsym,starDec,starInt,starStr,starArray,starHash,starParen,starGroup,starBlock,starKeyword,starCoreword

hi def link   starStrEscape   SpecialChar
hi def link   starStrEscOp    SpecialChar

hi def link   starSep         Keyword

hi def link   starLineComment Comment
hi def link   starComment     Comment

hi def link   starConstant    Constant

hi def link   starLabel       Function
hi def link   starName        Normal
hi def link   starType        Type
hi def link   starTag         Tag
hi def link   starMacroName   Special
hi def link   starLitSym      PreProc

hi def link   starDec         Number
hi def link   starInt         Number
hi def link   starStr         String

hi def link   starArray       Operator
hi def link   starHash        Operator
hi def link   starParen       Operator
hi def link   starGroup       Operator
hi def link   starBlock       Operator

hi def link   starAttribute   Underlined
hi def link   starKeyword     Statement
hi def link   starCoreword    Keyword