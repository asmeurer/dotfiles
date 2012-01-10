" Vim color file based on XCode's midnight color scheme and vim's slate
" Add below line to your vimrc file to ensure optimal experience (sets term to use
" 256, instead of 16, colors):
"   set t_Co=256
"
" Maintainer:	Aaron Meurer <asmeurer@gmail.com>

hi clear
set background=dark
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "icansee"

"color settings for these terminal types:
"Black		term=NONE cterm=NONE ctermfg=0 ctermbg=0
"DarkRed	term=NONE cterm=NONE ctermfg=1 ctermbg=0
"DarkGreen	term=NONE cterm=NONE ctermfg=2 ctermbg=0
"Brown		term=NONE cterm=NONE ctermfg=3 ctermbg=0
"DarkBlue	term=NONE cterm=NONE ctermfg=4 ctermbg=0
"DarkMagenta	term=NONE cterm=NONE ctermfg=5 ctermbg=0
"DarkCyan	term=NONE cterm=NONE ctermfg=6 ctermbg=0
"Gray		term=NONE cterm=NONE ctermfg=7 ctermbg=0
"DarkGray	term=NONE cterm=bold ctermfg=0 ctermbg=0
"Red		term=NONE cterm=bold ctermfg=1 ctermbg=0
"Green		term=NONE cterm=bold ctermfg=2 ctermbg=0
"Yellow		term=NONE cterm=bold ctermfg=3 ctermbg=0
"Blue		term=NONE cterm=bold ctermfg=4 ctermbg=0
"Magenta	term=NONE cterm=bold ctermfg=5 ctermbg=0
"Cyan		term=NONE cterm=bold ctermfg=6 ctermbg=0
"White		term=NONE cterm=bold ctermfg=7 ctermbg=0

SpecialKey     xxx term=bold ctermfg=2 guifg=yellowgreen
NonText        xxx term=bold cterm=bold ctermfg=12 gui=bold guifg=RoyalBlue guibg=grey15
Directory      xxx term=bold ctermfg=6 guifg=Cyan
ErrorMsg       xxx term=standout cterm=bold ctermfg=7 ctermbg=1 guifg=White guibg=Red
IncSearch      xxx term=reverse ctermfg=11 ctermbg=10 gui=reverse guifg=green guibg=black
Search         xxx term=reverse ctermfg=248 ctermbg=12 guifg=wheat guibg=peru
MoreMsg        xxx term=bold ctermfg=2 gui=bold guifg=SeaGreen
ModeMsg        xxx term=bold ctermfg=130 gui=bold guifg=goldenrod
LineNr         xxx term=underline ctermfg=3 guifg=grey50
Question       xxx term=standout ctermfg=10 gui=bold guifg=springgreen
StatusLine     xxx term=bold,reverse cterm=bold,reverse guifg=black guibg=#c2bfa5
StatusLineNC   xxx term=reverse cterm=reverse guifg=grey40 guibg=#c2bfa5
VertSplit      xxx term=reverse cterm=reverse guifg=grey40 guibg=#c2bfa5
Title          xxx term=bold cterm=bold ctermfg=11 gui=bold guifg=gold
Visual         xxx term=reverse cterm=reverse ctermbg=242 guifg=khaki guibg=olivedrab
VisualNOS      xxx term=bold,underline cterm=bold,underline gui=bold,underline
WarningMsg     xxx term=standout ctermfg=1 guifg=salmon
WildMenu       xxx term=standout ctermfg=0 ctermbg=3 guifg=Black guibg=Yellow
Folded         xxx term=standout ctermfg=248 ctermbg=242 guifg=grey40 guibg=black
FoldColumn     xxx term=standout ctermfg=4 ctermbg=7 guifg=grey20 guibg=black
DiffAdd        xxx term=bold ctermbg=4 guibg=DarkBlue
DiffChange     xxx term=bold ctermbg=5 guibg=DarkMagenta
DiffDelete     xxx term=bold cterm=bold ctermfg=4 ctermbg=6 gui=bold guifg=Blue guibg=DarkCyan
DiffText       xxx term=reverse cterm=bold ctermbg=1 gui=bold guibg=Red
SignColumn     xxx term=standout ctermfg=14 ctermbg=242 guifg=Cyan guibg=Grey
Conceal        xxx ctermfg=7 ctermbg=242 guifg=LightGrey guibg=DarkGrey
SpellBad       xxx term=reverse ctermbg=9 gui=undercurl guisp=Red
SpellCap       xxx term=reverse ctermbg=12 gui=undercurl guisp=Blue
SpellRare      xxx term=reverse ctermbg=13 gui=undercurl guisp=Magenta
SpellLocal     xxx term=underline ctermbg=14 gui=undercurl guisp=Cyan
Pmenu          xxx ctermbg=13 guibg=Magenta
PmenuSel       xxx ctermbg=242 guibg=DarkGrey
PmenuSbar      xxx ctermbg=248 guibg=Grey
PmenuThumb     xxx cterm=reverse gui=reverse
TabLine        xxx term=underline cterm=underline ctermfg=15 ctermbg=242 gui=underline guibg=DarkGrey
TabLineSel     xxx term=bold cterm=bold gui=bold
TabLineFill    xxx term=reverse cterm=reverse gui=reverse
CursorColumn   xxx term=reverse ctermbg=242 guibg=Grey40
CursorLine     xxx term=underline cterm=underline guibg=Grey40
ColorColumn    xxx term=reverse ctermbg=1 guibg=DarkRed
MatchParen     xxx term=reverse ctermbg=6 guibg=DarkCyan
Comment        xxx term=bold ctermfg=11 guifg=grey40
Constant       xxx term=underline ctermfg=130 guifg=#ffa0a0
Special        xxx term=bold ctermfg=130 guifg=darkkhaki
Identifier     xxx term=underline cterm=bold ctermfg=9 guifg=salmon
Statement      xxx term=bold ctermfg=81 gui=bold guifg=CornflowerBlue
PreProc        xxx term=underline ctermfg=9 guifg=red guibg=white
Type           xxx term=underline ctermfg=2 gui=bold guifg=CornflowerBlue
Underlined     xxx term=underline cterm=underline ctermfg=5 gui=underline guifg=#80a0ff
Ignore         xxx cterm=bold ctermfg=7 guifg=grey40
Error          xxx term=reverse cterm=bold ctermfg=7 ctermbg=1 guifg=White guibg=Red
Todo           xxx term=standout ctermfg=0 ctermbg=11 guifg=orangered guibg=yellow2
String         xxx ctermfg=6 guifg=SkyBlue
Character      xxx links to Constant
Number         xxx links to Constant
Boolean        xxx links to Constant
Float          xxx links to Number
Function       xxx ctermfg=130 guifg=navajowhite
Conditional    xxx links to Statement
Repeat         xxx links to Statement
Label          xxx links to Statement
Operator       xxx ctermfg=9 guifg=Red
Keyword        xxx links to Statement
Exception      xxx links to Statement
Include        xxx ctermfg=9 guifg=red
Define         xxx ctermfg=11 gui=bold guifg=gold
Macro          xxx links to PreProc
PreCondit      xxx links to PreProc
StorageClass   xxx links to Type
Structure      xxx ctermfg=10 guifg=green
Typedef        xxx links to Type
Tag            xxx links to Special
SpecialChar    xxx links to Special
Delimiter      xxx links to Special
SpecialComment xxx links to Special
Debug          xxx links to Special
Normal         xxx guifg=White guibg=grey15
Cursor         xxx guifg=slategrey guibg=khaki
SpellErrors    xxx cterm=bold ctermfg=7 ctermbg=1 guifg=White guibg=Red
"hi Comment	term=bold		ctermfg=Blue						guifg=Blue
"hi Comment	term=bold		ctermfg=DarkGray					guifg=DarkGray
"hi Constant	term=underline		ctermfg=DarkGreen			gui=NONE	guifg=DarkGreen
"hi Cursor									guibg=fg	guifg=Orchid
"hi Directory	term=bold		ctermfg=Cyan						guifg=Cyan
"hi Error	term=reverse		ctermbg=Red		ctermfg=White	guibg=Red	guifg=White
"hi ErrorMsg	term=standout		ctermbg=DarkRed		ctermfg=White	guibg=Red	guifg=White
"hi Identifier	term=underline		ctermfg=Cyan						guifg=Cyan
"hi Ignore				ctermfg=Black						guifg=bg
"hi IncSearch	term=reverse		cterm=reverse				gui=reverse
"hi LineNr	term=underline		ctermfg=DarkYellow					guifg=Yellow
"hi ModeMsg	term=bold		cterm=bold				gui=bold
"hi MoreMsg	term=bold		ctermfg=Green				gui=bold	guifg=SeaGreen
"hi NonText	term=bold		ctermfg=DarkGreen			gui=bold	guifg=DarkGreen
"hi Normal				ctermbg=Black		ctermfg=Gray	guibg=Black	guifg=Gray
"hi PreProc	term=underline		ctermfg=White						guifg=White
"hi Question	term=standout		ctermfg=Green				gui=bold	guifg=Green
"hi Search				ctermbg=Magenta		ctermfg=White	guibg=Magenta	guifg=White
"hi Special	term=bold		ctermfg=Red						guifg=Red
"hi SpecialKey	term=bold		ctermfg=Green						guifg=Green
"hi Statement	term=bold		ctermfg=Yellow				gui=NONE	guifg=Yellow
"hi StatusLine	term=reverse,bold 	cterm=reverse				gui=reverse
"hi StatusLineNC term=reverse		cterm=reverse				gui=reverse
"hi Title	term=bold		ctermfg=Magenta				gui=bold	guifg=Pink
"hi Todo		term=standout		ctermbg=DarkYellow	ctermfg=Black	guibg=Yellow	guifg=Black
"hi Type					ctermfg=Green				gui=NONE	guifg=Green
"hi Visual	term=reverse		cterm=reverse				guibg=DarkGreen	guifg=White
"hi WarningMsg	term=standout		ctermfg=Red						guifg=Red
