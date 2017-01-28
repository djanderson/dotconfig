" Turn on line numbers
set nu!
" Show cursor line
set cul
set autoindent smarttab
set noexpandtab tabstop=4 shiftwidth=4
set guifont=Monospace\ 14
set nohlsearch
set spelllang=en
nnoremap <F5> :set invpaste paste?<Enter>
imap <F5> <C-O><F5>
set pastetoggle=<F5>
colors ron
syntax on
set nobackup

" Set options if using spaces for indents (default).
function PySpacesCfg()
  set expandtab
  set tabstop=8
  set softtabstop=4
  set shiftwidth=4
endfunction

" Set options if using tabs for indents.
function PyTabsCfg()
  set noexpandtab
  set tabstop=4
  set softtabstop=4
  set shiftwidth=4
endfunction

function PyFileCfg()
  set smartindent
  set cinwords=if,elif,else,for,while,try,except,finally,def,class,with
  " <f2> key runs the current file in the Python interpreter
  map <f2> :w\|!python %<cr>
endfunction

" Return 1 if python in shebang line
function IsPythonFile()
  let firstline = getline(1)
  let filename = bufname("%")
  if firstline =~ "#!.*python.*"
  	return 1
  elseif filename =~ ".*\.py$"
    return 1
  else
    return 0
  endif
endfunction

" Return 1 if using tabs for indents, or 0 otherwise.
function IsTabIndent()
  let lnum = 1
  let got_cols = 0  " 1 if previous lines ended with columns
  while lnum <= 100
    let line = getline(lnum)
    let lnum = lnum + 1
    if got_cols == 1
      if line =~ "^\t\t"  " two tabs to prevent false positives
        return 1
      endif
    endif
    if line =~ ":\s*$"
      let got_cols = 1
    else
      let got_cols = 0
    endif
  endwhile
  return 0
endfunction

" Check current buffer and configure for tab or space indents.
function PyAutoCfg()
  if IsPythonFile()
    call PyFileCfg()
  endif
  if IsTabIndent()
    call PyTabsCfg()
  else
    call PySpacesCfg()
  endif
endfunction

" Hit \s to autocomplete global substitute with word under cursor
:nnoremap <Leader>s :%s/<C-r><C-w>/
:nnoremap <Leader>S :%s/\<<C-r><C-w>\>/

" Makes space let you insert a single character.
nmap <space> i_<esc>r

" Makes K split lines (the opposite of J).
nmap K i<cr><esc>k$

" Allow toggling between local and default mode
function TabToggle()
  if &expandtab
    echo "Not expanding tabs"
    set noexpandtab
  else
    echo "Expanding tabs"
    set expandtab
  endif
endfunction
nmap <F9> mz:execute TabToggle()<CR>'z

autocmd BufRead * call PyAutoCfg()
