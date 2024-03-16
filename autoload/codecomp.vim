hi def CodecompGhosttext guifg=#808080 ctermfg=7

let s:hlgroup = 'CodecompGhosttext'

if empty(prop_type_get(s:hlgroup))
  call prop_type_add(s:hlgroup, {'highlight': s:hlgroup})
endif

" TODO: move this to autoload directory
function! codecomp#Suggest(...)
  call codecomp#Reject()
  let l:code = codecomp#read_previous()
  let l:response = codecomp#langchain_invoke(l:code)
  call codecomp#show_suggestion(l:response)
endfunction

function! codecomp#Accept(...)
  if !get(s:, 'suggestion_line', v:false)
    return
  endif

  " remove the suggestion text
  call codecomp#clear_suggestion()

  let l:text = split(remove(s:, 'suggestion_text'), "\n", 1)
  let l:l_line = remove(s:, 'suggestion_line')

  " delete empty lines
  call deletebufline(bufname(), l:l_line + 1, line('.'))

  let modified_line = getline(l:l_line) . l:text[0]
  call setline(l:l_line, modified_line)
  call append(l:l_line, l:text[1:])
  call cursor(l:l_line + len(l:text) - 1, 0)
  call setcursorcharpos(l:l_line + len(l:text) - 1, col('$'))
endfunction

function! codecomp#Reject(...)
  if get(s:, 'suggestion_line', v:false)
   call codecomp#clear_suggestion()
   call remove(s:, 'suggestion_line')
  endif
endfunction

function! codecomp#clear_suggestion(...)
  call prop_remove({'type': s:hlgroup, 'all': v:true})
endfunction

function! codecomp#read_previous()
  let l:current_line = line('.')
  let l:start_line = l:current_line - 20
  if l:start_line <= 0
    let l:start_line = 1
  endif
  let l:end_line = l:current_line
  let l:lines = getline(l:start_line, l:end_line)
  return join(l:lines, "\n")
endfunction

function! codecomp#langchain_invoke(code)
 let l:api_url = 'http://localhost:8000/code_complete/invoke'
 let l:json_body = json_encode({
    \ 'input' : {
    \   'repo_path': expand('%:p'),
    \   'language': &filetype,
    \   'partial_code': a:code
    \ }
 \})
  let l:json_body = substitute(l:json_body, "'", "\\'", "g")
  let l:response_file = '/tmp/http_response.txt'
  let l:cmd = 'curl -s -S -X POST -d ''' . l:json_body . ''' -H "Content-Type: application/json" -o ' . l:response_file . ' ' . l:api_url
  let l:output = system(l:cmd)
  let l:response = readfile(l:response_file)
  call delete(l:response_file)
  return json_decode(l:response[0])['output']
endfunction

function! codecomp#show_suggestion(suggestion)
  " TODO: move this logic to the server

  " find the first non empty line
  let l:l_num = line('.')
  let l:current_line = getline(l:l_num)
  while empty(trim(l:current_line))
    let l:l_num = l:l_num - 1
    let l:current_line = getline(l:l_num)
  endwhile

  let l:c_pos = stridx(a:suggestion, l:current_line)
 
  if l:c_pos < 0
    echo "coudln't find the last line in the suggestions"
    return
  endif
  
  let l:text = a:suggestion[l:c_pos + strlen(l:current_line):]
  if empty(text)
    return
  endif
  let s:suggestion_text = l:text
  let s:suggestion_line = l:l_num
  let l:text = split(l:text, "\n", 1)

  call prop_add(l:l_num, strlen(getline(l:l_num)) + 1, {'type': s:hlgroup, 'text': text[0]})
  for line in l:text[1:]
    " since prop replaces spaces with tabs and removes empty lines
    if line =~ '^\\s*$' || empty(line)
      let line = ' '
    else
      let line = substitute(line, '\t', repeat(' ', &tabstop), 'g')
    endif

    call prop_add(l:l_num, 0, {'type': s:hlgroup, 'text_align': 'below', 'text': line})
  endfor
  redraw!
endfunction
