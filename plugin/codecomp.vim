" Title:        Codecomp Plugin
" Description:  A plugin to provide code suggestion from LLM using code in
" current project
" Last Change:  12 March 2024
" Maintainer:   Asutosh Palai <https://github.com/asutoshpalai>

" Prevents the plugin from being loaded multiple times. If the loaded
" variable exists, do nothing more. Otherwise, assign the loaded
" variable and continue running this instance of the plugin.
if exists("g:loaded_codecomp_plugin")
    finish
endif
let g:loaded_codecomp_plugin = 1

command! -nargs=0 Codecomp call codecomp#Suggest()
command! -nargs=0 CodecompAccept call codecomp#Accept()
command! -nargs=0 CodecompReject call codecomp#Reject()
