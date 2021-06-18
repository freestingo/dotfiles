" luafile $HOME/.config/nvim/plug-config/lua/freestingotelescope.lua
lua require('freestingotelescope')

" Lists files in your current working directory, respects .gitignore
nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<CR>

" Lists files in ~/.config/nvim/ (for easier vimrc editing)
nnoremap <leader>vff <cmd>lua require('freestingotelescope').find_neovim_config_files()<CR>

" Search for a string in your current working directory and get results live as you type (respecting .gitignore)
nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<CR>

" Search for a string in ~/.config/nvim/
nnoremap <leader>vfg <cmd>lua require('freestingotelescope').live_grep_config_files()<CR>

" Lists open buffers in current neovim instance
nnoremap <leader>bb <cmd>lua require('telescope.builtin').buffers()<CR>

" Lists available help tags and opens a new window with the relevant help info on <cr>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<CR>

" Searches for the string under your cursor in your current working directory
nnoremap <leader>* <cmd>lua require('telescope.builtin').grep_string()<CR>

" Lists files and folders in your current working directory, open files, navigate your filesystem, and create new files and folders
nnoremap <leader>fb <cmd>lua require('telescope.builtin').file_browser()<CR>

" Lists files and folders in ~/.config/nvim/
nnoremap <leader>vfb <cmd>lua require('freestingotelescope').file_browser_config_files()<CR>

" Lists any LSP actions for the word under the cursor, that can be triggered with <CR>
nnoremap <leader>wac <cmd>lua require('telescope.builtin').lsp_code_actions()<CR>

" Lists items from the current window's location list
nnoremap <leader>loc <cmd>lua require('telescope.builtin').loclist()<CR>

" Lists available plugin/user commands and runs them on <CR>
nnoremap <leader>cmd <cmd>lua require('telescope.builtin').commands()<CR>

" Lists vim options, allows you to edit the current value on <CR>
nnoremap <leader>vopt <cmd>lua require('telescope.builtin').vim_options()<CR>

" Lists vim registers, pastes the contents of the register on <CR>
nnoremap <leader>rg <cmd>lua require('telescope.builtin').registers()<CR>

" Live fuzzy search inside of the current open buffer
nnoremap <C-_> <cmd>lua require('freestingotelescope').current_buffer_search()<CR>

" Lists searches that were executed recently, and reruns them on <CR>
nnoremap <leader>se <cmd>lua require('telescope.builtin').search_history()<CR>

" Lists normal mode keymappings
nnoremap <leader>km <cmd>lua require('telescope.builtin').keymaps()<CR>

" Lists built-in pickers and run them on <CR>
nnoremap <leader>bi <cmd>lua require('telescope.builtin').builtin()<CR>

" Quick-fix mappings
nnoremap <leader>qn <cmd>cnext<CR>
nnoremap <leader>qp <cmd>cprevious<CR>

" telescope-coc extension mappings
nnoremap <leader>gr <cmd>Telescope coc references<CR>
nnoremap <leader>qf <cmd>Telescope coc diagnostics<CR>
nnoremap <leader>ac <cmd>Telescope coc code_actions<CR>
nnoremap <leader>ds <cmd>Telescope coc document_symbols<CR>
nnoremap <leader>ws <cmd>Telescope coc workspace_symbols<CR>

" telescope-Git mappings
nnoremap <leader>gitbr <cmd>lua require('telescope.builtin').git_branches()<CR>
nnoremap <leader>gitst <cmd>lua require('telescope.builtin').git_status()<CR>

