require('telescope').setup {
  defaults = {
    prompt_prefix = "> ",
    mappings = {
      i = {
        ["<c-a>"] = function() print("ciao raga ahah") end
      }
    }
  }
}

require('telescope').load_extension('fzf')
require('telescope').load_extension('coc')

-- declaring my own custom mappings here
local my_mappings = {}

my_mappings.current_buffer_search = function()
  require('telescope.builtin').current_buffer_fuzzy_find({
    prompt_position = "top",
    sorting_strategy = "ascending"
  })
end

my_mappings.find_neovim_config_files = function()
  require('telescope.builtin').find_files({
    prompt_title = "NeoVim Config",
    cwd = "~/.config/nvim"
  })
end

my_mappings.live_grep_config_files = function()
  require('telescope.builtin').live_grep({
    prompt_title = "Search in NeoVim Config",
    cwd = "~/.config/nvim"
  })
end

my_mappings.file_browser_config_files = function()
  require('telescope.builtin').file_browser({
    prompt_title = "Files in NeoVim Config",
    cwd = "~/.config/nvim"
  })
end

-- this is lua's way to export stuff from modules (kinda like oldschool javascript)
return my_mappings
