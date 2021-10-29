local actions = require('telescope.actions')

require('telescope').setup {
  defaults = {
    file_ignore_patterns = {
        "%.o"
      , "%.hi"
    },
    layout_config = {
      horizontal = {
        preview_width = 0.5,
        width = 0.93
      }
    },
    path_display = {
      truncate = 1
    },
    prompt_prefix = "> ",
    mappings = {
      i = {
        ["<c-a>"] = function() print("ciao raga ahah") end,
        -- disable default quick-fix-related actions and remapping them
        -- removing all key-shortcut conflicts with XMonad
        ["<m-q>"] = false,
        ["<c-q>"] = false,
        ["<c-q>"] = actions.send_selected_to_qflist + actions.open_qflist
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
    layout_config = {
      prompt_position = "top"
    },
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

my_mappings.find_xmonad_config_files = function()
  require('telescope.builtin').find_files({
    prompt_title = "XMonad Config",
    cwd = "~/.xmonad/lib"
  })
end

my_mappings.live_grep_xmonad_config_files = function()
  require('telescope.builtin').live_grep({
    prompt_title = "Search in XMonad Config",
    cwd = "~/.xmonad/lib"
  })
end

my_mappings.file_browser_xmonad_config_files = function()
  require('telescope.builtin').file_browser({
    prompt_title = "Files in XMonad Config",
    cwd = "~/.config/nvim"
  })
end

-- this is lua's way to export stuff from modules (kinda like oldschool javascript)
return my_mappings
