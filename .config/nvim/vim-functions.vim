fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun

fun! GotoBuffer(ctrlId)
      if (a:ctrlId > 9) || (a:ctrlId < 0)
            echo "CtrlID must be between 0 - 9!"
            return
      end

      let contents = g:win_ctrl_buf_list[a:ctrlId]
      if type(l:contents) != v:t_list
            echo "Nothing there"
            return
      end

      let bufh = l:contents[1]
      call nvim_win_set_buf(0, l:bufh)
endfun

let g:win_ctrl_buf_list = [0, 0, 0, 0]
fun! SetBuffer(ctrlId)
      if has_key(b:, "terminal_job_id") == 0
            echo "You must be in a terminal to execute this command!"
            return
      end

      if (a:ctrlId > 9) || (a:ctrlId < 0)
            echo "CtrlID must be between 0 - 9!"
            return
      end

      let g:win_ctrl_buf_list[a:ctrlId] = [b:terminal_job_id, nvim_win_get_buf(0)]
endfun

fun! SendTerminalCommand(ctrlId, command)
      if (a:ctrlId > 9) || (a:ctrlId < 0)
            echo "CtrlID must be between 0 - 9!"
            return
      end

      let contents = g:win_ctrl_buf_list[a:ctrlId]
      if type(l:contents) != v:t_list
            echo "Nothing there"
            return
      end

      let job_id = l:contents[0]
      call chansend(job_id, command)
endfun

fun! SmartJumpToEnd()
      let line = getline('.')
      let next_char = strpart(line, col('.')-1, 1)
      let next_two_chars = strpart(line, col('.')-1, 2)

      " TODO use a regex here... this is kinda dumb
      if (next_char == '"' || next_char == "'" || next_char == ")" || next_char == "]" || next_char == ">")
            return "\<Esc>\l\a"
      elseif (next_two_chars == " }")
            return "\<Esc>\l\l\a"
      else
            return "\<tab>"
      endif
endfun
