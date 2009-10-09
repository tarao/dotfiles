" See http://subtech.g.hatena.ne.jp/motemen/20080727/1217156437

function! HighlightConsoleCodes()
    0
    let register_save = @"
    while search('\[[0-9;]*m', 'c')
        normal! dfm

        let [lnum, col] = getpos('.')[1:2]
        if len(getline('.')) == col
            let col += 1
        endif
        let syntax_name = 'ConsoleCodeAt_' . bufnr('%') . '_' . lnum . '_' . col
        execute 'syntax region' syntax_name 'start=+\%' . lnum . 'l\%' . col . 'c+ end=+\%$+' 'contains=ALL'

        let highlight = ''
        for color_code in split(matchstr(@", '[0-9;]\+'), ';')
            if color_code == 0
                let highlight .= ' ctermfg=NONE ctermbg=NONE'
            elseif color_code == 1
                let highlight .= ' cterm=bold'
            elseif 30 <= color_code && color_code <= 37
                let highlight .= ' ctermfg=' . (color_code - 30)
            elseif color_code == 38
                " TODO
            elseif color_code == 39
                " TODO
            elseif 40 <= color_code && color_code <= 47
                let highlight .= ' ctermbg=' . (color_code - 40)
            elseif color_code == 49
                " TODO
            endif
        endfor
        if len(highlight)
            execute 'highlight' syntax_name highlight
        endif
    endwhile
    let @" = register_save
    0
endfunction

autocmd BufRead,StdinReadPost * if search('[[0-9;]*m', 'n') | set modifiable | call HighlightConsoleCodes() | set buftype=nofile nomodifiable | endif

" autocmd BufRead,StdinReadPost * if search('[[0-9;]*m', 'n') | call HighlightConsoleCodes() | set buftype=nofile nomodifiable | endif
" `:set modifiable | undo | syntax clear' to revert
