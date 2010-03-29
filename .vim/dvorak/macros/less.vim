" Give help
noremap h :call <SID>Help()<CR>
map J j
fun! s:Help()
  echo "<Space>   One page forward          b         One page backward"
  echo "H         Half a page forward       T         Half a page backward"
  echo "<Enter>   One line forward          t         One line backward"
  echo "G         End of file               g         Start of file"
  echo "N%        percentage in file"
  echo "\n"
  echo "/pattern  Search for pattern        ?pattern  Search backward for pattern"
  echo "n         next pattern match        N         Previous pattern match"
  echo "\n"
  echo ":n<Enter> Next file                 :p<Enter> Previous file"
  echo "\n"
  echo "q         Quit                      v         Edit file"
  let i = input("Hit Enter to continue")
endfun


" Scroll half a page forward
noremap <script> H <C-D><SID>L
map <C-D> H

" Scroll one line forward
map h <CR>

" Scroll half a page backward
noremap <script> T <C-U><SID>L

" Scroll one line backward
noremap <script> t <C-Y><SID>L
map y t
map <C-Y> t
map <C-P> t
map <C-K> t
