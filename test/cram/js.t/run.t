  $ dune build
  $ geckodriver > /dev/null 2> /dev/null &
  $ export GECKOPID=$!
  $ ls _build/default/basic
  META.basic
  assets
  basic.dune-package
  basic.html
  basic.install
  basic.template.html
  basic_bin.exe
  basic_bin.ml
  basic_bin.mli
  basic_js.bc.js
  basic_js.ml
  basic_js.mli
  src

  $ ../screenshot/screenshot.exe _build/default/basic/basic.html > image.png
  $ echo $PWD
  $TESTCASE_ROOT
  $ odiff reference.png image.png diff.png || true
  Images are identical
  $ magick identify -format "size: %wx%h\n" image.png
  size: 1282x962
  $ magick image.png +dither -colors 8 txt:- | awk 'NR>1{print $3}' | sort | uniq -c | sort -rn
  1232712 #000000FF
      190 #640000FF
      168 #DA0000FF
      110 #A20000FF
      104 #2F0000FF
  $ sha512sum image.png
  13bedbc5cdbdf70821140068c78717d31126ff450ff90cc5feeb94748c257ebee894b99ba1c27aebf311fda8cd267b0f9071c8ea71997f3a8389149ffb0417b3  image.png
  $ cp image.png /tmp
  $ cp diff.png /tmp
  cp: cannot stat 'diff.png': No such file or directory
  [1]
$ firefox /tmp/image.png
$ firefox /tmp/diff.png


  $ kill $GECKOPID
