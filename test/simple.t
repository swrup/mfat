  $ mfat make -s 2048 test.fat
  Formatted test.fat: 2048 sectors (1048576 bytes), FAT32
  $ mfat ls test.fat
  $ mfat write test.fat /HELLO.TXT - <<EOF
  > Hello, World!
  > EOF
  $ mfat ls test.fat
  HELLO.TXT (14)
  $ mfat cat test.fat /HELLO.TXT
  Hello, World!

  $ mfat mkdir test.fat /DOCS
  $ mfat ls test.fat
  HELLO.TXT (14)
  DOCS/
  $ mfat write test.fat /DOCS/README.TXT - <<EOF
  > Coucou!
  > EOF
  $ mfat ls test.fat /DOCS
  README.TXT (8)
  $ mfat cat test.fat /DOCS/README.TXT
  Coucou!
  $ mfat write test.fat /HELLO.TXT - <<EOF
  > New version!
  > EOF
  $ mfat cat test.fat /HELLO.TXT
  New version!
  $ mfat ls test.fat
  HELLO.TXT (13)
  DOCS/
  $ mfat rm test.fat /HELLO.TXT
  $ mfat ls test.fat
  DOCS/
  $ mfat rm test.fat /DOCS
  mfat: DOCS: directory not empty
  [124]
  $ mfat rm test.fat /DOCS/README.TXT
  $ mfat rm test.fat /DOCS
  $ mfat ls test.fat
  $ mfat make -s 2048 defrag.fat
  Formatted defrag.fat: 2048 sectors (1048576 bytes), FAT32
  $ printf "AAAA" | mfat write defrag.fat /A.TXT -
  $ printf "BBBB" | mfat write defrag.fat /B.TXT -
  $ printf "CCCC" | mfat write defrag.fat /C.TXT -
  $ mfat mkdir defrag.fat /SUB
  $ printf "SUB"| mfat write defrag.fat /SUB/N.TXT -
  $ mfat rm defrag.fat /B.TXT
  $ printf "DDDDDDDD" | mfat write defrag.fat /D.TXT -
  $ mfat ls defrag.fat
  A.TXT (4)
  D.TXT (8)
  C.TXT (4)
  SUB/
  $ mfat defrag defrag.fat
  Defragmented defrag.fat: 4 file(s), 1 directory(ies)
  $ mfat ls defrag.fat
  A.TXT (4)
  D.TXT (8)
  C.TXT (4)
  SUB/
  $ mfat cat defrag.fat /A.TXT
  AAAA
  $ mfat cat defrag.fat /C.TXT
  CCCC
  $ mfat cat defrag.fat /D.TXT
  DDDDDDDD
  $ mfat cat defrag.fat /SUB/N.TXT
  SUB
  $ mfat ls defrag.fat /SUB
  N.TXT (3)
  $ mfat make -s 2048 lfn.fat
  Formatted lfn.fat: 2048 sectors (1048576 bytes), FAT32
  $ mfat write lfn.fat /hello.txt - <<EOF
  > Hello with lowercase!
  > EOF
  $ mfat ls lfn.fat
  hello.txt (22)
  $ mfat cat lfn.fat /hello.txt
  Hello with lowercase!
  $ mfat write lfn.fat "/My Long Document.txt" - <<EOF
  > Long name content
  > EOF
  $ mfat ls lfn.fat
  hello.txt (22)
  My Long Document.txt (18)
  $ mfat cat lfn.fat "/My Long Document.txt"
  Long name content
  $ mfat mkdir lfn.fat /MyFolder
  $ mfat ls lfn.fat
  hello.txt (22)
  My Long Document.txt (18)
  MyFolder/
  $ mfat write lfn.fat /MyFolder/notes.md - <<EOF
  > Some notes
  > EOF
  $ mfat ls lfn.fat /MyFolder
  notes.md (11)
  $ mfat cat lfn.fat /MyFolder/notes.md
  Some notes
  $ mfat rm lfn.fat "/My Long Document.txt"
  $ mfat ls lfn.fat
  hello.txt (22)
  MyFolder/
  $ mfat write lfn.fat /hello.txt - <<EOF
  > Updated!
  > EOF
  $ mfat cat lfn.fat /hello.txt
  Updated!
  $ mfat ls lfn.fat
  hello.txt (9)
  MyFolder/
