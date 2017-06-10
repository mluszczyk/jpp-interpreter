set +e
set +x

cd src
stack exec bnfc grammar.cf
cd ../
rm src/*.bak
rm src/TestGrammar.hs
rm src/SkelGrammar.hs
ed -s src/PrintGrammar.hs << 'EOF'
0a
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
.
w
EOF
