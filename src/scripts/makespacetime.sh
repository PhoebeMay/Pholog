OCAMLRUNPARAM="h=1k,s=1k"



eval $(opam env --set-switch --switch 4.04.2+spacetime)
dune build main.exe
OCAML_SPACETIME_INTERVAL=100 _build/default/main.exe
x=`ls -t | head -n1`
echo $x
eval $(opam env --set-switch --switch 4.04.2)
prof_spacetime process -e _build/default/main.exe $x
y=`ls -t | head -n1`
echo $y
# prof_spacetime serve -p $y
prof_spacetime view -p $y
