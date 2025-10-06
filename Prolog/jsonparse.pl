%%%% Riccardo Ghilotti 879259
%%%% -*- mode: Prolog -*-

% questi 4 casi sono gestiti qui perchè altrove
% il codice non li riesce a gestire correttamente
jsonparse(JString, _) :-
    atom_chars(JString, JSDiv),
    skipSpaces(JSDiv, JSFin),
    skipSpacesEnd(JSFin, [ ']' ]), !, false.
jsonparse(JString, _) :-
    atom_chars(JString, JSDiv),
    skipSpaces(JSDiv, JSFin),
    skipSpacesEnd(JSFin, [ '}' ]), !, false.
jsonparse(JString, _) :-
    string_chars(JString, JSDiv),
    skipSpaces(JSDiv, JSFin),
    skipSpacesEnd(JSFin, [ ']' ]), !, false.
jsonparse(JString, _) :-
    string_chars(JString, JSDiv),
    skipSpaces(JSDiv, JSFin),
    skipSpacesEnd(JSFin, [ '}' ]), !, false.

jsonparse(JSONString, jsonobj(Members)) :-
    string_chars(JSONString, JSONDiv), skipSpaces(JSONDiv, JSD1),
    skipSpacesEnd(JSD1, JSD2), jsonparseM(JSD2, Members).
jsonparse(JSONString, jsonarray(Elements)) :-
    string_chars(JSONString, JSONDiv), skipSpaces(JSONDiv, JSD1),
    skipSpacesEnd(JSD1, JSD2), jsonparseE(JSD2, Elements).
jsonparse(JSONString, jsonobj(Members)) :-
    atom_chars(JSONString, JSONDiv), skipSpaces(JSONDiv, JSD1),
    skipSpacesEnd(JSD1, JSD2), jsonparseM(JSD2, Members).
jsonparse(JSONString, jsonarray(Elements)) :-
    atom_chars(JSONString, JSONDiv), skipSpaces(JSONDiv, JSD1),
    skipSpacesEnd(JSD1, JSD2), jsonparseE(JSD2, Elements).

% jsonparseM/2 (dove M sta per Members)
% è il predicato utilizzato per analizzare gli oggetti
jsonparseM([ '}' ], []).
jsonparseM([ '{' | JSONString], Members) :-
    jsonparseM(JSONString, Members).
jsonparseM([ ' ' | JSONString], Members) :-
    skipSpaces(JSONString, JStr),
    jsonparseM(JStr, Members).
jsonparseM([ '\n' | JSONString], Members) :-
    skipSpaces(JSONString, JStr),
    jsonparseM(JStr, Members).
jsonparseM([ '\t' | JSONString], Members) :-
    skipSpaces(JSONString, JStr),
    jsonparseM(JStr, Members).
jsonparseM([ '"' | JSONString], [ Pair | Members ]) :-
    jsonparseCheckAttribute(JSONString, [], Pair),
    jsonskipObj(JSONString, Members).


% jsonparseE/2 (dove E sta per elements)
% è il predicato utilizzato per analizzare gli array
jsonparseE([ ']' ], []).
jsonparseE([ '[' | JSONString ], [ Element | MoreElements ]) :-
    jsonparseCheckValue(JSONString, Element),
    jsonskipArray(JSONString, MoreElements).
jsonparseE([ '[' | JSONString], Elements) :-
    jsonparseE(JSONString, Elements).
jsonparseE([ ' ' | JSONString], Elements) :-
    skipSpaces(JSONString, JSFin),
    jsonparseE(JSFin, Elements).
jsonparseE([ '\n' | JSONString], Elements) :-
    skipSpaces(JSONString, JSFin),
    jsonparseE(JSFin, Elements).
jsonparseE([ '\t' | JSONString], Elements) :-
    skipSpaces(JSONString, JSFin),
    jsonparseE(JSFin, Elements).

% jsonskipObj/2 si occupa di saltare l'attributo
jsonskipObj([ '"' | JSONString ], Members) :-
    jsonskipObjColon(JSONString, Members).
jsonskipObj([ _ | JSONString ], Members) :-
    jsonskipObj(JSONString, Members).

% jsonskipObjColon/2 salta i fino ai ':'
jsonskipObjColon([ ':' | JSONString ], Members) :-
    jsonskipObjValue(JSONString, Members).
jsonskipObjColon([ ' ' | JSONString ], Elements) :-
    skipSpaces(JSONString, JSFin),
    jsonskipObjColon(JSFin, Elements).
jsonskipObjColon([ '\n' | JSONString ], Elements) :-
    skipSpaces(JSONString, JSFin),
    jsonskipObjColon(JSFin, Elements).
jsonskipObjColon([ '\t' | JSONString ], Elements) :-
    skipSpaces(JSONString, JSFin),
    jsonskipObjColon(JSFin, Elements).


% jsonskipObjValue/2 è il predicato che porta il puntatore al valore da saltare
jsonskipObjValue([ '[' | JSONString ], Members) :- !,
    jsonskipObjInArray(JSONString, Members, 0).
jsonskipObjValue([ '{' | JSONString ], Members) :- !,
    jsonskipObjInObj(JSONString, Members, 0).
jsonskipObjValue([ '"' | JSONString ], Members) :- !,
    jsonskipObjInApici(JSONString, Members).
jsonskipObjValue([ N | JSONString ], Members) :-
    atom_number(N, _),
    jsonskipObjNumber(JSONString, Members).
jsonskipObjValue([ 'n', 'u', 'l', 'l' | JSONString ], Members) :- !,
    jsonskipObjFin(JSONString, Members).
jsonskipObjValue([ 't', 'r', 'u', 'e' | JSONString ], Members) :- !,
    jsonskipObjFin(JSONString, Members).
jsonskipObjValue([ 'f', 'a', 'l', 's', 'e' | JSONString], Members) :-
    !,
    jsonskipObjFin(JSONString, Members).
jsonskipObjValue([ ' ' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipObjValue(JS1, Members).
jsonskipObjValue([ '\n' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipObjValue(JS1, Members).
jsonskipObjValue([ '\t' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipObjValue(JS1, Members).

% jsonskipObjNumber/2 salta i numeri
jsonskipObjNumber([ N | JSONString ], Members) :-
    atom_number(N, _),
    jsonskipObjNumber(JSONString, Members).
jsonskipObjNumber([ 'e' | JSONString ], Members) :- !,
    jsonskipObjNumber(JSONString, Members).
jsonskipObjNumber([ 'E' | JSONString ], Members) :- !,
    jsonskipObjNumber(JSONString, Members).
jsonskipObjNumber([ '+' | JSONString ], Members) :- !,
    jsonskipObjNumber(JSONString, Members).
jsonskipObjNumber([ '-' | JSONString ], Members) :- !,
    jsonskipObjNumber(JSONString, Members).
jsonskipObjNumber([ '.' | JSONString ], Members) :- !,
    jsonskipObjNumber(JSONString, Members).
jsonskipObjNumber([ C | JSONString ], Members) :- !,
    jsonskipObjFin([ C | JSONString ], Members).

% jsonskipObjInApici/2 salta tutto quello che c'è tra i due doppi apici
jsonskipObjInApici([ '"' | JSONString ], Members) :- !,
    jsonskipObjFin(JSONString, Members).
jsonskipObjInApici([ _ | JSONString ], Members) :- !,
    jsonskipObjInApici(JSONString, Members).

% jsonskipObjInArray/3 salta tutto quello che c'è nell' array
jsonskipObjInArray([ ']' | JSONString ], Members, 0) :- !,
    jsonskipObjFin(JSONString, Members).
jsonskipObjInArray([ '[' | JSONString ], Members, Qtemp) :- !,
    Qtemp1 is Qtemp + 1,
    jsonskipObjInArray(JSONString, Members, Qtemp1).
jsonskipObjInArray([ ']' | JSONString ], Members, Qtemp) :- !,
    Qtemp1 is Qtemp - 1,
    jsonskipObjInArray(JSONString, Members, Qtemp1).
jsonskipObjInArray([ _ | JSONString ], Members, Qtemp) :- !,
    jsonskipObjInArray(JSONString, Members, Qtemp).

% jsonskipObjInObj/2 salta tutto quello che c'è nell' oggetto
jsonskipObjInObj([ '}' | JSONString ], Members, 0) :- !,
    jsonskipObjFin(JSONString, Members).
jsonskipObjInObj([ '}' | JSONString ], Members, Gtemp) :- !,
    Gtemp1 is Gtemp - 1,
    jsonskipObjInObj(JSONString, Members, Gtemp1).
jsonskipObjInObj([ '{' | JSONString ], Members, Gtemp) :- !,
    Gtemp1 is Gtemp + 1,
    jsonskipObjInObj(JSONString, Members, Gtemp1).
jsonskipObjInObj([ _ | JSONString ], Members, Gtemp) :- !,
    jsonskipObjInObj(JSONString, Members, Gtemp).

% jsonskipObjFin/2 arriva al prossimo member
jsonskipObjFin([ ',' | JSONString ], Members) :- !,
    esisteAttr(JSONString),
    jsonparseM(JSONString, Members).
jsonskipObjFin([ '}' | JSONString ], Members) :- !,
    jsonparseM([ '}' | JSONString ], Members).
jsonskipObjFin([ ' ' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipObjFin(JS1, Members).
jsonskipObjFin([ '\n' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipObjFin(JS1, Members).
jsonskipObjFin([ '\t' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipObjFin(JS1, Members).

% jsonskipArray/2 salta tutto quello che c'è nell' array
jsonskipArray([ N | JSONString ], Elements) :-
    atom_number(N, _),
    jsonskipArrayNumber(JSONString, Elements).
jsonskipArray([ 'n', 'u', 'l', 'l' | JSONString], Members) :- !,
    jsonskipArrayFin(JSONString, Members).
jsonskipArray([ 't', 'r', 'u', 'e' | JSONString], Members) :- !,
    jsonskipObjFin(JSONString, Members).
jsonskipArray([ 'f', 'a', 'l', 's', 'e' | JSONString], Members) :- !,
    jsonskipArrayFin(JSONString, Members).
jsonskipArray([ '"' | JSONString ], Elements) :- !,
    jsonskipArrayInApici(JSONString, Elements).
jsonskipArray([ '[' | JSONString ], Elements) :- !,
    jsonskipArrayInArray(JSONString, Elements, 0).
jsonskipArray([ '{' | JSONString ], Elements) :- !,
    jsonskipArrayInObj(JSONString, Elements, 0).
jsonskipArray([ ' ' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipArray(JS1, Members).
jsonskipArray([ '\n' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipArray(JS1, Members).
jsonskipArray([ '\t' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipArray(JS1, Members).

% jsonskipArrayInObj/3 salta tutto quello che c'è nell' array
jsonskipArrayInObj([ '}' | JSONString ], Elements, 0) :- !,
    jsonskipArrayFin(JSONString, Elements).
jsonskipArrayInObj([ '}' | JSONString ], Elements, Gtemp) :- !,
    Gtemp1 is Gtemp - 1,
    jsonskipArrayInObj(JSONString, Elements, Gtemp1).
jsonskipArrayInObj([ '{' | JSONString ], Elements, Gtemp) :- !,
    Gtemp1 is Gtemp + 1,
    jsonskipArrayInObj(JSONString, Elements, Gtemp1).
jsonskipArrayInObj([ _ | JSONString ], Elements, Gtemp) :- !,
    jsonskipArrayInObj(JSONString, Elements, Gtemp).


% jsonskipArrayNumber/2 salta i numeri
jsonskipArrayNumber([ N | JSONString ], Members) :-
    atom_number(N, _),
    jsonskipArrayNumber(JSONString, Members).
jsonskipArrayNumber([ 'e' | JSONString ], Members) :- !,
    jsonskipArrayNumber(JSONString, Members).
jsonskipArrayNumber([ 'E' | JSONString ], Members) :- !,
    jsonskipArrayNumber(JSONString, Members).
jsonskipArrayNumber([ '+' | JSONString ], Members) :- !,
    jsonskipArrayNumber(JSONString, Members).
jsonskipArrayNumber([ '-' | JSONString ], Members) :- !,
    jsonskipArrayNumber(JSONString, Members).
jsonskipArrayNumber([ '.' | JSONString ], Members) :- !,
    jsonskipArrayNumber(JSONString, Members).
jsonskipArrayNumber([ C | JSONString ], Members) :- !,
    jsonskipArrayFin([ C | JSONString ], Members).

% jsonskipArrayInArray/2 salta tutto quello che è presente
% nell' array value
jsonskipArrayInArray([ ']' | JSONString ], Elements, 0) :- !,
    jsonskipArrayFin(JSONString, Elements).
jsonskipArrayInArray([ ']' | JSONString ], Elements, Qtemp) :- !,
    Qtemp1 is Qtemp - 1,
    jsonskipArrayInArray(JSONString, Elements, Qtemp1).
jsonskipArrayInArray([ '[' | JSONString ], Elements, Qtemp) :- !,
    Qtemp1 is Qtemp + 1,
    jsonskipArrayInArray(JSONString, Elements, Qtemp1).
jsonskipArrayInArray([ _ | JSONString ], Elements, Qtemp) :- !,
    jsonskipArrayInArray(JSONString, Elements, Qtemp).


% jsonskipArrayInApici/2 salta tutto quello che c'è tra apici
jsonskipArrayInApici([ '"' | JSONString ], Elements) :- !,
    jsonskipArrayFin(JSONString, Elements).
jsonskipArrayInApici([ _ | JSONString ], Elements) :- !,
    jsonskipArrayInApici(JSONString, Elements).

% jsonskipArrayFin/2 passa al prossimo valore
jsonskipArrayFin([ ',' | JSONString ], [ Element | MoreElements ]) :- !,
    jsonparseCheckValue(JSONString, Element),
    jsonskipArray(JSONString, MoreElements).
jsonskipArrayFin([ ']' | JSONString ], Elements) :-
    jsonparseE([ ']' | JSONString ], Elements).
jsonskipArrayFin([ ' ' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipArrayFin(JS1, Members).
jsonskipArrayFin([ '\n' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipArrayFin(JS1, Members).
jsonskipArrayFin([ '\t' | JSONString ], Members) :- !,
    skipSpaces(JSONString, JS1),
    jsonskipArrayFin(JS1, Members).


% esisteAttr/2 è un predicato utilizzato per controllare
% se è presente qualcosa dopo ','
esisteAttr([ '"' | _ ]).
esisteAttr([ '}' | _ ]) :- false.
esisteAttr([ _ | JSONString]) :-
    esisteAttr(JSONString).

% jsonparseCheckAttribute/3 controlla attribute e poi inizia a cercare value
jsonparseCheckAttribute([ '"' | JSONString ], Acc, (Attribute, Value)) :- !,
    reverse(Acc, [], Attribute),
    jsonparseFindValue(JSONString, Value).
jsonparseCheckAttribute([ C | JSONString ], Acc, Pair) :-
    jsonparseCheckAttribute(JSONString, [ C | Acc ], Pair). 

% jsonparseFindValue/2 controlla che ci sia ':' tra Attribute
% e Value e una volta trovato controlla Value
jsonparseFindValue([ ':' | JSONString ], Value) :- !,
    jsonparseCheckValue(JSONString, Value).
jsonparseFindValue([ ' ' | JSONString ], Value) :- !,
    jsonparseFindValue(JSONString, Value).

% jsonparseCheckValue/2 controlla che il valore di value sia
% corretto utilizzando anche altri predicati
jsonparseCheckValue([ 'n', 'u', 'l', 'l' | _ ], null).
jsonparseCheckValue([ 't', 'r', 'u', 'e' | _ ], true).
jsonparseCheckValue([ 'f', 'a', 'l', 's', 'e' | _ ], false).
jsonparseCheckValue([ ' ' | JSONString ], Value) :-
    jsonparseCheckValue(JSONString, Value).
jsonparseCheckValue([ '\n' | JSONString ], Value) :-
    skipSpaces(JSONString, JS1),
    jsonparseCheckValue(JS1, Value).
jsonparseCheckValue([ '\t' | JSONString ], Value) :-
    skipSpaces(JSONString, JS1),
    jsonparseCheckValue(JS1, Value).
jsonparseCheckValue([ ' ' | JSONString ], Value) :-
    skipSpaces(JSONString, JS1),
    jsonparseCheckValue(JS1, Value).
jsonparseCheckValue([ '"' | JSONString ], Value) :-
    jsonparseValueString(JSONString, [], Value).
jsonparseCheckValue([ '{' | JSONString ], jsonobj(Value)) :-
    jsonparseTakeObject([ '{' | JSONString ], 0, [], Obj),
    jsonparse(Obj, jsonobj(Value)).
jsonparseCheckValue([ '[' | JSONString ], jsonarray(Value)) :-
    jsonparseTakeArray([ '[' | JSONString ], 0, [], Arr),
    jsonparse(Arr, jsonarray(Value)).
jsonparseCheckValue([ '-' | JSONString ], Value) :-
    jsonparseValueNumber(JSONString, ['-'], Value).
jsonparseCheckValue([ N | JSONString ], Value) :-
    atom_number(N, _),
    jsonparseValueNumber([ N | JSONString ], [], Value).

% jsonparseTakeObject/4 prende l'oggetto che verrà analizzato
% in seguito
jsonparseTakeObject([ '}' | _ ], 1, Str, Obj) :- !,
    reverse([ '}' | Str], [], Obj).
jsonparseTakeObject([ '}' | JSONString ], N, Str, Obj) :- !,
    N1 is N - 1,
    jsonparseTakeObject(JSONString, N1, [ '}' | Str], Obj).
jsonparseTakeObject([ '{' | JSONString ], N, Str, Obj) :- !,
    N1 is N + 1,
    jsonparseTakeObject(JSONString, N1, [ '{' | Str], Obj).
jsonparseTakeObject([ C | JSONString ], N, Str, Obj) :- !,
    jsonparseTakeObject(JSONString, N, [ C | Str ], Obj).

% jsonparseTakeArray/4 prende l'array da analizzare
jsonparseTakeArray([ ']' | _ ], 1, Str, Arr) :- !,
    reverse([ ']' | Str], [], Arr).
jsonparseTakeArray([ ']' | JSONString ], N, Str, Arr) :- !,
    N1 is N - 1,
    jsonparseTakeArray(JSONString, N1, [  ']' | Str ], Arr).
jsonparseTakeArray([ '[' | JSONString ], N, Str, Arr) :- !,
    N1 is N + 1,
    jsonparseTakeArray(JSONString, N1, [ '[' | Str ], Arr).
jsonparseTakeArray([ C | JSONString ], N, Str, Arr) :- !,
    jsonparseTakeArray(JSONString, N, [ C | Str ], Arr).

% jsonparseValueNumber/3 controlla il numero
jsonparseValueNumber([ N | JSONString ], Acc, Value) :-
    atom_number(N, _),
    jsonparseValueNumber(JSONString, [ N | Acc ], Value).
jsonparseValueNumber([ '.' | JSONString ], Acc, Value) :-
    jsonparseValueNumber(JSONString, [ '.' | Acc ], Value).
jsonparseValueNumber([ 'e', '+' | JSONString ], Acc, Value) :-
    jsonparseValueNumber(JSONString, [ '+', 'e' | Acc ], Value).
jsonparseValueNumber([ 'e', '-' | JSONString ], Acc, Value) :-
    jsonparseValueNumber(JSONString, [ '-', 'e' | Acc ], Value).
jsonparseValueNumber([ 'E', '+' | JSONString ], Acc, Value) :-
    jsonparseValueNumber(JSONString, [ '+', 'e' | Acc ], Value).
jsonparseValueNumber([ 'E', '-' | JSONString ], Acc, Value) :-
    jsonparseValueNumber(JSONString, [ '-', 'e' | Acc ], Value).
jsonparseValueNumber([ 'e'  | JSONString ], Acc, Value) :-
    jsonparseValueNumber(JSONString, [ 'e' | Acc ], Value).
jsonparseValueNumber([ 'E' | JSONString ], Acc, Value) :-
    jsonparseValueNumber(JSONString, [ 'e' | Acc ], Value).
jsonparseValueNumber([ _ | _ ], Acc, Value) :- !,
    reverseN(Acc, [], Value).

% jsonparseValueString/3 controlla la stringa
jsonparseValueString([ '"' | _ ], Acc, Value) :- !,
    reverse(Acc, [], Value).
jsonparseValueString([ C | JSONString ], Acc, Value) :- !,
    jsonparseValueString(JSONString, [ C | Acc ], Value).

% jsonaccess/3
jsonaccess(jsonobj(X), [], jsonobj(X)).
jsonaccess(jsonobj([ (Attribute, Value) | _ ]), Attribute, Value) :-
    string(Attribute).
jsonaccess(jsonobj([ (Attribute, Value) | _ ]), [ Attribute ], Value).
jsonaccess(jsonobj([ (Attribute, Value) | _ ]),
	   [ Attribute | Fields ],
	   Result) :-
    jsonaccess(Value, Fields, Result).
jsonaccess(jsonobj([ _ | Members ]), Fields, Result) :-
    jsonaccess(jsonobj(Members), Fields, Result).
jsonaccess(jsonarray([ Element | _ ]), [ 0 ], Element).
jsonaccess(jsonarray([ Element | _ ]), [ 0 | Fields ], Result) :-
    jsonaccess(Element, Fields, Result).
jsonaccess(jsonarray([ _ | Elements ]), [ N | R ], Result) :-
    N1 is N - 1,
    jsonaccess(jsonarray(Elements), [ N1 | R ], Result).

% jsonread/2
jsonread(NomeFile, JSON) :-
    exists_file(NomeFile),
    file_to_string(NomeFile, String),
    jsonparse(String, JSON).

file_to_string(NomeFile, String) :-
    open(NomeFile, read, In),
    read_string(In, _, String),
    close(In).

% jsondump/2
jsondump(JSON, NomeFile) :-
    open(NomeFile, write, Out),
    jsondumpFileAperto(JSON, Out, 0),
    close(Out), write(NomeFile).

jsondumpFileAperto([], _, _).
jsondumpFileAperto(jsonobj(Members), Out, Tabs) :-
    Tabs1 is Tabs + 1,
    write(Out, "{"), write(Out, "\n"),
    jsondumpFileAperto(Members, Out, Tabs1),
    writeTabs(Tabs, Out), write(Out, "}").
jsondumpFileAperto(jsonarray(Elements), Out, Tabs) :-
    Tabs1 is Tabs + 1,
    write(Out, "["), write(Out, "\n"),
    jsondumpFileAperto(Elements, Out, Tabs1),
    writeTabs(Tabs, Out),
    write(Out, "]").
jsondumpFileAperto([ (Attribute, Value) ], Out, Tabs) :-
    writeTabs(Tabs, Out),
    write(Out, "\""), write(Out, Attribute),
    write(Out, "\""), write(Out, " : "),
    jsondumpWriteValue(Value, Out, Tabs),
    write(Out, "\n"),
    jsondumpFileAperto([], Out, 0).
jsondumpFileAperto([ (Attribute, Value) | Members ],
		   Out,
		   Tabs) :-
    writeTabs(Tabs, Out),
    write(Out, "\""), write(Out, Attribute),
    write(Out, "\""), write(Out, " : "),
    jsondumpWriteValue(Value, Out, Tabs),
    write(Out, ",\n"),
    jsondumpFileAperto(Members, Out, Tabs).
jsondumpFileAperto([ Value ], Out, Tabs) :-
    writeTabs(Tabs, Out),
    jsondumpWriteValue(Value, Out, Tabs),
    write(Out, "\n"),
    jsondumpFileAperto([], Out, 0).
jsondumpFileAperto([ Value | Elements ], Out, Tabs) :-
    writeTabs(Tabs, Out),
    jsondumpWriteValue(Value, Out, Tabs),
    write(Out, ",\n"),
    jsondumpFileAperto(Elements, Out, Tabs).

% jsondumpWriteValue/2 stampa Value in base al tipo
jsondumpWriteValue(Value, Out, _) :-
    string(Value), write(Out, "\""),
    write(Out, Value), write(Out, "\"").
jsondumpWriteValue(Value, Out, _) :-
    number(Value), write(Out, Value).
jsondumpWriteValue(null, Out, _) :-
    write(Out, null).
jsondumpWriteValue(true, Out, _) :-
    write(Out, true).
jsondumpWriteValue(false, Out, _) :-
    write(Out, false).
jsondumpWriteValue(jsonobj(Members), Out, Tabs) :-
    jsondumpFileAperto(jsonobj(Members), Out, Tabs).
jsondumpWriteValue(jsonarray(Elements), Out, Tabs) :-
    jsondumpFileAperto(jsonarray(Elements), Out, Tabs).

% writeTabs/2 indenta per bene il file JSON in modo tale che sia leggibile
writeTabs(0, _).
writeTabs(Tabs, Out) :-
    Tabs1 is Tabs - 1,
    write(Out, "\t"),
    writeTabs(Tabs1, Out).

% reverse/3 è utilizzato dalle stringhe per controllare
% se sono corrette
reverse([], [], "").
reverse([], Ribaltato, Finale) :-
    string_chars(Finale, Ribaltato).
reverse([ L | List ], Ribaltato, Finale) :-
    reverse(List, [ L | Ribaltato ], Finale).

% reverseN/3 invece è utilizzato dai numeri
reverseN([], [], []).
reverseN([], Ribaltato, Finale) :-
    number_chars(Finale, Ribaltato).
reverseN([ L | List ], Ribaltato, Finale) :-
    reverseN(List, [ L | Ribaltato], Finale).

% skipSpaces/2 è il predicato che salta gli spazi
skipSpaces([ ' ' | String ], FinStr) :-
    skipSpaces(String, FinStr).
skipSpaces([ '\n' | String ], FinStr) :-
    skipSpaces(String, FinStr).
skipSpaces([ '\t' | String ], FinStr) :-
    skipSpaces(String, FinStr).
skipSpaces(String, String).

% skipSpacesEnd/2 invece salta tutti gli spazi
% però partendo dalla fine
skipSpacesEnd(String, FinStr) :-
    string_chars(String, X),
    reverse(X, [], Y),
    string_chars(Y, Z),
    skipSpaces(Z, String1),
    reverse(String1, [], String2),
    string_chars(String2, FinStr).
