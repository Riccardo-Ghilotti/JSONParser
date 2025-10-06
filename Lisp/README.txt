Riccardo Ghilotti

cosa fanno le funzioni:
- jsonparseM e derivati: jsonparseM(embers) si occupano del controllo della stringa contentente l'oggetto principale e costruiscono la struttura che lo contiene
- jsonparseE e derivati: jsonparseE(elements) si occupano del controllo della stringa contentente l' array principale e costruiscono la struttora che lo contiene.
- gruppo skip: passa al prossimo gruppo/elemento e controlla la sintassi dell'elemento corrente.
- skipSpace, skipSpaceEnd, flatten e printTabs: usate come funzioni di supporto.