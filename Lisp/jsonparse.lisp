;;;; Riccardo Ghilotti 879259

;;;; jsonparse
(defun jsonparse (jsonstring)
  (cond ((and
	  (eql (char jsonstring 0) #\{)
	  (eql (char jsonstring (- (length jsonstring) 1)) #\}))
	 (cons 'JSONOBJ
	       (jsonparseM
		(subseq jsonstring 1 (- (length jsonstring) 1)))))
	((and
	  (eql (char jsonstring 0) #\[)
	  (eql (char jsonstring (- (length jsonstring) 1)) #\]))
	 (cons 'JSONARRAY
	       (jsonparseE
		(subseq jsonstring 1 (- (length jsonstring) 1)))))
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\Tab)
	     (eql (char jsonstring 0) #\LineFeed))
	 (jsonparse (spaceSkip (subseq jsonstring 1))))
	((or (eql (char jsonstring (- (length jsonstring) 1)) #\Space)
	     (eql (char jsonstring (- (length jsonstring) 1)) #\Tab)
	     (eql (char jsonstring (- (length jsonstring) 1)) #\LineFeed))
	 (jsonparse (spaceSkipEnd (subseq jsonstring
					  0
					  (- (length jsonstring) 1)))))
	(t (error "Errore di sintassi"))))

;;; spaceSkip salta tutti gli spazi e ritorna il resto
;;; della stringa quando trova un altro carattere
(defun spaceSkip (jsonstring)
  (cond ((equal jsonstring "") "")
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\LineFeed)
	     (eql (char jsonstring 0) #\Tab))
	 (spaceSkip (subseq jsonstring 1)))
	(t jsonstring)))

;;; spaceSkipEnd fa la stessa cosa ma al contrario,
;;; quindi si occupa dei caratteri alla fine della stringa
(defun spaceSkipEnd (jsonstring)
  (cond ((equal jsonstring "") "")
	((or (eql (char jsonstring (- (length jsonstring) 1)) #\Space)
	     (eql (char jsonstring (- (length jsonstring) 1)) #\Tab)
	     (eql (char jsonstring (- (length jsonstring) 1)) #\Linefeed))
	 (spaceSkipEnd (subseq jsonstring 0 (- (length jsonstring) 1))))
	(t jsonstring)))

;;;; jsonparseM è la funzione richiamata in caso
;;;; la stringa rappresenti un oggetto
(defun jsonparseM (jsonstring)
  (cond ((equal jsonstring "") nil)
	((eql (char jsonstring 0) #\")
	 (append
	  (list
	   (checkAttribute (subseq jsonstring 1) ""))
	  (skipToColon (subseq jsonstring 1))))
	((or
	  (eql (char jsonstring 0) #\Space)
	  (eql (char jsonstring 0) #\Tab)
	  (eql (char jsonstring 0) #\LineFeed))
	 (jsonparseM (spaceSkip (subseq jsonstring 1))))
	(t (error "Errore di sintassi"))))

;;;; jsonparseE invece viene richiamata in caso
;;;; la stringa rappresenti un array
(defun jsonparseE (jsonstring)
  (cond ((equal jsonstring "") nil)
	((or
	  (eql (char jsonstring 0) #\Space)
	  (eql (char jsonstring 0) #\Tab)
	  (eql (char jsonstring 0) #\LineFeed))
	 (jsonparseE (spaceSkip (subseq jsonstring 1))))
	(t
	 (append
	  (list (checkValue jsonstring))
	  (skipToValueE jsonstring)))))


;;;; checkAttribute controlla il valore di attribute
;;;; e poi passa al controllo su Value
(defun checkAttribute (jsonstring acc)
  (cond ((eql (char jsonstring 0) #\")
	 (list acc (findValue (subseq jsonstring 1))))
	(t
	 (checkAttribute (subseq jsonstring 1)
			 (string-append acc
					(char jsonstring 0))))))


;;;; findValue controlla che ci siano i ':'
;;;; e poi passa al controllo su Value
(defun findValue (jsonstring)
  (cond ((eql (char jsonstring 0) #\:)
	 (checkValue (subseq jsonstring 1)))
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\Tab)
	     (eql (char jsonstring 0) #\Linefeed))
	 (findValue (spaceSkip (subseq jsonstring 1))))
	(t
	 (error "Errore di sintassi"))))

;;;; checkValue comprende di che tipo è value
;;;; e poi passa al controllo
(defun checkValue (jsonstring)
  (cond((eql (char jsonstring 0) #\")   ;controllo stringhe
	(valueString (subseq jsonstring 1) ""))
       ((eql (char jsonstring 0) #\-)   ; controllo numeri
	(valueNumber (subseq jsonstring 1) "-"))
       ((numberp (digit-char-p (char jsonstring 0)))
	(valueNumber jsonstring ""))
       ((eql (char jsonstring 0) #\{)   ; controllo Oggetti
	(jsonparse (trovoObj jsonstring "" 0)))
       ((eql (char jsonstring 0) #\[)   ; controllo Array
	(jsonparse (trovoArray jsonstring "" 0)))
       ((or (eql (char jsonstring 0) #\t)   ;controllo Booleani
	    (eql (char jsonstring 0) #\f))
	(checkBoolean jsonstring))
       ((eql (char jsonstring 0) #\n)   ;controllo null
	(checkNull jsonstring))
       ((or (eql (char jsonstring 0) #\Space)  ;salto spazi
	    (eql (char jsonstring 0) #\Tab)
	    (eql (char jsonstring 0) #\LineFeed))
	(checkValue (spaceSkip (subseq jsonstring 1))))
       (t (error "Errore di sintassi"))))

;;;; valueString controlla se la stringa è corretta
(defun valueString (jsonstring acc)
  (cond ((equal jsonstring "")
	 (error "Errore di sintassi"))
	((eql (char jsonstring 0) #\") acc)
	(t
	 (valueString (subseq jsonstring 1)
		      (string-append acc (char jsonstring 0))))))

;;;; valueNumber controlla se il numero è corretto
(defun valueNumber (jsonstring acc)
  (cond ((equal jsonstring "") (parse-integer acc))
	((numberp (digit-char-p (char jsonstring 0)))
	 (valueNumber (subseq jsonstring 1)
		      (string-append acc (char jsonstring 0))))
	((or (eql (char jsonstring 0) #\e)
	     (eql (char jsonstring 0) #\E))
	 (valueNumberFloat jsonstring acc))
	((eql (char jsonstring 0) #\.)
	 (valueNumberFloat jsonstring acc))
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\Linefeed)
	     (eql (char jsonstring 0) #\Tab)
	     (eql (char jsonstring 0) #\,)
	     (eql (char jsonstring 0) #\})
	     (eql (char jsonstring 0) #\]))
	 (parse-integer acc))
	(t (error "Errore di sintassi nel numero"))))


;;;; valueNumberFloat viene chiamato in caso il numero sia
;;;; un floating point
(defun valueNumberFloat (jsonstring acc)
  (cond ((equal jsonstring "") (parse-float acc))
	((numberp (digit-char-p (char jsonstring 0)))
	 (valueNumberFloat (subseq jsonstring 1)
			   (string-append acc
					  (char jsonstring 0))))
	((and (or (eql (char jsonstring 0) #\e)
		  (eql (char jsonstring 0) #\E))
	      (eql (char jsonstring 1) #\+))
	 (valueNumberFloat (subseq jsonstring 2)
			   (string-append acc
					  (char jsonstring 0)
					  (char jsonstring 1))))
	((and (or (eql (char jsonstring 0) #\e)
		  (eql (char jsonstring 0) #\E))
	      (eql (char jsonstring 1) #\-))
	 (valueNumberFloat (subseq jsonstring 2)
			   (string-append acc
					  (char jsonstring 0)
					  (char jsonstring 1))))
	((or (eql (char jsonstring 0) #\e)
	     (eql (char jsonstring 0) #\E))
	 (valueNumberFloat (subseq jsonstring 1)
			   (string-append acc
					  (char jsonstring 0))))
	((eql (char jsonstring 0) #\.)
	 (valueNumberFloat (subseq jsonstring 1)
			   (string-append acc
					  (char jsonstring 0))))
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\Linefeed)
	     (eql (char jsonstring 0) #\Tab)
	     (eql (char jsonstring 0) #\,)
	     (eql (char jsonstring 0) #\})
	     (eql (char jsonstring 0) #\]))
	 (parse-float acc))
	(t (error "Errore di sintassi nel numero"))))


;;;; checkBoolean controlla i valori booleani
(defun checkBoolean (jsonstring)
  (cond ((and (>= (length jsonstring) 4)
	      (equal (subseq jsonstring 0 4) "true"))
	 'true)
	((and (>= (length jsonstring) 5)
	      (equal (subseq jsonstring 0 5) "false"))
	 'false)
	(t (error "Errore valore booleano scorretto"))))


;;;; checkNull controlla null
(defun checkNull (jsonstring)
  (cond ((and (>= (length jsonstring) 4)
	      (equal (subseq jsonstring 0 4) "null"))
	 'null)
	(t (error "Errore valore null scorretto"))))


;;;; trovoObj prende l'oggetto che poi dovrà essere
;;;; analizzato
(defun trovoObj (jsonstring acc numGraffe)
  (cond ((null (position #\} jsonstring))
	 (error "L'oggetto inserito non si chiude"))
	((and (position #\} jsonstring) (position #\{ jsonstring))
	 (if (< (position #\} jsonstring) (position #\{ jsonstring))
	     (if (= numGraffe 1)
		 (string-append acc
				(subseq jsonstring
					0
					(+ (position #\}
						     jsonstring)
					   1)))
		 (trovoObj (subseq jsonstring
				   (+ (position #\} jsonstring) 1))
			   (string-append acc
					  (subseq jsonstring
						  0
						  (+ (position #\}
							       jsonstring)
						     1)))
			   (- numGraffe 1)))
	     (trovoObj (subseq
			jsonstring
			(+ (position #\{ jsonstring) 1))
		       (string-append acc
				      (subseq jsonstring
					      0 (+ (position #\{
							     jsonstring)
						   1)))
		       (+ numGraffe 1))))
	((and (position #\} jsonstring)
	      (= numGraffe 1))
	 (string-append acc
			(subseq jsonstring
				0
				(+ (position #\} jsonstring) 1))))
	((position #\} jsonstring)
	 (trovoObj (subseq jsonstring
			   (+ (position #\} jsonstring) 1))
		   (string-append acc
				  (subseq jsonstring 0
					  (+ (position #\}
						       jsonstring)
					     1)))
		   (- numGraffe 1)))))


;;;; trovoArray fa la stessa cosa di trovoObj ma con gli array
(defun trovoArray (jsonstring acc numQuadre)
  (cond ((null (position #\] jsonstring))
	 (error "L'array inserito non si chiude"))
	((and (position #\] jsonstring) (position #\[ jsonstring))
	 (if (< (position #\] jsonstring) (position #\[ jsonstring))
	     (if (= numQuadre 1)
		 (string-append acc
				(subseq jsonstring
					0
					(+ (position #\] jsonstring) 1)))
		 (trovoArray (subseq jsonstring
				     (+ (position #\] jsonstring) 1))
			     (string-append acc
					    (subseq jsonstring 0
						    (+ (position #\] jsonstring)
						       1)))
			     (- numQuadre 1)))
	     (trovoArray (subseq jsonstring
				 (+ (position #\[ jsonstring) 1))
			 (string-append acc
					(subseq jsonstring 0
						(+ (position #\[ jsonstring)
						   1)))
			 (+ numQuadre 1))))
	((and (position #\] jsonstring) (= numQuadre 1))
	 (string-append acc (subseq jsonstring 0
				    (+ (position #\] jsonstring)
				       1))))
	((position #\] jsonstring)
	 (trovoArray (subseq jsonstring
			     (+ (position #\] jsonstring) 1))
		     (string-append acc
				    (subseq jsonstring 0
					    (+ (position #\] jsonstring) 1)))
		     (- numQuadre 1)))))

;;;; esisteAttr controlla se dopo il carattere ','
;;;; è presente un altro pair
(defun esisteAttr (jsonstring)
  (cond ((equal jsonstring "") nil)
	((eql (char jsonstring 0) #\") t)
	(t (esisteAttr (subseq jsonstring 1)))))

;;;; esisteValue controlla se dopo il carattere ','
;;;; è presente un altro valore
(defun esisteValue (jsonstring)
  (cond ((equal jsonstring "")
	 nil)
	((or (eql (char jsonstring 0) #\")
	     (eql (char jsonstring 0) #\{)
	     (eql (char jsonstring 0) #\[)
	     (digit-char-p (char jsonstring 0))
	     (eql (char jsonstring 0) #\t)
	     (eql (char jsonstring 0) #\n)
	     (eql (char jsonstring 0) #\f))
	 t)
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\Tab)
	     (eql (char jsonstring 0) #\Linefeed))
	 (esisteValue (spaceSkip (subseq jsonstring 1))))
	(t nil)))

;;;; skipToColon salta tutti i caratteri appartenenti
;;;; all' attributo
(defun skipToColon (jsonstring)
  (cond ((equal jsonstring "")
	 (error "Errore di sintassi"))
	((eql (char jsonstring 0) #\")
	 (skipColon (subseq jsonstring 1)))
	(t (skipToColon (subseq jsonstring 1)))))

;;;; skipColon salta tutti i caratteri fino ai ':'
(defun skipColon (jsonstring)
  (cond ((equal jsonstring "")
	 (error "Errore di sintassi"))
	((eql (char jsonstring 0) #\:)
	 (skipToValueM (subseq jsonstring 1)))
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\LineFeed)
	     (eql (char jsonstring 0) #\Tab))
	 (skipColon (spaceSkip (subseq jsonstring 1))))
	(t (error "Errore di sintassi"))))


;;;; skipToValueM salta i caratteri fino al valore
;;;; e ne riconosce il tipo
(defun skipToValueM (jsonstring)
  (cond ((equal jsonstring "")
	 (error "Errore di sintassi"))
	((eql (char jsonstring 0) #\")
	 (skipToNextInApiciM (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\{)
	 (skipToNextInObjM (subseq jsonstring 1) 0))
	((eql (char jsonstring 0) #\[)
	 (skipToNextInArrayM (subseq jsonstring 1) 0))
	((numberp (digit-char-p (char jsonstring 0)))
	 (skipToNextNumberM (subseq jsonstring 1)))
	((and (>= (length jsonstring) 4)
	      (equal (subseq jsonstring 0 4) "true"))
	 (skipToNextM (subseq jsonstring 4)))
	((and (>= (length jsonstring) 4)
	      (equal (subseq jsonstring 0 4) "null"))
	 (skipToNextM (subseq jsonstring 4)))
	((and (>= (length jsonstring) 5)
	      (equal (subseq jsonstring 0 5) "false"))
	 (skipToNextM (subseq jsonstring 5)))
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\Linefeed)
	     (eql (char jsonstring 0) #\Tab))
	 (skipToValueM (spaceSkip (subseq jsonstring 1))))
	))

;;;; skipToNextNumberM salta i numeri
(defun skipToNextNumberM (jsonstring)
  (cond ((equal jsonstring "")
	 (skipToNextM jsonstring))
	((numberp (digit-char-p (char jsonstring 0)))
	 (skipToNextNumberM (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\E)
	 (skipToNextNumberM (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\e)
	 (skipToNextNumberM (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\+)
	 (skipToNextNumberM (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\-)
	 (skipToNextNumberM (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\.)
	 (skipToNextNumberM (subseq jsonstring 1)))
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\LineFeed)
	     (eql (char jsonstring 0) #\,))
	 (skipToNextM jsonstring))
	(t (error "Errore di sintassi"))))

;;;; skipToNextInApiciM salta tutte le i caratteri tra
;;;; doppi apici
(defun skipToNextInApiciM (jsonstring)
  (cond ((eql (char jsonstring 0) #\")
	 (skipToNextM (subseq jsonstring 1)))
	(t (skipToNextInApiciM (subseq jsonstring 1)))))


;;;; skipToNextInObjM salta tutti i caratteri presenti
;;;; negli oggetti
(defun skipToNextInObjM (jsonstring numGraffe)
  (cond ((null (position #\} jsonstring))
	 (error "L'oggetto inserito non si chiude"))
	((and (position #\} jsonstring) (position #\{ jsonstring))
	 (if (< (position #\} jsonstring) (position #\{ jsonstring))
	     (if (= numGraffe 0)
		 (skipToNextM (subseq jsonstring
				      (+ (position #\} jsonstring)
					 1)))
		 (skipToNextInObjM (subseq jsonstring
					   (+ (position #\} jsonstring) 1))
				   (- numGraffe 1)))
	     (skipToNextInObjM (subseq jsonstring
				       (+ (position #\{ jsonstring) 1))
			       (+ numGraffe 1))))
	((and (position #\] jsonstring) (= numGraffe 0))
	 (skipToNextM (subseq jsonstring
			      (+ (position #\} jsonstring) 1))))
	((position #\] jsonstring)
	 (skipToNextInObjM (subseq jsonstring
				   (+ (position #\} jsonstring) 1))
			   (- numGraffe 1)))))
		 

;;;; skipToNextInArrayM salta tutti i caratteri presenti
;;;; negli array
(defun skipToNextInArrayM (jsonstring numQuadre)
  (cond ((null (position #\] jsonstring))
	 (error "L'array inserito non si chiude"))
	((and (position #\] jsonstring) (position #\[ jsonstring))
	 (if (< (position #\] jsonstring) (position #\[ jsonstring))
	     (if (= numQuadre 0)
		 (skipToNextM (subseq jsonstring
				      (+ (position #\] jsonstring)
					 1)))
		 (skipToNextInArrayM (subseq jsonstring
					     (+ (position #\] jsonstring)
						1))
				     (- numQuadre 1)))
	 (skipToNextInArrayM (subseq jsonstring
				     (+ (position #\[ jsonstring)
					1))
			     (+ numQuadre 1))))
	 ((and (position #\] jsonstring) (= numQuadre 0))
	  (skipToNextM (subseq jsonstring
			       (+ (position #\] jsonstring)
				  1))))
	 ((position #\] jsonstring)
	  (skipToNextInArrayM (subseq jsonstring
				      (+ (position #\] jsonstring)
					 1))
			      (- numQuadre 1)))))

;;;; skipToNextM controlla se c'è un altro pair
;;;; oppure se finisce l'oggetto
(defun skipToNextM (jsonstring)
  (cond ((equal jsonstring "")
	 (jsonparseM ""))
	((and (eql (char jsonstring 0) #\,)
	      (esisteAttr jsonstring))
	 (jsonparseM (subseq jsonstring 1)))
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\Linefeed))
	 (skipToNextM (spaceSkip (subseq jsonstring 1))))
	(t (error "Errore di sintassi"))))

;;;; skipToValueE salta tutti i caratteri fino a Value
(defun skipToValueE (jsonstring)
  (cond ((equal jsonstring "")
	 (error "Errore di sintassi"))
	((eql (char jsonstring 0) #\")
	 (skipToNextInApiciE (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\[)
	 (skipToNextInArrayE (subseq jsonstring 1) 0))
	((eql (char jsonstring 0) #\{)
	 (skipToNextInObjE (subseq jsonstring 1) 0))
	((numberp (digit-char-p (char jsonstring 0)))
	 (skipToNextNumberE (subseq jsonstring 1)))
	((and (>= (length jsonstring) 4)
	      (equal (subseq jsonstring 0 4) "true"))
	 (skipToNextE (subseq jsonstring 4)))
	((and (>= (length jsonstring) 4)
	      (equal (subseq jsonstring 0 4) "null"))
	 (skipToNextE (subseq jsonstring 4)))
	((and (>= (length jsonstring) 5)
	      (equal (subseq jsonstring 5) "false"))
	 (skipToNextE (subseq jsonstring 5)))
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\Linefeed)
	     (eql (char jsonstring 0) #\Tab))
	 (skipToValueE (spaceSkip (subseq jsonstring 1))))))


;;;; skipToNextNumberE salta tutti i caratteri
;;;; appartenenti al numero
(defun skipToNextNumberE (jsonstring)
  (cond ((equal jsonstring "")
	 (skipToNextE jsonstring))
	((numberp (digit-char-p (char jsonstring 0)))
	 (skipToNextNumberE (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\e)
	 (skipToNextNumberE (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\E)
	 (skipToNextNumberE (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\+)
	 (skipToNextNumberE (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\-)
	 (skipToNextNumberE (subseq jsonstring 1)))
	((eql (char jsonstring 0) #\.)
	 (skipToNextNumberE (subseq jsonstring 1)))
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\Linefeed)
	     (eql (char jsonstring 0) #\,))
	 (skipToNextE jsonstring))
	(t (error "Errore di sintassi"))))

;;;; skipToNextE controlla se c'è una virgola o se l'array
;;;; è terminato
(defun skipToNextE (jsonstring)
  (cond ((equal jsonstring "")
	 (jsonparseE ""))
	((and (eql (char jsonstring 0) #\,)
	      (esisteValue (subseq jsonstring 1)))
	 (jsonparseE (subseq jsonstring 1)))
	((or (eql (char jsonstring 0) #\Space)
	     (eql (char jsonstring 0) #\Linefeed)
	     (eql (char jsonstring 0) #\Tab))
	 (skipToNextE (spaceSkip (subseq jsonstring 1))))
	(t (error "Errore di sintassi"))))

;;;; skipToNextInApiciE salta tutti i caratteri presenti
;;;; tra doppi apici
(defun skipToNextInApiciE (jsonstring)
  (cond ((eql (char jsonstring 0) #\")
	 (skipToNextE (subseq jsonstring 1)))
	(t (skipToNextInApiciE (subseq jsonstring 1)))))

;;;; skipToNextInObjE salta tutti i caratteri presenti
;;;; nel prossimo oggetto
(defun skipToNextInObjE (jsonstring numGraffe)
  (cond ((null (position #\} jsonstring))
	 (error "L'oggetto inserito non si chiude"))
	((and (position #\} jsonstring) (position #\{ jsonstring))
	 (if (< (position #\} jsonstring) (position #\{ jsonstring))
	     (if (= numGraffe 0)
		 (skipToNextE (subseq jsonstring
				      (+ (position #\} jsonstring)
					 1)))
		 (skipToNextInObjE (subseq jsonstring
					   (+ (position #\} jsonstring)
					      1))
				   (- numGraffe 1)))
	     (skipToNextInObjE (subseq jsonstring
				       (+ (position #\{ jsonstring)
					  1))
			       (+ numGraffe 1))))
	((and (position #\} jsonstring) (= numGraffe 0))
	 (skipToNextE (subseq jsonstring
			      (+ (position #\} jsonstring)
				 1))))
	((position #\} jsonstring)
	 (skipToNextInObjE (subseq jsonstring
				   (+ (position #\} jsonstring)
				      1))
			   (- numGraffe 1)))))

;;;; skipToNextInArrayE salta tutti i caratteri presenti
;;;; nel prossimo array
(defun skipToNextInArrayE (jsonstring numQuadre)
  (cond ((null (position #\] jsonstring))
	 (error "L'array inserito non si chiude"))
	((and (position #\] jsonstring) (position #\[ jsonstring))
	 (if (< (position #\] jsonstring) (position #\[ jsonstring))
	     (if (= numQuadre 0)
		 (skipToNextE (subseq jsonstring
				      (+ (position #\] jsonstring)
					 1)))
		 (skipToNextInArrayE (subseq jsonstring
					     (+ (position #\] jsonstring)
						1))
				     (- numQuadre 1)))
	     (skipToNextInArrayE (subseq jsonstring
					 (+ (position #\[ jsonstring)
					    1))
				 (+ numQuadre 1))))
	((and (position #\] jsonstring) (= numQuadre 0))
	 (skipToNextE (subseq jsonstring
			      (+ (position #\] jsonstring)
				 1))))
	((position #\] jsonstring)
	 (skipToNextInArrayE (subseq jsonstring
				     (+ (position #\] jsonstring)
					1))
			     (- numQuadre 1)))))


;;;; jsonaccess
(defun jsonaccess (jsobj &rest fields)
  (let ((Ffields (flatten fields)))
    (cond  ((null jsobj)
	    (error "L'oggetto non contiene il campo indicato"))
	   ((null (car Ffields))
	    (cond ((eql (length jsobj) 1)
		   (car jsobj))
		  (t jsobj)))
	   ((eql (first jsobj) 'JSONOBJ)
	    (jsonaccess (cdr jsobj) Ffields))
	   ((eql (first jsobj) 'JSONARRAY)
	    (jsonaccessInArray (cdr jsobj) Ffields))
	   ((eql (first (car jsobj)) 'JSONOBJ)
	    (jsonaccess (cdr (car jsobj)) Ffields))
	   ((eql (car (car jsobj)) 'JSONARRAY)
	    (jsonaccessInArray (cdr (car jsobj)) Ffields))
	   ((equal (car (first jsobj)) (first Ffields))
	    (jsonaccess (cdr (car jsobj)) (rest FFields)))
	   (t (jsonaccess (cdr jsobj) Ffields)))))


;;;; jsonaccessInArray si occupa dei casi in cui
;;;; è richiesta la ricerca negli array
(defun jsonaccessInArray (jsobj &rest fields)
  (let ((Ffields (flatten fields)))
    (cond ((null jsobj)
	   (error "L'oggetto non contiene il campo indiciato"))
	  ((not (numberp (first Ffields)))
	   (error "il campo indicato non è un numero"))
	  ((and (= (first Ffields) 0) (null (rest Ffields)))
	   (first jsobj))
	  ((= (first Ffields) 0)
	   (jsonaccess jsobj (rest Ffields)))
	  ((> (first Ffields) 0)
	   (jsonaccessInArray (rest jsobj)
			      (append
			       (list (- (first Ffields) 1))
			       (rest Ffields)))))))


;;;; flatten è utilizzata come funzione di supporto
;;;; per diverse funzioni
(defun flatten (l)
  (cond ((null l) nil)
	((atom (first l)) (cons (first l) (flatten (rest l))))
	((listp (first l))
	 (append (flatten (first l)) (flatten (rest l))))))


;;;; jsonread
(defun jsonread (file)
  (with-open-file (in file
		      :direction :input
		      :if-does-not-exist :error)
    (jsonreadInFile in "" )))


;;;; jsonreadInFile è richiamata quando jsonread
;;;; è riuscita ad aprire il file da analizzare
(defun jsonreadInFile (in str)
  (let ((e (read-char in nil 'eof)))
    (cond ((eql e 'eof)
	   (jsonparse str))
	  (t
	   (jsonreadInFile in
			   (string-append str e))))))


;;;; jsondump
(defun jsondump (JSON file)
  (with-open-file (out file
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (jsondumpinFile JSON out 0))
  file)


;;;; jsondumpinFile inizia a scrivere dopo
;;;; che jsondump è riuscito ad aprire/creare il file
(defun jsondumpinFile (JSON out tabs)
  (cond ((eql (car JSON) 'jsonobj)
	 (printTabs tabs out)
	 (format out "{~%")
	 (printPair (cdr JSON) out (+ tabs 1))
	 (format out "}"))
	((eql (car JSON) 'jsonarray)
	 (printTabs (+ tabs 1) out)
	 (format out "[~%")
	 (printElements (cdr JSON) out (+ tabs 1))
	 (format out "]"))))


;;;; printPair stampa le coppie Attributo Valore
(defun printPair (Members out tabs)
  (cond ((null Members) t)
	(t
	 (printTabs tabs out)
	 (format out "~S :" (car (car Members)))
	 (printValue (car (cdr (car Members))) out tabs)
	 (if (not (null (cdr Members)))
	     (format out ",~%"))
	 (printPair (cdr Members) out tabs))))


;;;; printElements stampa il contenuto degli array
(defun printElements (Elements out tabs)
  (cond ((null Elements) t)
	(t
	 (printTabs tabs out)
	 (printValue (car Elements) out tabs)
	 (if (not (null (cdr Elements)))
	     (format out ",~%"))
	 (printElements (cdr Elements) out tabs))))


;;;; printValue stampa value in base al tipo
(defun printValue (Value out tabs)
  (cond ((eql Value 'true)
	 (format out " true"))
	((eql Value 'false)
	 (format out " false"))
	((eql Value 'null)
	 (format out " null"))
	((numberp Value)
	 (format out " ~A" Value))
	((stringp Value)
	 (format out " ~S" Value))
	((or (eql (car Value) 'jsonobj)
	     (eql (car Value) 'jsonarray))
	 (format out "~%")
	 (jsondumpInFile Value out tabs))))


;;;; printTabs stampa degli spazi bianchi per leggibilità
(defun printTabs (tabs out)
  (cond ((= tabs 0) )
	(t (format out "       ")
	   (printTabs (- tabs 1) out))))
