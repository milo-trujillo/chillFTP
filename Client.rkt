(module client racket/base
  (require racket/tcp)

  ;close two ports, lazy
  (define (close-connection in out)
    (close-input-port in)
    (close-output-port out))

  ;send data and flush
  (define (send-message data port)
    (display data port)
    (flush-output port))

  ;get the 3-digit message that begins certain ftp responses
  ;return an integer message, or #f
  (define (get-control str)
    (string->number (substring str 0 3)))

  (define (control-execute control-num in-state out-state)
    (case control-num
      [(110) "foo"]))

  ;connect to a server, and perform ftp stuffs
  (define (connect ip)
    ;throw an exception if we can't connect to the server
    (with-handlers ([exn:fail:network?
		     (lambda (exn)
		       (error "Couldn't connect to" ip exn))])
      ;bind the input and output streams to variables
      (define-values (control-in control-out) (tcp-connect ip 21))
      (let loop ()
	;print all the information coming from the server
	(for ([incoming (in-lines control-in)])
	  ;and execute control seqs, they do nothing now
	  (control-execute (get-control incoming)
			   control-in
			   control-out)
	  (display incoming)
	  (newline))
	(let ((message (read-line)))
	  ;send user input to the server, currently blocks..
	  (send-message message control-out)
	  (unless (equal? (string-upcase message) "QUIT")
	    (loop))))
      (close-connection control-in control-out)))

  (provide connect get-control))
