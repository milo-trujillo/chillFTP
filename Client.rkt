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
    (with-handlers ([exn:fail:network?
		     (lambda (exn)
		       (printf "Couldn't connect to ~a :: ~a\n" ip exn)
		       (error "Sorry :("))])
      (define-values (control-in control-out) (tcp-connect ip 21))
      (let loop ()
	(for ([incoming (in-lines control-in)])
	  (control-execute (get-control incoming)
			   control-in
			   control-out)
	  (display incoming)
	  (newline))
	(send-message (read-line) control-out)
	(loop))
      (close-connection control-in control-out)))

  (provide connect get-control))
