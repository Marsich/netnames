#!/usr/local/bin/newlisp

(module "getopts.lsp")

(constant '*whois* "/usr/bin/whois")

(define (ip? ip-string-to-check)
	(regex "(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)" ip-string-to-check)	
)



;; Main begins here - - - - - - - - - - - - - - - - - - - - - - - - - - -

(shortopt "h" (getopts:usage) nil "Print this help message")
(shortopt "o" (setf output-file getopts:arg) "filename" "Output file")
(shortopt "i" (setf input-file getopts:arg) "filename" "Input file")
(longopt "help" (getopts:usage) nil "Print this help message")

(getopts (2 (main-args)))

(if (nil? input-file) (getopts:usage))
(if (nil? output-file) (getopts:usage))

;- - - Check file existence
(if (not (file? input-file))
	(begin
		(println "Input file [" input-file "] not found!")
		(exit)
	)
)


(setf output-content "")

(setf file-content (read-file input-file))
(dolist (IP (parse file-content "\n" 0))	
		(setf myip (ip? IP))
		(if myip
			(begin
				(setf myip (nth 0 $it))				
				(setf whois-data (exec (string *whois* " " myip)))
				(dolist (ln whois-data)
					(if (find "netname:" ln)
						(begin
							(setf ip+netname (string myip ";" (trim (slice ln 8))))
							(println ip+netname)
							(setf output-content (string $it "\n" ip+netname))
						)
					)
				)	
			)
		)
)
(write-file output-file output-content)
(println "\n- - - DONE - - -")

(exit)