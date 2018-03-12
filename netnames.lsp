#!/usr/local/bin/newlisp

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
;; For more information, please refer to <http://unlicense.org>


(module "getopts.lsp")

(constant '*whois* "/usr/bin/whois") ; Path to whois command

;- - - Test if a string is a valid IPv4
(define (ip? ip-string-to-check)
	(regex "(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)" ip-string-to-check)	
)


;; Main begins here - - - - - - - - - - - - - - - - - - - - - - - - - - -

(shortopt "h" (getopts:usage) nil "Print this help message")
(shortopt "o" (setf output-file getopts:arg) "filename" "Output file")
(shortopt "i" (setf input-file getopts:arg) "filename" "Input file")
(longopt "help" (getopts:usage) nil "Print this help message")

(getopts (2 (main-args)))

;- - - Check if input & output files are provided.
(if (nil? input-file) (getopts:usage))
(if (nil? output-file) (getopts:usage))

;- - - Check for existence of input file.
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
					(if (find "netname:" (lower-case ln))
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
