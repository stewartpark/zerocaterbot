#lang racket

(require net/http-client)
(require net/url)
(require net/uri-codec)
(require racket/format)
(require racket/date)
(require json)


(define SLACK_WEBHOOK_URL (getenv "SLACK_WEBHOOK_URL"))
(define ZEROCATER_ID (getenv      "ZEROCATER_ID"))

(define (format-date date)
  (string-join (list
    (number->string (date-year date))
    (~a (number->string (date-month date)) #:width 2 #:pad-string "0" #:align 'right)
    (~a (number->string (- (date-day date) 1)) #:width 2 #:pad-string "0" #:align 'right)
  ) "")
)

(display (format-date (current-date)))

(define (get-lunch-description)
    (let-values [(
      (errcode header port)
      (http-sendrecv/url (string->url (string-join (list "https://zerocater.com/calendar?id=" ZEROCATER_ID) "")))
      )]

      (let (
            [resp_body (port->string port)]
            [desc_re (regexp
              (string-join
                (list
                  "VALUE=DATE-TIME:"
                  (format-date (current-date))
                  "T120000.+?DESCRIPTION:(.+?)(END|URL)"
                )
                ""
            ))]
        )
        (car (cdr (regexp-match desc_re resp_body)))
      )
    )
)

(define (post-to-slack msg)
    (let (
            [payload (string-join (list "{\"text\": \"" (string-replace (string-replace msg "\n" "") "\r" "") "\"}") "")]
        )
        (display payload)
        (let-values [((errcode header port)
            (http-sendrecv/url
                (string->url SLACK_WEBHOOK_URL)
                #:method "POST"
                #:headers (list "Content-Type: application/x-www-form-urlencoded")
                #:data (alist->form-urlencoded
                  (list (cons 'payload payload))
                )
            ))]
            (display (port->string port))
        )
    )
)

(post-to-slack (get-lunch-description))
