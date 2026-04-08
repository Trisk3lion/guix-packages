(define-module (trisk services base)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:export (greetd-tuigreet-session))

(define-record-type* <greetd-tuigreet-session>
  greetd-tuigreet-session make-greetd-tuigreet-session
  greetd-tuigreet-session?
  this-greetd-tuigreet-session
  (tuigreet greetd-tuigreet-session-tuigreet
            (default tuigreet))
  (command-args greetd-tuigreet-session-args
                (default '("--issue"
                           "--time"
                           "--user-menu"
                           "--asterisks"
                           "--remember"
                           "--remember-session"
                           "--power-shutdown" "loginctl poweroff"
                           "--power-reboot" "loginctl reboot"))))

(define-gexp-compiler (greetd-tuigreet-session-compiler
                       (session <greetd-tuigreet-session>)
                       system target)
  (match-record session <greetd-tuigreet-session> (tuigreet command-args)
    (let ((tuigreet (file-append tuigreet "/bin/tuigreet")))
      (lower-object
       (program-file "tuigreet-wrapper"
         #~(execl #$tuigreet #$tuigreet #$@command-args))))))
